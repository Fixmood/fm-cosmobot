"""FM 后台 v2 API：一次性补齐「能看能控」所需的数据接口。

设计：
- 已有的 /api/*（overview / logs / models / collections / rpc / domain 代理）继续复用；
- 这里只补缺的：总览聚合、群详情、拟人模式读写、成员反查、消息搜索、
  工具调用审计与统计、番茄小说书架。
- 全部只读为主，唯一写操作是拟人模式的 scopes / proactive 配置（文件格式保持
  bot 侧读取格式不变）。
"""
from __future__ import annotations

import calendar
import json
import os
import re
import secrets
import sqlite3
import threading
import time
from typing import Any, Callable

RUNTIME = "/runtime"
BOT_DB = os.path.join(RUNTIME, "cosmobot.sqlite3")
HUMANLIKE_DIR = os.path.join(RUNTIME, "humanlike")


# ── 基础读取 ──────────────────────────────────────────────
def _bot_db() -> sqlite3.Connection:
    return sqlite3.connect(f"file:{BOT_DB}?mode=ro", uri=True, timeout=5)


def _query(sql: str, params: tuple = ()) -> list[dict]:
    try:
        conn = _bot_db()
    except Exception:
        return []
    try:
        conn.row_factory = sqlite3.Row
        return [dict(row) for row in conn.execute(sql, params).fetchall()]
    except Exception:
        return []
    finally:
        conn.close()


def _read(path: str, limit: int = 200000) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return handle.read(limit)
    except OSError:
        return ""


def _write(path: str, text: str) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", encoding="utf-8") as handle:
        handle.write(text)


def _toml(path: str) -> dict:
    values: dict[str, Any] = {}
    section = ""
    for line in _read(path).splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if stripped.startswith("[") and stripped.endswith("]"):
            section = stripped[1:-1].strip()
            continue
        if "=" not in stripped:
            continue
        key, _, raw = stripped.partition("=")
        raw = raw.strip()
        if raw.startswith('"') and raw.endswith('"'):
            raw = raw[1:-1]
        values[f"{section}.{key.strip()}" if section else key.strip()] = raw
    return values


def _config() -> dict:
    raw = _toml(os.path.join(RUNTIME, "config.toml"))
    def pick(key: str) -> str:
        for prefix in ("handler.ask.", "agent.", ""):
            value = raw.get(prefix + key)
            if value:
                return value
        return ""

    base = {k: pick(k) for k in
            ("agent_max_turns", "recent_chat_context_max_chars",
             "recent_chat_context_limit", "recent_chat_context_minutes")}
    chat_models = {k: v for k, v in raw.items() if k.startswith("llm.chat_provider")}
    image_models = {k: v for k, v in raw.items() if k.startswith("llm.image_provider")}
    ask = {k: v for k, v in raw.items() if k.startswith("handler.ask")}
    return {"base": base, "chat_models": chat_models, "image_models": image_models, "ask": ask}


# ── 拟人模式 ──────────────────────────────────────────────
def humanlike_scopes() -> list[str]:
    raw = _read(os.path.join(HUMANLIKE_DIR, "proactive.txt"), 8192)
    scopes: list[str] = []
    for row in raw.splitlines():
        key = (row.split("\t")[0] if "\t" in row else row.split(" ")[0]).strip()
        if key and key not in scopes:
            scopes.append(key)
    return scopes


def humanlike_state() -> dict:
    scopes = humanlike_scopes()
    sticky = [line.strip() for line in _read(os.path.join(HUMANLIKE_DIR, "scopes.txt")).splitlines() if line.strip()]
    proactive_rows = []
    for row in _read(os.path.join(HUMANLIKE_DIR, "proactive.txt"), 8192).splitlines():
        parts = [p.strip() for p in row.split("\t")]
        if len(parts) >= 4:
            proactive_rows.append({"scope": parts[0], "date": parts[1], "limit": parts[2], "ts": parts[3]})
    files = []
    try:
        files = sorted(os.listdir(HUMANLIKE_DIR))
    except OSError:
        pass
    return {"scopes": scopes, "scope_count": len(scopes), "scope_file": sticky,
            "proactive": proactive_rows, "files": files}


def humanlike_set_scopes(scopes: list[str]) -> dict:
    clean = [s.strip() for s in scopes if isinstance(s, str) and s.strip()]
    _write(os.path.join(HUMANLIKE_DIR, "scopes.txt"), "\n".join(clean) + ("\n" if clean else ""))
    now = int(time.time())
    today = time.strftime("%Y-%m-%d")
    rows = []
    for scope in clean:
        rows.append(f"{scope}\t{today}\t20\t{now}")
    _write(os.path.join(HUMANLIKE_DIR, "proactive.txt"), "\n".join(rows) + ("\n" if rows else ""))
    return humanlike_state()


# ── 成员与消息 ────────────────────────────────────────────
def member_search(query: str, limit: int = 30000) -> dict:
    query = (query or "").strip()
    if not query:
        return {"matches": []}
    limit = max(1, min(60000, limit))
    rows = _query(
        "SELECT sender_id, sender_username, recorded_at FROM chat_log "
        "WHERE is_bot=0 AND sender_id IS NOT NULL "
        "AND (sender_username LIKE ? OR sender_id = ?) "
        "ORDER BY id DESC LIMIT ?",
        (f"%{query}%", query, limit),
    )
    ids: list[str] = []
    for row in rows:
        sid = row["sender_id"] or ""
        if sid and sid not in ids:
            ids.append(sid)
    matches = []
    for sid in ids[:8]:
        names = _query(
            "SELECT DISTINCT sender_username FROM chat_log "
            "WHERE sender_id=? AND sender_username IS NOT NULL ORDER BY sender_username",
            (sid,),
        )
        count = _query("SELECT COUNT(*) n FROM chat_log WHERE sender_id=?", (sid,))
        last = _query("SELECT MAX(recorded_at) at FROM chat_log WHERE sender_id=?", (sid,))
        matches.append({
            "user_id": sid,
            "aliases": [n["sender_username"] for n in names],
            "messages": int(count[0]["n"]) if count else 0,
            "last_seen": str((last[0]["at"] if last else "") or "")[:19],
        })
    return {"matches": matches}


def message_search(query: str, chat: str, days: int, limit: int) -> dict:
    limit = max(1, min(200, limit))
    sql = ("SELECT chat_id, sender_id, sender_username, is_bot, body_text, recorded_at, image_urls "
           "FROM chat_log WHERE 1=1")
    params: list[Any] = []
    if query:
        sql += " AND body_text LIKE ?"
        params.append(f"%{query}%")
    if chat:
        sql += " AND chat_id = ?"
        params.append(chat)
    if days > 0:
        sql += " AND recorded_at >= ?"
        params.append(time.time() - days * 86400)
    sql += " ORDER BY id DESC LIMIT ?"
    params.append(limit)
    rows = _query(sql, tuple(params))
    return {"rows": rows, "count": len(rows)}


# ── 工具审计 ──────────────────────────────────────────────
def _audit_events(kind: str, days: int, limit: int) -> list[dict]:
    sql = "SELECT event_json, occurred_at FROM audit_log WHERE event_kind=? AND occurred_at >= ? ORDER BY id DESC LIMIT ?"
    since = time.strftime("%Y-%m-%dT%H:%M:%S", time.gmtime(time.time() - max(1, days) * 86400))
    rows = _query(sql, (kind, since, max(1, min(2000, limit))))
    out = []
    for row in rows:
        try:
            payload = json.loads(row["event_json"])
        except Exception:
            continue
        out.append({"at": str(row["occurred_at"])[:19], "event": payload})
    return out


def tools_audit(days: int, tool: str, status: str, limit: int) -> dict:
    events = _audit_events("tool_call_finished", days, limit)
    rows = []
    for item in events:
        payload = item["event"] or {}
        name = payload.get("toolName") or (payload.get("toolCall") or {}).get("name") or ""
        state = payload.get("status") or ""
        if tool and tool not in name:
            continue
        if status and status != state:
            continue
        rows.append({"at": item["at"], "tool": name, "status": state,
                     "result": str(payload.get("result") or "")[:400]})
    return {"rows": rows, "count": len(rows)}


def tools_stats(days: int) -> dict:
    events = _audit_events("tool_call_finished", days, 5000)
    stats: dict[str, dict] = {}
    for item in events:
        payload = item["event"] or {}
        name = payload.get("toolName") or (payload.get("toolCall") or {}).get("name") or "unknown"
        state = payload.get("status") or "unknown"
        entry = stats.setdefault(name, {"tool": name, "calls": 0, "failures": 0, "last": ""})
        entry["calls"] += 1
        if state not in ("ok", "success"):
            entry["failures"] += 1
        entry["last"] = max(str(entry["last"] or ""), str(item["at"] or ""))
    rows = sorted(stats.values(), key=lambda e: -e["calls"])
    return {"rows": rows, "total_calls": sum(r["calls"] for r in rows)}


# ── 群 ───────────────────────────────────────────────────
def groups_overview(domain: Callable[[str], Any]) -> dict:
    raw = domain("/groups") or {}
    items = raw.get("groups") or raw.get("data") or raw
    if isinstance(items, dict):
        items = items.get("groups", [])
    scopes = set(humanlike_scopes())
    result = []
    for group in items if isinstance(items, list) else []:
        gid = str(group.get("group_id") or group.get("id") or "")
        result.append({
            "group_id": gid,
            "name": group.get("display_name") or group.get("name") or gid,
            "status": group.get("status") or "",
            "humanlike": f"qq:group:{gid}" in scopes,
            "features": group.get("features") or {},
        })
    counts = {str(row.get("chat_id")): row.get("n") for row in _query(
        "SELECT chat_id, COUNT(*) n FROM chat_log GROUP BY chat_id")}
    for row in result:
        row["messages"] = int(counts.get(str(row["group_id"])) or 0)
    result.sort(key=lambda r: -r["messages"])
    return {"groups": result}


def group_detail(group_id: str, domain: Callable[[str], Any]) -> dict:
    detail = {
        "group_id": group_id,
        "humanlike": f"qq:group:{group_id}" in set(humanlike_scopes()),
        "messages": _query("SELECT COUNT(*) n FROM chat_log WHERE chat_id=?", (group_id,))[0]["n"]
        if _query("SELECT COUNT(*) n FROM chat_log WHERE chat_id=?", (group_id,)) else 0,
        "recent": _query(
            "SELECT sender_username, body_text, is_bot, recorded_at FROM chat_log WHERE chat_id=? "
            "ORDER BY id DESC LIMIT 30", (group_id,)),
        "top_senders": _query(
            "SELECT sender_username, COUNT(*) n FROM chat_log WHERE chat_id=? AND is_bot=0 "
            "GROUP BY sender_id ORDER BY n DESC LIMIT 10", (group_id,)),
        "domain_state": domain(f"/group-state?group_id={group_id}") or {},
    }
    return detail



def _post_domain(path: str, payload: dict) -> dict:
    """domain 的 /novel/* 是 POST 接口，后台这边补一个 POST 客户端。"""
    import urllib.request
    base = os.environ.get("FM_DOMAIN_URL", "http://fm-domain:8077").rstrip("/")
    body = json.dumps(payload or {}).encode("utf-8")
    request = urllib.request.Request(base + path, data=body,
                                     headers={"Content-Type": "application/json"}, method="POST")
    try:
        with urllib.request.urlopen(request, timeout=15) as response:
            data = json.loads(response.read().decode("utf-8", "replace"))
        return data if isinstance(data, dict) else {"ok": False}
    except Exception as exc:
        return {"ok": False, "error": f"{exc}"}


# ── 番茄书架 ──────────────────────────────────────────────
def novels(domain: getattr) -> dict:
    result = _post_domain("/novel/stats", {})
    if isinstance(result, dict) and result.get("ok"):
        return result
    return {"ok": False, "error": result.get("error") if isinstance(result, dict) else "未知错误"}


def novel_detail(book_id: str, domain: Callable[[str], Any]) -> dict:
    return domain(f"/novel/book?book_id={book_id}") or {}


def overview(domain: Callable[[str], Any], rpc: Callable[[str, Any], Any]) -> dict:
    config = _config()
    chat_models = [v for k, v in config["chat_models"].items() if k.endswith(".model")]
    efforts = [v for k, v in config["chat_models"].items() if k.endswith(".reasoning_effort")]
    health = domain("/health") or {}
    stats = domain("/stats") or {}
    inner = stats.get("data") if isinstance(stats, dict) else None
    stats = inner if isinstance(inner, dict) else stats if isinstance(stats, dict) else {}
    counts = {}
    for row in _query("SELECT COUNT(*) n FROM chat_log"):
        counts["messages"] = row["n"]
    for row in _query("SELECT COUNT(*) n FROM chat_log WHERE is_bot=1"):
        counts["bot_messages"] = row["n"]
    for row in _query("SELECT COUNT(DISTINCT sender_id) n FROM chat_log WHERE is_bot=0"):
        counts["senders"] = row["n"]
    last = _query("SELECT MAX(recorded_at) at FROM chat_log")
    return {
        "config": config,
        "chat_model": chat_models[0] if chat_models else "",
        "reasoning_effort": efforts[0] if efforts else "",
        "health": health,
        "stats": stats,
        "chat": {**counts, "last_at": str((last[0]["at"] if last else "") or "")[:19]},
        "humanlike": humanlike_state(),
        "tools": tools_stats(7),
        "now": time.time(),
    }




# ── 数据浏览器：任意表 ────────────────────────────────────
BROWSE_TABLES = {
    "chat_log": ("聊天记录", ["chat_id", "sender_id", "sender_username", "is_bot", "body_text", "recorded_at"]),
    "audit_log": ("审计事件", ["event_kind", "occurred_at", "event_json"]),
    "sessions": ("会话", None),
    "session_messages": ("会话消息", None),
    "threads": ("话题线程", None),
    "media_files": ("媒体文件", None),
    "resources": ("资源", None),
    "scheduled_messages": ("定时消息", None),
    "lifecycle_actions": ("生命周期动作", None),
}


def browse_tables() -> dict:
    out = []
    for name, (label, cols) in BROWSE_TABLES.items():
        count = _query(f"SELECT COUNT(*) n FROM {name}")
        out.append({"table": name, "label": label, "rows": int(count[0]["n"]) if count else 0,
                    "columns": cols or []})
    return {"tables": out}


def browse_rows(table: str, q: str, limit: int, offset: int) -> dict:
    if table not in BROWSE_TABLES:
        return {"error": "不允许浏览该表"}
    limit = max(1, min(300, limit))
    offset = max(0, offset)
    sql = f"SELECT * FROM {table}"
    params: list[Any] = []
    if q:
        sql += " WHERE CAST((" + ("body_text" if table == "chat_log" else "event_json" if table == "audit_log" else "rowid") + ") AS TEXT) LIKE ?"
        params.append(f"%{q}%")
    sql += " LIMIT ? OFFSET ?"
    params += [limit, offset]
    rows = _query(sql, tuple(params))
    total = _query(f"SELECT COUNT(*) n FROM {table}")
    clean = []
    for row in rows:
        item = {}
        for key, value in row.items():
            if isinstance(value, bytes):
                item[key] = f"<{len(value)} bytes>"
            elif isinstance(value, str) and len(value) > 500:
                item[key] = value[:500] + "…"
            else:
                item[key] = value
        clean.append(item)
    return {"table": table, "rows": clean, "total": int(total[0]["n"]) if total else 0,
            "limit": limit, "offset": offset}


def all_members(limit: int = 500) -> dict:
    rows = _query(
        "SELECT sender_id, sender_username, COUNT(*) n, MAX(recorded_at) last_at "
        "FROM chat_log WHERE is_bot=0 AND sender_id IS NOT NULL "
        "GROUP BY sender_id ORDER BY n DESC LIMIT ?",
        (max(1, min(2000, limit)),),
    )
    seen: dict[str, dict] = {}
    for row in rows:
        sid = row["sender_id"]
        entry = seen.setdefault(sid, {"user_id": sid, "messages": 0, "last_at": "", "aliases": []})
        entry["messages"] += int(row["n"])
        name = row["sender_username"] or ""
        if name and name not in entry["aliases"]:
            entry["aliases"].append(name)
        if str(row["last_at"] or "") > entry["last_at"]:
            entry["last_at"] = str(row["last_at"] or "")[:19]
    return {"members": sorted(seen.values(), key=lambda e: -e["messages"]), "count": len(seen)}


# ── 统一错误类型（app.py 里转换成 HTTP 状态码 + {"ok":false,"error":...}）──
class ApiError(Exception):
    """业务错误：带上 HTTP 状态码，交给 app.py 的 v2 分派统一返回。"""

    def __init__(self, message: str, status: int = 400):
        super().__init__(message)
        self.message = str(message)
        self.status = int(status)


# ── 时区工具：库里的 DATETIME 都是 UTC 文本（"2026-11-14 14:52:14.649+0000"）──
LOCAL_OFFSET_HOURS = 8  # 服务器 TZ=Asia/Shanghai，库里存 UTC


def _local_time(value: object) -> str:
    text = str(value or "")[:19]
    try:
        epoch = calendar.timegm(time.strptime(text, "%Y-%m-%d %H:%M:%S"))
    except ValueError:
        return text
    return time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(epoch + LOCAL_OFFSET_HOURS * 3600))


def _day_window(days: int) -> tuple[int, str, list[str]]:
    """返回 (天数, UTC 起点字符串, 本地日期列表)，用于补零连续日期。"""
    try:
        days = int(days or 14)
    except (TypeError, ValueError):
        days = 14
    days = max(1, min(90, days))
    local_now = time.time() + LOCAL_OFFSET_HOURS * 3600
    today_start = local_now - (local_now % 86400)
    first_local = today_start - (days - 1) * 86400
    since = time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(first_local - LOCAL_OFFSET_HOURS * 3600))
    dates = [time.strftime("%Y-%m-%d", time.gmtime(first_local + i * 86400)) for i in range(days)]
    return days, since, dates


def _int_param(value: object, default: int, low: int, high: int) -> int:
    try:
        number = int(value)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        number = default
    return max(low, min(high, number))


def _group_names(domain: Any) -> dict[str, str]:
    """从 fm-domain 的 /groups 取群名；取不到就返回空 dict（调用方回退显示 id）。"""
    names: dict[str, str] = {}
    if not callable(domain):
        return names
    try:
        result = domain("/groups") or {}
    except Exception:
        return names
    items = result.get("data") if isinstance(result, dict) else result
    if isinstance(items, dict):
        items = items.get("groups") or items.get("data") or []
    for group in items if isinstance(items, list) else []:
        if not isinstance(group, dict):
            continue
        gid = str(group.get("group_id") or group.get("id") or "").strip()
        name = str(group.get("display_name") or group.get("name") or "").strip()
        if gid and name:
            names[gid] = name
    return names


def _chat_label(chat_id: str, kind: str, names: dict[str, str]) -> str:
    name = names.get(str(chat_id)) or ""
    if name:
        return name
    return ("私聊 " if "Private" in (kind or "") else "群 ") + str(chat_id)


# ── 图表数据（聊天量 / 工具调用 / 活跃群）─────────────────────
def charts_messages(days: int = 14, chat: str = "") -> list[dict]:
    """按天统计聊天量：返回 [{date,total,bot,human}]，日期按本地时区，缺日补 0。"""
    days, since, dates = _day_window(days)
    sql = (
        "SELECT substr(datetime(substr(recorded_at,1,19), '+%d hours'),1,10) day, "
        "COUNT(*) total, SUM(CASE WHEN is_bot=1 THEN 1 ELSE 0 END) bot, "
        "SUM(CASE WHEN is_bot=0 THEN 1 ELSE 0 END) human "
        "FROM chat_log WHERE substr(recorded_at,1,19) >= ?" % LOCAL_OFFSET_HOURS
    )
    params: list[Any] = [since]
    if chat:
        sql += " AND CAST(chat_id AS TEXT) = ?"
        params.append(str(chat))
    sql += " GROUP BY day"
    buckets = {str(row.get("day") or ""): row for row in _query(sql, tuple(params))}
    out = []
    for date in dates:
        row = buckets.get(date) or {}
        out.append({
            "date": date,
            "total": int(row.get("total") or 0),
            "bot": int(row.get("bot") or 0),
            "human": int(row.get("human") or 0),
        })
    return out


def charts_tools(days: int = 14) -> list[dict]:
    """按天统计工具调用次数：返回 [{date,count}]，缺日补 0。"""
    days, since, dates = _day_window(days)
    rows = _query(
        "SELECT substr(datetime(substr(occurred_at,1,19), '+%d hours'),1,10) day, COUNT(*) count "
        "FROM audit_log WHERE event_kind='tool_call_finished' "
        "AND substr(occurred_at,1,19) >= ? GROUP BY day" % LOCAL_OFFSET_HOURS,
        (since,),
    )
    buckets = {str(row.get("day") or ""): int(row.get("count") or 0) for row in rows}
    return [{"date": date, "count": buckets.get(date, 0)} for date in dates]


def top_groups(days: int = 30, limit: int = 15, domain: Any = None) -> list[dict]:
    """最近 N 天消息量最多的会话：[{chat_id,name,messages,...}]。"""
    days, since, _ = _day_window(days)
    limit = _int_param(limit, 15, 1, 100)
    rows = _query(
        "SELECT CAST(chat_id AS TEXT) chat_id, MAX(kind_key) kind, COUNT(*) messages, "
        "SUM(CASE WHEN is_bot=1 THEN 1 ELSE 0 END) bot, "
        "SUM(CASE WHEN is_bot=0 THEN 1 ELSE 0 END) human, MAX(recorded_at) last_at "
        "FROM chat_log WHERE chat_id IS NOT NULL AND substr(recorded_at,1,19) >= ? "
        "GROUP BY chat_id ORDER BY messages DESC LIMIT ?",
        (since, limit),
    )
    names = _group_names(domain)
    out = []
    for row in rows:
        chat_id = str(row.get("chat_id") or "")
        kind = str(row.get("kind") or "")
        out.append({
            "chat_id": chat_id,
            "name": _chat_label(chat_id, kind, names),
            "messages": int(row.get("messages") or 0),
            "bot": int(row.get("bot") or 0),
            "human": int(row.get("human") or 0),
            "kind": kind,
            "last_at": _local_time(row.get("last_at")),
        })
    return out


_TOOL_CATALOG_CACHE: dict[str, Any] = {"at": 0.0, "days": 0, "rows": []}
_TOOL_CATALOG_LOCK = threading.Lock()


def tools_catalog(days: int = 0) -> list[dict]:
    """工具调用总览：[{name,calls,failures,last}]；days=0 表示全部历史（默认，带 30s 缓存）。"""
    days = _int_param(days, 0, 0, 90)
    now = time.time()
    with _TOOL_CATALOG_LOCK:
        if (
            days == int(_TOOL_CATALOG_CACHE.get("days") or 0)
            and _TOOL_CATALOG_CACHE.get("rows")
            and now - float(_TOOL_CATALOG_CACHE.get("at") or 0) < 30
        ):
            return list(_TOOL_CATALOG_CACHE["rows"])
    sql = "SELECT event_json, occurred_at FROM audit_log WHERE event_kind='tool_call_finished'"
    params: tuple = ()
    if days > 0:
        _, since, _ = _day_window(days)
        sql += " AND substr(occurred_at,1,19) >= ?"
        params = (since,)
    stats: dict[str, dict] = {}
    for row in _query(sql, params):
        try:
            payload = json.loads(str(row.get("event_json") or "{}"))
        except (TypeError, ValueError):
            continue
        if not isinstance(payload, dict):
            continue
        tool_call = payload.get("toolCall")
        name = str(
            payload.get("toolName")
            or (tool_call.get("name") if isinstance(tool_call, dict) else "")
            or "unknown"
        )
        state = str(payload.get("status") or "unknown")
        entry = stats.setdefault(name, {"name": name, "calls": 0, "failures": 0, "last": ""})
        entry["calls"] += 1
        if state != "ok":
            entry["failures"] += 1
        at = _local_time(row.get("occurred_at"))
        if at > str(entry["last"]):
            entry["last"] = at
    rows = sorted(stats.values(), key=lambda e: (-int(e["calls"]), str(e["name"])))
    with _TOOL_CATALOG_LOCK:
        _TOOL_CATALOG_CACHE.update({"at": now, "days": days, "rows": rows})
    return rows


# ── 触发方式（config.trigger RPC 优先，文件 / 集合兜底）────────────
TRIGGER_MODES: list[tuple[str, str]] = [
    ("prefix", "前缀（FM 空格）"),
    ("mention", "@提及"),
    ("reply", "回复消息"),
    ("name", "叫名字"),
    ("keyword", "关键词"),
    ("image", "图片"),
    ("file", "文件"),
    ("audio", "语音"),
    ("sticker", "贴纸"),
    ("all", "群内全部消息"),
]
_TRIGGER_MODE_ALIASES: dict[str, str] = {mode: mode for mode, _ in TRIGGER_MODES}
_TRIGGER_MODE_ALIASES.update({
    "前缀": "prefix", "fm空格": "prefix", "fm 空格": "prefix",
    "@": "mention", "@fm": "mention", "提及": "mention",
    "回复": "reply", "名字": "name", "叫名": "name",
    "关键词": "keyword", "图片": "image", "文件": "file",
    "语音": "audio", "贴纸": "sticker", "全部": "all", "全部群消息": "all",
})
_TRIGGER_FILE = os.path.join(RUNTIME, "trigger-config.json")


def _modes_catalog() -> list[dict]:
    return [{"id": mode, "label": label} for mode, label in TRIGGER_MODES]


def _canonical_mode(value: object) -> str | None:
    raw = str(value or "").strip()
    if not raw:
        return None
    return _TRIGGER_MODE_ALIASES.get(raw.lower()) or _TRIGGER_MODE_ALIASES.get(raw)


def _trigger_scope(group_id: object) -> str:
    gid = str(group_id or "").strip()
    if not gid:
        raise ApiError("缺少 group_id（QQ 群号）")
    # 已带平台前缀（如 "qq:216063968"）时原样使用；matrix 的房间 id 自带冒号。
    return gid if ":" in gid else "qq:" + gid


def _scope_group_id(scope: str) -> str:
    return scope.split(":", 1)[1] if ":" in scope else scope


def _write_json_atomic(path: str, payload: object) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    temporary = path + ".tmp"
    with open(temporary, "w", encoding="utf-8") as handle:
        json.dump(payload, handle, ensure_ascii=False)
    os.replace(temporary, path)


def _trigger_file_all() -> dict[str, Any] | None:
    raw = _read(_TRIGGER_FILE)
    if not raw:
        return None
    try:
        data = json.loads(raw)
    except (TypeError, ValueError):
        return None
    return data if isinstance(data, dict) else None


def _trigger_items_from_rpc(rpc: Callable[..., Any] | None) -> tuple[list[dict] | None, str, str]:
    """返回 (原始配置列表, 数据来源, 错误说明)。"""
    if not callable(rpc):
        return None, "", "未注入 RPC 客户端"
    try:
        result = rpc("config.trigger", {"action": "list"})
    except Exception as exc:  # noqa: BLE001
        return None, "", str(exc)
    if not isinstance(result, dict) or not result.get("ok"):
        error = result.get("error") if isinstance(result, dict) else ""
        return None, "", str(error or "RPC 返回失败")
    data = result.get("data")
    if not isinstance(data, list):
        return None, "", "RPC 返回结构异常"
    items = []
    for item in data:
        if not isinstance(item, dict):
            continue
        items.append({"scope": str(item.get("scope") or ""), "config": item.get("config")})
    return items, "rpc:config.trigger", ""


def triggers_get(group_id: object, rpc: Callable[..., Any] | None, collections: Any = None) -> dict:
    """读取触发方式配置。优先 config.trigger RPC；不可用时退回 trigger-config.json / 后台集合。"""
    scope = _trigger_scope(group_id) if str(group_id or "").strip() else ""
    items, source, error = _trigger_items_from_rpc(rpc)
    if items is None:
        file_data = _trigger_file_all()
        if file_data is not None:
            items = [{"scope": str(key), "config": value} for key, value in file_data.items()]
            source = "file:" + _TRIGGER_FILE
            error = ""
    if items is None and callable(collections):
        raw_collection = collections() or {}
        values = raw_collection.values() if isinstance(raw_collection, dict) else raw_collection
        items = [
            {"scope": str(item.get("scope") or item.get("id") or ""), "config": item.get("config") or item}
            for item in values
            if isinstance(item, dict)
        ]
        source = "collections:/api/collections/triggers"
        error = ""
    if items is None:
        items = []
        source = "unavailable"
    normalized = []
    for item in items:
        item_scope = str(item.get("scope") or "")
        config = item.get("config") if isinstance(item.get("config"), dict) else {}
        modes = [str(mode) for mode in (config.get("modes") or [])]
        keywords = [str(word) for word in (config.get("keywords") or [])]
        normalized.append({
            "scope": item_scope,
            "group_id": _scope_group_id(item_scope),
            "modes": modes,
            "keywords": keywords,
        })
    if scope:
        match = next((item for item in normalized if item["scope"] == scope), None)
        return {
            "source": source,
            "group_id": _scope_group_id(scope),
            "scope": scope,
            "exists": match is not None,
            "config": {"modes": match["modes"], "keywords": match["keywords"]} if match else None,
            "modes_catalog": _modes_catalog(),
            "total_configured": len(normalized),
            "note": error,
        }
    normalized.sort(key=lambda item: item["group_id"])
    return {
        "source": source,
        "count": len(normalized),
        "groups": normalized,
        "modes_catalog": _modes_catalog(),
        "note": error,
    }


def triggers_set(
    group_id: object,
    modes: object,
    keywords: object,
    rpc: Callable[..., Any] | None,
) -> dict:
    """写入触发方式。先探测 RPC；不可用时写 trigger-config.json（机器人的真实配置源）。"""
    scope = _trigger_scope(group_id)
    raw_modes = modes if isinstance(modes, list) else []
    parsed: list[str] = []
    unknown: list[str] = []
    for mode in raw_modes:
        canonical = _canonical_mode(mode)
        if canonical is None:
            unknown.append(str(mode))
        elif canonical not in parsed:
            parsed.append(canonical)
    if unknown:
        raise ApiError("不支持的触发方式：" + "、".join(unknown) + "；可用：prefix/mention/reply/name/keyword/image/file/audio/sticker/all")
    if keywords is not None and not isinstance(keywords, list):
        raise ApiError("keywords 必须是字符串数组")
    action = "set" if parsed else "clear"
    current_items, _, current_error = _trigger_items_from_rpc(rpc)
    if isinstance(keywords, list):
        clean_keywords = [str(word).strip() for word in keywords if str(word).strip()]
    else:
        clean_keywords = []
        if current_items:
            match = next((item for item in current_items if item.get("scope") == scope), None)
            config = (match or {}).get("config") if isinstance(match, dict) else None
            if isinstance(config, dict):
                clean_keywords = [str(word) for word in (config.get("keywords") or [])]
    rpc_error = ""
    if callable(rpc):
        params: dict[str, Any] = {"action": action, "scope": scope}
        if action == "set":
            params["modes"] = parsed
            params["keywords"] = clean_keywords
        try:
            result = rpc("config.trigger", params)
        except Exception as exc:  # noqa: BLE001
            result = {"ok": False, "error": str(exc)}
        if isinstance(result, dict) and result.get("ok"):
            try:
                readback = rpc("config.trigger", {"action": "get", "scope": scope})
            except Exception:  # noqa: BLE001
                readback = {"ok": False}
            data = readback.get("data") if isinstance(readback, dict) and readback.get("ok") else {}
            config = data.get("config") if isinstance(data, dict) else None
            verified = bool(readback.get("ok")) if isinstance(readback, dict) else False
            if action == "clear":
                verified = verified and not config
            return {
                "source": "rpc:config.trigger",
                "scope": scope,
                "group_id": _scope_group_id(scope),
                "action": action,
                "saved": True,
                "applied": bool((result.get("data") or {}).get("applied", True)),
                "verified": verified,
                "config": config if isinstance(config, dict) else None,
                "modes_catalog": _modes_catalog(),
            }
        rpc_error = str(result.get("error") or "") if isinstance(result, dict) else ""
    # 兜底：直接改机器人读取的 trigger-config.json（原子替换）。
    file_data = _trigger_file_all()
    if file_data is None:
        raise ApiError("写回触发配置失败（RPC 与配置文件都不可用）：" + (rpc_error or current_error or "未知错误"), 502)
    if action == "clear":
        file_data.pop(scope, None)
        new_config = None
    else:
        new_config = {"keywords": clean_keywords, "modes": parsed}
        file_data[scope] = new_config
    _write_json_atomic(_TRIGGER_FILE, file_data)
    readback_data = _trigger_file_all() or {}
    stored = readback_data.get(scope)
    verified = (stored == new_config) if action == "set" else (scope not in readback_data)
    return {
        "source": "file:" + _TRIGGER_FILE,
        "fallback": True,
        "fallback_reason": rpc_error or current_error or "RPC 不可用",
        "scope": scope,
        "group_id": _scope_group_id(scope),
        "action": action,
        "saved": True,
        "applied": None,
        "verified": verified,
        "config": stored if isinstance(stored, dict) else None,
        "modes_catalog": _modes_catalog(),
    }


# ── 人设（config.persona RPC 优先，memory 文件兜底读）────────────
PERSONA_SCOPES: list[dict] = [
    {"scope": "group_default", "label": "默认群人设", "needs_id": False},
    {"scope": "private_default", "label": "默认私聊人设", "needs_id": False},
    {"scope": "group", "label": "指定群", "needs_id": True},
    {"scope": "private", "label": "指定用户私聊", "needs_id": True},
    {"scope": "member", "label": "成员风格", "needs_id": True},
]
_PERSONA_DIRS = {"group": "group_persona", "private": "private_persona", "member": "member_style"}


def _persona_target(scope: object, ident: object) -> tuple[str, str]:
    raw = str(scope or "").strip().lower()
    ident = str(ident or "").strip()
    if raw in ("", "group_default"):
        return "group_default", ""
    if raw == "private_default":
        return "private_default", ""
    kind, _, sub = raw.partition(":")
    if kind in _PERSONA_DIRS:
        target = sub.strip() or ident
        if not target:
            raise ApiError(f"scope={kind} 需要提供 id（群号 / QQ 号）")
        if kind == "group" and not target.isdigit():
            raise ApiError("group scope 的 id 必须是数字群号")
        return kind, target
    raise ApiError("scope 只支持 group_default / private_default / group:<群号> / private:<QQ> / member:<QQ>")


def _persona_file_path(kind: str, ident: str) -> str:
    if kind == "group_default":
        return os.path.join(RUNTIME, "memory", "qq", "group_persona", "_default.md")
    if kind == "private_default":
        return os.path.join(RUNTIME, "memory", "qq", "private_persona", "_default.md")
    return os.path.join(RUNTIME, "memory", "qq", _PERSONA_DIRS[kind], ident + ".md")


def _persona_rpc_scope(kind: str, ident: str) -> str:
    return kind + (":" + ident if ident else "")


def _persona_rpc_params(action: str, kind: str, ident: str, content: str | None = None) -> dict:
    params: dict[str, Any] = {"action": action, "scope": kind}
    if ident:
        params["id"] = ident
    if content is not None:
        params["content"] = content
    return params


def _persona_groups(domain: Any) -> list[dict]:
    names = _group_names(domain)
    return [
        {"group_id": gid, "name": name}
        for gid, name in sorted(names.items(), key=lambda pair: pair[1])
    ]


def persona_read(scope: object, ident: object, rpc: Callable[..., Any] | None, domain: Any = None) -> dict:
    kind, target_id = _persona_target(scope, ident)
    rpc_scope = _persona_rpc_scope(kind, target_id)
    error = ""
    if callable(rpc):
        try:
            result = rpc("config.persona", _persona_rpc_params("get", kind, target_id))
        except Exception as exc:  # noqa: BLE001
            result = {"ok": False, "error": str(exc)}
        if isinstance(result, dict) and result.get("ok"):
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            content = str(data.get("content") or "")
            return {
                "source": "rpc:config.persona",
                "scope": rpc_scope,
                "scope_kind": kind,
                "id": target_id,
                "exists": bool(data.get("saved")),
                "content": content,
                "length": len(content),
                "saved": bool(data.get("saved")),
                "applied": bool(data.get("applied")),
                "scopes": PERSONA_SCOPES,
                "groups": _persona_groups(domain),
                "note": "",
            }
        error = str(result.get("error") or "") if isinstance(result, dict) else ""
    path = _persona_file_path(kind, target_id)
    if os.path.isfile(path):
        content = _read(path)
        return {
            "source": "file:" + path,
            "scope": rpc_scope,
            "scope_kind": kind,
            "id": target_id,
            "exists": bool(content.strip()),
            "content": content,
            "length": len(content),
            "saved": True,
            "applied": None,
            "scopes": PERSONA_SCOPES,
            "groups": _persona_groups(domain),
            "note": "RPC 不可用，已从 memory 文件读取：" + (error or "未知原因"),
        }
    raise ApiError("读取人设失败：" + (error or "RPC 与 memory 文件都不可用"), 502)


def persona_write(
    scope: object,
    ident: object,
    text: object,
    action: object,
    rpc: Callable[..., Any] | None,
) -> dict:
    kind, target_id = _persona_target(scope, ident)
    rpc_scope = _persona_rpc_scope(kind, target_id)
    requested = str(action or "").strip().lower()
    content = str(text or "")
    if requested == "clear":
        write_action = "clear"
        rpc_action = "clear"
        content = ""
    else:
        write_action = "set"
        rpc_action = "set"
        if not content.strip():
            raise ApiError("人设内容不能为空（清空请传 action=clear）")
        if len(content) > 1000:
            raise ApiError(f"人设内容超过机器人 1000 字上限（当前 {len(content)} 字）")
    if not callable(rpc):
        raise ApiError("RPC 不可用，无法安全写入人设（避免绕过机器人内存仓库直接改文件）", 502)
    try:
        result = rpc("config.persona", _persona_rpc_params(rpc_action, kind, target_id, content if rpc_action == "set" else None))
    except Exception as exc:  # noqa: BLE001
        result = {"ok": False, "error": str(exc)}
    if not isinstance(result, dict) or not result.get("ok"):
        message = str(result.get("error") or "") if isinstance(result, dict) else ""
        raise ApiError("写回人设失败：" + (message or "RPC 返回失败"), 502)
    try:
        readback = rpc("config.persona", _persona_rpc_params("get", kind, target_id))
    except Exception:  # noqa: BLE001
        readback = {"ok": False}
    data = readback.get("data") if isinstance(readback, dict) and readback.get("ok") else {}
    current = str(data.get("content") or "") if isinstance(data, dict) else ""
    verified = bool(readback.get("ok")) if isinstance(readback, dict) else False
    verified = verified and ((current == content) if write_action == "set" else not current)
    return {
        "source": "rpc:config.persona",
        "scope": rpc_scope,
        "scope_kind": kind,
        "id": target_id,
        "action": write_action,
        "saved": True,
        "applied": bool((result.get("data") or {}).get("applied", True)),
        "verified": verified,
        "content": current,
        "length": len(current),
        "scopes": PERSONA_SCOPES,
    }


# ── 定时消息（scheduled_messages，只读列表 + 受控取消）────────────
def _scheduled_preview(raw: object) -> str:
    text = str(raw or "")
    try:
        payload = json.loads(text)
    except (TypeError, ValueError):
        return text[:200]
    if isinstance(payload, dict):
        for key in ("prompt", "text", "content", "message"):
            value = payload.get(key)
            if isinstance(value, str) and value.strip():
                return value[:200]
        original = payload.get("original_message")
        if isinstance(original, dict):
            for key in ("text", "prompt"):
                value = original.get(key)
                if isinstance(value, str) and value.strip():
                    return value[:200]
    return text[:200]


def scheduled_list(
    limit: object = 50,
    offset: object = 0,
    chat: object = "",
    sender: object = "",
    q: object = "",
    future_only: bool = False,
) -> dict:
    limit = _int_param(limit, 50, 1, 300)
    offset = _int_param(offset, 0, 0, 100000)
    where = " WHERE 1=1"
    params: list[Any] = []
    if chat:
        where += " AND CAST(chat_id AS TEXT) = ?"
        params.append(str(chat).strip())
    if sender:
        where += " AND (sender_id = ? OR sender_username LIKE ?)"
        params.extend([str(sender).strip(), "%" + str(sender).strip() + "%"])
    if q:
        where += " AND message_json LIKE ?"
        params.append("%" + str(q).strip() + "%")
    if future_only:
        where += " AND due_at_unix_seconds > ?"
        params.append(int(time.time()))
    total_row = _query("SELECT COUNT(*) n FROM scheduled_messages" + where, tuple(params))
    rows = _query(
        "SELECT id, due_at_unix_seconds, platform_key, CAST(chat_id AS TEXT) chat_id, sender_id, "
        "sender_username, message_json FROM scheduled_messages" + where
        + " ORDER BY due_at_unix_seconds ASC, id ASC LIMIT ? OFFSET ?",
        tuple(params + [limit, offset]),
    )
    now = time.time()
    out = []
    for row in rows:
        due = int(row.get("due_at_unix_seconds") or 0)
        try:
            due_text = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(due)) if due else ""
        except (OverflowError, OSError, ValueError):
            due_text = str(due)
        out.append({
            "id": int(row.get("id") or 0),
            "due_at": due_text,
            "due_at_unix": due,
            "remaining_seconds": max(0, due - int(now)),
            "due": due > now,
            "platform": str(row.get("platform_key") or ""),
            "chat_id": str(row.get("chat_id") or ""),
            "sender_id": str(row.get("sender_id") or ""),
            "sender_username": str(row.get("sender_username") or ""),
            "preview": _scheduled_preview(row.get("message_json")),
        })
    return {
        "rows": out,
        "total": int((total_row[0].get("n") if total_row else 0) or 0),
        "limit": limit,
        "offset": offset,
        "now": int(now),
    }


def _bot_db_rw() -> sqlite3.Connection:
    connection = sqlite3.connect(BOT_DB, timeout=8)
    connection.execute("PRAGMA busy_timeout=8000")
    return connection


def scheduled_cancel(row_id: object) -> dict:
    """取消一条待发定时消息。

    scheduled_messages 表结构里没有 canceled 字段（只有 due_at/message_json），
    机器人侧到期发送后也是直接 DELETE 该行；所以「取消」= 带条件地删除单行，
    并返回被删行快照作为审计证据。绝不改 message_json / due_at 去耍花招。
    """
    try:
        schedule_id = int(row_id)  # type: ignore[arg-type]
    except (TypeError, ValueError):
        raise ApiError("id 必须是整数（scheduled_messages.id）")
    if schedule_id <= 0:
        raise ApiError("id 必须是正整数")
    connection = _bot_db_rw()
    try:
        connection.row_factory = sqlite3.Row
        row = connection.execute(
            "SELECT id, due_at_unix_seconds, platform_key, chat_id, sender_id, sender_username, message_json "
            "FROM scheduled_messages WHERE id = ?",
            (schedule_id,),
        ).fetchone()
        if row is None:
            raise ApiError("定时消息不存在（可能已被机器人发送或已取消）", 404)
        snapshot = {key: row[key] for key in row.keys()}
        snapshot["message_json"] = str(snapshot.get("message_json") or "")[:400]
        cursor = connection.execute("DELETE FROM scheduled_messages WHERE id = ?", (schedule_id,))
        connection.commit()
        deleted = int(cursor.rowcount or 0)
        left = int(connection.execute(
            "SELECT COUNT(*) FROM scheduled_messages WHERE id = ?", (schedule_id,)
        ).fetchone()[0])
    finally:
        connection.close()
    if left != 0:
        raise ApiError("取消未生效：该行仍存在，请重试", 500)
    if deleted != 1:
        raise ApiError("该定时消息在取消瞬间已被机器人发送或删除", 404)
    return {
        "id": schedule_id,
        "cancelled": True,
        "deleted": deleted,
        "mode": "delete_row",
        "table": "scheduled_messages",
        "row": snapshot,
        "note": "该表没有 canceled 字段；取消=删除待发行（机器人到期发送后同样删除该行）。",
    }


# ── 成员档案 ──────────────────────────────────────────────
def member_profile(user_id: object, domain: Any = None) -> dict:
    uid = str(user_id or "").strip()
    if not uid:
        raise ApiError("缺少成员 id（QQ 号）")
    alias_rows = _query(
        "SELECT sender_username, COUNT(*) n FROM chat_log "
        "WHERE sender_id = ? AND is_bot = 0 AND sender_username IS NOT NULL AND sender_username <> '' "
        "GROUP BY sender_username ORDER BY n DESC",
        (uid,),
    )
    aliases = [str(row.get("sender_username")) for row in alias_rows]
    totals = _query(
        "SELECT COUNT(*) n, MIN(recorded_at) first_at, MAX(recorded_at) last_at "
        "FROM chat_log WHERE sender_id = ? AND is_bot = 0",
        (uid,),
    )
    total_row = totals[0] if totals else {}
    messages = int(total_row.get("n") or 0)
    group_rows = _query(
        "SELECT CAST(chat_id AS TEXT) chat_id, MAX(kind_key) kind, COUNT(*) n, MAX(recorded_at) last_at "
        "FROM chat_log WHERE sender_id = ? AND is_bot = 0 GROUP BY chat_id ORDER BY n DESC",
        (uid,),
    )
    names = _group_names(domain)
    kinds: dict[str, str] = {}
    per_group = []
    for row in group_rows:
        chat_id = str(row.get("chat_id") or "")
        kind = str(row.get("kind") or "")
        kinds[chat_id] = kind
        per_group.append({
            "chat_id": chat_id,
            "name": _chat_label(chat_id, kind, names),
            "kind": kind,
            "messages": int(row.get("n") or 0),
            "last_at": _local_time(row.get("last_at")),
        })
    recent_rows = _query(
        "SELECT CAST(chat_id AS TEXT) chat_id, body_text, image_urls, recorded_at "
        "FROM chat_log WHERE sender_id = ? AND is_bot = 0 ORDER BY id DESC LIMIT 20",
        (uid,),
    )
    recent = []
    for row in recent_rows:
        chat_id = str(row.get("chat_id") or "")
        images = str(row.get("image_urls") or "")
        recent.append({
            "chat_id": chat_id,
            "name": _chat_label(chat_id, kinds.get(chat_id, ""), names),
            "text": str(row.get("body_text") or "")[:300],
            "images": images.count("media:") + images.count("http"),
            "at": _local_time(row.get("recorded_at")),
        })
    if not messages and not aliases:
        raise ApiError("没有该成员（" + uid + "）的聊天记录", 404)
    return {
        "user_id": uid,
        "aliases": aliases,
        "messages": messages,
        "first_seen": _local_time(total_row.get("first_at")),
        "last_seen": _local_time(total_row.get("last_at")),
        "per_group": per_group,
        "recent": recent,
        "groups_named": len(names),
    }


# ── 部署面板：走宿主机 ops-agent 队列（fm-admin 容器没有 docker socket）──
OPS_BASE = os.environ.get("FM_OPS_BASE", "/data").rstrip("/") or "/data"
OPS_QUEUE = os.path.join(OPS_BASE, "ops-queue")
OPS_RESULTS = os.path.join(OPS_BASE, "ops-results")
OPS_STATE = os.path.join(OPS_BASE, "ops-agent-state.json")
OPS_ALLOWED = ("images", "inspect", "restart")
# images 采集在 208 个 30GB 级镜像的机器上要 ~17s，所以后台线程缓存快照、别让 HTTP 等它。
_OPS_SNAPSHOT: dict[str, Any] = {"data": None, "at": 0.0, "images_at": 0.0, "running_at": 0.0}
_OPS_SNAPSHOT_LOCK = threading.Lock()
_OPS_WAKE = threading.Event()
_OPS_WORKER_LOCK = threading.Lock()
_OPS_WORKER_STARTED = False
_OPS_IMAGES_TTL = 120.0
_OPS_RUNNING_TTL = 20.0
_OPS_FIRST_WAIT = 26.0


def _ops_agent_state() -> dict:
    """读 ops-agent 每轮写的心跳文件，判断宿主机代理是否在跑。"""
    for path in (OPS_STATE, os.path.join(OPS_RESULTS, "agent-state.json")):
        if not os.path.isfile(path):
            continue
        try:
            data = json.loads(_read(path))
        except (TypeError, ValueError):
            continue
        if not isinstance(data, dict):
            continue
        last = float(data.get("last_scan") or data.get("updated_at") or 0)
        age = time.time() - last if last else None
        data["online"] = bool(age is not None and age < 90)
        data["age_seconds"] = round(age, 1) if age is not None else None
        data["heartbeat_path"] = path
        return data
    return {"online": False, "age_seconds": None, "heartbeat_path": OPS_STATE,
            "note": "未找到 ops-agent 心跳文件（宿主机代理未安装/未运行）"}


def _ops_enqueue(op: object, target: object = "") -> str:
    op_text = str(op or "").strip().lower()
    target_text = str(target or "").strip().lower()
    if op_text not in OPS_ALLOWED:
        raise ApiError("不允许的运维操作：" + op_text + "（白名单：" + " / ".join(OPS_ALLOWED) + "）", 400)
    if op_text == "restart" and target_text != "bot":
        raise ApiError("restart 只允许 target=bot", 400)
    request_id = f"{int(time.time() * 1000)}-{secrets.token_hex(4)}"
    payload = {
        "id": request_id,
        "op": op_text,
        "target": target_text,
        "created_at": time.time(),
        "requested_by": "fm-admin",
    }
    try:
        _write_json_atomic(os.path.join(OPS_QUEUE, request_id + ".json"), payload)
    except OSError as exc:
        raise ApiError("无法写入运维队列 " + OPS_QUEUE + "：" + str(exc), 503)
    return request_id


def _ops_result(request_id: object) -> dict | None:
    rid = str(request_id or "").strip()
    if not rid or not re.fullmatch(r"[0-9A-Za-z._-]+", rid):
        return None
    path = os.path.join(OPS_RESULTS, rid + ".json")
    if not os.path.isfile(path):
        return None
    try:
        data = json.loads(_read(path))
    except (TypeError, ValueError):
        return None
    return data if isinstance(data, dict) else None


def _ops_wait(request_ids: list[str], timeout: float = 8.0) -> dict[str, dict | None]:
    deadline = time.time() + max(0.5, float(timeout))
    pending = list(request_ids)
    found: dict[str, dict | None] = {}
    while pending and time.time() < deadline:
        for request_id in list(pending):
            result = _ops_result(request_id)
            if result is not None:
                found[request_id] = result
                pending.remove(request_id)
        if pending:
            time.sleep(0.15)
    for request_id in pending:
        found[request_id] = None
    return found


def _ops_prune_results(max_age_seconds: int = 7200) -> None:
    try:
        names = os.listdir(OPS_RESULTS)
    except OSError:
        return
    cutoff = time.time() - max_age_seconds
    for name in names:
        if not name.endswith(".json") or name == "agent-state.json":
            continue
        path = os.path.join(OPS_RESULTS, name)
        try:
            if os.path.getmtime(path) < cutoff:
                os.unlink(path)
        except OSError:
            continue


def _ops_sample(timeout: float = 45.0) -> dict | None:
    """向宿主机代理要一轮 images/inspect，只补过期的部分，返回合并后的快照。"""
    agent = _ops_agent_state()
    if not agent.get("online"):
        return None
    now = time.time()
    with _OPS_SNAPSHOT_LOCK:
        snapshot = dict(_OPS_SNAPSHOT.get("data") or {})
        images_at = float(_OPS_SNAPSHOT.get("images_at") or 0.0)
        running_at = float(_OPS_SNAPSHOT.get("running_at") or 0.0)
    images = list(snapshot.get("images") or [])
    running = dict(snapshot.get("running") or {})
    ops: list[str] = []
    if not images or now - images_at > _OPS_IMAGES_TTL:
        ops.append("images")
    if not running or now - running_at > _OPS_RUNNING_TTL:
        ops.append("inspect")
    if not ops:
        return snapshot or None
    _ops_prune_results()
    requests: dict[str, str] = {}
    for op in ops:
        requests[op] = _ops_enqueue(op)
    started = time.time()
    results = _ops_wait(list(requests.values()), timeout=timeout)
    missing: list[str] = []
    for op, request_id in requests.items():
        result = results.get(request_id)
        if not result or not result.get("ok"):
            missing.append(op)
            continue
        payload = result.get("result") if isinstance(result.get("result"), dict) else {}
        if op == "images":
            images = payload.get("images") or []
            images_at = time.time()
        elif op == "inspect":
            running = payload.get("container") or {}
            running_at = time.time()
    data = {
        "source": "host-ops-agent",
        "agent_online": True,
        "agent": agent,
        "images": images,
        "running": running,
        "requests": requests,
        "missing": missing,
        "elapsed_ms": round((time.time() - started) * 1000),
        "checked_at": time.time(),
        "images_age_seconds": round(time.time() - images_at, 1) if images_at else None,
        "running_age_seconds": round(time.time() - running_at, 1) if running_at else None,
        "note": "" if not missing else "部分运维请求尚未完成（" + "、".join(missing) + "），后台会自动重试。",
    }
    with _OPS_SNAPSHOT_LOCK:
        _OPS_SNAPSHOT["data"] = data
        _OPS_SNAPSHOT["at"] = time.time()
        if images_at:
            _OPS_SNAPSHOT["images_at"] = images_at
        if running_at:
            _OPS_SNAPSHOT["running_at"] = running_at
    return data


def _ops_worker_loop() -> None:
    while True:
        try:
            _ops_sample(timeout=45.0)
        except Exception:  # noqa: BLE001  后台线程不能死
            pass
        _OPS_WAKE.wait(12.0)
        _OPS_WAKE.clear()


def _ensure_ops_worker() -> None:
    global _OPS_WORKER_STARTED
    with _OPS_WORKER_LOCK:
        if _OPS_WORKER_STARTED:
            return
        _OPS_WORKER_STARTED = True
        thread = threading.Thread(target=_ops_worker_loop, name="fm-ops-snapshot", daemon=True)
        thread.start()


def _snapshot_response(snapshot: dict) -> dict:
    data = dict(snapshot)
    with _OPS_SNAPSHOT_LOCK:
        at = float(_OPS_SNAPSHOT.get("at") or 0.0)
    data["stale_seconds"] = round(time.time() - at, 1) if at else None
    data["sampling"] = False
    return data


def deploy_status(request_id: object = "", force: bool = False) -> dict:
    """GET /api/v2/deploy/status：读宿主机代理的镜像/容器状态。

    宿主机 docker images 全量枚举要 ~17s，所以由后台线程缓存快照；首次请求最多等
    ~26s 拿第一份快照，之后立刻返回缓存（带 stale_seconds / sampling 字段）。
    """
    request_id = str(request_id or "").strip()
    if request_id:
        result = _ops_result(request_id)
        return {"request_id": request_id, "done": result is not None, "result": result}
    agent = _ops_agent_state()
    if not agent.get("online"):
        return {
            "source": "host-ops-agent",
            "agent_online": False,
            "agent": agent,
            "images": [],
            "running": {},
            "requests": {},
            "missing": ["images", "inspect"],
            "sampling": False,
            "note": "宿主机运维代理离线（心跳超时），无法读取 docker 状态。",
            "checked_at": time.time(),
        }
    _ensure_ops_worker()
    marker = time.time()
    if force:
        with _OPS_SNAPSHOT_LOCK:
            _OPS_SNAPSHOT["images_at"] = 0.0
            _OPS_SNAPSHOT["running_at"] = 0.0
        _OPS_WAKE.set()
    with _OPS_SNAPSHOT_LOCK:
        early = _OPS_SNAPSHOT.get("data")
    deadline = time.time() + (_OPS_FIRST_WAIT if (force or not early) else 1.0)
    while time.time() < deadline:
        with _OPS_SNAPSHOT_LOCK:
            snapshot = _OPS_SNAPSHOT.get("data")
            snapshot_at = float(_OPS_SNAPSHOT.get("at") or 0.0)
        if snapshot and (not force or snapshot_at >= marker):
            return _snapshot_response(snapshot)
        time.sleep(0.2)
    with _OPS_SNAPSHOT_LOCK:
        snapshot = _OPS_SNAPSHOT.get("data")
    if snapshot and not force:
        return _snapshot_response(snapshot)
    _OPS_WAKE.set()
    return {
        "source": "host-ops-agent",
        "agent_online": True,
        "agent": agent,
        "images": [],
        "running": {},
        "requests": {},
        "missing": ["images", "inspect"],
        "sampling": True,
        "note": "宿主机代理正在首次采集（镜像列表约 20 秒），请稍后重试本接口。",
        "checked_at": time.time(),
    }


def deploy_act(action: object, target: object = "bot") -> dict:
    """POST /api/v2/deploy/act：入队并立即返回 request_id（结果用 status?request_id= 轮询）。"""
    action_text = str(action or "").strip().lower()
    target_text = str(target or "bot").strip().lower() or "bot"
    if action_text == "list":
        ops = ("images", "inspect")
    elif action_text == "restart":
        ops = ("restart",)
        if target_text != "bot":
            raise ApiError("restart 只允许 target=bot", 400)
    else:
        raise ApiError("不支持的 action：" + (action_text or "(空)") + "（可用 list / restart）", 400)
    agent = _ops_agent_state()
    requests = {op: _ops_enqueue(op, "bot" if op == "restart" else "") for op in ops}
    if action_text == "restart":
        with _OPS_SNAPSHOT_LOCK:
            _OPS_SNAPSHOT["running_at"] = 0.0
        _OPS_WAKE.set()
    return {
        "action": action_text,
        "target": "bot" if action_text == "restart" else target_text,
        "queued": True,
        "requests": requests,
        "request_id": requests[ops[0]],
        "agent_online": bool(agent.get("online")),
        "flow": "GET /api/v2/deploy/status?request_id=<request_id> 轮询结果；list 的 images/inspect 结果也可直接看 /api/v2/deploy/status",
    }


# ── 操作台：可执行的动作（白名单）────────────────────────
ACTIONS = [
    {"name": "humanlike_set", "label": "设置拟人模式会话", "params": "scopes: [qq:group:<id>, ...]", "desc": "全量覆盖：只有列表里的会话开着拟人模式"},
    {"name": "proactive_set", "label": "设置某会话主动发言上限", "params": "scope, limit", "desc": "改 proactive.txt 里该会话的每日上限"},
    {"name": "send_message", "label": "以 FM 身份发一条消息", "params": "chat_id, text", "desc": "直接调用后台的发消息接口"},
]


def action_catalog() -> dict:
    return {"actions": ACTIONS}


def run_action(name: str, payload: dict, send_message) -> dict:
    if name == "humanlike_set":
        scopes = payload.get("scopes") if isinstance(payload.get("scopes"), list) else []
        return {"ok": True, "result": humanlike_set_scopes(scopes)}
    if name == "proactive_set":
        scope = str(payload.get("scope") or "").strip()
        limit = str(payload.get("limit") or "20").strip()
        if not scope:
            return {"ok": False, "error": "缺少 scope"}
        today = time.strftime("%Y-%m-%d")
        rows = []
        for row in _read(os.path.join(HUMANLIKE_DIR, "proactive.txt"), 8192).splitlines():
            parts = row.split("\t")
            if parts and parts[0].strip() == scope:
                rows.append(f"{scope}\t{parts[1] if len(parts) > 1 else today}\t{limit}\t{int(time.time())}")
            else:
                rows.append(row)
        if not any(r.startswith(scope + "\t") for r in rows):
            rows.append(f"{scope}\t{today}\t{limit}\t{int(time.time())}")
        _write(os.path.join(HUMANLIKE_DIR, "proactive.txt"), "\n".join(rows) + "\n")
        return {"ok": True, "result": humanlike_state()}
    if name == "send_message":
        chat_id = str(payload.get("chat_id") or "").strip()
        text = str(payload.get("text") or "").strip()
        if not chat_id or not text:
            return {"ok": False, "error": "缺少 chat_id 或 text"}
        return {"ok": True, "result": send_message(chat_id, text)}
    return {"ok": False, "error": "未知动作"}


ROUTES: dict[str, Callable[..., dict]] = {
    "overview": overview,
    "humanlike": humanlike_state,
    "members": member_search,
    "messages": message_search,
    "tools/audit": tools_audit,
    "tools/stats": tools_stats,
    "tools/catalog": tools_catalog,
    "charts/messages": charts_messages,
    "charts/tools": charts_tools,
    "top/groups": top_groups,
    "groups": groups_overview,
    "group": group_detail,
    "triggers": triggers_get,
    "triggers/set": triggers_set,
    "persona": persona_read,
    "scheduled": scheduled_list,
    "scheduled/cancel": scheduled_cancel,
    "members/profile": member_profile,
    "deploy/status": deploy_status,
    "deploy/act": deploy_act,
    "novels": novels,
    "novel": novel_detail,
    "browse/tables": browse_tables,
    "members/all": all_members,
    "actions": action_catalog,
}
