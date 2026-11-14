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
import csv
import hashlib
import io
import json
import os
import re
import secrets
import shutil
import sqlite3
import threading
import time
from typing import Any, Callable
from urllib.parse import urlencode

try:  # Python 3.11+ 自带 tomllib；admin 容器与宿主机运维代理都是 3.11。
    import tomllib
except ModuleNotFoundError:  # pragma: no cover - 老版本 Python 兜底
    tomllib = None  # type: ignore[assignment]

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
        "config": _mask_secret_values(config),
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
        # QA P2-3：以前返回 200 + {"ok":true,"data":{"error":...}}，前端会当成“空数据”；
        # 统一成 400 + {"ok":false,"error":...}（与其他接口错误语义一致）。
        raise ApiError("不允许浏览该表：%s（可用表：%s）"
                       % (table or "(空)", " / ".join(sorted(BROWSE_TABLES))), 400)
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
# 心跳多久没更新算离线（QA/Lead：统一阈值 90s）
OPS_AGENT_ONLINE_TTL = int(os.environ.get("FM_OPS_AGENT_TTL", "90") or 90)
# 白名单 op 必须与宿主机 ops_agent.py 的 ALLOWED_OPS 保持一致（v3 扩展 + v4 运维增强）。
OPS_ALLOWED = ("images", "inspect", "restart", "images_detail", "logs", "metrics",
               "cron", "rollback", "backup_domain",
               # v4 新增：stats（多容器 docker stats 快照）/ cron_list（只读）/
               #          cron_set（写入未实现，代理返回 501）/ config_backup（config.toml 备份）
               "stats", "cron_list", "cron_set", "config_backup")
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
    """读 ops-agent 心跳文件，判断宿主机代理是否在跑。

    2026-11-15（QA/Lead 反馈「误报离线」）：统一 state schema 并兼容旧字段名。
    新 schema（ops_agent >= 2.1.2 写入）：
        started_at / started_at_text        进程启动时间（epoch 秒 + 本地文本）
        updated_at / updated_at_text        最近一次心跳写入时间
        heartbeat(bool)                     是否由独立心跳线程写入
        scans / processed_total / mode ...
    旧字段（last_scan / last_scan_text / ts / heartbeat_at）继续兼容读取。
    在线判定：心跳年龄 < OPS_AGENT_ONLINE_TTL（默认 90s）。
    """
    now = time.time()
    for path in (OPS_STATE, os.path.join(OPS_RESULTS, "agent-state.json")):
        if not os.path.isfile(path):
            continue
        try:
            data = json.loads(_read(path))
        except (TypeError, ValueError):
            continue
        if not isinstance(data, dict):
            continue
        updated = 0.0
        for key in ("updated_at", "last_scan", "heartbeat_at", "ts"):
            try:
                value = float(data.get(key) or 0)
            except (TypeError, ValueError):
                value = 0.0
            if value > 0:
                updated = value
                break
        age = now - updated if updated else None
        data["updated_at"] = updated or None
        if not data.get("updated_at_text") and updated:
            data["updated_at_text"] = _local_time(
                time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(updated)))
        try:
            data["started_at"] = float(data.get("started_at") or 0) or None
        except (TypeError, ValueError):
            data["started_at"] = None
        if not data.get("started_at_text") and data.get("started_at"):
            data["started_at_text"] = _local_time(
                time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(data["started_at"])))
        data["last_scan"] = updated or None
        data["last_scan_text"] = data.get("last_scan_text") or data.get("updated_at_text") or ""
        data["online"] = bool(age is not None and age < OPS_AGENT_ONLINE_TTL)
        data["age_seconds"] = round(age, 1) if age is not None else None
        data["online_ttl_seconds"] = OPS_AGENT_ONLINE_TTL
        data["heartbeat_path"] = path
        data["schema_version"] = 2
        data["schema_fields"] = {
            "has_updated_at": bool(updated),
            "has_started_at": bool(data.get("started_at")),
            "has_version": bool(data.get("version")),
        }
        return data
    return {"online": False, "age_seconds": None, "heartbeat_path": OPS_STATE,
            "updated_at": None, "started_at": None, "schema_version": 2,
            "online_ttl_seconds": OPS_AGENT_ONLINE_TTL,
            "note": "未找到 ops-agent 心跳文件（宿主机代理未安装/未运行）"}


def _ops_enqueue(op: object, target: object = "", params: object = None) -> str:
    """入队一个白名单运维请求（params 只允许简单标量，防止队列注入）。"""
    op_text = str(op or "").strip().lower()
    target_text = str(target or "").strip().lower()
    if op_text not in OPS_ALLOWED:
        raise ApiError("不允许的运维操作：%s（白名单：%s）"
                       % (op_text or "(空)", " / ".join(OPS_ALLOWED)), 400)
    if op_text == "restart" and target_text != "bot":
        raise ApiError("restart 只允许 target=bot", 400)
    clean_params: dict[str, Any] = {}
    if isinstance(params, dict):
        for key, value in params.items():
            key_text = str(key).strip()[:40]
            if not re.fullmatch(r"[0-9A-Za-z_.-]{1,40}", key_text):
                raise ApiError("运维参数名非法：" + key_text, 400)
            if isinstance(value, bool):
                clean_params[key_text] = value
            elif isinstance(value, (int, float)):
                clean_params[key_text] = value
            elif isinstance(value, str):
                if len(value) > 200 or "\x00" in value or "\n" in value or "\r" in value:
                    raise ApiError("运维参数值非法：" + key_text, 400)
                clean_params[key_text] = value
            elif value is None:
                continue
            else:
                raise ApiError("运维参数类型不支持：" + key_text, 400)
    request_id = f"{int(time.time() * 1000)}-{secrets.token_hex(4)}"
    payload = {
        "id": request_id,
        "op": op_text,
        "target": target_text,
        "params": clean_params,
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



# ══════════════════════════════════════════════════════════════════════
# v3 扩展（2026-11-15）：配置 / 记忆 / 模型 / 群能力 / 运维监控
#                       / 导出 / 备份 / 日志流 / 自检 / 会话与 Token
#
# 约定：
#   * 所有写操作先备份、再原子写、再回读校验，失败抛 ApiError（带 HTTP 状态码）；
#   * 所有返回都是可 JSON 序列化的 dict，交给 app.py 统一包 {"ok":true,"data":...}；
#   * 只读数据库（/runtime/cosmobot.sqlite3 以 mode=ro 打开），绝不改 bot 自己的表结构。
# ══════════════════════════════════════════════════════════════════════

CONFIG_TOML_PATH = os.path.join(RUNTIME, "config.toml")
MODEL_SELECTION_PATH = os.path.join(RUNTIME, "chat-model-selection")
MEMORY_ROOT = os.path.join(RUNTIME, "memory")
BACKUP_DIR = os.path.join(OPS_BASE, "backups")
CONFIG_MAX_BYTES = 512 * 1024
MEMORY_MAX_BYTES = 256 * 1024
CHAT_EXPORT_MAX_ROWS = 200000
LOG_TAIL_MAX = 2000


# ── A. 配置编辑：GET/POST /api/v2/config/toml ───────────────────────────
def _read_bytes(path: str) -> bytes:
    with open(path, "rb") as handle:
        return handle.read()


def _sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def _stamp() -> str:
    return time.strftime("%Y%m%d-%H%M%S")


def _list_runtime_backups(prefix: str, limit: int = 10) -> list[dict]:
    """列出 /runtime 下某个前缀的备份文件（新→旧）。"""
    directory = os.path.dirname(prefix)
    base = os.path.basename(prefix)
    try:
        names = os.listdir(directory)
    except OSError:
        return []
    items = []
    for name in names:
        if not name.startswith(base):
            continue
        path = os.path.join(directory, name)
        try:
            stat = os.stat(path)
        except OSError:
            continue
        items.append({"file": name, "path": path, "size": stat.st_size,
                      "mtime": int(stat.st_mtime),
                      "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime))})
    items.sort(key=lambda item: item["mtime"], reverse=True)
    return items[:max(1, limit)]


def _flatten_toml(value: object, prefix: str = "") -> dict[str, object]:
    out: dict[str, object] = {}
    if isinstance(value, dict):
        for key, item in value.items():
            key_text = str(key)
            out.update(_flatten_toml(item, prefix + "." + key_text if prefix else key_text))
    else:
        out[prefix] = value
    return out


_SECRET_KEY_HINTS = ("api_key", "apikey", "token", "secret", "password", "passwd", "authorization")


def _mask_secret_text(text: object) -> object:
    value = str(text)
    if len(value) >= 12:
        return value[:4] + "…" + value[-4:]
    return "***" if value else ""


def _mask_flat_value(key: str, value: object) -> object:
    lowered = key.lower()
    if any(hint in lowered for hint in _SECRET_KEY_HINTS) and value not in ("", None, "<missing>"):
        return _mask_secret_text(value)
    return value


def _config_diff(before: dict[str, object], after: dict[str, object], limit: int = 80) -> dict:
    keys = sorted(set(before) | set(after))
    changes = []
    for key in keys:
        old = before.get(key, "<missing>")
        new = after.get(key, "<missing>")
        if old == new:
            continue
        changes.append({"key": key, "before": _mask_flat_value(key, old), "after": _mask_flat_value(key, new)})
    sections = sorted({item["key"].split(".", 1)[0] for item in changes})
    return {"count": len(changes), "sections": sections, "changes": changes[:max(1, limit)],
            "truncated": len(changes) > limit}


def _config_parsed() -> tuple[dict | None, str]:
    if tomllib is None:
        return None, "服务端缺少 tomllib（需要 Python 3.11+）"
    try:
        raw = _read_bytes(CONFIG_TOML_PATH)
    except OSError as exc:
        return None, "读取 config.toml 失败：" + str(exc)
    try:
        parsed = tomllib.loads(raw.decode("utf-8"))
    except UnicodeDecodeError as exc:
        return None, "config.toml 不是合法 UTF-8：" + str(exc)
    except tomllib.TOMLDecodeError as exc:
        return None, "config.toml 语法错误：" + str(exc)
    return (parsed if isinstance(parsed, dict) else {}), ""


def config_toml_read() -> dict:
    """GET /api/v2/config/toml：原文 + 行数 + sha256 + 备份列表。"""
    try:
        raw = _read_bytes(CONFIG_TOML_PATH)
    except OSError as exc:
        raise ApiError("读取 config.toml 失败：" + str(exc), 500)
    try:
        text = raw.decode("utf-8")
        decode_error = ""
    except UnicodeDecodeError as exc:
        text = raw.decode("utf-8", "replace")
        decode_error = str(exc)
    parsed, parse_error = _config_parsed()
    stat = os.stat(CONFIG_TOML_PATH)
    lines = text.count("\n") + (0 if text.endswith("\n") or not text else 1)
    return {
        "path": CONFIG_TOML_PATH,
        "content": text,
        "lines": lines,
        "bytes": len(raw),
        "sha256": _sha256_bytes(raw),
        "mtime": int(stat.st_mtime),
        "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
        "mode": oct(stat.st_mode & 0o777),
        "valid": parsed is not None,
        "parse_error": decode_error or parse_error,
        "top_sections": sorted(parsed.keys()) if isinstance(parsed, dict) else [],
        "chat_providers": sorted(((parsed or {}).get("llm") or {}).get("chat_provider", {}).keys())
        if isinstance(parsed, dict) else [],
        "backups": _list_runtime_backups(CONFIG_TOML_PATH + ".bak-admin-", 10),
        "restart_required_hint": "config.toml 由 bot 启动时加载；保存后需重启 fm-cosmobot 才会生效（POST /api/v2/deploy/act action=restart）。",
    }


def _set_provider_effort(text: str, provider: str, effort: str) -> str:
    """在 TOML 原文里改某个 chat provider 的 reasoning_effort（保留其余格式/注释）。"""
    try:
        section = re.compile(r"^\s*\[\s*llm\.chat_provider\." + re.escape(provider) + r"\s*\]\s*$")
    except re.error:
        return text
    key_re = re.compile(r"^(\s*reasoning_effort\s*=\s*)(.+?)(\s*)$")
    lines = text.splitlines(keepends=True)
    in_section = False
    header_index = -1
    for index, line in enumerate(lines):
        stripped = line.strip()
        if stripped.startswith("[") and stripped.endswith("]"):
            in_section = bool(section.match(line))
            if in_section:
                header_index = index
            continue
        if in_section:
            match = key_re.match(line)
            if match:
                ending = "\n" if line.endswith("\n") else ""
                lines[index] = match.group(1) + '"' + effort + '"' + ending
                return "".join(lines)
    if header_index >= 0:
        insert_at = header_index + 1
        indent = "  "
        lines.insert(insert_at, indent + 'reasoning_effort = "' + effort + '"\n')
        return "".join(lines)
    return text


def config_toml_write(content: object, actor: str = "admin") -> dict:
    """POST /api/v2/config/toml：备份 → tomllib 校验 → 原子写 → sha256 回读校验。"""
    if tomllib is None:
        raise ApiError("服务端缺少 tomllib（需要 Python 3.11+），无法校验 TOML", 500)
    if not isinstance(content, str) or not content.strip():
        raise ApiError("content 不能为空（完整 TOML 文本）")
    try:
        raw = content.encode("utf-8")
    except UnicodeEncodeError as exc:
        raise ApiError("content 含有非法字符：" + str(exc))
    if len(raw) > CONFIG_MAX_BYTES:
        raise ApiError("配置过大：%d 字节，上限 %d 字节" % (len(raw), CONFIG_MAX_BYTES), 413)
    try:
        new_parsed = tomllib.loads(content)
    except tomllib.TOMLDecodeError as exc:
        raise ApiError("TOML 语法错误：%s" % exc)
    if not isinstance(new_parsed, dict):
        raise ApiError("TOML 顶层必须是表（table）")
    required_any = {"llm", "handler", "driver", "storage", "rpc", "memory", "tool", "log"}
    if not required_any.intersection(new_parsed.keys()):
        raise ApiError("配置缺少必要顶层段（llm/handler/driver/storage/rpc 至少一个，当前只有 %s）"
                       % (", ".join(sorted(new_parsed.keys())) or "空"))
    try:
        old_raw = _read_bytes(CONFIG_TOML_PATH)
    except OSError:
        old_raw = b""
    before_flat: dict[str, object] = {}
    if old_raw:
        try:
            before_flat = _flatten_toml(tomllib.loads(old_raw.decode("utf-8", "replace")))
        except (tomllib.TOMLDecodeError, UnicodeDecodeError):
            before_flat = {}
    new_flat = _flatten_toml(new_parsed)
    # 同一秒内多次保存时用随机后缀避免备份互相覆盖
    backup_path = CONFIG_TOML_PATH + ".bak-admin-" + _stamp() + "-" + secrets.token_hex(2)
    try:
        os.makedirs(os.path.dirname(CONFIG_TOML_PATH), exist_ok=True)
        if old_raw:
            temporary_backup = backup_path + ".tmp"
            with open(temporary_backup, "wb") as handle:
                handle.write(old_raw)
            os.chmod(temporary_backup, 0o600)
            os.replace(temporary_backup, backup_path)
    except OSError as exc:
        raise ApiError("写配置备份失败：" + str(exc), 500)
    temporary = CONFIG_TOML_PATH + ".tmp-admin-" + secrets.token_hex(4)
    try:
        with open(temporary, "w", encoding="utf-8", newline="") as handle:
            handle.write(content)
        os.chmod(temporary, 0o600)
        os.replace(temporary, CONFIG_TOML_PATH)
    except OSError as exc:
        try:
            os.unlink(temporary)
        except OSError:
            pass
        raise ApiError("写 config.toml 失败：" + str(exc) + ("（已备份：" + backup_path + "）" if old_raw else ""), 500)
    try:
        readback = _read_bytes(CONFIG_TOML_PATH)
        readback_text = readback.decode("utf-8")
        tomllib.loads(readback_text)
    except (OSError, UnicodeDecodeError, tomllib.TOMLDecodeError) as exc:
        raise ApiError("写入后回读/校验失败：%s（已保留备份：%s）" % (exc, backup_path), 500)
    sha_ok = _sha256_bytes(readback) == _sha256_bytes(raw) and readback == raw
    if not sha_ok:
        raise ApiError("写入后 sha256 校验不一致（已保留备份：%s）" % backup_path, 500)
    return {
        "path": CONFIG_TOML_PATH,
        "backup": backup_path if old_raw else "",
        "bytes": len(readback),
        "lines": readback_text.count("\n") + (0 if readback_text.endswith("\n") or not readback_text else 1),
        "sha256": _sha256_bytes(readback),
        "verified": True,
        "changed": readback != old_raw,
        "diff": _config_diff(before_flat, new_flat),
        "mode": oct(os.stat(CONFIG_TOML_PATH).st_mode & 0o777),
        "restart_required": True,
        "applied": False,
        "note": "已备份并校验；config.toml 在 bot 启动时加载，需要重启 fm-cosmobot 生效（POST /api/v2/deploy/act {\"action\":\"restart\"}）。",
    }


# ── B. 记忆 / 人设文件：/runtime/memory/** ─────────────────────────────
def _memory_clean_rel(rel_path: object) -> str:
    raw = str(rel_path or "").strip().replace("\\", "/").lstrip("/")
    parts = [part for part in raw.split("/") if part not in ("", ".")]
    if any(part == ".." for part in parts):
        raise ApiError("非法路径：不允许出现 ..")
    if any(part.lower() in (".git", "__pycache__") for part in parts):
        raise ApiError("不允许访问 .git / __pycache__")
    return "/".join(parts)


def _memory_resolve(rel_path: object, allow_empty: bool = False) -> tuple[str, str]:
    rel = _memory_clean_rel(rel_path)
    if not rel and not allow_empty:
        raise ApiError("缺少 path（相对于 /runtime/memory 的路径）")
    root_real = os.path.realpath(MEMORY_ROOT)
    target_real = os.path.realpath(os.path.join(MEMORY_ROOT, rel))
    if target_real == root_real and rel:
        raise ApiError("非法路径")
    if not (target_real == root_real or target_real.startswith(root_real + os.sep)):
        raise ApiError("路径越界：只允许 /runtime/memory/**", 403)
    if "/.git/" in target_real + os.sep or target_real.endswith(os.sep + ".git"):
        raise ApiError("不允许访问 .git", 403)
    return rel, target_real


def memory_tree(rel_path: object = "", max_entries: int = 2000, max_depth: int = 6) -> dict:
    """GET /api/v2/memory/tree：/runtime/memory 目录树（文件+大小+修改时间）。"""
    rel = _memory_clean_rel(rel_path)
    root_real = os.path.realpath(MEMORY_ROOT)
    base_real = os.path.realpath(os.path.join(MEMORY_ROOT, rel)) if rel else root_real
    if not (base_real == root_real or base_real.startswith(root_real + os.sep)):
        raise ApiError("路径越界：只允许 /runtime/memory/**", 403)
    if not os.path.isdir(base_real):
        raise ApiError("目录不存在：/runtime/memory/" + rel, 404)
    entries: list[dict] = []
    truncated = False
    queue: list[tuple[str, str, int]] = [(base_real, rel, 0)]
    while queue:
        current, current_rel, depth = queue.pop(0)
        try:
            names = os.listdir(current)
        except OSError:
            continue
        names.sort(key=lambda name: (not os.path.isdir(os.path.join(current, name)), name.lower()))
        for name in names:
            if name.lower() in (".git", "__pycache__"):
                continue
            full = os.path.join(current, name)
            child_rel = (current_rel + "/" + name) if current_rel else name
            try:
                stat = os.stat(full)
            except OSError:
                continue
            is_dir = os.path.isdir(full)
            entries.append({
                "path": child_rel,
                "name": name,
                "type": "dir" if is_dir else "file",
                "depth": depth + 1,
                "size": 0 if is_dir else stat.st_size,
                "mtime": int(stat.st_mtime),
                "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
            })
            if len(entries) >= max_entries:
                truncated = True
                break
            if is_dir and depth + 1 < max_depth:
                queue.append((full, child_rel, depth + 1))
        if truncated:
            break
    dirs = sum(1 for item in entries if item["type"] == "dir")
    return {
        "root": MEMORY_ROOT,
        "path": rel,
        "entries": entries,
        "count": len(entries),
        "dirs": dirs,
        "files": len(entries) - dirs,
        "truncated": truncated,
        "max_entries": max_entries,
        "max_depth": max_depth,
        "backup_suffix": "*.bak-admin-<时间>",
        "note": "只允许 /runtime/memory/**；.git / __pycache__ 已隐藏。",
    }


def memory_file_read(rel_path: object) -> dict:
    """GET /api/v2/memory/file?path=：读 UTF-8 文本（上限 256KB）。"""
    rel, target = _memory_resolve(rel_path)
    if os.path.isdir(target):
        raise ApiError("这是一个目录，不是文件：" + rel)
    if not os.path.isfile(target):
        raise ApiError("文件不存在：" + rel, 404)
    try:
        stat = os.stat(target)
        raw = _read_bytes(target)
    except OSError as exc:
        raise ApiError("读取文件失败：" + str(exc), 500)
    if len(raw) > MEMORY_MAX_BYTES:
        raise ApiError("文件过大：%d 字节，读写上限 %d 字节（可用数据浏览器/文件系统查看）"
                       % (len(raw), MEMORY_MAX_BYTES), 413)
    try:
        text = raw.decode("utf-8")
    except UnicodeDecodeError as exc:
        raise ApiError("不是 UTF-8 文本文件：" + str(exc), 415)
    return {
        "path": rel,
        "absolute": target,
        "content": text,
        "bytes": len(raw),
        "lines": text.count("\n") + (0 if text.endswith("\n") or not text else 1),
        "sha256": _sha256_bytes(raw),
        "mtime": int(stat.st_mtime),
        "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
        "limit_bytes": MEMORY_MAX_BYTES,
    }


def memory_file_write(rel_path: object, content: object, actor: str = "admin") -> dict:
    """POST /api/v2/memory/file：白名单路径 + 备份 + 原子写 + sha256 回读校验。"""
    rel, target = _memory_resolve(rel_path)
    if os.path.isdir(target):
        raise ApiError("目标是一个目录，不能写入：" + rel)
    if not isinstance(content, str):
        raise ApiError("content 必须是字符串")
    try:
        raw = content.encode("utf-8")
    except UnicodeEncodeError as exc:
        raise ApiError("content 含有非法字符：" + str(exc))
    if len(raw) > MEMORY_MAX_BYTES:
        raise ApiError("内容过大：%d 字节，上限 %d 字节" % (len(raw), MEMORY_MAX_BYTES), 413)
    backup_path = ""
    try:
        if os.path.isfile(target):
            # 同一秒内多次保存时用随机后缀避免备份互相覆盖
            backup_path = target + ".bak-admin-" + _stamp() + "-" + secrets.token_hex(2)
            with open(backup_path, "wb") as handle:
                handle.write(_read_bytes(target))
    except OSError as exc:
        raise ApiError("写备份失败：" + str(exc), 500)
    temporary = target + ".tmp-admin-" + secrets.token_hex(4)
    try:
        os.makedirs(os.path.dirname(target), exist_ok=True)
        with open(temporary, "w", encoding="utf-8", newline="") as handle:
            handle.write(content)
        os.replace(temporary, target)
    except OSError as exc:
        try:
            os.unlink(temporary)
        except OSError:
            pass
        raise ApiError("写文件失败：" + str(exc), 500)
    try:
        readback = _read_bytes(target)
    except OSError as exc:
        raise ApiError("写入后回读失败：" + str(exc) + "（备份：" + (backup_path or "无") + "）", 500)
    verified = readback == raw
    if not verified:
        raise ApiError("写入后 sha256 校验不一致（备份：%s）" % (backup_path or "无"), 500)
    return {
        "path": rel,
        "absolute": target,
        "backup": backup_path,
        "bytes": len(readback),
        "sha256": _sha256_bytes(readback),
        "verified": True,
        "created": not bool(backup_path),
        "applied": True,
        "note": "记忆文件由 bot 运行时直接读取，保存即生效；已保留 .bak-admin-<时间> 备份。",
    }


# ── C. 模型：GET /api/v2/models · POST /api/v2/models/switch ───────────
def _mask_api_key(value: object) -> str:
    text = str(value or "")
    if not text:
        return ""
    if len(text) >= 12:
        return text[:4] + "…" + text[-4:]
    return "***"


def _llm_sections() -> tuple[dict, dict, dict, str]:
    parsed, error = _config_parsed()
    if parsed is None:
        raise ApiError(error or "解析 config.toml 失败", 500)
    llm = parsed.get("llm") if isinstance(parsed.get("llm"), dict) else {}
    chat = llm.get("chat_provider") if isinstance(llm.get("chat_provider"), dict) else {}
    image = llm.get("image_provider") if isinstance(llm.get("image_provider"), dict) else {}
    audio = llm.get("audio_provider") if isinstance(llm.get("audio_provider"), dict) else {}
    return chat, image, audio, error


def models_overview(rpc: Callable[..., Any] | None = None) -> dict:
    """GET /api/v2/models：解析 config.toml 的聊天/图片/语音 provider + 当前选择。"""
    chat, image, audio, error = _llm_sections()
    selection = _read(MODEL_SELECTION_PATH, 4096).strip()
    rpc_models: list[dict] = []
    rpc_error = ""
    current_provider = ""
    if callable(rpc):
        try:
            status = rpc("config.model", {"action": "status"})
        except Exception as exc:  # noqa: BLE001
            status = {"ok": False, "error": str(exc)}
        if isinstance(status, dict) and status.get("ok"):
            data = status.get("data") if isinstance(status.get("data"), dict) else {}
            for item in data.get("models") or []:
                if not isinstance(item, dict):
                    continue
                rpc_models.append({
                    "provider": str(item.get("provider") or ""),
                    "model": str(item.get("model") or ""),
                    "current": bool(item.get("current")),
                    "configured_default": bool(item.get("configured_default")),
                })
                if item.get("current"):
                    current_provider = str(item.get("provider") or "")
        else:
            rpc_error = str(status.get("error") or "") if isinstance(status, dict) else "RPC 返回异常"
    if not current_provider:
        current_provider = selection
    providers = []
    for name in sorted(chat.keys()):
        profile = chat.get(name) if isinstance(chat.get(name), dict) else {}
        providers.append({
            "provider": name,
            "model": str(profile.get("model") or ""),
            "base_url": str(profile.get("base_url") or ""),
            "reasoning_effort": str(profile.get("reasoning_effort") or ""),
            "timeout": profile.get("timeout"),
            "api_key_configured": bool(profile.get("api_key")),
            "api_key_masked": _mask_api_key(profile.get("api_key")),
            "current": name == current_provider,
            "source": "config.toml",
        })
    image_providers = []
    for name in sorted(image.keys()):
        profile = image.get(name) if isinstance(image.get(name), dict) else {}
        image_providers.append({
            "provider": name,
            "model": str(profile.get("model") or ""),
            "base_url": str(profile.get("base_url") or ""),
            "protocol": str(profile.get("protocol") or ""),
            "timeout": profile.get("timeout"),
            "api_key_configured": bool(profile.get("api_key")),
            "api_key_masked": _mask_api_key(profile.get("api_key")),
        })
    audio_providers = []
    for name in sorted(audio.keys()):
        profile = audio.get(name) if isinstance(audio.get(name), dict) else {}
        audio_providers.append({
            "provider": name,
            "model": str(profile.get("model") or ""),
            "base_url": str(profile.get("base_url") or ""),
            "api_key_configured": bool(profile.get("api_key")),
            "api_key_masked": _mask_api_key(profile.get("api_key")),
        })
    current_entry = next((item for item in providers if item["provider"] == current_provider), None)
    return {
        "config_path": CONFIG_TOML_PATH,
        "selection_path": MODEL_SELECTION_PATH,
        "selection_file": {"path": MODEL_SELECTION_PATH, "content": selection, "exists": os.path.isfile(MODEL_SELECTION_PATH)},
        "current_provider": current_provider,
        "current_model": (current_entry or {}).get("model", ""),
        "current_effort": (current_entry or {}).get("reasoning_effort", ""),
        "chat_providers": providers,
        "image_providers": image_providers,
        "audio_providers": audio_providers,
        "chat_provider_names": [item["provider"] for item in providers],
        "efforts": ["low", "medium", "high"],
        "rpc": {"ok": not bool(rpc_error), "error": rpc_error, "models": rpc_models,
                "endpoint": "config.model action=status"},
        "switch_endpoint": "POST /api/v2/models/switch {provider, effort?}",
        "note": "切 model 走 RPC config.model switch（带 probe，热生效）；只改 reasoning_effort 时写 config.toml 并需要重启 bot。",
    }


def models_switch(provider: object, effort: object, rpc: Callable[..., Any] | None) -> dict:
    """POST /api/v2/models/switch：RPC 在线切换（via=rpc）或改 config.toml 的 effort（via=file，需重启）。"""
    name = str(provider or "").strip()
    if not name:
        raise ApiError("缺少 provider（例如 deepseek-flash / deepseek-flash-high）")
    chat, _, _, _ = _llm_sections()
    if name not in chat:
        raise ApiError("未知 chat provider：%s（可用：%s）" % (name, "、".join(sorted(chat)) or "无"), 400)
    profile = chat.get(name) if isinstance(chat.get(name), dict) else {}
    file_effort = str(profile.get("reasoning_effort") or "")
    wanted_effort = str(effort or "").strip().lower()
    if wanted_effort and wanted_effort not in ("low", "medium", "high"):
        raise ApiError("effort 只支持 low / medium / high")
    # 只改 effort（且和配置文件不同）→ 文件模式，必须重启
    if wanted_effort and wanted_effort != file_effort:
        try:
            text = _read_bytes(CONFIG_TOML_PATH).decode("utf-8")
        except (OSError, UnicodeDecodeError) as exc:
            raise ApiError("读取 config.toml 失败：" + str(exc), 500)
        new_text = _set_provider_effort(text, name, wanted_effort)
        if new_text == text:
            raise ApiError("在 config.toml 里找不到 provider %s 的 reasoning_effort 行，无法修改" % name, 500)
        saved = config_toml_write(new_text, "admin")
        return {
            "via": "file",
            "provider": name,
            "previous_effort": file_effort,
            "effort": wanted_effort,
            "restart_required": True,
            "applied": False,
            "saved": saved,
            "note": "reasoning_effort 已写入 config.toml（含备份+校验），需要重启 fm-cosmobot 生效。",
        }
    if not callable(rpc):
        raise ApiError("Cosmobot RPC 未启用，无法在线切换模型；可改 config.toml 后重启 bot。", 502)
    switch = rpc("config.model", {"action": "switch", "target": name})
    switch_ok = isinstance(switch, dict) and bool(switch.get("ok"))
    switch_error = str(switch.get("error") or "") if isinstance(switch, dict) else "RPC 返回异常"
    # 探测 + 选择可能超过 4s 默认超时：超时后回读状态再判断是否已经切换成功。
    current, current_model, current_effort = "", "", ""
    status = rpc("config.model", {"action": "status"})
    if isinstance(status, dict) and status.get("ok"):
        data = status.get("data") if isinstance(status.get("data"), dict) else {}
        for item in data.get("models") or []:
            if not isinstance(item, dict) or not item.get("current"):
                continue
            current = str(item.get("provider") or "")
            current_model = str(item.get("model") or "")
            break
    if not switch_ok and current != name:
        raise ApiError("在线切换模型失败：" + (switch_error or "RPC 返回失败"), 502)
    switch_data = switch.get("data") if isinstance(switch, dict) and isinstance(switch.get("data"), dict) else {}
    applied = current == name
    current_entry = next((value for key, value in chat.items() if key == current), {})
    current_effort = str((current_entry or {}).get("reasoning_effort") or "") if isinstance(current_entry, dict) else ""
    return {
        "via": "rpc",
        "provider": name,
        "effort": current_effort or file_effort,
        "selected_model": current_model or str(profile.get("model") or ""),
        "restart_required": not applied,
        "applied": applied,
        "rpc_response": {"saved": bool(switch_data.get("saved", applied)),
                         "applied": bool(switch_data.get("applied", applied)),
                         "error": switch_error},
        "verify": {"current_provider": current, "matched": applied,
                   "probe": "bot 侧 probeChatModel 通过后才会切换"},
        "note": "" if applied else "RPC 已返回但状态回读未确认切换，请到 /api/v2/models 复查。",
    }


# ── D. 群能力：代理 fm-domain /group-capability ────────────────────────
GROUP_CAPABILITIES: list[dict] = [
    {"id": "agent", "label": "AI Agent"},
    {"id": "library", "label": "文库"},
    {"id": "contest", "label": "赛文"},
    {"id": "ai_contest", "label": "AI 赛文"},
    {"id": "scores", "label": "成绩"},
    {"id": "recall", "label": "撤回"},
    {"id": "repeat", "label": "复读"},
    {"id": "poke", "label": "拍一拍"},
    {"id": "score_archive", "label": "成绩归档"},
]
GROUP_CAPABILITY_IDS = [item["id"] for item in GROUP_CAPABILITIES]


def group_capability_read(group_id: object, domain: Callable[..., Any] | None) -> dict:
    gid = str(group_id or "").strip()
    if not gid:
        raise ApiError("缺少 group_id（QQ 群号）")
    if not callable(domain):
        raise ApiError("domain 客户端未注入", 500)
    result = domain("/group-capability", urlencode({"group_id": gid}))
    if not isinstance(result, dict) or not result.get("ok"):
        error = str(result.get("error") or "") if isinstance(result, dict) else ""
        raise ApiError("读取群能力失败（fm-domain）：" + (error or "未知错误"), 502)
    data = result.get("data") if isinstance(result.get("data"), dict) else {}
    capabilities = data.get("capabilities") if isinstance(data.get("capabilities"), dict) else {}
    return {
        "group_id": gid,
        "capabilities": {key: bool(capabilities.get(key, True)) for key in GROUP_CAPABILITY_IDS},
        "raw": data,
        "catalog": GROUP_CAPABILITIES,
        "source": "domain:/group-capability",
    }


def group_capability_write(group_id: object, capability: object, enabled: object,
                           domain: Callable[..., Any] | None,
                           domain_post: Callable[..., Any] | None) -> dict:
    gid = str(group_id or "").strip()
    feature = str(capability or "").strip()
    if not gid:
        raise ApiError("缺少 group_id（QQ 群号）")
    if feature not in GROUP_CAPABILITY_IDS:
        raise ApiError("未知能力：%s（可用：%s）" % (feature, " / ".join(GROUP_CAPABILITY_IDS)), 400)
    if not isinstance(enabled, bool):
        raise ApiError("enabled 必须是布尔值 true/false")
    if not callable(domain_post):
        raise ApiError("domain POST 客户端未注入", 500)
    result = domain_post("/group-capability", {"group_id": gid, "capability": feature, "enabled": enabled})
    if not isinstance(result, dict) or not result.get("ok"):
        error = str(result.get("error") or "") if isinstance(result, dict) else ""
        raise ApiError("写入群能力失败（fm-domain）：" + (error or "未知错误"), 502)
    readback_ok = False
    verified = False
    if callable(domain):
        check = domain("/group-capability", urlencode({"group_id": gid, "capability": feature}))
        if isinstance(check, dict) and check.get("ok"):
            readback_ok = True
            payload = check.get("data") if isinstance(check.get("data"), dict) else {}
            verified = bool(payload.get("enabled")) == enabled
    if not readback_ok:
        # 回读失败时退回全量能力表再看一次
        check_all = domain("/group-capability", urlencode({"group_id": gid})) if callable(domain) else None
        if isinstance(check_all, dict) and check_all.get("ok"):
            payload = check_all.get("data") if isinstance(check_all.get("data"), dict) else {}
            capabilities = payload.get("capabilities") if isinstance(payload.get("capabilities"), dict) else {}
            verified = bool(capabilities.get(feature)) == enabled
    return {
        "group_id": gid,
        "capability": feature,
        "enabled": enabled,
        "saved": True,
        "verified": verified,
        "response": result.get("data"),
        "readback": readback_ok,
        "source": "domain:/group-capability",
        "note": "" if verified else "domain 已接受写入，但回读未确认（可能被默认继承覆盖），请刷新复查。",
    }


# ── E. 运维监控增强：宿主机 ops-agent 扩展 op ─────────────────────────
# 说明：入队函数 _ops_enqueue(op, target, params) 在下面被统一扩展（见 v3 patch），
# 队列内容只允许 id/op/target/params 四个字段 + 简单标量参数。


def ops_request(op: object, params: object = None, timeout: float = 15.0,
                target: object = "") -> dict:
    """入队 + 等待结果；代理离线/超时/执行失败都转成带状态码的 ApiError。"""
    op_text = str(op or "").strip().lower()
    if op_text not in OPS_ALLOWED:
        raise ApiError("不允许的运维操作：" + op_text, 400)
    agent = _ops_agent_state()
    if not agent.get("online"):
        raise ApiError("宿主机运维代理离线（最后心跳 %s 秒前），无法执行 op=%s。"
                       % (agent.get("age_seconds"), op_text), 503)
    started = time.time()
    request_id = _ops_enqueue(op_text, target, params)
    results = _ops_wait([request_id], timeout=max(1.0, float(timeout)))
    result = results.get(request_id)
    elapsed_ms = round((time.time() - started) * 1000)
    if result is None:
        raise ApiError("宿主机运维代理 %.1fs 内未完成 op=%s（request_id=%s；可轮询 GET /api/v2/ops/result?request_id=%s）"
                       % (timeout, op_text, request_id, request_id), 504)
    if not result.get("ok"):
        error = str(result.get("error") or "")[:400]
        # v4：宿主机代理可以带 error_status（如 cron_set 固定 501），透传成对应 HTTP 状态。
        try:
            error_status = int(result.get("error_status") or 502)
        except (TypeError, ValueError):
            error_status = 502
        if error_status < 400 or error_status > 599:
            error_status = 502
        raise ApiError("运维操作 %s 失败：%s（request_id=%s）" % (op_text, error or "未知错误", request_id),
                       error_status)
    payload = result.get("result")
    return {
        "op": op_text,
        "request_id": request_id,
        "agent_online": True,
        "elapsed_ms": elapsed_ms,
        "duration_ms": result.get("duration_ms"),
        "started_at": result.get("started_at"),
        "finished_at": result.get("finished_at"),
        "agent": result.get("agent"),
        "result": payload if isinstance(payload, dict) else {"value": payload},
    }


def ops_result_view(request_id: object) -> dict:
    rid = str(request_id or "").strip()
    if not rid:
        raise ApiError("缺少 request_id")
    if not re.fullmatch(r"[0-9A-Za-z._-]{1,80}", rid):
        raise ApiError("request_id 格式不正确")
    result = _ops_result(rid)
    agent = _ops_agent_state()
    return {
        "request_id": rid,
        "done": result is not None,
        "ok": bool(result.get("ok")) if isinstance(result, dict) else None,
        "result": result,
        "agent_online": bool(agent.get("online")),
        "agent_age_seconds": agent.get("age_seconds"),
    }


def ops_logs(tail: object = 200, since: object = "") -> dict:
    """GET /api/v2/ops/logs：docker logs fm-cosmobot --tail/--since（经宿主机代理）。"""
    tail_n = _int_param(tail, 200, 1, LOG_TAIL_MAX)
    since_text = str(since or "").strip()
    if since_text and not re.fullmatch(r"[0-9A-Za-z:._+T-]{4,64}", since_text):
        raise ApiError("since 参数格式不正确（应为 docker 支持的 RFC3339 时间戳或 unix 秒）")
    out = ops_request("logs", {"tail": tail_n, "since": since_text}, timeout=45.0)
    payload = out.get("result") or {}
    lines = payload.get("lines") if isinstance(payload.get("lines"), list) else []
    return {
        "container": payload.get("container") or "fm-cosmobot",
        "tail": tail_n,
        "since": since_text,
        "lines": lines,
        "count": len(lines),
        "truncated": bool(payload.get("truncated")),
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "source": "host-ops-agent:docker logs",
    }


def ops_metrics() -> dict:
    """GET /api/v2/ops/metrics：宿主机负载/内存/磁盘/uptime + 容器 docker stats。"""
    out = ops_request("metrics", {}, timeout=45.0)
    payload = out.get("result") or {}
    return {
        "host": payload.get("host") or {},
        "docker": payload.get("docker") or {},
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "source": "host-ops-agent",
    }


def ops_images(live: bool = False) -> dict:
    """GET /api/v2/ops/images：镜像 tag/大小/创建时间。

    默认复用后台线程缓存的 docker 快照（docker images 全量枚举在这台机器上要 ~17s），
    live=true 时才现场让宿主机代理跑 images_detail。
    """
    if not live:
        with _OPS_SNAPSHOT_LOCK:
            snapshot = dict(_OPS_SNAPSHOT.get("data") or {})
            snapshot_at = float(_OPS_SNAPSHOT.get("at") or 0.0)
        images = snapshot.get("images") if isinstance(snapshot.get("images"), list) else []
        if images:
            return {
                "source": "host-ops-agent(cache)",
                "live": False,
                "images": images,
                "count": len(images),
                "stale_seconds": round(time.time() - snapshot_at, 1) if snapshot_at else None,
                "agent_online": snapshot.get("agent_online"),
                "note": "缓存快照；加 ?live=1 可现场重新采集（约数秒）。",
            }
        # 缓存还没就绪：触发后台采集但绝不阻塞 HTTP（docker images 首采约 20 秒）。
        _ensure_ops_worker()
        _OPS_WAKE.set()
        return {
            "source": "host-ops-agent(cache-warming)",
            "live": False,
            "images": [],
            "count": 0,
            "warming": True,
            "note": "镜像缓存首次采集约 20 秒，稍后刷新本接口；需要立即拿到结果可加 ?live=1。",
        }
    out = ops_request("images_detail", {}, timeout=90.0)
    payload = out.get("result") or {}
    images = payload.get("images") if isinstance(payload.get("images"), list) else []
    return {
        "source": "host-ops-agent:images_detail",
        "live": True,
        "images": images,
        "count": len(images),
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "note": "",
    }


def ops_cron() -> dict:
    """GET /api/v2/ops/cron：宿主机 crontab -l（只读）。"""
    out = ops_request("cron", {}, timeout=45.0)
    payload = out.get("result") or {}
    lines = payload.get("lines") if isinstance(payload.get("lines"), list) else []
    return {
        "lines": lines,
        "count": len(lines),
        "raw": payload.get("raw") or "",
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "source": "host-ops-agent:crontab -l",
        "note": "只读展示，后台不提供改 crontab 的接口。",
    }


def ops_rollback(image: object) -> dict:
    """POST /api/v2/ops/rollback：校验镜像存在后返回「受保护，需人工执行」的安全计划。

    不自动重建容器（会中断 QQ 会话/需要完整 run 参数），宿主机代理只做只读校验，
    返回当前容器参数 + 人工回滚步骤，避免后台一键搞挂 bot。
    """
    image_text = str(image or "").strip()
    if not re.fullmatch(r"fm-cosmobot:runtime-[A-Za-z0-9._-]{1,90}", image_text):
        raise ApiError("image 必须形如 fm-cosmobot:runtime-<tag>（只允许回滚到 runtime 镜像）")
    out = ops_request("rollback", {"image": image_text}, timeout=60.0)
    payload = out.get("result") or {}
    return {
        "image": image_text,
        "protected": bool(payload.get("protected", True)),
        "executed": bool(payload.get("executed", False)),
        "image_exists": bool(payload.get("image_exists")),
        "image_id": payload.get("image_id") or "",
        "image_created": payload.get("image_created") or "",
        "image_size": payload.get("image_size") or "",
        "current": payload.get("current") or {},
        "manual_steps": payload.get("manual_steps") or [],
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "note": payload.get("note") or "回滚受保护：自动重建容器会中断 QQ 会话，必须人工按步骤执行。",
    }


# ── F. 导出 CSV ────────────────────────────────────────────────────────
def export_chat_csv(days: object = 7, chat: object = "", limit_rows: int = CHAT_EXPORT_MAX_ROWS) -> Any:
    """GET /api/v2/export/chat：聊天记录 CSV（分页拼接，逐块 yield str）。"""
    days_value, since, _ = _day_window(_int_param(days, 7, 1, 90))
    chat_text = str(chat or "").strip()
    limit_rows = max(1, min(CHAT_EXPORT_MAX_ROWS, int(limit_rows)))
    buffer = io.StringIO()
    writer = csv.writer(buffer, lineterminator="\n")

    def flush() -> str:
        chunk = buffer.getvalue()
        buffer.seek(0)
        buffer.truncate(0)
        return chunk

    yield "\ufeff"
    writer.writerow(["id", "recorded_at_utc", "recorded_at_local", "chat_id", "chat_kind",
                     "sender_id", "sender_username", "is_bot", "text", "image_urls"])
    yield flush()
    emitted = 0
    cursor = 0
    while emitted < limit_rows:
        sql = ("SELECT id, recorded_at, chat_id, kind_key, sender_id, sender_username, is_bot, "
               "body_text, image_urls FROM chat_log WHERE id > ? AND substr(recorded_at,1,19) >= ?")
        params: list[Any] = [cursor, since]
        if chat_text:
            sql += " AND CAST(chat_id AS TEXT) = ?"
            params.append(chat_text)
        sql += " ORDER BY id ASC LIMIT 2000"
        rows = _query(sql, tuple(params))
        if not rows:
            break
        for row in rows:
            cursor = max(cursor, int(row.get("id") or 0))
            writer.writerow([
                int(row.get("id") or 0),
                str(row.get("recorded_at") or "")[:19],
                _local_time(row.get("recorded_at")),
                str(row.get("chat_id") or ""),
                str(row.get("kind_key") or ""),
                str(row.get("sender_id") or ""),
                str(row.get("sender_username") or ""),
                1 if row.get("is_bot") else 0,
                str(row.get("body_text") or ""),
                str(row.get("image_urls") or ""),
            ])
            emitted += 1
            if emitted >= limit_rows:
                break
        yield flush()
        if len(rows) < 2000:
            break


def export_audit_csv(audit_items: list[dict], days: object = 7) -> Any:
    """GET /api/v2/export/audit：后台审计日志 CSV（最近审计窗口）。"""
    days_value = _int_param(days, 7, 1, 365)
    since = time.time() - days_value * 86400
    buffer = io.StringIO()
    writer = csv.writer(buffer, lineterminator="\n")
    yield "\ufeff"
    writer.writerow(["at_epoch", "at_local", "action", "collection", "item_id", "actor"])
    yield buffer.getvalue()
    buffer.seek(0)
    buffer.truncate(0)
    count = 0
    for item in reversed(audit_items or []):
        try:
            at = float(item.get("at") or 0)
        except (TypeError, ValueError):
            at = 0.0
        if at < since:
            continue
        writer.writerow([int(at), time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(at)),
                         str(item.get("action") or ""), str(item.get("collection") or ""),
                         str(item.get("item_id") or ""), str(item.get("actor") or "")])
        count += 1
        if count % 200 == 0:
            yield buffer.getvalue()
            buffer.seek(0)
            buffer.truncate(0)
    yield buffer.getvalue()


# ── G. 备份：/api/v2/backup/list · /api/v2/backup/create ───────────────
def _human_size(size: object) -> str:
    try:
        value = float(size)
    except (TypeError, ValueError):
        return "-"
    for unit in ("B", "KB", "MB", "GB", "TB"):
        if value < 1024 or unit == "TB":
            return ("%.0f %s" if unit == "B" else "%.1f %s") % (value, unit)
        value /= 1024.0
    return "-"


def _verify_sqlite_file(path: str) -> dict:
    result = {"ok": False, "error": "", "quick_check": "", "tables": [], "table_count": 0}
    try:
        connection = sqlite3.connect(f"file:{path}?mode=ro", uri=True, timeout=15)
        try:
            quick = connection.execute("PRAGMA quick_check").fetchone()
            result["quick_check"] = str(quick[0]) if quick else ""
            tables = [str(row[0]) for row in connection.execute(
                "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name").fetchall()]
            result["tables"] = tables
            result["table_count"] = len(tables)
            result["ok"] = result["quick_check"] == "ok"
            if not result["ok"]:
                result["error"] = "quick_check=" + result["quick_check"]
        finally:
            connection.close()
    except sqlite3.Error as exc:
        result["error"] = str(exc)
    return result


def _prune_dir_backups(prefix: str, keep: int, keep_name: str = "") -> list[str]:
    """按 mtime 保留最近 keep 个 <prefix>*.sqlite3，其它删除；返回被删文件名。"""
    try:
        names = [name for name in os.listdir(BACKUP_DIR)
                 if name.startswith(prefix) and name.endswith(".sqlite3")]
    except OSError:
        return []
    items = []
    for name in names:
        path = os.path.join(BACKUP_DIR, name)
        try:
            items.append((os.path.getmtime(path), name))
        except OSError:
            continue
    items.sort(reverse=True)
    pruned = []
    for _mtime, name in items[max(0, keep):]:
        if name == keep_name:
            continue
        try:
            os.unlink(os.path.join(BACKUP_DIR, name))
            pruned.append(name)
        except OSError:
            continue
    return pruned


def backup_list() -> dict:
    """GET /api/v2/backup/list：/data/backups/*.sqlite3 列表 + 剩余空间。"""
    items = []
    try:
        names = os.listdir(BACKUP_DIR)
    except OSError:
        names = []
    now = time.time()
    for name in names:
        if not name.endswith(".sqlite3"):
            continue
        path = os.path.join(BACKUP_DIR, name)
        try:
            stat = os.stat(path)
        except OSError:
            continue
        database = "bot" if name.startswith("cosmobot") else "domain" if name.startswith("fm-domain") else "other"
        items.append({
            "file": name,
            "path": path,
            "db": database,
            "size": stat.st_size,
            "size_human": _human_size(stat.st_size),
            "mtime": int(stat.st_mtime),
            "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
            "age_seconds": int(now - stat.st_mtime),
        })
    items.sort(key=lambda item: item["mtime"], reverse=True)
    try:
        os.makedirs(BACKUP_DIR, exist_ok=True)
        usage = shutil.disk_usage(BACKUP_DIR)
        free = usage.free
        total = usage.total
    except OSError:
        free = total = 0
    return {
        "dir": BACKUP_DIR,
        "backups": items,
        "count": len(items),
        "total_size": sum(item["size"] for item in items),
        "total_size_human": _human_size(sum(item["size"] for item in items)),
        "free_bytes": free,
        "free_human": _human_size(free),
        "disk_total_human": _human_size(total),
        "note": "bot 备份在后台容器内直接用 sqlite3 在线备份；domain 备份由宿主机 ops-agent 完成（容器里看不到 domain 数据目录）。",
    }


def backup_create_bot(actor: str = "admin") -> dict:
    """POST /api/v2/backup/create {db:"bot"}：sqlite3 在线备份 + quick_check 校验。"""
    try:
        os.makedirs(BACKUP_DIR, exist_ok=True)
    except OSError as exc:
        raise ApiError("无法创建备份目录 " + BACKUP_DIR + "：" + str(exc), 500)
    try:
        source_size = os.path.getsize(BOT_DB)
        free = shutil.disk_usage(BACKUP_DIR).free
    except OSError as exc:
        raise ApiError("读取数据库/磁盘信息失败：" + str(exc), 500)
    if free < source_size * 1.3 + 50 * 1024 * 1024:
        raise ApiError("磁盘剩余空间不足：%s，备份需要约 %s" % (_human_size(free), _human_size(source_size * 1.3)), 507)
    name = "cosmobot-" + _stamp() + ".sqlite3"
    path = os.path.join(BACKUP_DIR, name)
    started = time.time()
    try:
        source = _bot_db()
        try:
            target = sqlite3.connect(path, timeout=30)
            try:
                source.backup(target, pages=8192, sleep=0.001)
            finally:
                target.close()
        finally:
            source.close()
    except sqlite3.Error as exc:
        try:
            os.unlink(path)
        except OSError:
            pass
        raise ApiError("bot 数据库备份失败：" + str(exc), 500)
    size = os.path.getsize(path)
    verify = _verify_sqlite_file(path)
    if not verify["ok"]:
        raise ApiError("备份文件校验失败：%s（文件保留：%s）" % (verify.get("error") or "未知", path), 500)
    pruned = _prune_dir_backups("cosmobot-", 5, keep_name=name)
    return {
        "db": "bot",
        "source": BOT_DB,
        "path": path,
        "file": name,
        "size": size,
        "size_human": _human_size(size),
        "source_size": source_size,
        "duration_ms": round((time.time() - started) * 1000),
        "verify": verify,
        "verified": True,
        "pruned": pruned,
        "keep": 5,
        "note": "在线备份（sqlite3 backup API），已 quick_check 校验；超过 5 份的旧 bot 备份已自动清理。",
    }


def backup_create_domain(actor: str = "admin") -> dict:
    """POST /api/v2/backup/create {db:"domain"}：经宿主机代理备份 /opt/fm-domain/data/fm-domain.sqlite3。"""
    out = ops_request("backup_domain", {}, timeout=180.0)
    payload = out.get("result") or {}
    if not payload.get("file"):
        raise ApiError("domain 备份未返回文件信息", 500)
    file_name = str(payload.get("file") or "")
    return {
        "db": "domain",
        "source": payload.get("source") or "/opt/fm-domain/data/fm-domain.sqlite3",
        "path": os.path.join(BACKUP_DIR, file_name),
        "host_path": payload.get("path") or "",
        "file": file_name,
        "size": payload.get("size") or 0,
        "size_human": payload.get("size_human") or _human_size(payload.get("size")),
        "duration_ms": payload.get("duration_ms"),
        "verify": payload.get("verify") or {},
        "verified": bool(payload.get("verified")),
        "pruned": payload.get("pruned") or [],
        "keep": 3,
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "note": "domain 备份由宿主机 root cron 代理执行（后台容器没有 domain 数据目录），已 quick_check 校验。",
    }


# ── H. 自检矩阵内部项（HTTP 自检在 app.py 里做，这样每项都有真实 HTTP 状态码）──
def selftest_internal(domain: Callable[..., Any] | None, rpc: Callable[..., Any] | None) -> list[dict]:
    checks: list[dict] = []

    def run(name: str, fn: Callable[[], Any], required: bool = True) -> None:
        started = time.monotonic()
        detail: Any = None
        error = ""
        ok = False
        try:
            detail = fn()
            ok = True
        except Exception as exc:  # noqa: BLE001
            error = str(exc)[:400]
        checks.append({
            "name": name,
            "kind": "internal",
            "method": "INTERNAL",
            "path": "",
            "http_status": None,
            "ms": round((time.monotonic() - started) * 1000, 1),
            "ok": ok,
            "level": "ok" if ok else ("fail" if required else "warn"),
            "required": required,
            "error": error,
            "detail": detail,
        })

    def sqlite_count(table: str) -> dict:
        rows = _query(f"SELECT COUNT(*) n FROM {table}")
        if not rows:
            raise RuntimeError("查询失败（表不存在或数据库不可读）：" + table)
        return {"table": table, "rows": int(rows[0]["n"])}

    run("sqlite:chat_log", lambda: sqlite_count("chat_log"))
    run("sqlite:audit_log", lambda: sqlite_count("audit_log"))
    run("sqlite:scheduled_messages", lambda: sqlite_count("scheduled_messages"))
    run("sqlite:media_files", lambda: sqlite_count("media_files"))

    def check_config() -> dict:
        parsed, error = _config_parsed()
        if parsed is None:
            raise RuntimeError(error or "解析失败")
        llm = parsed.get("llm") if isinstance(parsed.get("llm"), dict) else {}
        return {"path": CONFIG_TOML_PATH, "top_sections": sorted(parsed.keys()),
                "chat_providers": sorted((llm.get("chat_provider") or {}).keys()),
                "bytes": os.path.getsize(CONFIG_TOML_PATH)}

    run("runtime:config.toml", check_config)

    def check_memory() -> dict:
        total = 0
        directories = 0
        for _root, dirs, files in os.walk(MEMORY_ROOT):
            dirs[:] = [name for name in dirs if name not in (".git", "__pycache__")]
            directories += len(dirs)
            total += len(files)
            if total > 50000:
                break
        if total == 0:
            raise RuntimeError("memory 目录里没有文件：" + MEMORY_ROOT)
        return {"root": MEMORY_ROOT, "files": total, "dirs": directories}

    run("runtime:memory", check_memory)

    def check_runtime_writes() -> dict:
        probe = os.path.join(RUNTIME, ".fm-admin-selftest-" + secrets.token_hex(4))
        try:
            with open(probe, "w", encoding="utf-8") as handle:
                handle.write("ok")
            os.unlink(probe)
        except OSError as exc:
            raise RuntimeError("/runtime 不可写：" + str(exc))
        return {"path": RUNTIME, "writable": True}

    run("runtime:write", check_runtime_writes, required=False)

    def check_ops_agent() -> dict:
        agent = _ops_agent_state()
        if not agent.get("online"):
            raise RuntimeError("宿主机运维代理离线：最后心跳 %s 秒前" % agent.get("age_seconds"))
        return {"online": True, "age_seconds": agent.get("age_seconds"),
                "updated_at": agent.get("updated_at"),
                "updated_at_text": agent.get("updated_at_text"),
                "started_at_text": agent.get("started_at_text"),
                "online_ttl_seconds": agent.get("online_ttl_seconds", OPS_AGENT_ONLINE_TTL),
                "version": agent.get("version"), "mode": agent.get("mode"),
                "scans": agent.get("scans"), "allowed_ops": agent.get("allowed_ops")}

    run("ops:agent-heartbeat", check_ops_agent)

    def check_ops_queue() -> dict:
        probe = os.path.join(OPS_QUEUE, ".selftest-" + secrets.token_hex(4))
        try:
            os.makedirs(OPS_QUEUE, exist_ok=True)
            with open(probe, "w", encoding="utf-8") as handle:
                handle.write("ok")
            os.unlink(probe)
        except OSError as exc:
            raise RuntimeError("运维队列不可写：" + str(exc))
        return {"queue": OPS_QUEUE, "results": OPS_RESULTS, "writable": True}

    run("ops:queue-writable", check_ops_queue)

    def check_ops_result_dir() -> dict:
        try:
            names = [name for name in os.listdir(OPS_RESULTS) if name.endswith(".json")]
        except OSError as exc:
            raise RuntimeError("读运维结果目录失败：" + str(exc))
        return {"results": OPS_RESULTS, "json_files": len(names)}

    run("ops:results", check_ops_result_dir, required=False)

    def check_backup_dir() -> dict:
        try:
            os.makedirs(BACKUP_DIR, exist_ok=True)
            usage = shutil.disk_usage(BACKUP_DIR)
            probe = os.path.join(BACKUP_DIR, ".selftest-" + secrets.token_hex(4))
            with open(probe, "w", encoding="utf-8") as handle:
                handle.write("ok")
            os.unlink(probe)
        except OSError as exc:
            raise RuntimeError("备份目录不可写：" + str(exc))
        return {"dir": BACKUP_DIR, "free_human": _human_size(usage.free), "writable": True}

    run("backup:dir-writable", check_backup_dir)

    def check_auth_file() -> dict:
        # 不 import app（app.py 以 __main__ 运行，重复 import 会造出第二个模块实例）；
        # 直接按环境变量推导 auth.json 路径读脱敏统计。
        state_path = os.environ.get("FM_ADMIN_STATE", "/data/fm-admin-state.json")
        auth_path = os.environ.get("FM_ADMIN_AUTH") or os.path.join(os.path.dirname(state_path), "auth.json")
        try:
            with open(auth_path, "r", encoding="utf-8") as handle:
                auth = json.load(handle)
        except (OSError, ValueError) as exc:
            raise RuntimeError("读取 auth.json 失败：" + str(exc))
        tokens = auth.get("tokens") or [] if isinstance(auth, dict) else []
        return {"path": auth_path, "token_count": len(tokens),
                "enabled_tokens": sum(1 for token in tokens if isinstance(token, dict) and token.get("enabled", True)),
                "admin_tokens": sum(1 for token in tokens if isinstance(token, dict)
                                    and token.get("is_admin") and token.get("enabled", True))}

    run("security:auth-file", check_auth_file, required=False)

    if callable(rpc):
        def check_rpc_snapshot() -> dict:
            result = rpc("config.snapshot")
            if not isinstance(result, dict) or not result.get("ok"):
                error = str(result.get("error") or "") if isinstance(result, dict) else ""
                raise RuntimeError("RPC config.snapshot 失败：" + (error or "未知"))
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            return {"models": len(data.get("models") or []),
                    "image_models": len(data.get("image_models") or []),
                    "triggers": len(data.get("triggers") or [])}

        run("rpc:config.snapshot", check_rpc_snapshot)

        def check_rpc_model() -> dict:
            result = rpc("config.model", {"action": "status"})
            if not isinstance(result, dict) or not result.get("ok"):
                error = str(result.get("error") or "") if isinstance(result, dict) else ""
                raise RuntimeError("RPC config.model status 失败：" + (error or "未知"))
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            models = data.get("models") if isinstance(data.get("models"), list) else []
            return {"models": len(models),
                    "current": next((item.get("provider") for item in models if item.get("current")), "")}

        run("rpc:config.model", check_rpc_model)

    if callable(domain):
        def domain_health() -> dict:
            result = domain("/health")
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /health 失败"))
            return {"data": result.get("data")}

        run("domain:/health", domain_health)

        def domain_stats() -> dict:
            result = domain("/stats")
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /stats 失败"))
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            return {key: data.get(key) for key in ("groups", "library_texts", "contest_texts", "score_records") if key in data}

        run("domain:/stats", domain_stats)

        def domain_groups() -> dict:
            result = domain("/groups")
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /groups 失败"))
            data = result.get("data")
            groups = data if isinstance(data, list) else []
            return {"groups": len(groups)}

        run("domain:/groups", domain_groups)

        def domain_capability() -> dict:
            result = domain("/group-capability", urlencode({"group_id": "216063968"}))
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /group-capability 失败"))
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            capabilities = data.get("capabilities") if isinstance(data.get("capabilities"), dict) else {}
            return {"group_id": data.get("group_id"), "capabilities": len(capabilities)}

        run("domain:/group-capability", domain_capability)

        def domain_archive() -> dict:
            result = domain("/archive/status")
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /archive/status 失败"))
            data = result.get("data") if isinstance(result.get("data"), dict) else {}
            return {"status": data.get("status"), "sources": len(data.get("sources") or {})}

        run("domain:/archive/status", domain_archive)

        def domain_library() -> dict:
            result = domain("/library/search", urlencode({"q": "", "limit": 2}))
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /library/search 失败"))
            data = result.get("data")
            return {"rows": len(data) if isinstance(data, list) else 0}

        run("domain:/library/search", domain_library)

        def domain_contest() -> dict:
            result = domain("/contest/search", urlencode({"q": "", "limit": 2}))
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /contest/search 失败"))
            data = result.get("data")
            return {"rows": len(data) if isinstance(data, list) else 0}

        run("domain:/contest/search", domain_contest)

        def domain_scores() -> dict:
            result = domain("/scores", urlencode({"limit": 2}))
            if not isinstance(result, dict) or not result.get("ok"):
                raise RuntimeError(str(result.get("error") or "domain /scores 失败"))
            data = result.get("data")
            return {"rows": len(data) if isinstance(data, list) else 0}

        run("domain:/scores", domain_scores)

        # 领域侧新接口（domain-api 已在 2026-11-15 上线；/version、/contests/list、
        # /library/list、/typing/summary、/reports/daily 视为必需项，chart-data 仍可选）。
        optional_domain = [
            ("domain:/version", "/version", "", True),
            ("domain:/contests/list", "/contests/list", "", True),
            ("domain:/library/list", "/library/list", urlencode({"limit": 2}), True),
            ("domain:/typing/summary", "/typing/summary", urlencode({"days": 7}), True),
            ("domain:/reports/daily", "/reports/daily", "", True),
            ("domain:/contest/chart-data", "/contest/chart-data", urlencode({"contest": "HB", "days": 7}), False),
        ]
        for name, path, query, required_flag in optional_domain:
            def optional_probe(path: str = path, query: str = query) -> dict:
                result = domain(path, query)
                if not isinstance(result, dict) or not result.get("ok"):
                    raise RuntimeError(str(result.get("error") or ("domain %s 失败" % path)))
                data = result.get("data")
                if isinstance(data, dict) and data.get("error") == "not found":
                    raise RuntimeError("domain 尚未实现该接口（其他线在补）")
                return {"path": path, "type": type(data).__name__,
                        "size": len(data) if isinstance(data, (list, dict)) else None}
            run(name, optional_probe, required=required_flag)

    return checks


# ── I. 通用只读 domain 代理（app.py 里做路径白名单 + query 透传）─────────
DOMAIN_PROXY_ALLOW = (
    "/novel/", "/contests/", "/contest/", "/library/", "/typing/", "/reports/",
    "/groups", "/group-state", "/group-capability", "/scores", "/stats",
    "/archive/", "/health", "/version", "/recalls", "/competition/", "/analysis/",
    "/ai-contest/", "/repeat-follow/", "/bot-guard/", "/recent_messages",
)


# ── v3 ROUTES 注册（真正分派在 app.py handle_api_v2，这里同步登记便于查表）──
ROUTES: dict[str, Callable[..., dict]] = {
    # v2（保留）
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
    # v3 新增（GET 读取器；写操作在 app.py 分派里单独接）
    "config/toml": config_toml_read,
    "memory/tree": memory_tree,
    "memory/file": memory_file_read,
    "models": models_overview,
    "group/capability": group_capability_read,
    "ops/logs": ops_logs,
    "ops/metrics": ops_metrics,
    "ops/images": ops_images,
    "ops/cron": ops_cron,
    "ops/result": ops_result_view,
    "backup/list": backup_list,
    "selftest": selftest_internal,
}

def _mask_secret_values(value):
    """把 config 里的 api_key/token/secret 打成 ***masked***（2026-11-15 QA P1）。"""
    if isinstance(value, dict):
        out = {}
        for key, item in value.items():
            if isinstance(item, (dict, list)):
                out[key] = _mask_secret_values(item)
            elif isinstance(item, str) and any(hint in str(key).lower() for hint in ("api_key", "apikey", "token", "secret", "password")):
                out[key] = "***masked***"
            else:
                out[key] = item
        return out
    if isinstance(value, list):
        return [_mask_secret_values(item) for item in value]
    return value
