"""FM 后台 v2 API：一次性补齐「能看能控」所需的数据接口。

设计：
- 已有的 /api/*（overview / logs / models / collections / rpc / domain 代理）继续复用；
- 这里只补缺的：总览聚合、群详情、拟人模式读写、成员反查、消息搜索、
  工具调用审计与统计、番茄小说书架。
- 全部只读为主，唯一写操作是拟人模式的 scopes / proactive 配置（文件格式保持
  bot 侧读取格式不变）。
"""
from __future__ import annotations

import json
import os
import sqlite3
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
    "groups": groups_overview,
    "group": group_detail,
    "novels": novels,
    "novel": novel_detail,
    "browse/tables": browse_tables,
    "members/all": all_members,
    "actions": action_catalog,
}
