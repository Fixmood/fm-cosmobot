#!/usr/bin/env python3
"""Small FM Control Center API and static file server."""

from __future__ import annotations

import json
import hashlib
import mimetypes
import os
import re
import secrets
import threading
import time
import subprocess
import tomllib
from concurrent.futures import ThreadPoolExecutor
from datetime import datetime, timedelta
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlparse, unquote
from urllib.request import Request, urlopen
from urllib.error import HTTPError


ROOT = Path(__file__).resolve().parent
STATE_PATH = Path(os.environ.get("FM_ADMIN_STATE", "/data/fm-admin-state.json"))
AUTH_PATH = Path(os.environ.get("FM_ADMIN_AUTH", str(STATE_PATH.parent / "auth.json")))
DOMAIN_URL = os.environ.get("FM_DOMAIN_URL", "http://127.0.0.1:8077").rstrip("/")
RPC_ENABLED = os.environ.get("FM_RPC_ENABLED", "false").strip().lower() in {"1", "true", "yes", "on"}
RPC_HOST = os.environ.get("FM_RPC_HOST", "127.0.0.1")
RPC_PORT = int(os.environ.get("FM_RPC_PORT", "38765"))
RPC_TOKEN = os.environ.get("FM_RPC_TOKEN", "").strip()
RPC_TIMEOUT = float(os.environ.get("FM_RPC_TIMEOUT", "4"))
RUNTIME_CONFIG = os.environ.get("FM_RUNTIME_CONFIG", "").strip()
LOCK = threading.RLock()
SESSIONS: dict[str, dict] = {}
LOGIN_FAILURES: dict[str, list[float]] = {}
SESSION_TTL = 24 * 60 * 60
LOGIN_LOCKOUT_AFTER = 5
LOGIN_LOCKOUT_SECONDS = 60
COLLECTIONS = {"groups", "triggers", "personas", "models"}
VERSION_LIMIT = 100
ERROR_LIMIT = 100
DOMAIN_READS = {
    "groups": "/groups",
    "recent-messages": "/recent_messages",
    "library": "/library/search?limit=50",
    "contests": "/contest/search?limit=50",
    "scores": "/scores?limit=50",
    "stats": "/stats",
    "archive": "/archive/status",
}


def password_hash(salt: str, password: str) -> str:
    return hashlib.sha256((salt + password).encode("utf-8")).hexdigest()


def auth_default() -> dict:
    password = secrets.token_urlsafe(12)[:16]
    salt = secrets.token_hex(16)
    value = {
        "admin_username": "admin",
        "admin_password_hash": password_hash(salt, password),
        "salt": salt,
        "tokens": [],
    }
    AUTH_PATH.parent.mkdir(parents=True, exist_ok=True)
    AUTH_PATH.write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding="utf-8")
    try:
        AUTH_PATH.chmod(0o600)
    except OSError:
        pass
    print(f"[INIT] 管理员初始密码：{password}", flush=True)
    return value


def read_auth() -> dict:
    with LOCK:
        try:
            value = json.loads(AUTH_PATH.read_text(encoding="utf-8"))
        except (OSError, ValueError, TypeError):
            value = auth_default()
        if not isinstance(value, dict):
            value = auth_default()
        value.setdefault("admin_username", "admin")
        value.setdefault("tokens", [])
        try:
            AUTH_PATH.chmod(0o600)
        except OSError:
            pass
        return value


def write_auth(value: dict) -> None:
    with LOCK:
        AUTH_PATH.parent.mkdir(parents=True, exist_ok=True)
        temporary = AUTH_PATH.with_suffix(AUTH_PATH.suffix + ".tmp")
        temporary.write_text(json.dumps(value, ensure_ascii=False, indent=2), encoding="utf-8")
        temporary.replace(AUTH_PATH)
        try:
            AUTH_PATH.chmod(0o600)
        except OSError:
            pass


def client_ip(handler: BaseHTTPRequestHandler) -> str:
    return str(handler.client_address[0]) if handler.client_address else "unknown"


def login_locked(ip: str) -> bool:
    now = time.time()
    attempts = [at for at in LOGIN_FAILURES.get(ip, []) if now - at < LOGIN_LOCKOUT_SECONDS]
    LOGIN_FAILURES[ip] = attempts
    return len(attempts) >= LOGIN_LOCKOUT_AFTER


def login_failed(ip: str) -> None:
    now = time.time()
    LOGIN_FAILURES[ip] = [at for at in LOGIN_FAILURES.get(ip, []) if now - at < LOGIN_LOCKOUT_SECONDS] + [now]


def clear_login_failures(ip: str) -> None:
    LOGIN_FAILURES.pop(ip, None)


def session_from_request(handler: BaseHTTPRequestHandler) -> dict | None:
    raw = handler.headers.get("Cookie", "")
    cookies = {}
    for part in raw.split(";"):
        if "=" in part:
            key, value = part.strip().split("=", 1)
            cookies[key] = value
    session_id = cookies.get("fm_session", "")
    with LOCK:
        session = SESSIONS.get(session_id)
        if not session:
            return None
        if time.time() - float(session.get("login_time", 0)) >= SESSION_TTL:
            SESSIONS.pop(session_id, None)
            return None
        return {**session, "session_id": session_id}


def masked_token(token: dict) -> dict:
    value = str(token.get("value", ""))
    return {**token, "value": f"{value[:4]}...{value[-4:]}" if len(value) >= 8 else "********", "token": None}


def read_state() -> dict:
    with LOCK:
        if not STATE_PATH.exists():
            return {"groups": {}, "triggers": {}, "personas": {}, "models": {}, "audit": [], "config_versions": [], "recent_errors": []}
        try:
            value = json.loads(STATE_PATH.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            value = {}
        if not isinstance(value, dict):
            value = {}
        return {
            **{name: value.get(name, {}) for name in COLLECTIONS},
            "audit": value.get("audit", [])[-200:],
            "config_versions": value.get("config_versions", [])[-VERSION_LIMIT:],
            "recent_errors": value.get("recent_errors", [])[-ERROR_LIMIT:],
        }


def write_state(state: dict) -> None:
    with LOCK:
        STATE_PATH.parent.mkdir(parents=True, exist_ok=True)
        temporary = STATE_PATH.with_suffix(STATE_PATH.suffix + ".tmp")
        temporary.write_text(json.dumps(state, ensure_ascii=False, indent=2), encoding="utf-8")
        temporary.replace(STATE_PATH)


def sanitize_error(message: object) -> str:
    """Keep telemetry useful without persisting credentials from a failed call."""
    text = re.sub(
        r"(?i)(Bearer\s+|api[_-]?key\s*=|token\s*=|password\s*=)[^\s&;,)\]}]+",
        r"\1[redacted]",
        str(message),
    )
    return text[:500]


def record_error(component: str, error: object, actor: str = "system", path: str | None = None) -> None:
    with LOCK:
        state = read_state()
        item = {
            "component": component,
            "error": sanitize_error(error),
            "actor": actor[:80] or "system",
            "at": time.time(),
        }
        if path:
            item["path"] = path
        state["recent_errors"].append(item)
        state["recent_errors"] = state["recent_errors"][-ERROR_LIMIT:]
        write_state(state)


def record_audit(action: str, collection: str, item_id: str, actor: str = "admin") -> None:
    state = read_state()
    state["audit"].append({
        "action": action,
        "collection": collection,
        "item_id": item_id,
        "actor": actor[:80] or "admin",
        "at": time.time(),
    })
    write_state(state)


def fetch_domain(path: str, query: str = "") -> dict:
    target = DOMAIN_URL + path + (f"?{query}" if query else "")
    request = Request(target, headers={"Accept": "application/json"})
    try:
        with urlopen(request, timeout=12) as response:
            return {"ok": True, "data": json.loads(response.read().decode("utf-8"))}
    except Exception as error:  # The dashboard must show partial outages instead of failing entirely.
        result = {"ok": False, "error": sanitize_error(error), "path": path}
        record_error("domain", result["error"], path=path)
        return result


def global_search(keyword: str) -> dict:
    """Search read-only domain indexes concurrently and return small result sets."""
    q = keyword.strip()[:120]
    if not q:
        return {"groups": [], "users": [], "messages": [], "library": [], "contests": [], "scores": []}
    jobs = {
        "groups": lambda: fetch_domain("/groups"),
        "messages": lambda: fetch_domain("/recent_messages", urlencode({"q": q, "limit": 20})),
        "library": lambda: fetch_domain("/library/search", urlencode({"q": q, "limit": 20})),
        "contests": lambda: fetch_domain("/contest/search", urlencode({"q": q, "limit": 20})),
        "scores": lambda: fetch_domain("/scores", urlencode({"q": q, "limit": 20})),
    }
    def items(value):
        if isinstance(value, list):
            return value
        if isinstance(value, dict):
            for key in ("items", "rows", "results", "data"):
                if isinstance(value.get(key), list):
                    return value[key]
        return []
    results = {}
    with ThreadPoolExecutor(max_workers=len(jobs)) as executor:
        futures = {name: executor.submit(job) for name, job in jobs.items()}
        for name, future in futures.items():
            try:
                result = future.result()
                results[name] = items(result.get("data")) if result.get("ok") else []
            except Exception:
                results[name] = []
    groups = [x for x in results.get("groups", []) if q.lower() in str(x.get("display_name", x.get("group_name", ""))).lower()]
    messages = results.get("messages", [])
    users = []
    seen_users = set()
    for item in messages:
        user = str(item.get("sender_name") or item.get("sender_id") or "").strip()
        if user and q.lower() in user.lower() and user not in seen_users:
            seen_users.add(user)
            users.append({"sender_name": user, "sender_id": item.get("sender_id")})
    return {
        "groups": groups[:5],
        "users": users[:5],
        "messages": [x for x in messages if q.lower() in str(x.get("text", "")).lower()][:5],
        "library": results.get("library", [])[:5],
        "contests": results.get("contests", [])[:5],
        "scores": results.get("scores", [])[:5],
    }


def fetch_domain_binary(path: str, query: str = "") -> tuple[bytes, str] | None:
    target = DOMAIN_URL + path + (f"?{query}" if query else "")
    request = Request(target, headers={"Accept": "image/png"})
    try:
        with urlopen(request, timeout=8) as response:
            return response.read(), response.headers.get_content_type()
    except Exception as error:
        record_error("domain", error)
        return None


def proxy_domain_post(path: str, payload: dict) -> dict:
    request = Request(DOMAIN_URL + path, data=json.dumps(payload).encode("utf-8"),
                      headers={"Accept": "application/json", "Content-Type": "application/json"}, method="POST")
    try:
        with urlopen(request, timeout=4) as response:
            return {"ok": True, "data": json.loads(response.read().decode("utf-8"))}
    except HTTPError as error:
        return {"ok": False, "error": f"FM Domain 返回 HTTP {error.code}。"}
    except Exception as error:
        return {"ok": False, "error": sanitize_error(error)}


def test_model_connection(payload: dict) -> dict:
    base_url = str(payload.get("base_url") or "").rstrip("/")
    model = str(payload.get("model") or "").strip()
    api_key = str(payload.get("api_key") or "").strip()
    if not base_url or not model or not api_key:
        return {"ok": False, "error": "测试连接需要 base_url、model 和 api_key。"}
    started = time.monotonic()
    request = Request(base_url + "/chat/completions", data=json.dumps({"model": model, "messages": [{"role": "user", "content": "ping"}], "max_tokens": 8}).encode(), headers={"Authorization": "Bearer " + api_key, "Content-Type": "application/json"}, method="POST")
    try:
        with urlopen(request, timeout=15) as response:
            json.loads(response.read().decode("utf-8"))
        return {"ok": True, "elapsed_ms": round((time.monotonic() - started) * 1000)}
    except HTTPError as error:
        return {"ok": False, "error": f"模型接口返回 HTTP {error.code}。"}
    except Exception as error:
        return {"ok": False, "error": sanitize_error(error)}


def compare_models(payload: dict) -> dict:
    prompt = str(payload.get("prompt") or "").strip()
    requested = payload.get("models")
    if not prompt:
        return {"ok": False, "error": "测试 prompt 不能为空。"}
    snapshot = fetch_rpc("config.snapshot")
    snapshot_data = snapshot.get("data") if snapshot.get("ok") else {}
    configured = snapshot_data.get("models", []) if isinstance(snapshot_data, dict) else []
    if isinstance(requested, list) and requested:
        names = {str(x.get("model") if isinstance(x, dict) else x) for x in requested}
        configured = [x for x in configured if str(x.get("model") or x.get("name")) in names]
    if not configured:
        return {"ok": False, "error": "当前运行时没有可用于对比的模型。"}

    def run(item):
        name = str(item.get("model") or item.get("name") or item.get("provider") or "-")
        base_url = str(item.get("base_url") or item.get("baseUrl") or "").rstrip("/")
        api_key = str(item.get("api_key") or item.get("apiKey") or "").strip()
        if not base_url or not api_key:
            return {"model": name, "ok": False, "error": "运行时配置缺少接口地址或 API Key。"}
        started = time.monotonic()
        request = Request(base_url + "/chat/completions", data=json.dumps({"model": name, "messages": [{"role": "user", "content": prompt}], "max_tokens": 512}).encode(), headers={"Authorization": "Bearer " + api_key, "Content-Type": "application/json"}, method="POST")
        try:
            with urlopen(request, timeout=30) as response:
                result = json.loads(response.read().decode("utf-8"))
            choices = result.get("choices") or []
            message = choices[0].get("message", {}) if choices else {}
            return {"model": name, "ok": True, "content": message.get("content", ""), "elapsed_ms": round((time.monotonic() - started) * 1000), "usage": result.get("usage")}
        except HTTPError as error:
            return {"model": name, "ok": False, "error": f"模型接口返回 HTTP {error.code}。", "elapsed_ms": round((time.monotonic() - started) * 1000)}
        except Exception as error:
            return {"model": name, "ok": False, "error": sanitize_error(error), "elapsed_ms": round((time.monotonic() - started) * 1000)}

    with ThreadPoolExecutor(max_workers=min(3, len(configured))) as executor:
        results = list(executor.map(run, configured[:3]))
    return {"ok": True, "results": results}


def test_persona(payload: dict) -> dict:
    persona = str(payload.get("persona") or payload.get("content") or "").strip()
    message = str(payload.get("message") or "").strip()
    if not persona or not message:
        return {"ok": False, "error": "人设内容和测试消息不能为空。"}
    snapshot = fetch_rpc("config.snapshot")
    config = snapshot.get("data") if snapshot.get("ok") else {}
    models = config.get("models", []) if isinstance(config, dict) else []
    model = next((x for x in models if isinstance(x, dict) and x.get("current")), None)
    if not isinstance(model, dict):
        model = models[0] if models and isinstance(models[0], dict) else {}
    name = str(model.get("model") or model.get("name") or model.get("provider") or "").strip()
    base_url = str(model.get("base_url") or model.get("baseUrl") or "").rstrip("/")
    api_key = str(model.get("api_key") or model.get("apiKey") or "").strip()
    if RUNTIME_CONFIG:
        try:
            with open(RUNTIME_CONFIG, "rb") as config_file:
                providers = tomllib.load(config_file).get("llm", {}).get("chat_provider", {})
            private_model = providers.get(str(model.get("provider") or ""), {})
            if isinstance(private_model, dict):
                base_url = str(private_model.get("base_url") or base_url).rstrip("/")
                api_key = str(private_model.get("api_key") or api_key).strip()
                name = str(private_model.get("model") or name).strip()
        except (OSError, tomllib.TOMLDecodeError):
            pass
    if not name or not base_url or not api_key:
        return {"ok": False, "error": "当前运行时模型缺少接口地址、模型 ID 或 API Key。"}
    started = time.monotonic()
    request = Request(base_url + "/chat/completions", data=json.dumps({
        "model": name,
        "messages": [{"role": "system", "content": persona}, {"role": "user", "content": message}],
        "max_tokens": 512,
    }).encode(), headers={"Authorization": "Bearer " + api_key, "Content-Type": "application/json"}, method="POST")
    try:
        with urlopen(request, timeout=30) as response:
            result = json.loads(response.read().decode("utf-8"))
        choices = result.get("choices") or []
        content = (choices[0].get("message") or {}).get("content", "") if choices else ""
        return {"ok": True, "model": name, "content": content, "elapsed_ms": round((time.monotonic() - started) * 1000), "usage": result.get("usage")}
    except HTTPError as error:
        return {"ok": False, "error": f"模型接口返回 HTTP {error.code}。", "elapsed_ms": round((time.monotonic() - started) * 1000)}
    except Exception as error:
        return {"ok": False, "error": sanitize_error(error), "elapsed_ms": round((time.monotonic() - started) * 1000)}


def score_user_detail(username: str) -> dict:
    """Aggregate all competition scores for one user, including legacy raw shapes."""
    result = fetch_domain("/competition/scores", urlencode({"name": username, "limit": 5000, "include_total": 1}))
    payload = result.get("data") if result.get("ok") else {}
    source_items = payload if isinstance(payload, list) else (payload.get("items", []) if isinstance(payload, dict) else [])
    ai_result = fetch_domain("/ai-contest/scores", urlencode({"name": username, "limit": 5000, "include_total": 1}))
    ai_payload = ai_result.get("data") if ai_result.get("ok") else {}
    ai_items = ai_payload if isinstance(ai_payload, list) else (ai_payload.get("items", []) if isinstance(ai_payload, dict) else [])
    for item in ai_items:
        if isinstance(item, dict):
            item = dict(item)
            item["source_group"] = "AI赛文榜"
            item["event_type"] = "AI赛文榜"
            source_items.append(item)
    def unwrap(item):
        value = item
        for key in ("raw", "source", "data", "record"):
            if isinstance(value, dict) and isinstance(value.get(key), dict):
                value = {**value, **value[key]}
        return value if isinstance(value, dict) else {}
    def text(item, *keys):
        item = unwrap(item)
        for key in keys:
            value = item.get(key)
            if value is not None and str(value).strip():
                return str(value).strip()
        return ""
    def number(item, *keys):
        value = text(item, *keys).replace("%", "").replace(",", "")
        try:
            return float(value)
        except ValueError:
            return None
    def event_name(item):
        explicit = text(item, "event_type", "competition_type")
        source = text(item, "source")
        group = text(item, "source_group", "group_name")
        value = explicit or source or group or text(item, "competition", "contest", "title", "name") or "其他赛事"
        lowered = value.lower()
        if group in {"AI每日赛文", "AI赛文榜"} or explicit == "AI赛文榜": return "AI赛文榜"
        if value in {"tiger", "虎杯"} or "tiger" in lowered: return "虎杯榜"
        if value in {"champ", "锦标赛"} or "champ" in lowered or "锦标赛" in value: return "锦标赛榜"
        if value in {"comp", "speed", "fast", "极速杯"} or "极速杯" in value: return "极速杯榜"
        if source in {"cosmobot_live", "qq_live", "cosmobot_backfill", ""} and group:
            if group == "AI每日赛文" or group == "AI赛文榜": return "AI赛文榜"
            if group in {"虎杯", "锦标赛", "极速杯"}: return group + "榜"
            return group + "群赛文榜"
        return value if value.endswith("榜") else value + "榜"
    rows = []
    needle = username.strip().lower()
    seen_records = set()
    for original in source_items:
        item = unwrap(original)
        identity = text(item, "user_name", "username", "sender_name", "name", "user_id", "sender_id")
        if needle and needle not in identity.lower():
            continue
        record_key = text(item, "record_id", "message_id", "id")
        if not record_key:
            record_key = "|".join((text(item, "ts", "received_at", "occurred_at"), text(item, "speed"), text(item, "group_id", "group_name", "source_group")))
        if record_key in seen_records:
            continue
        seen_records.add(record_key)
        speed = number(item, "speed", "best_speed", "average_speed")
        accuracy = number(item, "accuracy", "acc", "accuracy_rate", "key_rate")
        occurred = text(item, "occurred_at", "ts", "received_at", "timestamp", "time", "created_at", "score_time", "date", "competition_date")
        display_time = occurred
        try:
            stamp = float(occurred)
            if stamp > 100000000:
                display_time = time.strftime("%m-%d %H:%M", time.localtime(stamp))
        except (TypeError, ValueError):
            pass
        rows.append({
            "occurred_at": occurred, "date": display_time, "display_time": display_time,
            "event_type": event_name(item), "title": text(item, "title", "contest", "competition", "source_group") or event_name(item),
            "group_name": text(item, "group_name", "source_group", "group_id") or "-", "group_id": text(item, "group_id"),
            "speed": speed, "keystroke": number(item, "keystroke", "key", "kpm"),
            "code_length": number(item, "code_length", "codelength", "codeLength"),
            "accuracy": accuracy, "acc": accuracy,
        })
    def sort_key(row):
        try: return float(row["occurred_at"])
        except (ValueError, TypeError): return 0
    rows.sort(key=sort_key, reverse=True)
    speeds = [x["speed"] for x in rows if x["speed"] is not None and x["speed"] > 0]
    accuracies = [x["accuracy"] for x in rows if x["accuracy"] is not None and x["accuracy"] > 0]
    categories = {}
    for row in rows:
        bucket = categories.setdefault(row["event_type"], [])
        bucket.append(row)
    category_rows = [{"name": name, "attempts": len(values), "best_speed": max((x["speed"] for x in values if x["speed"] is not None), default=0), "average_speed": round(sum(x["speed"] for x in values if x["speed"] is not None) / max(1, len([x for x in values if x["speed"] is not None])), 2)} for name, values in categories.items()]
    trend = list(reversed(rows[:500]))
    return {"ok": True, "data": {
        "username": username, "attempts": len(rows), "best_speed": max(speeds, default=0),
        "average_speed": round(sum(speeds) / len(speeds), 2) if speeds else 0,
        "best_accuracy": max(accuracies, default=0), "average_accuracy": round(sum(accuracies) / len(accuracies), 2) if accuracies else 0,
        "trend": [{"date": x["date"], "speed": x["speed"] or 0, "accuracy": x["accuracy"] or 0, "event_type": x["event_type"]} for x in trend],
        "categories": category_rows, "history": rows, "total": len(rows), "best": sorted(rows, key=lambda x: x["speed"] or 0, reverse=True)[:5],
    }}


def score_user_sources(username: str) -> dict:
    detail = score_user_detail(username)
    payload = detail.get("data", {}) if isinstance(detail, dict) else {}
    categories = payload.get("categories", []) if isinstance(payload, dict) else []
    return {"ok": True, "data": {
        "username": username,
        "sources": [{"name": str(item.get("name")), "attempts": int(item.get("attempts", 0) or 0)} for item in categories if item.get("name")],
    }}


def score_user_fm_detail(username: str, days: int = 30) -> dict:
    """Read daily leaderboard rows through FM's public APIs, without score tables."""
    days = max(1, min(int(days or 30), 365))
    today = datetime.now().date()
    sources = [("champ", "锦标赛榜"), ("comp", "极速杯榜"), ("tiger", "虎杯榜")]
    sources += [
        ("极速联赛", "极速联赛群赛文榜"), ("五笔修炼基地", "五笔修炼基地群赛文榜"),
        ("帝隆", "帝隆群赛文榜"), ("梦幻打字阁", "梦幻打字阁群赛文榜"),
        ("092五笔正规闲聊群", "092五笔正规闲聊群群赛文榜"), ("倉頡之友", "倉頡之友群赛文榜"),
        ("小鹤进修班", "小鹤进修班群赛文榜"),
    ]
    jobs = []
    for offset in range(days):
        date = (today - timedelta(days=offset)).isoformat()
        for source, label in sources:
            jobs.append((date, source, label))
    def query_one(job):
        date, source, label = job
        try:
            result = fetch_domain("/competition/live", urlencode({"source": source, "date": date, "soft": 1}))
            payload = result.get("data") if result.get("ok") else {}
            rows = payload.get("rows", []) if isinstance(payload, dict) else []
            return date, label, rows
        except Exception:
            return date, label, []
    matches = []
    with ThreadPoolExecutor(max_workers=12) as executor:
        for date, label, rows in executor.map(query_one, jobs):
            for row in rows:
                if not isinstance(row, dict):
                    continue
                name = str(row.get("name") or row.get("user_name") or "").strip()
                if username.strip().lower() not in name.lower():
                    continue
                matches.append({
                    "display_time": date, "date": date, "occurred_at": date,
                    "event_type": label, "title": label, "group_name": label,
                    "speed": row.get("speed"), "keystroke": row.get("key"),
                    "code_length": row.get("code"), "accuracy": row.get("acc"), "acc": row.get("acc"),
                    "user_name": name,
                })
    # AI leaderboard is also consumed as an FM endpoint; it is not read from
    # the admin/domain score tables.
    for offset in range(days):
        date = (today - timedelta(days=offset)).isoformat()
        try:
            result = fetch_domain("/ai-contest/leaderboard", urlencode({"date": date}))
            payload = result.get("data") if result.get("ok") else {}
            rows = payload.get("rows", []) if isinstance(payload, dict) else []
            for row in rows:
                name = str(row.get("name") or row.get("user_name") or "").strip() if isinstance(row, dict) else ""
                if username.strip().lower() in name.lower():
                    matches.append({"display_time": date, "date": date, "occurred_at": date, "event_type": "AI赛文榜", "title": "AI赛文榜", "group_name": "AI赛文榜", "speed": row.get("speed"), "keystroke": row.get("key"), "code_length": row.get("code"), "accuracy": row.get("acc"), "acc": row.get("acc"), "user_name": name})
        except Exception:
            pass
    matches.sort(key=lambda row: (row["date"], row["event_type"]), reverse=True)
    speeds = [float(x["speed"]) for x in matches if str(x.get("speed", "")).replace(".", "", 1).isdigit()]
    accuracy = [float(x["accuracy"]) for x in matches if str(x.get("accuracy", "")).replace(".", "", 1).isdigit()]
    grouped = {}
    for row in matches: grouped.setdefault(row["event_type"], []).append(row)
    categories = [{"name": name, "attempts": len(rows), "best_speed": max((float(x["speed"]) for x in rows if str(x.get("speed", "")).replace(".", "", 1).isdigit()), default=0), "average_speed": round(sum(float(x["speed"]) for x in rows if str(x.get("speed", "")).replace(".", "", 1).isdigit()) / max(1, len([x for x in rows if str(x.get("speed", "")).replace(".", "", 1).isdigit()])), 2)} for name, rows in grouped.items()]
    return {"ok": True, "data": {"username": username, "attempts": len(matches), "best_speed": max(speeds, default=0), "average_speed": round(sum(speeds) / len(speeds), 2) if speeds else 0, "best_accuracy": max(accuracy, default=0), "average_accuracy": round(sum(accuracy) / len(accuracy), 2) if accuracy else 0, "total": len(matches), "trend": matches, "history": matches, "categories": categories}}


def error_stats() -> dict:
    errors = list(read_state().get("recent_errors", []))
    by_component = {}
    daily = {}
    for item in errors:
        component = str(item.get("component") or "system")
        by_component[component] = by_component.get(component, 0) + 1
        stamp = float(item.get("at", 0) or 0)
        day = time.strftime("%Y-%m-%d", time.localtime(stamp)) if stamp else "未知"
        daily[day] = daily.get(day, 0) + 1
    now = time.time()
    trend = [{"date": time.strftime("%Y-%m-%d", time.localtime(now - 86400 * i)), "count": daily.get(time.strftime("%Y-%m-%d", time.localtime(now - 86400 * i)), 0)} for i in range(6, -1, -1)]
    return {"ok": True, "data": {"by_component": [{"name": k, "count": v} for k, v in sorted(by_component.items())], "trend": trend, "items": list(reversed(errors))[:100]}}


def model_stats() -> dict:
    result = fetch_rpc("audit.recent", {"limit": 500})
    raw = result.get("data") if result.get("ok") else []
    events = raw if isinstance(raw, list) else (raw.get("entries", raw.get("items", [])) if isinstance(raw, dict) else [])
    totals = {}
    for event in events:
        value = event.get("event", event) if isinstance(event, dict) else {}
        if not isinstance(value, dict):
            continue
        model = str(value.get("model") or value.get("model_name") or value.get("provider") or "未标注")
        usage = value.get("usage") if isinstance(value.get("usage"), dict) else {}
        tokens = int(usage.get("total_tokens") or value.get("total_tokens") or 0)
        elapsed = float(value.get("elapsed_ms") or value.get("duration_ms") or value.get("latency_ms") or 0)
        item = totals.setdefault(model, {"name": model, "calls": 0, "tokens": 0, "elapsed": 0})
        item["calls"] += 1
        item["tokens"] += tokens
        item["elapsed"] += elapsed
    models = [{**x, "average_ms": round(x["elapsed"] / x["calls"]) if x["calls"] else 0} for x in totals.values()]
    return {"ok": True, "data": {"models": models, "distribution": [{"name": x["name"], "value": x["tokens"]} for x in models], "trend": []}}


def log_stream() -> dict:
    overview = observability_overview({})
    errors = overview.get("recent_errors", [])
    return {"ok": True, "data": {"items": [{**x, "level": "ERROR", "service": x.get("component", "admin")} for x in errors], "services": overview.get("domain", {})}}


def fetch_rpc(method: str, params: dict | None = None) -> dict:
    """Call one read-only RPC method without exposing the bearer token."""
    if not RPC_ENABLED:
        result = {"ok": False, "error": "Cosmobot RPC 未启用。"}
        record_error("rpc", result["error"])
        return result
    if not RPC_TOKEN:
        result = {"ok": False, "error": "Cosmobot RPC 已启用，但未配置 FM_RPC_TOKEN。"}
        record_error("rpc", result["error"])
        return result
    try:
        import websocket

        request = {"jsonrpc": "2.0", "id": "fm-admin-1", "method": method, "params": params or {}}
        connection = websocket.create_connection(
            f"ws://{RPC_HOST}:{RPC_PORT}/rpc",
            timeout=RPC_TIMEOUT,
            header=[f"Authorization: Bearer {RPC_TOKEN}"],
        )
        try:
            connection.send(json.dumps(request, ensure_ascii=False))
            response = json.loads(connection.recv())
        finally:
            connection.close()
        if "error" in response:
            error = response["error"]
            message = error.get("message", "RPC 请求失败") if isinstance(error, dict) else str(error)
            result = {"ok": False, "error": sanitize_error(message)}
            record_error("rpc", result["error"])
            return result
        return {"ok": True, "data": response.get("result")}
    except Exception as error:  # Runtime telemetry must degrade independently.
        result = {"ok": False, "error": sanitize_error(error)}
        record_error("rpc", result["error"])
        return result


# ── RPC 转发白名单 ────────────────────────────────────────────────────────
# 管理面板可用的机器人 RPC 方法。面板本身已鉴权，这里是纵深防御：
# 一个被窃取的会话 cookie 不应该等于机器人的完全控制权。
RPC_ALLOW = {
    # 会话（读）
    "chat.list_sessions", "chat.get_session", "chat.history",
    # 会话（写）
    "chat.open_session", "chat.send", "chat.rename_session", "chat.delete_session",
    "chat.fork", "chat.upload_attachment",
    # 媒体
    "media.stats", "media.get", "media.delete", "media.gc", "media.resolve_source",
    # 资源
    "resource.list", "resource.detail", "resource.rename", "resource.keep_alive",
    "resource.make_permanent", "resource.destroy", "resource.destroy_associated",
    # 并发与任务
    "concurrency.list", "concurrency.lookup", "concurrency.await", "concurrency.cancel",
    # 直发消息（机器人自有扩展方法）
    "send_direct_message",
}

# 只读的遥测类方法按前缀放行
# P3-a: 前缀白名单负责「这个方法能不能转发」，动作级策略负责「这个动作能不能做」。
#
# 踩过的坑：第一版把 config.model / config.image_model 从 RPC_ALLOW_PREFIX 里删掉，
# 想靠「不放行整个方法」来挡住危险的 add/edit。结果连内部读路径一起挡了——
# 设置页的「模型」表走的是 runtime_config_result("model") -> config.model，
# 于是页面上的模型列表变成 403。前缀该留着，拦截交给下面的动作策略。
RPC_ALLOW_PREFIX = ("audit.", "config.snapshot", "config.model", "config.image_model",
                    "config.persona", "config.trigger")


# P3-a: 配置类方法的动作级白名单。
# None = 该方法名下所有动作都不允许（表里没写的按允许处理）。
# 集合 = 只有列出的动作放行。
#
# 为什么需要这个：config.model / config.image_model 的 add / edit 要传
# base_url + api_key（机器人侧 RuntimeConfig.hs:120-131、142-149），
# 属于「能改基础设施」的操作，不该由面板的通用转发口暴露。
# 读状态（status）要留着，因为「设置」页要显示当前模型与密钥是否已配置。
RPC_ACTION_POLICY = {
    "config.model": {"status", "get", "list"},
    "config.image_model": {"status", "get", "list"},
    "config.persona": {"status", "get", "list", "set", "clear"},
    "config.trigger": {"status", "get", "list", "set", "clear"},
}


def rpc_action_allowed(method: str, params: object) -> bool:
    """配置类方法还要看动作。不在策略表里的方法不受此限。"""
    allowed = RPC_ACTION_POLICY.get(method)
    if allowed is None:
        return True
    action = ""
    if isinstance(params, dict):
        action = str(params.get("action") or "").strip().lower()
    if not action:
        # 不带 action 的调用按读处理（机器人侧默认也是 status 语义）
        return True
    return action in allowed


def rpc_method_allowed(method: str) -> bool:
    """管理面板允许转发的 RPC 方法。"""
    if method in RPC_ALLOW:
        return True
    return any(method.startswith(prefix) for prefix in RPC_ALLOW_PREFIX)


def query_value(query: dict, name: str, default: str = "") -> str:
    values = query.get(name, [])
    return str(values[-1]).strip() if values else default


def filtered_audit(query: dict) -> list[dict]:
    state = read_state()
    actor = query_value(query, "actor").lower()
    action = query_value(query, "action").lower()
    collection = query_value(query, "collection").lower()
    try:
        start = float(query_value(query, "from", query_value(query, "start", "0")) or 0)
    except ValueError:
        start = 0
    try:
        end = float(query_value(query, "to", query_value(query, "end", str(time.time()))) or time.time())
    except ValueError:
        end = time.time()
    try:
        limit = max(1, min(int(query_value(query, "limit", "200")), 200))
    except ValueError:
        limit = 200
    items = []
    for item in reversed(state["audit"]):
        try:
            item_at = float(item.get("at", 0))
        except (TypeError, ValueError):
            continue
        if not start <= item_at <= end:
            continue
        if actor and actor not in str(item.get("actor", "admin")).lower():
            continue
        if action and action not in str(item.get("action", "")).lower():
            continue
        if collection and collection not in str(item.get("collection", "")).lower():
            continue
        items.append(item)
        if len(items) >= limit:
            break
    return items


def config_sync_status(snapshot_result: dict) -> dict:
    versions = read_state()["config_versions"]
    latest = versions[-1] if versions else None
    status = {
        "ok": True,
        "last_version_id": latest.get("id") if latest else None,
        "saved": latest is not None,
        "applied": False,
        "reason": None,
    }
    if not snapshot_result.get("ok"):
        status.update(ok=False, applied=False, reason=snapshot_result.get("error", "无法读取 Cosmobot 当前配置。"))
        return status
    if latest is None:
        status["applied"] = True
        status["reason"] = "暂无后台配置变更版本。"
        return status
    current = snapshot_result.get("data")
    status["applied"] = current == latest.get("after")
    if not status["applied"]:
        status["reason"] = "已保存配置与 Cosmobot 当前快照不一致，可能尚未应用或已被其他来源修改。"
    return status


# ── P2: 把并发条目分成「常驻服务 / 底层噪声 / 真正的活」 ──────────────────
# 机器人把 scheduler.worker、qq.connection 这类常驻服务也注册进并发表，
# 它们永远不写 finishedAt，所以「未结束」不等于「在运行」。
# ⚠️ 机器人常驻 worker 的标签分隔符**不统一**：多数是点号（media.gc / qq.connection），
# 但 Bot/Resource.hs 里那个回收 worker 登记的是 "resource expiry" —— **空格**。
# 这里以前写死 resource\.expiry，于是它永远落不进 services：它没有 finishedAt，
# 启动满 5 分钟后就被算成「疑似卡住」，并且混进 running。
# 后果是仪表盘长期红字「疑似卡住 1 · 超过 5 分钟未结束」，健康分 issues 里
# 也多一条假的「有 1 个任务卡住超过 5 分钟」——错的数字看着精确，最危险。
#
# 常驻 worker 的完整名单（机器人源码里 withWorker 的字面量，共 5 处）：
#   qq.connection        media.gc        scheduler.worker
#   discord.gateway      resource expiry  ← 空格，就是上面那个
# 加新常驻服务前，重新数一遍，别只改这一处：
#   grep -rn 'withWorker' /opt/fm-cosmobot/source/cosmobot/lib/
# 这里 resource 用 [.\s] 两种分隔符都认；discord 也一并放进来
# （Discord 现在没配置、标签不会出现，但配了就同样是「永远不结束」的 worker）。
SERVICE_LABEL = re.compile(
    r"^(main\.|rpc|scheduler|message\.|qq|stream\.|media\.gc|resource[.\s]expiry"
    r"|discord\.|db\.|http|metrics|health|watchdog)",
    re.IGNORECASE,
)
NOISE_LABEL = re.compile(
    r"^(command|stdout|stderr|shell|exec|pty|spawn|pipe|log|trace|debug|flush)",
    re.IGNORECASE,
)
STUCK_AFTER_SECONDS = 5 * 60
# 只把最近这么多条带出中控；统计仍基于全量。
TASK_ENTRIES_LIMIT = 100
WORK_WINDOW_SECONDS = 24 * 3600


def parse_task_time(value) -> float:
    if not value:
        return 0.0
    try:
        return datetime.fromisoformat(str(value).replace("Z", "+00:00")).timestamp()
    except (TypeError, ValueError):
        return 0.0


def classify_task_entries(entries: list) -> dict:
    """Return honest counts. "Unfinished" is not the same as "running"."""
    services, noise, work = [], [], []
    for item in entries:
        if not isinstance(item, dict):
            continue
        label = str(item.get("label") or "")
        if SERVICE_LABEL.match(label):
            services.append(item)
        elif NOISE_LABEL.match(label):
            noise.append(item)
        else:
            work.append(item)

    now = time.time()
    busy = [item for item in work if not item.get("finishedAt")]
    stuck = [
        item for item in busy
        if (started := parse_task_time(item.get("startedAt")))
        and now - started > STUCK_AFTER_SECONDS
    ]
    recent = [
        item for item in work
        if (started := parse_task_time(item.get("startedAt")))
        and now - started <= WORK_WINDOW_SECONDS
    ]
    return {
        "total": len(entries),
        "services": len(services),
        "noise": len(noise),
        "running": len(busy),
        "stuck": len(stuck),
        "done_recent": len([item for item in recent if item.get("finishedAt")]),
        "failed_recent": len([item for item in recent if item.get("error")]),
    }


def config_state_from_snapshot(snapshot, domain_stats) -> dict:
    """Counts that actually mean something.

    Was: len(read_state()[name]) for name in COLLECTIONS — that counted the admin
    panel's own local collections, which are empty by default, so the dashboard
    always showed 群 0 / 人格 0 / 模型 0.
    """
    data = snapshot.get("data") if isinstance(snapshot, dict) else None
    data = data if isinstance(data, dict) else {}

    def size(key):
        value = data.get(key)
        return len(value) if isinstance(value, list) else 0

    groups = 0
    stats = domain_stats.get("data") if isinstance(domain_stats, dict) else None
    if isinstance(stats, dict):
        groups = int(stats.get("groups") or 0)
    personas = size("group_personas") + size("private_personas") + size("member_styles")
    return {
        "groups": groups,
        "personas": personas,
        "models": size("models") + size("image_models"),
        "triggers": size("triggers"),
    }


def observability_overview(query: dict) -> dict:
    """Collect independent component states; one outage must not abort the overview."""
    jobs = {
        "domain_health": lambda: fetch_domain("/health"),
        "domain_stats": lambda: fetch_domain("/stats"),
        "rpc_snapshot": lambda: fetch_rpc("config.snapshot"),
        "rpc_tasks": lambda: fetch_rpc("concurrency.list"),
    }
    results = {}
    with ThreadPoolExecutor(max_workers=len(jobs)) as executor:
        futures = {name: executor.submit(job) for name, job in jobs.items()}
        for name, future in futures.items():
            try:
                results[name] = future.result()
            except Exception as error:
                component = "rpc" if name.startswith("rpc_") else "domain"
                record_error(component, error)
                results[name] = {"ok": False, "error": sanitize_error(error)}
    domain_health = results["domain_health"]
    domain_stats = results["domain_stats"]
    rpc_snapshot = results["rpc_snapshot"]
    rpc_tasks = results["rpc_tasks"]
    task_data = rpc_tasks.get("data") if rpc_tasks.get("ok") else None
    entries = task_data.get("entries", []) if isinstance(task_data, dict) else (task_data if isinstance(task_data, list) else [])
    errors = list(reversed(read_state()["recent_errors"]))[:20]
    return {
        "ok": True,
        "generated_at": time.time(),
        "config_sync": config_sync_status(rpc_snapshot),
        "rpc": {"ok": rpc_snapshot["ok"] and rpc_tasks["ok"], "reason": None if rpc_snapshot["ok"] and rpc_tasks["ok"] else (rpc_snapshot.get("error") or rpc_tasks.get("error"))},
        "domain": {"ok": domain_health["ok"] and domain_stats["ok"], "reason": None if domain_health["ok"] and domain_stats["ok"] else (domain_health.get("error") or domain_stats.get("error"))},
        "domain_stats": domain_stats,
        # P2: running 以前是 len(entries)——那是全部条目数（实测 2343），
        # 而真正在跑的只有个位数。现在按标签分类后再统计。
        "tasks": {
            "ok": rpc_tasks["ok"],
            # 统计在全量上算，但只带出最近若干条。
            # 机器人这张表是只增不减的流水（实测每小时 +59 条），
            # 全量转发会让这个接口的响应随时间无限膨胀（现在已 567 KB）。
            **classify_task_entries(entries),
            "entries": entries[-TASK_ENTRIES_LIMIT:],
            "entries_total": len(entries),
            "reason": None if rpc_tasks["ok"] else rpc_tasks.get("error"),
        },
        "recent_errors": errors,
        "audit": filtered_audit(query),
        # P2: 以前这里是中控自己 state 文件的条目数，跟机器人和 domain 都无关，
        # 所以永远显示全零。改成从机器人配置快照与 domain 统计里算。
        "state": config_state_from_snapshot(rpc_snapshot, domain_stats),
        # P2: rpc_snapshot 本来就取到了，只是没放进返回。前端靠它显示真实的人格/模型。
        "snapshot": (rpc_snapshot.get("data") if rpc_snapshot.get("ok") else None),
    }


def health_score() -> dict:
    overview = observability_overview({})
    errors = overview.get("recent_errors", [])
    recent = [x for x in errors if time.time() - float(x.get("at", 0) or 0) <= 3600]
    domain_ok = bool(overview.get("domain", {}).get("ok"))
    rpc_ok = bool(overview.get("rpc", {}).get("ok"))
    sync = overview.get("config_sync", {})
    # P2: 以前只看 saved —— 只要存过版本就给满分，哪怕 applied 是假的。
    # 「后台显示的配置」与「机器人实际在跑的配置」不一致时不能算健康。
    config_points = 15 if (sync.get("saved") and sync.get("applied")) else 8 if sync.get("saved") else 0
    tasks_ok = overview.get("tasks", {}).get("ok")
    tasks_stuck = int(overview.get("tasks", {}).get("stuck") or 0)
    score = (30 if domain_ok else 0) + (25 if len(recent) <= 2 else 15 if len(recent) <= 10 else 0) + (20 if rpc_ok else 0) + config_points + (10 if tasks_ok else 0)
    issues = []
    if not domain_ok: issues.append({"issue":"FM Domain 离线","suggestion":"检查 Domain 容器和数据库连接。"})
    if not rpc_ok: issues.append({"issue":"Cosmobot RPC 不可用","suggestion":"检查 RPC 服务和令牌配置。"})
    if len(recent) > 2: issues.append({"issue":f"最近 1 小时有 {len(recent)} 条错误","suggestion":"查看运行状态和审计日志定位失败请求。"})
    if sync.get("saved") and not sync.get("applied"):
        issues.append({
            "issue": "后台保存的配置与机器人当前配置不一致",
            "suggestion": "到「设置 → 配置版本」比对，确认改动是否真的生效，必要时回滚。",
        })
    if tasks_stuck:
        issues.append({
            "issue": f"有 {tasks_stuck} 个任务卡住超过 5 分钟",
            "suggestion": "到仪表盘「机器人状态」查看并取消。",
        })
    return {"ok": True, "score": score, "issues": issues, "dimensions":{"service":30 if domain_ok else 0,"errors":25 if len(recent)<=2 else 15 if len(recent)<=10 else 0,"rpc":20 if rpc_ok else 0,"config":config_points,"processing":10 if tasks_ok else 0}}


def runtime_config_result(kind: str, payload: dict | None = None) -> dict:
    if kind not in {"persona", "trigger", "model", "image_model"}:
        return {"ok": False, "error": "运行时配置类型不存在。"}
    method = f"config.{kind}"
    params = payload or {"action": "status"}
    # P3-a: 这里以前直接 fetch_rpc()，把白名单整个绕过去了。
    # PUT/DELETE /api/runtime/config/{kind} 走的就是这个函数，所以
    # 「白名单里没放 config.persona」并不妨碍用它改人设。
    if not rpc_method_allowed(method) or not rpc_action_allowed(method, params):
        return {"ok": False, "error": f"该配置操作未被允许：{method}"}
    return fetch_rpc(method, params)


def safe_payload(payload: object) -> object:
    if isinstance(payload, dict):
        return {key: "[redacted]" if key.lower() in {"api_key", "token", "password"} else safe_payload(value) for key, value in payload.items()}
    if isinstance(payload, list):
        return [safe_payload(value) for value in payload]
    return payload


def config_diff(before: object, after: object, path: str = "") -> list[dict]:
    if isinstance(before, dict) and isinstance(after, dict):
        changes = []
        for key in sorted(set(before) | set(after)):
            child = f"{path}.{key}" if path else str(key)
            changes.extend(config_diff(before.get(key), after.get(key), child))
        return changes
    if isinstance(before, list) and isinstance(after, list):
        if before == after:
            return []
        return [{"path": path, "before": before, "after": after}]
    if before != after:
        return [{"path": path, "before": before, "after": after}]
    return []


def save_config_version(before: dict, after: dict, kind: str, payload: dict, actor: str) -> str | None:
    changes = config_diff(before, after)
    if not changes:
        return None
    version_id = f"cfg-{int(time.time() * 1000)}-{secrets.token_hex(3)}"
    state = read_state()
    state["config_versions"].append({
        "id": version_id,
        "at": time.time(),
        "actor": actor,
        "kind": kind,
        "operation": payload.get("action", "update"),
        "request": safe_payload(payload),
        "before": before,
        "after": after,
        "changes": changes,
    })
    state["config_versions"] = state["config_versions"][-VERSION_LIMIT:]
    write_state(state)
    return version_id


def snapshot_value(snapshot: dict, kind: str, request: dict) -> object:
    action = request.get("action")
    if kind == "persona":
        scope = request.get("scope")
        if scope == "private_default": return snapshot.get("private_default")
        if scope == "group_default": return snapshot.get("group_default")
        collection = {"private": "private_personas", "group": "group_personas", "member": "member_styles"}.get(scope)
        if collection:
            return next((item.get("content") for item in snapshot.get(collection, []) if str(item.get("id")) == str(request.get("id"))), None)
    if kind == "trigger":
        return next((item.get("config") for item in snapshot.get("triggers", []) if item.get("scope") == request.get("scope")), None)
    if kind == "model":
        return next((item for item in snapshot.get("models", []) if item.get("provider") == request.get("target") or item.get("model") == request.get("target")), None)
    if kind == "image_model":
        return next((item for item in snapshot.get("image_models", []) if item.get("provider") == request.get("target") or item.get("model") == request.get("target")), None)
    return None


def rollback_payload(version: dict) -> tuple[str, dict] | tuple[None, str]:
    kind = version.get("kind")
    request = version.get("request") or {}
    before = version.get("before") or {}
    if kind == "persona":
        content = snapshot_value(before, kind, request)
        payload = dict(request)
        payload["action"] = "clear" if content is None else "set"
        if content is not None:
            payload["content"] = content
        return "persona", payload
    if kind == "trigger":
        config = snapshot_value(before, kind, request)
        if config is None:
            return "trigger", {"action": "clear", "scope": request.get("scope", "")}
        return "trigger", {"action": "set", "scope": request.get("scope", ""), "modes": config.get("modes", []), "keywords": config.get("keywords", [])}
    if kind in {"model", "image_model"}:
        action = request.get("action")
        if action in {"switch", "reset"}:
            collection = "image_models" if kind == "image_model" else "models"
            previous = next((item for item in before.get(collection, []) if item.get("current")), None)
            if previous and previous.get("provider"):
                return "model", {"action": "switch", "target": previous["provider"]}
            return None, "回滚模型选择失败：版本中没有可恢复的当前模型。"
        return None, "模型配置变更的版本不保存 API Key，无法安全完整恢复；当前版本仍保留，可手动修复。"
    return None, "不支持的配置版本类型。"


def validate_item(item: object) -> tuple[dict | None, str | None]:
    if not isinstance(item, dict):
        return None, "请求体必须是 JSON 对象。"
    item_id = str(item.get("id", "")).strip()
    if not item_id:
        return None, "请求体必须包含非空 id。"
    clean = dict(item)
    clean["id"] = item_id
    clean["updated_at"] = time.time()
    return clean, None


class Api(BaseHTTPRequestHandler):
    server_version = "FMControl/0.1"

    def log_message(self, _format: str, *_args: object) -> None:
        return

    def authorized(self) -> bool:
        return session_from_request(self) is not None

    def current_session(self) -> dict | None:
        return session_from_request(self)

    def require_auth(self, api: bool = True) -> bool:
        if self.authorized():
            return True
        if api:
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "未登录或登录已过期，请先登录。")
        else:
            self.send_response(HTTPStatus.FOUND)
            self.send_header("Location", "/login")
            self.end_headers()
        return False

    def require_admin(self) -> bool:
        if not self.require_auth():
            return False
        session = self.current_session()
        if not session or not session.get("is_admin"):
            self.send_error_json(HTTPStatus.FORBIDDEN, "只读用户无权执行修改操作。")
            return False
        return True

    def handle_login(self) -> None:
        ip = client_ip(self)
        if login_locked(ip):
            self.send_error_json(HTTPStatus.TOO_MANY_REQUESTS, "登录失败次数过多，请 1 分钟后重试。")
            return
        payload = self.read_json()
        if not isinstance(payload, dict):
            login_failed(ip)
            self.send_error_json(HTTPStatus.BAD_REQUEST, "登录请求格式不正确。")
            return
        auth = read_auth()
        login_type = str(payload.get("type", "admin"))
        username = str(payload.get("username", ""))
        is_admin = False
        token_id = None
        valid = False
        if login_type == "admin":
            password = str(payload.get("password", ""))
            valid = secrets.compare_digest(username, str(auth.get("admin_username", "admin"))) and secrets.compare_digest(
                password_hash(str(auth.get("salt", "")), password), str(auth.get("admin_password_hash", ""))
            )
            is_admin = valid
        elif login_type == "token":
            supplied = str(payload.get("token", ""))
            for token in auth.get("tokens", []):
                expires = token.get("expires_at")
                expired = bool(expires and float(expires) <= time.time())
                if token.get("enabled", True) and not expired and secrets.compare_digest(supplied, str(token.get("value", ""))):
                    valid = True
                    username = str(token.get("name") or "token-user")
                    token_id = token.get("id")
                    break
        else:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "不支持的登录方式。")
            return
        if not valid:
            login_failed(ip)
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "用户名、密码或 Token 不正确。")
            return
        clear_login_failures(ip)
        session_id = secrets.token_hex(16)
        with LOCK:
            SESSIONS[session_id] = {"username": username, "login_time": time.time(), "is_admin": is_admin, "token_id": token_id}
        self.send_response(HTTPStatus.OK)
        self.send_header("Set-Cookie", f"fm_session={session_id}; Path=/; HttpOnly; SameSite=Lax; Max-Age={SESSION_TTL}")
        self.send_header("Content-Type", "application/json; charset=utf-8")
        body = json.dumps({"ok": True, "username": username, "is_admin": is_admin}, ensure_ascii=False).encode("utf-8")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def handle_token_list(self) -> None:
        session = self.current_session()
        if not session or not session.get("is_admin"):
            self.send_error_json(HTTPStatus.FORBIDDEN, "只有管理员可以管理 Token。")
            return
        self.send_json(HTTPStatus.OK, {"ok": True, "items": [masked_token(item) for item in read_auth().get("tokens", [])]})

    def handle_token_create(self) -> None:
        session = self.current_session()
        if not session or not session.get("is_admin"):
            self.send_error_json(HTTPStatus.FORBIDDEN, "只有管理员可以管理 Token。")
            return
        payload = self.read_json()
        name = str(payload.get("name", "")).strip() if isinstance(payload, dict) else ""
        if not name:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "Token 名称不能为空。")
            return
        expires_at = payload.get("expires_at") if isinstance(payload, dict) else None
        if expires_at:
            try:
                expires_at = float(expires_at)
                if expires_at <= time.time():
                    raise ValueError
            except (TypeError, ValueError):
                self.send_error_json(HTTPStatus.BAD_REQUEST, "过期时间必须是未来时间戳。")
                return
        token = secrets.token_hex(16)
        item = {"id": secrets.token_hex(16), "value": token, "name": name[:120], "created_at": time.time(), "created_by": session["username"], "expires_at": expires_at, "enabled": True}
        auth = read_auth()
        auth.setdefault("tokens", []).append(item)
        write_auth(auth)
        record_audit("create", "tokens", item["id"], session["username"])
        self.send_json(HTTPStatus.OK, {"ok": True, "token": token, "item": masked_token(item)})

    def handle_token_revoke(self, token_id: str) -> None:
        session = self.current_session()
        if not session or not session.get("is_admin"):
            self.send_error_json(HTTPStatus.FORBIDDEN, "只有管理员可以管理 Token。")
            return
        auth = read_auth()
        item = next((x for x in auth.get("tokens", []) if str(x.get("id")) == token_id), None)
        if item is None:
            self.send_error_json(HTTPStatus.NOT_FOUND, "Token 不存在。")
            return
        item["enabled"] = False
        item["revoked_at"] = time.time()
        write_auth(auth)
        record_audit("revoke", "tokens", token_id, session["username"])
        self.send_json(HTTPStatus.OK, {"ok": True})

    def handle_password_change(self) -> None:
        session = self.current_session()
        payload = self.read_json()
        old_password = str(payload.get("old_password", "")) if isinstance(payload, dict) else ""
        new_password = str(payload.get("new_password", "")) if isinstance(payload, dict) else ""
        confirm_password = str(payload.get("confirm_password", "")) if isinstance(payload, dict) else ""
        auth = read_auth()
        if not session or not session.get("is_admin"):
            self.send_error_json(HTTPStatus.FORBIDDEN, "只有管理员可以修改密码。")
            return
        if not secrets.compare_digest(password_hash(str(auth.get("salt", "")), old_password), str(auth.get("admin_password_hash", ""))):
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "旧密码不正确。")
            return
        if len(new_password) < 8:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "新密码至少需要 8 位。")
            return
        if new_password != confirm_password:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "两次输入的新密码不一致。")
            return
        salt = secrets.token_hex(16)
        auth["salt"] = salt
        auth["admin_password_hash"] = password_hash(salt, new_password)
        write_auth(auth)
        record_audit("change_password", "auth", session["username"], session["username"])
        self.send_json(HTTPStatus.OK, {"ok": True})

    def send_json(self, status: int, payload: object) -> None:
        encoded = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

    def send_bytes(self, status: int, payload: bytes, content_type: str) -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        self.wfile.write(payload)

    def send_error_json(self, status: int, message: str) -> None:
        self.send_json(status, {"ok": False, "error": message})

    def read_json(self) -> object:
        try:
            length = int(self.headers.get("Content-Length", "0"))
            return json.loads(self.rfile.read(length).decode("utf-8"))
        except (ValueError, json.JSONDecodeError, UnicodeDecodeError):
            return None

    def handle_rpc_proxy(self) -> None:
        """转发白名单内的 RPC 方法给机器人，供管理面板使用。"""
        payload = self.read_json()
        if not isinstance(payload, dict):
            self.send_error_json(HTTPStatus.BAD_REQUEST, "请求体必须是 JSON 对象。")
            return
        method = str(payload.get("method", "")).strip()
        params = payload.get("params")
        if not method:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "缺少 method。")
            return
        if not isinstance(params, dict):
            params = {}
        if not rpc_method_allowed(method):
            record_audit("rpc-blocked", "rpc", method, "admin")
            self.send_error_json(HTTPStatus.FORBIDDEN, "方法未在允许列表中：" + method)
            return
        # P3-a: 配置类方法还要看动作。config.model / config.image_model 的
        # add / edit 需要传 base_url + api_key，属于改基础设施，不放行。
        if not rpc_action_allowed(method, params):
            action = str(params.get("action") or "")
            record_audit("rpc-blocked", "rpc", f"{method}:{action}", "admin")
            self.send_error_json(
                HTTPStatus.FORBIDDEN,
                f"该动作未被允许：{method} action={action or '(空)'}",
            )
            return
        result = fetch_rpc(method, params)
        record_audit("rpc", "rpc", method, "admin")
        self.send_json(HTTPStatus.OK if result.get("ok") else HTTPStatus.BAD_GATEWAY, result)

    def do_GET(self) -> None:
        request = urlparse(self.path)
        if request.path == "/login":
            self.send_static("login.html")
            return
        if request.path == "/" and not self.authorized():
            self.require_auth(api=False)
            return
        if request.path.startswith("/static/"):
            self.send_static(request.path.removeprefix("/static/"))
            return
        if request.path == "/api/health":
            self.send_json(HTTPStatus.OK, {"ok": True, "service": "fm-control-center", "auth_required": True})
        elif request.path == "/api/auth/me":
            session = self.current_session()
            if not session:
                self.send_error_json(HTTPStatus.UNAUTHORIZED, "未登录或登录已过期，请先登录。")
            else:
                self.send_json(HTTPStatus.OK, {"ok": True, "username": session["username"], "is_admin": bool(session.get("is_admin"))})
        elif request.path == "/api/auth/tokens":
            self.handle_token_list()
        elif not self.require_auth():
            return
        elif request.path == "/api/search":
            keyword = parse_qs(request.query).get("q", [""])[0]
            self.send_json(HTTPStatus.OK, {"ok": True, "data": global_search(keyword)})
        elif request.path == "/api/overview":
            self.send_json(HTTPStatus.OK, {
                "ok": True,
                "generated_at": time.time(),
                "domain": fetch_domain("/health"),
                "stats": fetch_domain("/stats"),
                "archive": fetch_domain("/archive/status"),
                # P2: 同上，改成从机器人快照与 domain 统计算，而不是中控本地集合。
                # 这个分支里没有 stats / rpc_snapshot 变量，所以自己取一次。
                "state": config_state_from_snapshot(
                    fetch_rpc("config.snapshot"),
                    fetch_domain("/stats"),
                ),
            })
        elif request.path == "/api/observability/overview":
            self.send_json(HTTPStatus.OK, observability_overview(parse_qs(request.query)))
        elif request.path == "/api/health/score":
            self.send_json(HTTPStatus.OK, health_score())
        elif request.path == "/api/errors/stats":
            self.send_json(HTTPStatus.OK, error_stats())
        elif request.path == "/api/logs/stream":
            self.send_json(HTTPStatus.OK, log_stream())
        elif request.path == "/api/models/stats":
            self.send_json(HTTPStatus.OK, model_stats())
        elif request.path == "/api/dashboard/advanced":
            domain_result = fetch_domain("/stats/advanced")
            audit_result = fetch_rpc("audit.recent", {"limit": 500})
            self.send_json(HTTPStatus.OK, {"ok": True, "domain": domain_result, "audit": audit_result})
        elif request.path.startswith("/api/domain/scores/user/fm/"):
            username = unquote(request.path.removeprefix("/api/domain/scores/user/fm/").strip("/"))
            try:
                days = min(max(int(parse_qs(request.query).get("days", ["30"])[0]), 1), 365)
            except ValueError:
                days = 30
            self.send_json(HTTPStatus.OK, score_user_fm_detail(username, days))
        elif request.path == "/api/domain/user/score-sources":
            username = parse_qs(request.query).get("username", [""])[0].strip()
            self.send_json(HTTPStatus.OK, score_user_sources(username))
        elif request.path.startswith("/api/domain/scores/user/"):
            username = unquote(request.path.removeprefix("/api/domain/scores/user/").strip("/"))
            self.send_json(HTTPStatus.OK, score_user_detail(username))
        elif request.path == "/api/logs":
            items = filtered_audit(parse_qs(request.query))
            self.send_json(HTTPStatus.OK, {"ok": True, "items": items, "total": len(items)})
        elif request.path.startswith("/api/domain/"):
            name = request.path.removeprefix("/api/domain/")
            if name.startswith("reports/"):
                result = fetch_domain_binary("/reports/" + name.removeprefix("reports/"), request.query)
                if result is None:
                    self.send_error_json(HTTPStatus.SERVICE_UNAVAILABLE, "数据服务暂时不可用：报表接口无法访问。")
                else:
                    self.send_bytes(HTTPStatus.OK, result[0], result[1] or "image/png")
            else:
                path = DOMAIN_READS.get(name, "/" + name)
                result = fetch_domain(path, request.query)
                if isinstance(result.get("data"), list):
                    result["items"] = result["data"]
                    result["total"] = int(result.get("total", len(result["data"])))
                self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.SERVICE_UNAVAILABLE, result)
        elif request.path == "/api/runtime/audit":
            result = fetch_rpc("audit.recent", {"limit": 50})
            if isinstance(result.get("data"), list):
                result["items"] = result["data"]
                result["total"] = int(result.get("total", len(result["data"])))
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
        elif request.path == "/api/runtime/media":
            result = fetch_rpc("media.stats", {"limit": 20})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
        elif request.path == "/api/runtime/concurrency":
            result = fetch_rpc("concurrency.list")
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
        elif request.path == "/api/runtime/config":
            result = fetch_rpc("config.snapshot")
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
        elif request.path == "/api/runtime/config/models":
            result = fetch_rpc("config.model", {"action": "status"})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, {"ok": True, "items": (result.get("data") or {}).get("models", [])} if result["ok"] else result)
        elif request.path == "/api/runtime/config/image_models":
            result = fetch_rpc("config.image_model", {"action": "status"})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, {"ok": True, "items": (result.get("data") or {}).get("models", [])} if result["ok"] else result)
        elif request.path == "/api/runtime/config/versions":
            self.send_json(HTTPStatus.OK, {"ok": True, "items": list(reversed(read_state()["config_versions"]))})
        elif request.path.startswith("/api/runtime/config/versions/") and request.path.endswith("/diff"):
            version_id = request.path.removeprefix("/api/runtime/config/versions/").removesuffix("/diff").strip("/")
            version = next((item for item in read_state()["config_versions"] if item["id"] == version_id), None)
            if version is None:
                self.send_error_json(HTTPStatus.NOT_FOUND, "配置版本不存在。")
            else:
                self.send_json(HTTPStatus.OK, {"ok": True, "version_id": version_id, "changes": version.get("changes", [])})
        elif request.path.startswith("/api/runtime/config/versions/"):
            version_id = request.path.removeprefix("/api/runtime/config/versions/").strip("/")
            version = next((item for item in read_state()["config_versions"] if item["id"] == version_id), None)
            if version is None:
                self.send_error_json(HTTPStatus.NOT_FOUND, "配置版本不存在。")
            else:
                self.send_json(HTTPStatus.OK, {"ok": True, "version": version})
        elif request.path.startswith("/api/collections/"):
            self.collection_get(request.path.removeprefix("/api/collections/"), parse_qs(request.query))
        elif request.path == "/":
            self.send_static("index.html")
        elif request.path.startswith("/static/"):
            self.send_static(request.path.removeprefix("/static/"))
        elif request.path == "/favicon.ico":
            self.send_error_json(HTTPStatus.NOT_FOUND, "资源不存在。")
        else:
            self.send_error_json(HTTPStatus.NOT_FOUND, "接口不存在。")

    def collection_get(self, raw_path: str, _query: dict) -> None:
        parts = [part for part in raw_path.split("/") if part]
        if not parts or parts[0] not in COLLECTIONS or len(parts) > 2:
            self.send_error_json(HTTPStatus.NOT_FOUND, "管理资源不存在。")
            return
        items = read_state()[parts[0]]
        if len(parts) == 1:
            self.send_json(HTTPStatus.OK, {"ok": True, "items": list(items.values())})
        else:
            item = items.get(parts[1])
            if item is None:
                self.send_error_json(HTTPStatus.NOT_FOUND, "记录不存在。")
            else:
                self.send_json(HTTPStatus.OK, {"ok": True, "item": item})

    def do_POST(self) -> None:
        request = urlparse(self.path)
        if request.path == "/api/login":
            self.handle_login()
            return
        if request.path == "/api/logout":
            session = self.current_session()
            if session:
                with LOCK:
                    SESSIONS.pop(session["session_id"], None)
            body = json.dumps({"ok": True}, ensure_ascii=False).encode("utf-8")
            self.send_response(HTTPStatus.OK)
            self.send_header("Set-Cookie", "fm_session=; Path=/; HttpOnly; SameSite=Lax; Max-Age=0")
            self.send_header("Content-Type", "application/json; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return
        if request.path == "/api/auth/tokens":
            if not self.require_admin():
                return
            self.handle_token_create()
            return
        if request.path == "/api/auth/password":
            if not self.require_admin():
                return
            self.handle_password_change()
            return
        if request.path == "/api/rpc":
            if not self.require_admin():
                return
            self.handle_rpc_proxy()
            return
        if not self.require_admin():
            return
        if request.path == "/api/send-message":
            payload = self.read_json()
            if not isinstance(payload, dict):
                self.send_error_json(HTTPStatus.BAD_REQUEST, "请求体必须是 JSON 对象。")
                return
            required = ["target_type", "target_id", "content"]
            if any(not str(payload.get(key, "")).strip() for key in required):
                self.send_error_json(HTTPStatus.BAD_REQUEST, "target_type、target_id 和 content 不能为空。")
                return
            rpc_payload = {
                "platform": str(payload.get("platform", "qq")),
                "target_type": str(payload["target_type"]),
                "target_id": str(payload["target_id"]),
                "content": str(payload["content"]),
                "media": [str(x) for x in payload.get("media", []) if str(x).strip()],
            }
            result = fetch_rpc("send_direct_message", rpc_payload)
            if result["ok"]:
                record_audit("send", "message", f'{rpc_payload["target_type"]}:{rpc_payload["target_id"]}', "admin")
                self.send_json(HTTPStatus.OK, result)
            else:
                self.send_json(HTTPStatus.BAD_GATEWAY, result)
            return
        if request.path == "/api/models/test":
            payload = self.read_json()
            result = test_model_connection(payload if isinstance(payload, dict) else {})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_REQUEST, result)
            return
        if request.path == "/api/models/compare":
            payload = self.read_json()
            result = compare_models(payload if isinstance(payload, dict) else {})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_REQUEST, result)
            return
        if request.path == "/api/persona/test":
            payload = self.read_json()
            result = test_persona(payload if isinstance(payload, dict) else {})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_REQUEST, result)
            return
        if request.path.startswith("/api/system/restart/"):
            service = request.path.removeprefix("/api/system/restart/").strip("/")
            allowed = {"admin": "fm-admin", "domain": "fm-domain", "cosmobot": "fm-cosmobot"}
            if service not in allowed:
                self.send_error_json(HTTPStatus.BAD_REQUEST, "不支持的服务名称。")
                return
            try:
                subprocess.run(["docker", "restart", allowed[service]], check=True, timeout=30, capture_output=True)
                self.send_json(HTTPStatus.OK, {"ok": True, "service": service})
            except Exception as error:
                self.send_error_json(HTTPStatus.BAD_GATEWAY, sanitize_error(error))
            return
        if request.path in {"/api/domain/group-state", "/api/domain/group-capability", "/api/domain/repeat-follow"}:
            payload = self.read_json()
            if not isinstance(payload, dict):
                self.send_error_json(HTTPStatus.BAD_REQUEST, "请求体必须是 JSON 对象。")
                return
            domain_path = {"/api/domain/group-state": "/group-state", "/api/domain/group-capability": "/group-capability", "/api/domain/repeat-follow": "/repeat-follow/state"}[request.path]
            result = proxy_domain_post(domain_path, payload)
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.SERVICE_UNAVAILABLE, result)
            return
        if request.path.startswith("/api/runtime/config/"):
            kind = request.path.removeprefix("/api/runtime/config/").strip("/")
            if kind == "rollback":
                self.runtime_config_rollback()
            else:
                self.runtime_config_write(kind)
        elif request.path.startswith("/api/collections/"):
            self.collection_write(request.path.removeprefix("/api/collections/"), replace=False)
        else:
            self.send_error_json(HTTPStatus.NOT_FOUND, "接口不存在。")

    def do_PUT(self) -> None:
        request = urlparse(self.path)
        if not self.require_admin():
            return
        if request.path.startswith("/api/runtime/config/"):
            self.runtime_config_write(request.path.removeprefix("/api/runtime/config/"))
        elif request.path.startswith("/api/collections/"):
            self.collection_write(request.path.removeprefix("/api/collections/"), replace=True)
        else:
            self.send_error_json(HTTPStatus.NOT_FOUND, "接口不存在。")

    def runtime_config_write(self, raw_kind: str) -> None:
        kind = raw_kind.strip("/")
        payload = self.read_json()
        if not isinstance(payload, dict):
            self.send_error_json(HTTPStatus.BAD_REQUEST, "运行时配置请求必须是 JSON 对象。")
            return
        before_result = fetch_rpc("config.snapshot")
        result = runtime_config_result(kind, payload)
        if result["ok"] and before_result["ok"]:
            after_result = fetch_rpc("config.snapshot")
            if after_result["ok"]:
                actor = self.headers.get("X-FM-Admin-Actor", "admin").strip()[:80] or "admin"
                version_id = save_config_version(before_result["data"], after_result["data"], kind, payload, actor)
                if version_id:
                    result["version_id"] = version_id
        if not result["ok"]:
            record_error("config_sync", result.get("error", "运行时配置同步失败。"), self.headers.get("X-FM-Admin-Actor", "admin"))
        elif isinstance(result.get("data"), dict) and result["data"].get("applied") is False:
            record_error("config_sync", result["data"].get("reason", "配置保存成功但尚未生效。"), self.headers.get("X-FM-Admin-Actor", "admin"))
        self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)

    def runtime_config_rollback(self) -> None:
        payload = self.read_json()
        version_id = payload.get("version_id") if isinstance(payload, dict) else None
        version = next((item for item in read_state()["config_versions"] if item["id"] == version_id), None)
        if version is None:
            self.send_error_json(HTTPStatus.NOT_FOUND, "配置版本不存在。")
            return
        kind, restore = rollback_payload(version)
        if kind is None:
            self.send_error_json(HTTPStatus.CONFLICT, restore)
            return
        before_result = fetch_rpc("config.snapshot")
        if not before_result["ok"]:
            self.send_json(HTTPStatus.BAD_GATEWAY, before_result)
            return
        result = runtime_config_result(kind, restore)
        if not result["ok"]:
            self.send_json(HTTPStatus.BAD_GATEWAY, result)
            return
        after_result = fetch_rpc("config.snapshot")
        if not after_result["ok"]:
            record_error("config_sync", "回滚已执行，但无法确认当前生效状态。", self.headers.get("X-FM-Admin-Actor", "admin"))
            self.send_json(HTTPStatus.BAD_GATEWAY, {"ok": False, "error": "回滚已执行，但无法确认当前生效状态。"})
            return
        actor = self.headers.get("X-FM-Admin-Actor", "admin").strip()[:80] or "admin"
        rollback_record = {
            "id": f"cfg-{int(time.time() * 1000)}-{secrets.token_hex(3)}",
            "at": time.time(), "actor": actor, "kind": kind, "operation": "rollback",
            "target_version_id": version_id, "request": safe_payload(restore),
            "before": before_result["data"], "after": after_result["data"],
            "changes": config_diff(before_result["data"], after_result["data"]),
        }
        state = read_state()
        state["config_versions"].append(rollback_record)
        state["config_versions"] = state["config_versions"][-VERSION_LIMIT:]
        write_state(state)
        result["version_id"] = rollback_record["id"]
        self.send_json(HTTPStatus.OK, result)

    def collection_write(self, raw_path: str, replace: bool) -> None:
        parts = [part for part in raw_path.split("/") if part]
        if len(parts) != 1 and not (replace and len(parts) == 2):
            self.send_error_json(HTTPStatus.BAD_REQUEST, "新增使用 /api/collections/{resource}，修改使用 /api/collections/{resource}/{id}。")
            return
        collection = parts[0]
        if collection not in COLLECTIONS:
            self.send_error_json(HTTPStatus.NOT_FOUND, "管理资源不存在。")
            return
        item, error = validate_item(self.read_json())
        if error:
            self.send_error_json(HTTPStatus.BAD_REQUEST, error)
            return
        state = read_state()
        target_id = parts[1] if replace else item["id"]
        if replace and target_id not in state[collection]:
            self.send_error_json(HTTPStatus.NOT_FOUND, "记录不存在。")
            return
        if not replace and target_id in state[collection]:
            self.send_error_json(HTTPStatus.CONFLICT, "id 已存在，请使用 PUT 修改。")
            return
        item["id"] = target_id
        state[collection][target_id] = item
        write_state(state)
        actor = self.headers.get("X-FM-Admin-Actor", "admin").strip()[:80] or "admin"
        record_audit("update" if replace else "create", collection, target_id, actor)
        self.send_json(HTTPStatus.OK, {"ok": True, "item": item})

    def do_DELETE(self) -> None:
        request = urlparse(self.path)
        if request.path.startswith("/api/auth/tokens/"):
            if not self.require_admin():
                return
            self.handle_token_revoke(request.path.removeprefix("/api/auth/tokens/").strip("/"))
            return
        if not self.require_admin():
            return
        if request.path.startswith("/api/runtime/config/"):
            kind = request.path.removeprefix("/api/runtime/config/").strip("/")
            result = runtime_config_result(kind, {"action": "clear"})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
            return
        parts = [part for part in request.path.removeprefix("/api/collections/").split("/") if part]
        if len(parts) != 2 or parts[0] not in COLLECTIONS:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "删除需要提供资源名称和 id。")
            return
        state = read_state()
        if state[parts[0]].pop(parts[1], None) is None:
            self.send_error_json(HTTPStatus.NOT_FOUND, "记录不存在。")
            return
        write_state(state)
        actor = self.headers.get("X-FM-Admin-Actor", "admin").strip()[:80] or "admin"
        record_audit("delete", parts[0], parts[1], actor)
        self.send_json(HTTPStatus.OK, {"ok": True, "deleted": parts[1]})

    def send_static(self, name: str, head_only: bool = False) -> None:
        candidate = (ROOT / "static" / name).resolve()
        if candidate.parent != (ROOT / "static").resolve() or not candidate.is_file():
            self.send_error_json(HTTPStatus.NOT_FOUND, "页面不存在。")
            return
        body = candidate.read_bytes()
        content_types = {
            ".html": "text/html; charset=utf-8",
            ".css": "text/css; charset=utf-8",
            ".js": "application/javascript; charset=utf-8",
            ".json": "application/json; charset=utf-8",
            ".png": "image/png",
            ".jpg": "image/jpeg",
            ".jpeg": "image/jpeg",
            ".gif": "image/gif",
            ".svg": "image/svg+xml",
        }
        content_type = content_types.get(candidate.suffix.lower()) or mimetypes.guess_type(candidate.name)[0] or "application/octet-stream"
        if content_type.startswith("text/") and "charset=" not in content_type:
            content_type += "; charset=utf-8"
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", content_type)
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if not head_only:
            self.wfile.write(body)

    def do_HEAD(self) -> None:
        request = urlparse(self.path)
        if request.path == "/":
            if not self.authorized():
                self.send_response(HTTPStatus.FOUND)
                self.send_header("Location", "/login")
                self.end_headers()
            else:
                self.send_static("index.html", head_only=True)
        elif request.path == "/login":
            self.send_static("login.html", head_only=True)
        elif request.path.startswith("/static/"):
            self.send_static(request.path.removeprefix("/static/"), head_only=True)
        else:
            self.send_error(HTTPStatus.NOT_FOUND)


def main() -> None:
    host = os.environ.get("FM_ADMIN_HOST", "127.0.0.1")
    port = int(os.environ.get("FM_ADMIN_PORT", "8090"))
    read_auth()
    print(f"FM Control Center listening on http://{host}:{port}", flush=True)
    ThreadingHTTPServer((host, port), Api).serve_forever()


if __name__ == "__main__":
    main()
