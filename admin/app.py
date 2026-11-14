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
# v4：全接口限流（按登录 session 计数；令牌桶 = 平均 RATE_LIMIT_PER_MIN 次/分钟，
# 突发容量 RATE_LIMIT_BURST）。默认 60 次/分钟 + 突发 120，环境变量可调，
# 自检/内部探针带 X-FM-Selftest: 1 头不计数。
RATE_LIMIT_PER_MIN = max(1, int(os.environ.get("FM_ADMIN_RATE_PER_MIN", "60") or 60))
RATE_LIMIT_BURST = max(
    RATE_LIMIT_PER_MIN,
    int(os.environ.get("FM_ADMIN_RATE_BURST", str(RATE_LIMIT_PER_MIN * 2)) or (RATE_LIMIT_PER_MIN * 2)),
)
RATE_BUCKETS: dict[str, dict] = {}
RATE_LOCK = threading.RLock()
RATE_BUCKET_MAX_KEYS = 500
RATE_LIMITED_TOTAL = {"count": 0}
# v4：慢接口日志阈值（>2s 记 error，进 /api/errors/stats；SSE/CSV 流式接口不计）
SLOW_REQUEST_MS = float(os.environ.get("FM_ADMIN_SLOW_MS", "2000") or 2000)
SLOW_PATH_SKIP = ("logs/stream", "export/", "selftest")
COLLECTIONS = {"groups", "triggers", "personas", "models"}
VERSION_LIMIT = 100
ERROR_LIMIT = 100
DOMAIN_READS = {
    "groups": "/groups",
    # v3: recent-messages 不再转发 domain（domain 没有 /recent_messages 路由，历史 404）；
    # do_GET 里改为直接查 bot sqlite 的 chat_log（数据更真实、也避免 503）。
    "library": "/library/search",
    "contests": "/contest/search",
    "scores": "/scores",
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


# ══ v4：限流（令牌桶，按 session id / IP）════════════════════════════
def rate_limit_config() -> dict:
    return {
        "per_min": RATE_LIMIT_PER_MIN,
        "burst": RATE_LIMIT_BURST,
        "slow_ms": SLOW_REQUEST_MS,
        "bucket_keys": len(RATE_BUCKETS),
        "key_max": RATE_BUCKET_MAX_KEYS,
        "limited_total": int(RATE_LIMITED_TOTAL.get("count") or 0),
        "env": {"FM_ADMIN_RATE_PER_MIN": os.environ.get("FM_ADMIN_RATE_PER_MIN"),
                "FM_ADMIN_RATE_BURST": os.environ.get("FM_ADMIN_RATE_BURST"),
                "FM_ADMIN_SLOW_MS": os.environ.get("FM_ADMIN_SLOW_MS")},
        "note": "每个登录 session 平均 60 次/分钟（突发 120 个令牌），超出返回 429 + Retry-After；"
                "自检内部探针带 X-FM-Selftest: 1 头免计数。",
    }


def rate_limit_check(key: str, cost: float = 1.0) -> tuple[bool, int, int]:
    """令牌桶：返回 (是否放行, 建议重试秒数, 剩余令牌)。"""
    now = time.time()
    rate = RATE_LIMIT_PER_MIN / 60.0
    with RATE_LOCK:
        bucket = RATE_BUCKETS.get(key)
        if bucket is None:
            if len(RATE_BUCKETS) >= RATE_BUCKET_MAX_KEYS:
                # 简单清理：丢掉两个小时没动过的桶
                stale = [name for name, item in RATE_BUCKETS.items()
                         if now - float(item.get("at") or 0) > 7200]
                for name in stale:
                    RATE_BUCKETS.pop(name, None)
                if len(RATE_BUCKETS) >= RATE_BUCKET_MAX_KEYS:
                    RATE_BUCKETS.clear()
            bucket = {"tokens": float(RATE_LIMIT_BURST), "at": now}
            RATE_BUCKETS[key] = bucket
        elapsed = max(0.0, now - float(bucket.get("at") or now))
        bucket["tokens"] = min(float(RATE_LIMIT_BURST), float(bucket.get("tokens") or 0.0) + elapsed * rate)
        bucket["at"] = now
        if bucket["tokens"] >= cost:
            bucket["tokens"] -= cost
            return True, 0, int(bucket["tokens"])
        needed = (cost - float(bucket["tokens"])) / rate
        RATE_LIMITED_TOTAL["count"] = int(RATE_LIMITED_TOTAL.get("count") or 0) + 1
        return False, max(1, int(needed + 0.999)), 0


def rate_limit_key_for(session: dict | None, handler: BaseHTTPRequestHandler) -> str:
    if session and session.get("session_id"):
        return "session:" + str(session["session_id"])
    if session and session.get("username"):
        return "user:" + str(session.get("username"))
    return "ip:" + client_ip(handler)


def kick_admin_session(ident: object, current: dict | None = None, allow_self: bool = False) -> dict:
    """DELETE /api/v2/sessions/{id}：踢掉一个后台登录会话。

    id 支持三种写法：完整 session id、GET /api/v2/sessions 返回的脱敏形式
    （abcdef…wxyz，中间是 U+2026）、或前后缀片段。默认不能踢自己（?self=1 放开）。
    """
    text = str(ident or "").strip()
    if not text:
        raise ValueError("缺少 session id（用 GET /api/v2/sessions 的 session_id，脱敏值也行）")
    current_id = str((current or {}).get("session_id") or "")
    plain = text.replace("\u2026", "...")

    def matches(session_id: str) -> bool:
        if text in (session_id, _mask_session_id(session_id)):
            return True
        if "..." in plain:
            prefix, _, suffix = plain.partition("...")
            return bool(prefix) and session_id.startswith(prefix) and (not suffix or session_id.endswith(suffix))
        if len(text) >= 8 and (session_id.startswith(text) or session_id.endswith(text)):
            return True
        return False

    with LOCK:
        candidates = list(SESSIONS.keys())
    matched = [session_id for session_id in candidates if matches(session_id)]
    kicked: list[dict] = []
    skipped: list[dict] = []
    with LOCK:
        for session_id in matched:
            if session_id == current_id and not allow_self:
                skipped.append({"session_id": _mask_session_id(session_id), "reason": "这是当前会话"})
                continue
            session = SESSIONS.pop(session_id, None)
            if session is None:
                continue
            kicked.append({
                "session_id": _mask_session_id(session_id),
                "username": str(session.get("username") or ""),
                "is_admin": bool(session.get("is_admin")),
                "login_at_text": time.strftime("%Y-%m-%d %H:%M:%S",
                                               time.localtime(float(session.get("login_time") or 0)))
                if session.get("login_time") else "",
            })
        remaining = len(SESSIONS)
    return {
        "requested": text if len(text) <= 40 else text[:40],
        "matched": len(matched),
        "kicked": kicked,
        "count": len(kicked),
        "skipped": skipped,
        "remaining": remaining,
        "note": "被踢的会话立即失效（cookie 仍在浏览器里但服务端已删）。可用 ?self=1 强制踢当前会话。",
    }


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
    # 既有 bug：DOMAIN_READS 里有些 path 自带 query（如 /library/search?limit=50），
    # 再拼 "?" + query 会变成 "...?limit=50?limit=2"，domain 侧 int("50?limit=2") 直接抛异常 → 503。
    # 所以 path 已带 ? 时用 & 续接。
    target = DOMAIN_URL + path
    if query:
        target += ("&" if "?" in path else "?") + query
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
    # 同 fetch_domain：path 自带 query 时必须用 & 续接，否则 domain 收到两个 "?"。
    target = DOMAIN_URL + path
    if query:
        target += ("&" if "?" in path else "?") + query
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


def fetch_rpc(method: str, params: dict | None = None, timeout: float | None = None) -> dict:
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
            timeout=(float(timeout) if timeout is not None else RPC_TIMEOUT),
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
    # v3: 允许 switch —— 「模型切换」页用它在线切 provider。bot 侧会先
    # probeChatModel（真实发一条 "Reply with exactly: OK"）再落盘，且只接受
    # 配置文件里已存在的 provider 名，不会引入新的 base_url / api_key。
    "config.model": {"status", "get", "list", "switch"},
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


def _int_arg(value: object, default: int = 0) -> int:
    """查询参数转整数；非法值回退默认值，避免 v2 接口因为 ?days=abc 直接 500。"""
    try:
        return int(str(value).strip())
    except (TypeError, ValueError):
        return default


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



# ==================== 当前功能与状态（/now） ====================
# 2026-11-14 所有者要求：后台跟上现在的功能。只读聚合，不写任何状态。

def _read_text(path: str, limit: int = 4096) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return handle.read(limit)
    except OSError:
        return ""


def _parse_toml_scalars(text: str) -> dict:
    """按 section 取值：聊天模型要取 [llm.chat_provider.*] 那段，别抓到生图模型。"""
    wanted = ("model", "reasoning_effort", "agent_max_turns",
              "recent_chat_context_max_chars", "recent_chat_context_limit",
              "system_prompt", "command", "name")
    values = {}
    section = ""
    for line in text.splitlines():
        stripped = line.strip()
        if not stripped or stripped.startswith("#"):
            continue
        if stripped.startswith("[") and stripped.endswith("]"):
            section = stripped[1:-1].strip()
            continue
        if "=" not in stripped:
            continue
        key, _, raw = stripped.partition("=")
        key = key.strip()
        raw = raw.strip()
        if raw.startswith('"') and raw.endswith('"'):
            raw = raw[1:-1]
        if key not in wanted:
            continue
        # 聊天模型只在 llm.chat_provider.* 段里生效；image_provider 的 model 不要覆盖
        if key == "model" and not section.startswith("llm.chat_provider"):
            continue
        if key == "reasoning_effort" and not section.startswith("llm.chat_provider"):
            continue
        values[key] = raw
    return values


def _sqlite_counts() -> dict:
    import sqlite3
    path = "/runtime/cosmobot.sqlite3"
    result = {"total": 0, "bot_messages": 0, "senders": 0, "last_at": "", "top_groups": []}
    try:
        conn = sqlite3.connect(f"file:{path}?mode=ro", uri=True, timeout=5)
    except Exception:
        return result
    try:
        result["total"] = conn.execute("SELECT COUNT(*) FROM chat_log").fetchone()[0]
        result["bot_messages"] = conn.execute("SELECT COUNT(*) FROM chat_log WHERE is_bot=1").fetchone()[0]
        result["senders"] = conn.execute("SELECT COUNT(DISTINCT sender_id) FROM chat_log WHERE is_bot=0").fetchone()[0]
        row = conn.execute("SELECT MAX(recorded_at) FROM chat_log").fetchone()
        result["last_at"] = str(row[0])[:19] if row and row[0] else ""
        for chat_id, count in conn.execute(
            "SELECT chat_id, COUNT(*) c FROM chat_log GROUP BY chat_id ORDER BY c DESC LIMIT 5"
        ):
            result["top_groups"].append({"id": str(chat_id), "count": count})
    except Exception:
        pass
    finally:
        conn.close()
    return result


NOW_CAPABILITIES = [
    "拟人模式（人设 / 分段气泡 / 打字节奏）",
    "群门控（不被@也能接话，续聊窗口 120s）",
    "主动发言（每天上限、静默时段、模型决定说不说）",
    "表情包（face→emoji、mface 图片）",
    "长任务预告（第一次调慢工具时一句）",
    "聊天记录全文搜索（chat_search，所有成员）",
    "成员反查（昵称/QQ → 全部曾用昵称）",
    "跟打文库（发一篇 / 继续 / 难度 / 计数）",
    "赛文成绩图（官网优先、成绩库兜底、宽图）",
    "积分/成绩统计（均速、键准、排行、曲线）",
    "番茄小说（书架、续读、跳章、单章成绩）",
    "记忆（persona / 成员风格 / 聊天记忆 / 便签）",
    "触发方式（prefix / mention / keyword / 名字 / 回复）",
    "部署锁与双会话协同（_session_bridge）",
]

NOW_COMMANDS = [
    "fm 看下我近一个月的群赛文成绩曲线",
    "fm 我的555有多少条",
    "fm 生图：一只橘猫",
    "fm 翻翻聊天记录有没有提过虎码",
    "fm 弘辰杰是谁",
    "fm 帮我查下今天有什么 AI 新闻",
]


def now_snapshot() -> dict:
    import os
    import time

    config_path = "/runtime/config.toml"
    config = _parse_toml_scalars(_read_text(config_path, 200000))
    try:
        config_mtime = time.strftime("%Y-%m-%d %H:%M", time.localtime(os.path.getmtime(config_path)))
    except OSError:
        config_mtime = ""

    # 拟人会话清单：优先 scopes.txt；现在实际写在 proactive.txt 的第一列
    scopes = [line.strip() for line in _read_text("/runtime/humanlike/scopes.txt").splitlines() if line.strip()]
    proactive_raw = _read_text("/runtime/humanlike/proactive.txt", 8192)
    proactive_scopes = []
    for row in proactive_raw.splitlines():
        key = row.split("\t")[0].strip() if "\t" in row else row.split()[0].strip() if row.strip() else ""
        if key and key not in proactive_scopes:
            proactive_scopes.append(key)
    if proactive_scopes:
        scopes = proactive_scopes + [s for s in scopes if s not in proactive_scopes]
    try:
        humanlike_files = sorted(os.listdir("/runtime/humanlike"))
    except OSError:
        humanlike_files = []

    domain_health = fetch_domain("/health")
    domain_stats = fetch_domain("/stats")
    stats = {}
    if isinstance(domain_stats, dict):
        inner = domain_stats.get("data")
        stats = inner if isinstance(inner, dict) else domain_stats
        if isinstance(stats.get("tables"), dict):
            stats = stats["tables"]

    return {
        "generated_at": time.time(),
        "bot": {
            "model": config.get("model", ""),
            "reasoning_effort": config.get("reasoning_effort", ""),
            "agent_max_turns": config.get("agent_max_turns", ""),
            "recent_context_chars": config.get("recent_chat_context_max_chars", ""),
            "recent_context_limit": config.get("recent_chat_context_limit", ""),
            "system_prompt_chars": len(config.get("system_prompt", "")),
            "config_mtime": config_mtime,
        },
        "humanlike": {
            "scopes": scopes,
            "scope_count": len(scopes),
            "proactive": _read_text("/runtime/humanlike/proactive.txt", 512).strip(),
            "files": humanlike_files,
        },
        "chat": _sqlite_counts(),
        "domain": {
            "health_ok": bool(isinstance(domain_health, dict) and (domain_health.get("ok") or domain_health.get("status") in ("ok", "healthy"))),
            "stats": stats if isinstance(stats, dict) else {},
        },
        "features": {
            "capabilities": NOW_CAPABILITIES,
            "commands": NOW_COMMANDS,
            "note": "能力清单更新于 2026-11-14；拟人模式是否生效以「已开启会话」为准。",
        },
    }


# ══════════════════════════════════════════════════════════════════════
# v3 扩展辅助（2026-11-15）：通用只读 domain 代理 / 会话 / Token / 自检 HTTP 探针
# 放在 class Api 之前，供 handle_api_v2 分派和自检使用。
# ══════════════════════════════════════════════════════════════════════

def _mask_session_id(session_id: object) -> str:
    text = str(session_id or "")
    return (text[:6] + "…" + text[-4:]) if len(text) > 12 else "***"


def list_admin_sessions(current: dict | None = None) -> dict:
    """GET /api/v2/sessions：当前后台登录会话（不含完整 session id）。"""
    now = time.time()
    current_id = str((current or {}).get("session_id") or "")
    with LOCK:
        rows = list(SESSIONS.items())
    items = []
    for session_id, session in rows:
        login_at = float(session.get("login_time") or 0)
        age = max(0.0, now - login_at) if login_at else None
        items.append({
            "session_id": _mask_session_id(session_id),
            "username": str(session.get("username") or ""),
            "is_admin": bool(session.get("is_admin")),
            "login_at": login_at,
            "login_at_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(login_at)) if login_at else "",
            "age_seconds": int(age) if age is not None else None,
            "remaining_seconds": max(0, int(SESSION_TTL - age)) if age is not None else None,
            "from_token": bool(session.get("token_id")),
            "current": bool(current_id and session_id == current_id),
        })
    items.sort(key=lambda item: item.get("login_at") or 0, reverse=True)
    return {
        "sessions": items,
        "count": len(items),
        "ttl_seconds": SESSION_TTL,
        "now": now,
        "note": "session id 为脱敏后的值；后台 token 会话默认只读，is_admin=true 的自动化 token 才可写。",
    }


def list_admin_tokens() -> dict:
    """GET /api/v2/tokens：脱敏后的 token 列表（含 is_admin / 过期 / 禁用状态）。"""
    auth = read_auth()
    now = time.time()
    items = []
    for token in auth.get("tokens", []) or []:
        masked = masked_token(token)
        expires = token.get("expires_at")
        try:
            expires_value = float(expires) if expires else None
        except (TypeError, ValueError):
            expires_value = None
        revoked_at = token.get("revoked_at")
        try:
            revoked_value = float(revoked_at) if revoked_at else None
        except (TypeError, ValueError):
            revoked_value = None
        items.append({
            **masked,
            "is_admin": bool(token.get("is_admin", False)),
            "enabled": bool(token.get("enabled", True)),
            "expired": bool(expires_value and expires_value <= now),
            "expires_at": expires_value,
            "expires_at_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(expires_value)) if expires_value else "",
            "revoked_at_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(revoked_value)) if revoked_value else "",
        })
    items.sort(key=lambda item: float(item.get("created_at") or 0), reverse=True)
    return {
        "tokens": items,
        "count": len(items),
        "enabled_count": sum(1 for item in items if item["enabled"] and not item["expired"]),
        "admin_count": sum(1 for item in items if item["is_admin"] and item["enabled"] and not item["expired"]),
        "now": now,
        "note": "token 值只显示首尾各 4 位；撤销=置 enabled=false 并踢掉用该 token 登录的会话。",
    }


def revoke_admin_token(token_id: object) -> dict | None:
    """撤销（禁用）一个后台 token；不存在返回 None。"""
    tid = str(token_id or "").strip()
    if not tid:
        return None
    auth = read_auth()
    target = next((token for token in (auth.get("tokens") or []) if str(token.get("id")) == tid), None)
    if target is None:
        return None
    target["enabled"] = False
    target["revoked_at"] = time.time()
    write_auth(auth)
    dropped = 0
    with LOCK:
        for session_id, session in list(SESSIONS.items()):
            if str(session.get("token_id") or "") == tid:
                SESSIONS.pop(session_id, None)
                dropped += 1
    return {
        "id": tid,
        "name": str(target.get("name") or ""),
        "is_admin": bool(target.get("is_admin", False)),
        "revoked": True,
        "enabled": False,
        "revoked_at": int(time.time()),
        "sessions_dropped": dropped,
    }


def domain_proxy_get(path: object, extra_query: str = "") -> dict:
    """GET /api/v2/domain?path=：通用只读 domain 代理（白名单前缀，仅 GET）。

    兼容两种写法：
      /api/v2/domain?path=/library/list&limit=2      （query 走独立参数）
      /api/v2/domain?path=/library/list%3Flimit%3D2  （query 塞在 path 里）
    """
    import api_v2 as v2module
    rel = str(path or "").strip()
    if not rel:
        raise ValueError("缺少 path 参数（例如 /stats、/novel/stats）")
    rel, _, embedded = rel.partition("?")
    if not rel.startswith("/"):
        rel = "/" + rel
    if ".." in rel or "//" in rel:
        raise ValueError("非法 path：" + rel)
    allowed = any(rel == prefix.rstrip("/") or rel.startswith(prefix)
                  for prefix in v2module.DOMAIN_PROXY_ALLOW)
    if not allowed:
        raise PermissionError("path 不在只读 domain 代理白名单内：" + rel)
    combined_query = "&".join(part for part in (extra_query, embedded) if part)
    result = fetch_domain(rel, combined_query)
    if not isinstance(result, dict) or not result.get("ok"):
        error = str(result.get("error") or "") if isinstance(result, dict) else ""
        raise RuntimeError("fm-domain 请求失败：" + (error or "未知错误"))
    data = result.get("data")
    return {"path": rel, "query": combined_query, "source": "domain", "data": data,
            "count": len(data) if isinstance(data, (list, dict)) else None}


# 自检环节逐个 HTTP 探测的接口清单：(名称, path, 是否必需)
SELFTEST_HTTP_ROUTES: list[tuple[str, str, bool]] = [
    ("v2:overview", "/api/v2/overview", True),
    ("v2:charts/messages", "/api/v2/charts/messages?days=7", True),
    ("v2:charts/tools", "/api/v2/charts/tools?days=7", True),
    ("v2:top/groups", "/api/v2/top/groups?days=7&limit=5", True),
    ("v2:tools/catalog", "/api/v2/tools/catalog", True),
    ("v2:tools/stats", "/api/v2/tools/stats?days=7", True),
    ("v2:groups", "/api/v2/groups", True),
    ("v2:triggers", "/api/v2/triggers", True),
    ("v2:persona", "/api/v2/persona?scope=group_default", True),
    ("v2:scheduled", "/api/v2/scheduled?limit=5", True),
    ("v2:members/all", "/api/v2/members/all?limit=5", True),
    ("v2:browse/tables", "/api/v2/browse/tables", True),
    ("v2:novels", "/api/v2/novels", False),
    ("v2:config/toml", "/api/v2/config/toml", True),
    ("v2:memory/tree", "/api/v2/memory/tree", True),
    ("v2:models", "/api/v2/models", True),
    ("v2:group/capability", "/api/v2/group/capability?id=216063968", True),
    ("v2:backup/list", "/api/v2/backup/list", True),
    # sessions/tokens 需要 is_admin 会话；只读 token 调用自检时降级为 warn，不算失败
    ("v2:sessions", "/api/v2/sessions", False),
    ("v2:tokens", "/api/v2/tokens", False),
    ("v2:domain?path=/health", "/api/v2/domain?path=/health", True),
    ("v2:domain?path=/stats", "/api/v2/domain?path=/stats", True),
    ("v2:ops/logs", "/api/v2/ops/logs?tail=5", False),
    ("v2:ops/metrics", "/api/v2/ops/metrics", False),
    ("v2:ops/cron", "/api/v2/ops/cron", False),
    ("v2:ops/images", "/api/v2/ops/images", False),
    ("domain:library", "/api/domain/library?limit=2", True),
    ("domain:contests", "/api/domain/contests?limit=2", True),
    ("domain:scores", "/api/domain/scores?limit=2", True),
    ("domain:stats", "/api/domain/stats", True),
    ("domain:groups", "/api/domain/groups", True),
    ("domain:archive", "/api/domain/archive", True),
    ("domain:recent-messages", "/api/domain/recent-messages?limit=2", False),
    ("v2:domain?path=/contests/list", "/api/v2/domain?path=/contests/list", True),
    ("v2:domain?path=/library/list", "/api/v2/domain?path=/library/list&limit=2", True),
    ("v2:domain?path=/typing/summary", "/api/v2/domain?path=/typing/summary&days=7", True),
    ("v2:domain?path=/reports/daily", "/api/v2/domain?path=/reports/daily", True),
    ("v2:domain?path=/version", "/api/v2/domain?path=/version", True),
    ("v2:domain?path=/contest/chart-data", "/api/v2/domain?path=/contest/chart-data&contest=HB&days=7", False),
    ("v2:domain?path=/novel/stats", "/api/v2/domain?path=/novel/stats", True),
    # ── v4 新增接口（分析 / 内容管理 / 运维 v2 / 健康历史）────────────
    ("v4:analytics/messages", "/api/v2/analytics/messages?days=7", True),
    ("v4:analytics/groups", "/api/v2/analytics/groups?days=7&limit=5", True),
    ("v4:analytics/senders", "/api/v2/analytics/senders?days=7&limit=5", True),
    ("v4:analytics/latency", "/api/v2/analytics/latency?days=7", True),
    ("v4:analytics/tokens", "/api/v2/analytics/tokens?days=7", True),
    ("v4:library", "/api/v2/library?limit=2", True),
    ("v4:contests", "/api/v2/contests", True),
    ("v4:members/leaderboard", "/api/v2/members/leaderboard?days=7&limit=3", True),
    ("v4:novels(cached+q)", "/api/v2/novels?limit=2", False),
    ("v4:ops/stats", "/api/v2/ops/stats", False),
    ("v4:ops/cron", "/api/v2/ops/cron", False),
    ("v4:health/history", "/api/v2/health/history?days=7", False),
    ("v4:cache/stats", "/api/v2/cache/stats", False),
]

# 写接口清单（自检不自动执行，避免副作用；这里登记给「自检」页做验收清单）
SELFTEST_WRITE_ENDPOINTS: list[dict] = [
    {"method": "POST", "path": "/api/v2/config/toml", "note": "保存配置（备份+tomllib+sha 校验+审计）"},
    {"method": "POST", "path": "/api/v2/memory/file", "note": "写记忆/人设文件（白名单+备份+sha 校验+审计）"},
    {"method": "POST", "path": "/api/v2/models/switch", "note": "切换模型（RPC probe 热切换；改 effort 走文件+重启）"},
    {"method": "POST", "path": "/api/v2/group/capability", "note": "改群能力（代理 domain POST + 回读校验+审计）"},
    {"method": "POST", "path": "/api/v2/ops/rollback", "note": "真实安全回滚（需 confirm:true；宿主机代理 stop→rename→run→健康检查→失败自动恢复）"},
    {"method": "POST", "path": "/api/v2/ops/config-backup", "note": "备份 /runtime/config.toml 到 /opt/fm-admin/data/backups（宿主机代理执行）"},
    {"method": "POST", "path": "/api/v2/ops/cron-set", "note": "cron 写入未实现（固定返回 501）；只读用 GET /api/v2/ops/cron"},
    {"method": "DELETE", "path": "/api/v2/sessions/{id}", "note": "踢掉一个后台登录会话（脱敏 id 也可；默认不许踢自己，?self=1 放开）"},
    {"method": "GET", "path": "/api/v2/export/chat", "note": "下载聊天 CSV（流式+审计）"},
    {"method": "GET", "path": "/api/v2/export/audit", "note": "下载审计 CSV（流式+审计）"},
    {"method": "POST", "path": "/api/v2/backup/create", "note": "创建 bot/domain 备份（校验+审计）"},
    {"method": "POST", "path": "/api/v2/tokens/revoke", "note": "撤销 token（踢会话+审计）"},
    {"method": "GET", "path": "/api/v2/logs/stream", "note": "SSE 实时日志流"},
]


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

    # ══ v4：限流闸门 ═════════════════════════════════════════════════
    def v2_rate_gate(self) -> bool:
        """每个 /api/v2 请求过令牌桶；超限返回 429（带 Retry-After）。

        自检内部探针（X-FM-Selftest: 1）不计数，否则 40+ 项自检会把自己的限流打爆。
        """
        if str(self.headers.get("X-FM-Selftest") or "") == "1":
            return True
        session = self.current_session()
        key = rate_limit_key_for(session, self)
        allowed, retry_after, remaining = rate_limit_check(key)
        if allowed:
            return True
        payload = json.dumps({
            "ok": False,
            "error": "请求过于频繁：每个会话平均 %d 次/分钟（突发 %d），请 %d 秒后重试。"
                     % (RATE_LIMIT_PER_MIN, RATE_LIMIT_BURST, retry_after),
            "retry_after": retry_after,
            "rate_limit": {"per_min": RATE_LIMIT_PER_MIN, "burst": RATE_LIMIT_BURST},
        }, ensure_ascii=False).encode("utf-8")
        self.send_response(HTTPStatus.TOO_MANY_REQUESTS)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Retry-After", str(retry_after))
        self.send_header("X-RateLimit-Limit", str(RATE_LIMIT_PER_MIN))
        self.send_header("X-RateLimit-Burst", str(RATE_LIMIT_BURST))
        self.send_header("X-RateLimit-Remaining", str(remaining))
        self.send_header("Content-Length", str(len(payload)))
        self.end_headers()
        try:
            self.wfile.write(payload)
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass
        return False

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
                    # v3: token 记录可选 is_admin（默认 false，保持既有 token 只读语义）。
                    # 只有 admin 会话创建 token 时才能显式指定；用于夜间自动化写操作。
                    is_admin = bool(token.get("is_admin", False))
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
        is_admin = False
        if isinstance(payload, dict):
            is_admin = bool(payload.get("is_admin", False))
        item = {"id": secrets.token_hex(16), "value": token, "name": name[:120],
                "created_at": time.time(), "created_by": session["username"],
                "expires_at": expires_at, "enabled": True, "is_admin": is_admin}
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
        elif request.path == "/console":
            self.send_static("console.html")
        elif request.path == "/now":
            self.send_static("now.html")
        elif request.path == "/api/now":
            self.send_json(HTTPStatus.OK, {"ok": True, "data": now_snapshot()})
        elif request.path.startswith("/api/v2/"):
            self.handle_api_v2(request)
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
            elif name in ("recent-messages", "messages"):
                # domain 没有 /recent_messages 路由（历史 404）：最近消息直接查 bot sqlite 的 chat_log。
                import api_v2 as v2module
                query = parse_qs(request.query)
                data = v2module.message_search(
                    (query.get("q") or [""])[0],
                    (query.get("chat") or query.get("group_id") or [""])[0],
                    _int_arg((query.get("days") or ["0"])[0], 0),
                    _int_arg((query.get("limit") or ["50"])[0], 50),
                )
                self.send_json(HTTPStatus.OK, {
                    "ok": True,
                    "data": data["rows"],
                    "items": data["rows"],
                    "total": data["count"],
                })
            else:
                path = DOMAIN_READS.get(name, "/" + name)
                query_text = request.query or ""
                # 旧的 limit=50 默认值从 path 挪到这里：用户传了 limit 就用用户的，不拼第二个 "?"。
                if name in ("library", "contests", "scores") and "limit=" not in query_text:
                    query_text = (query_text + "&" if query_text else "") + "limit=50"
                result = fetch_domain(path, query_text)
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
            # 2026-11-14：根路径改为新控制台（旧版挪到 /classic）
            self.send_static("console.html")
        elif request.path == "/classic":
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


    def handle_api_v2(self, request) -> None:
        """v4 包装层：限流闸门 + 慢接口日志（>2s 记 error），业务在 _handle_api_v2。"""
        started = time.monotonic()
        if not self.v2_rate_gate():
            return
        try:
            self._handle_api_v2(request)
        finally:
            elapsed_ms = (time.monotonic() - started) * 1000.0
            path = str(getattr(request, "path", "") or "")
            if elapsed_ms >= SLOW_REQUEST_MS and not any(token in path for token in SLOW_PATH_SKIP):
                try:
                    record_error("slow", "慢接口 %.0fms（阈值 %.0fms）：%s"
                                 % (elapsed_ms, SLOW_REQUEST_MS, path), "admin", path)
                except Exception:  # noqa: BLE001
                    pass

    def _handle_api_v2(self, request) -> None:
        """一次性补齐的后台 v2 接口（见 api_v2.py）。"""
        import api_v2 as v2
        import api_v4 as v4
        query = parse_qs(request.query)
        arg = lambda name, default="": (query.get(name, [default])[0] or default)
        route = request.path[len("/api/v2/"):].strip("/")
        actor = (self.headers.get("X-FM-Admin-Actor") or "admin").strip()[:80] or "admin"

        def method_not_allowed(allow: str) -> None:
            self.send_error_json(HTTPStatus.METHOD_NOT_ALLOWED, "该接口只接受 " + allow + " 请求。")

        try:
            if route == "overview":
                data = v2.overview(fetch_domain, fetch_rpc)
            elif route == "charts/messages":
                data = v2.charts_messages(_int_arg(arg("days", "14"), 14), arg("chat"))
            elif route == "charts/tools":
                data = v2.charts_tools(_int_arg(arg("days", "14"), 14))
            elif route == "top/groups":
                data = v2.top_groups(_int_arg(arg("days", "30"), 30), _int_arg(arg("limit", "15"), 15), fetch_domain)
            elif route == "tools/catalog":
                data = v2.tools_catalog(_int_arg(arg("days", "0"), 0))
            elif route == "triggers":
                data = v2.triggers_get(arg("group_id"), fetch_rpc, lambda: read_state().get("triggers") or {})
            elif route == "triggers/set":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                data = v2.triggers_set(
                    payload.get("group_id"),
                    payload.get("modes"),
                    payload.get("keywords"),
                    fetch_rpc,
                )
                record_audit("set", "triggers", str(payload.get("group_id") or ""), actor)
            elif route == "persona":
                if self.command == "POST":
                    if not self.require_admin():
                        return
                    payload = self.read_json()
                    if not isinstance(payload, dict):
                        raise v2.ApiError("请求体必须是 JSON 对象")
                    data = v2.persona_write(
                        payload.get("scope") or arg("scope"),
                        payload.get("id") or arg("id"),
                        payload.get("text"),
                        payload.get("action"),
                        fetch_rpc,
                    )
                    record_audit(str(data.get("action") or "set"), "persona", str(data.get("scope") or ""), actor)
                else:
                    data = v2.persona_read(arg("scope"), arg("id"), fetch_rpc, fetch_domain)
            elif route == "scheduled":
                data = v2.scheduled_list(
                    arg("limit", "50"), arg("offset", "0"), arg("chat"), arg("sender"), arg("q"),
                    arg("future").lower() in {"1", "true", "yes", "on"},
                )
            elif route == "scheduled/cancel":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                data = v2.scheduled_cancel(payload.get("id"))
                record_audit("cancel", "scheduled", str(data.get("id") or ""), actor)
            elif route == "members/profile":
                data = v2.member_profile(arg("id"), fetch_domain)
            elif route == "deploy/status":
                data = v2.deploy_status(
                    arg("request_id"),
                    arg("force").lower() in {"1", "true", "yes", "on"},
                )
            elif route == "deploy/act":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                data = v2.deploy_act(payload.get("action"), payload.get("target") or "bot")
                record_audit("deploy", "ops", f"{payload.get('action')}:{payload.get('target') or 'bot'}", actor)
            elif route == "humanlike":
                if self.command == "POST":
                    payload = self.read_json() or {}
                    scopes = payload.get("scopes") if isinstance(payload, dict) else []
                    data = v2.humanlike_set_scopes(scopes if isinstance(scopes, list) else [])
                else:
                    data = v2.humanlike_state()
            elif route == "members":
                data = v2.member_search(arg("q"))
            elif route == "messages":
                data = v2.message_search(arg("q"), arg("chat"), _int_arg(arg("days", "0"), 0), _int_arg(arg("limit", "100"), 100))
            elif route == "tools/audit":
                data = v2.tools_audit(int(arg("days", "7") or 7), arg("tool"), arg("status"), int(arg("limit", "200") or 200))
            elif route == "tools/stats":
                data = v2.tools_stats(_int_arg(arg("days", "7"), 7))
            elif route == "groups":
                data = v2.groups_overview(fetch_domain)
            elif route == "group":
                data = v2.group_detail(arg("id"), fetch_domain)
            elif route == "browse/tables":
                data = v2.browse_tables()
            elif route == "browse/rows":
                data = v2.browse_rows(arg("table"), arg("q"), _int_arg(arg("limit", "50"), 50), _int_arg(arg("offset", "0"), 0))
            elif route == "members/all":
                data = v2.all_members(_int_arg(arg("limit", "500"), 500))
            elif route == "actions":
                data = v2.action_catalog()
            elif route == "actions/run" and self.command == "POST":
                payload = self.read_json() or {}
                action_name = str(payload.get("name") or "")
                if action_name == "send_message":
                    data = {"ok": True, "result": fetch_rpc("chat.send", {
                        "chat_id": str(payload.get("chat_id") or ""),
                        "text": str(payload.get("text") or ""),
                    })}
                else:
                    data = v2.run_action(action_name, payload, None)
            elif route == "novels":
                # v4：加 30s 缓存 + q 关键字搜索 / 分页（domain /novel/stats 是 POST，缓存 30s）
                data = v4.novels_cached(fetch_domain, arg("q"), arg("limit", "20"), arg("offset", "0"))
            elif route == "novel":
                data = v4.novel_detail_cached(fetch_domain, arg("id"))
            # ── v3: 配置编辑（A）──────────────────────────────────────────
            elif route == "config/toml":
                if self.command == "POST":
                    if not self.require_admin():
                        return
                    payload = self.read_json()
                    if not isinstance(payload, dict):
                        raise v2.ApiError("请求体必须是 JSON 对象")
                    data = v2.config_toml_write(payload.get("content"), actor)
                    record_audit("save", "config.toml", str(data.get("sha256") or "")[:12], actor)
                elif self.command == "GET":
                    data = v2.config_toml_read()
                else:
                    method_not_allowed("GET, POST")
                    return
            # ── v3: 记忆 / 人设文件（B）──────────────────────────────────
            elif route == "memory/tree":
                data = v2.memory_tree(arg("path"))
            elif route == "memory/file":
                if self.command == "POST":
                    if not self.require_admin():
                        return
                    payload = self.read_json()
                    if not isinstance(payload, dict):
                        raise v2.ApiError("请求体必须是 JSON 对象")
                    data = v2.memory_file_write(payload.get("path") or arg("path"),
                                                payload.get("content"), actor)
                    record_audit("save", "memory", str(data.get("path") or ""), actor)
                elif self.command == "GET":
                    data = v2.memory_file_read(arg("path"))
                else:
                    method_not_allowed("GET, POST")
                    return
            # ── v3: 模型（C）────────────────────────────────────────────
            elif route == "models":
                data = v2.models_overview(fetch_rpc)
            elif route == "models/switch":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                provider = str(payload.get("provider") or "").strip()
                effort = str(payload.get("effort") or "").strip()
                data = v2.models_switch(
                    provider, effort,
                    lambda method, params=None: fetch_rpc(method, params, timeout=45.0),
                )
                record_audit("switch", "model",
                             provider + (":" + effort if effort else ""), actor)
            # ── v3: 群能力（D，代理 fm-domain）──────────────────────────
            elif route == "group/capability":
                if self.command == "POST":
                    if not self.require_admin():
                        return
                    payload = self.read_json()
                    if not isinstance(payload, dict):
                        raise v2.ApiError("请求体必须是 JSON 对象")
                    group_id = str(payload.get("group_id") or payload.get("id") or "").strip()
                    capability = str(payload.get("capability") or payload.get("feature") or "").strip()
                    enabled = payload.get("enabled")
                    data = v2.group_capability_write(group_id, capability, enabled,
                                                     fetch_domain, proxy_domain_post)
                    record_audit("set", "capability", group_id + ":" + capability, actor)
                elif self.command == "GET":
                    data = v2.group_capability_read(arg("id") or arg("group_id"), fetch_domain)
                else:
                    method_not_allowed("GET, POST")
                    return
            # ── v3: 运维监控（E）────────────────────────────────────────
            elif route == "ops/logs":
                # v4：支持 grep 过滤、最多 500 行
                data = v4.ops_logs_v2(arg("tail", "200"), arg("since"), arg("grep"))
            elif route == "ops/metrics":
                data = v2.ops_metrics()
            elif route == "ops/images":
                data = v2.ops_images(arg("live").lower() in {"1", "true", "yes", "on"})
            elif route == "ops/cron":
                # v4：只读 crontab（解析成 schedule/command；cron_set 未实现→501）
                data = v4.ops_cron_list()
            elif route == "ops/result":
                data = v2.ops_result_view(arg("request_id"))
            elif route == "ops/rollback":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                # v4：真实安全回滚（宿主机代理 stop→rename→run→健康检查→失败自动恢复），
                # 必须带 confirm:true；返回结果里带 result_file 路径供审计。
                data = v4.ops_rollback_safe(
                    payload.get("image"),
                    payload.get("confirm"),
                    payload.get("keep_old", True),
                )
                record_audit("rollback", "ops", str(payload.get("image") or ""), actor)
            # ── v3: 导出 CSV（F）────────────────────────────────────────
            elif route == "export/chat":
                if self.command != "GET":
                    method_not_allowed("GET")
                    return
                days = _int_arg(arg("days", "7"), 7)
                chat_id = arg("chat")
                record_audit("export", "chat", "days=%d;chat=%s" % (days, chat_id or "-"), actor)
                self.stream_csv(
                    v2.export_chat_csv(days, chat_id),
                    "fm-chat-%s.csv" % time.strftime("%Y%m%d-%H%M%S"),
                )
                return
            elif route == "export/audit":
                if self.command != "GET":
                    method_not_allowed("GET")
                    return
                days = _int_arg(arg("days", "7"), 7)
                record_audit("export", "audit", "days=%d" % days, actor)
                self.stream_csv(
                    v2.export_audit_csv(read_state().get("audit") or [], days),
                    "fm-audit-%s.csv" % time.strftime("%Y%m%d-%H%M%S"),
                )
                return
            # ── v3: 备份（G）────────────────────────────────────────────
            elif route == "backup/list":
                data = v2.backup_list()
            elif route == "backup/create":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                database = str(payload.get("db") or "bot").strip().lower()
                if database == "bot":
                    data = v2.backup_create_bot(actor)
                elif database == "domain":
                    data = v2.backup_create_domain(actor)
                else:
                    raise v2.ApiError("db 只支持 bot / domain", 400)
                record_audit("backup", database, str(data.get("file") or ""), actor)
            # ── v3: 日志流 SSE（H）─────────────────────────────────────
            elif route == "logs/stream":
                if self.command != "GET":
                    method_not_allowed("GET")
                    return
                self.handle_logs_stream(arg("tail", "100"), arg("since"), arg("seconds", "180"))
                return
            # ── v3: 自检矩阵（I）────────────────────────────────────────
            elif route == "selftest":
                self.handle_selftest()
                return
            # ── v3: 会话与 Token（J）───────────────────────────────────
            elif route == "sessions":
                if not self.require_admin():
                    return
                data = list_admin_sessions(self.current_session())
            elif route == "tokens":
                if not self.require_admin():
                    return
                data = list_admin_tokens()
            elif route == "tokens/revoke":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                token_id = str(payload.get("id") or "").strip()
                if not token_id:
                    raise v2.ApiError("缺少 id（token id）")
                revoked = revoke_admin_token(token_id)
                if revoked is None:
                    raise v2.ApiError("Token 不存在：" + token_id, 404)
                record_audit("revoke", "tokens", token_id, actor)
                data = revoked
            # ── v3: 通用只读 domain 代理（前端不用猜 old proxy 名字）────
            elif route == "domain":
                if self.command != "GET":
                    method_not_allowed("GET")
                    return
                extra = {key: values[0] for key, values in query.items()
                         if key != "path" and values}
                try:
                    data = domain_proxy_get(arg("path"), urlencode(extra))
                except ValueError as exc:
                    raise v2.ApiError(str(exc), 400)
                except PermissionError as exc:
                    raise v2.ApiError(str(exc), 403)
                except RuntimeError as exc:
                    raise v2.ApiError(str(exc), 502)
            # ══ v4: A. 分析类（sqlite chat_log / audit_log）═════════════════
            elif route == "analytics/messages":
                data = v4.analytics_messages(arg("days", "14"))
            elif route == "analytics/groups":
                data = v4.analytics_groups(arg("days", "30"), arg("limit", "50"), fetch_domain)
            elif route == "analytics/senders":
                data = v4.analytics_senders(arg("days", "7"), arg("limit", "20"), fetch_domain)
            elif route == "analytics/latency":
                data = v4.analytics_latency(arg("days", "7"))
            elif route == "analytics/tokens":
                data = v4.analytics_tokens(arg("days", "7"))
            # ══ v4: B. 内容管理（代理 fm-domain + TTL 缓存）════════════════
            elif route == "library":
                data = v4.library_list(fetch_domain, arg("limit", "20"), arg("offset", "0"),
                                       arg("q"), arg("difficulty"), arg("group_id"))
            elif route == "library/text":
                data = v4.library_text(fetch_domain, arg("id"))
            elif route == "contests":
                data = v4.contests(fetch_domain)
            elif route == "members/leaderboard":
                data = v4.members_leaderboard(fetch_domain, arg("days", "7"), arg("limit", "20"),
                                              arg("group_id"), arg("metric"))
            # ══ v4: C. 运维增强（宿主机 ops-agent v2）═════════════════════
            elif route == "ops/stats":
                data = v4.ops_stats(arg("force"))
            elif route == "ops/cron-set":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                payload = self.read_json()
                if not isinstance(payload, dict):
                    raise v2.ApiError("请求体必须是 JSON 对象")
                data = v4.ops_cron_set(payload.get("line") or arg("line"))
            elif route == "ops/config-backup":
                if self.command != "POST":
                    method_not_allowed("POST")
                    return
                if not self.require_admin():
                    return
                data = v4.ops_config_backup()
                record_audit("backup", "config.toml", str(data.get("file") or ""), actor)
            # ══ v4: D. 健康历史 / 缓存状态 ═════════════════════════════════
            elif route == "health/history":
                data = v4.health_history(arg("days", "7"))
            elif route == "cache/stats":
                data = v4.cache_stats(rate_limit_config())
            else:
                self.send_error_json(HTTPStatus.NOT_FOUND, "未知的 v2 接口")
                return
        except v2.ApiError as exc:
            record_error("v2", exc.message, actor, request.path)
            self.send_error_json(exc.status, exc.message)
            return
        except Exception as exc:  # noqa: BLE001
            record_error("v2", f"v2 接口失败：{exc}", actor, request.path)
            self.send_error_json(HTTPStatus.INTERNAL_SERVER_ERROR, f"v2 接口失败：{exc}")
            return
        self.send_json(HTTPStatus.OK, {"ok": True, "data": data})

    # ══ v3 扩展：SSE 日志流 / CSV 导出流 / 自检矩阵 ══════════════════════════

    def _sse_send(self, event: str, payload: dict) -> None:
        """发一个 SSE data 帧。

        兼容性考虑：用「默认 message 事件」而不是命名事件（event: xxx），
        这样前端用 es.onmessage 也能直接收到；事件类型放在 data.type 里，
        需要区分的实现可以用 addEventListener('message') + 判断 type。
        """
        body = {"type": event}
        body.update(payload or {})
        data = json.dumps(body, ensure_ascii=False)
        frame = ("data: %s\n\n" % data).encode("utf-8")
        self.wfile.write(frame)
        self.wfile.flush()

    def _sse_comment(self, text: str) -> None:
        """SSE 注释行（keepalive），浏览器不会当消息派发。"""
        self.wfile.write((": %s\n\n" % str(text).replace("\n", " ")).encode("utf-8"))
        self.wfile.flush()

    def handle_logs_stream(self, tail: str = "100", since: str = "", seconds: str = "180") -> None:
        """GET /api/v2/logs/stream：真正的 text/event-stream。

        后台容器没有 docker socket，所以 SSE 的数据源是宿主机 ops-agent 的
        `docker logs fm-cosmobot --timestamps --since <上一次时间戳>`；每轮把新行
        以 event: log 推给前端，附带 ping 心跳。客户端断开（BrokenPipe）即退出。
        """
        import api_v2 as v2
        try:
            tail_n = max(1, min(500, int(tail or 100)))
        except (TypeError, ValueError):
            tail_n = 100
        try:
            duration = max(5, min(300, int(seconds or 180)))
        except (TypeError, ValueError):
            duration = 180
        cursor = str(since or "").strip()
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", "text/event-stream; charset=utf-8")
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Accel-Buffering", "no")
        self.send_header("Connection", "keep-alive")
        self.end_headers()
        deadline = time.time() + duration
        first = True
        last_key: tuple[str, str] | None = None
        try:
            self._sse_send("hello", {"source": "host-ops-agent:docker logs",
                                     "container": "fm-cosmobot", "tail": tail_n,
                                     "duration_seconds": duration, "at": int(time.time())})
            while time.time() < deadline:
                try:
                    payload = v2.ops_logs(tail=tail_n if first else 500,
                                          since="" if first else cursor)
                    lines = payload.get("lines") or []
                except v2.ApiError as exc:
                    self._sse_send("error", {"message": exc.message, "status": exc.status})
                    break
                sent = 0
                for line in lines:
                    line_ts = str(line.get("ts") or "")
                    key = (line_ts, str(line.get("text") or ""))
                    # --since 是包含式边界：跳过上一批已经推过的最后一行
                    if not first and key == last_key:
                        continue
                    last_key = key
                    self._sse_send("log", {"ts": line_ts,
                                           "text": str(line.get("text") or "")[:4000],
                                           "stream": line.get("stream") or ""})
                    sent += 1
                    if line_ts:
                        cursor = line_ts
                self._sse_send("ping", {"at": int(time.time()), "sent": sent,
                                        "cursor": cursor, "remaining_seconds": int(deadline - time.time())})
                self._sse_comment("keepalive at=%d" % int(time.time()))
                first = False
                time.sleep(2.0 if sent else 3.0)
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass
        finally:
            try:
                self._sse_send("end", {"reason": "duration-timeout"})
            except Exception:  # noqa: BLE001  客户端已断开时忽略
                pass
            self.close_connection = True

    def stream_csv(self, chunks, filename: str) -> None:
        """把 CSV 生成器流式写给浏览器（不缓冲、无 Content-Length，Connection: close）。"""
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", "text/csv; charset=utf-8")
        self.send_header("Content-Disposition", 'attachment; filename="%s"' % filename)
        self.send_header("Cache-Control", "no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Connection", "close")
        self.end_headers()
        self.close_connection = True
        try:
            for chunk in chunks:
                if chunk:
                    self.wfile.write(chunk.encode("utf-8"))
            self.wfile.flush()
        except (BrokenPipeError, ConnectionResetError, OSError):
            pass

    def _selftest_http_checks(self) -> list[dict]:
        """以当前 cookie 对本机接口发真实 HTTP 请求，拿到每项状态码与耗时。

        v4：探针带 X-FM-Selftest: 1 头 → 免限流计数（否则 50+ 项自检会把自己的
        60 次/分钟限流打爆，自检结果会全是 429）。
        """
        port = int(getattr(self.server, "server_address", ("", 8090))[1] or 8090)
        cookie = str(self.headers.get("Cookie") or "")
        checks: list[dict] = []

        def probe(name: str, path: str, required: bool = True) -> None:
            started = time.monotonic()
            status: int | None = None
            body = b""
            error_text = ""
            # 宿主机代理类接口（ops/*）要走队列 + docker 命令，最坏情况要等一个 cron 周期；
            # 给它们更长的超时，避免自检把「代理正在两轮之间」误报成失败（仍是 optional 项）。
            probe_timeout = 70 if path.startswith("/api/v2/ops/") else 25
            try:
                request = Request("http://127.0.0.1:%d%s" % (port, path),
                                  headers={"Cookie": cookie, "Accept": "application/json",
                                           "X-FM-Selftest": "1",
                                           "User-Agent": "fm-admin-selftest/1.0"})
                with urlopen(request, timeout=probe_timeout) as response:
                    status = int(response.status)
                    # 有的接口（memory/tree、deploy 快照、镜像列表）响应几十 KB，必须读全，
                    # 否则 JSON 截断会误报「响应不是 JSON」。
                    body = response.read(4 * 1024 * 1024)
            except HTTPError as error:
                status = int(error.code)
                try:
                    body = error.read(4000)
                except Exception:  # noqa: BLE001
                    body = b""
            except Exception as error:  # noqa: BLE001
                error_text = "%s: %s" % (type(error).__name__, str(error)[:300])
            elapsed = round((time.monotonic() - started) * 1000, 1)
            ok = bool(status and 200 <= status < 300)
            parsed: object = None
            if ok and body:
                try:
                    parsed = json.loads(body.decode("utf-8", "replace"))
                except (ValueError, UnicodeDecodeError):
                    ok = False
                    error_text = "响应不是 JSON"
                if isinstance(parsed, dict) and parsed.get("ok") is False:
                    ok = False
                    error_text = str(parsed.get("error") or "ok=false")
            elif ok and not body:
                ok = False
                error_text = "响应为空"
            checks.append({
                "name": name,
                "kind": "http",
                "method": "GET",
                "path": path,
                "http_status": status,
                "ms": elapsed,
                "ok": ok,
                "level": "ok" if ok else ("fail" if required else "warn"),
                "required": bool(required),
                "error": error_text,
            })

        for name, path, required in SELFTEST_HTTP_ROUTES:
            probe(name, path, required)
        # v4：文库正文要动态取一个真实 id 才能探（id 是哈希，别硬编码）
        try:
            import api_v4 as v4
            library = v4.library_list(fetch_domain, 1, 0)
            items = library.get("items") or []
            text_id = str((items[0] or {}).get("id") or "") if items else ""
        except Exception:  # noqa: BLE001
            text_id = ""
        if text_id:
            probe("v4:library/text", "/api/v2/library/text?id=" + text_id, True)
        else:
            checks.append({
                "name": "v4:library/text", "kind": "http", "method": "GET", "path": "",
                "http_status": None, "ms": 0.0, "ok": False, "level": "warn",
                "required": False, "error": "拿不到文库 id，跳过正文探针（/library/list 无数据）",
            })
        return checks

    def handle_selftest(self) -> None:
        """GET /api/v2/selftest：内部资源项 + 逐接口 HTTP 探针，红/绿/黄色验收清单。"""
        import api_v2 as v2
        import api_v4 as v4
        started = time.monotonic()
        internal: list[dict] = []
        harness_error = ""
        try:
            internal = v2.selftest_internal(fetch_domain, fetch_rpc)
        except Exception as exc:  # noqa: BLE001  自检本身绝不能 500
            harness_error = "%s: %s" % (type(exc).__name__, str(exc)[:300])
        try:
            internal = internal + v4.selftest_v4_checks()
        except Exception as exc:  # noqa: BLE001
            harness_error = (harness_error + "; " if harness_error else "") + \
                "v4 checks: %s: %s" % (type(exc).__name__, str(exc)[:200])
        http_checks = self._selftest_http_checks()
        checks = internal + http_checks
        required_failed = [item for item in checks if item.get("required") and not item.get("ok")]
        optional_failed = [item for item in checks if not item.get("required") and not item.get("ok")]
        passed = [item for item in checks if item.get("ok")]
        summary = {
            "total": len(checks),
            "passed": len(passed),
            "failed": len(required_failed) + len(optional_failed),
            "failed_required": len(required_failed),
            "failed_optional": len(optional_failed),
            "levels": {
                "ok": sum(1 for item in checks if item.get("level") == "ok"),
                "warn": sum(1 for item in checks if item.get("level") == "warn"),
                "fail": sum(1 for item in checks if item.get("level") == "fail"),
            },
            "wall_ms": round((time.monotonic() - started) * 1000, 1),
            "failed_names": [item.get("name") for item in required_failed],
            "optional_failed_names": [item.get("name") for item in optional_failed],
        }
        self.send_json(HTTPStatus.OK, {"ok": True, "data": {
            "ok": not required_failed,
            "checked_at": time.time(),
            "checked_at_text": time.strftime("%Y-%m-%d %H:%M:%S"),
            "base_url": "http://127.0.0.1:%d" % int(getattr(self.server, "server_address", ("", 8090))[1] or 8090),
            "checks": checks,
            "summary": summary,
            "harness_error": harness_error,
            "write_endpoints": SELFTEST_WRITE_ENDPOINTS,
            "note": "required=false 的项（宿主机代理慢操作 / domain 新接口未上线）标黄，不影响整体通过；写接口不自动执行，已由人工 curl 验证。",
        }})

    def do_POST(self) -> None:
        request = urlparse(self.path)
        if request.path.startswith("/api/v2/"):
            if not self.require_auth():
                return
            self.handle_api_v2(request)
            return
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
        if request.path.startswith("/api/v2/"):
            # v4：DELETE /api/v2/sessions/{id} 踢会话（id 支持脱敏形式；默认不许踢自己）
            if not self.v2_rate_gate():
                return
            if request.path.startswith("/api/v2/sessions"):
                if not self.require_admin():
                    return
                ident = unquote(request.path[len("/api/v2/sessions"):].strip("/"))
                query = parse_qs(request.query)
                allow_self = (query.get("self") or ["0"])[0].lower() in {"1", "true", "yes", "on"}
                try:
                    data = kick_admin_session(ident, self.current_session(), allow_self)
                except ValueError as exc:
                    self.send_error_json(HTTPStatus.BAD_REQUEST, str(exc))
                    return
                if not data.get("matched"):
                    self.send_error_json(HTTPStatus.NOT_FOUND,
                                         "没有匹配的会话：" + str(ident)[:60] +
                                         "（可用 GET /api/v2/sessions 查看 session_id）")
                    return
                actor = (self.headers.get("X-FM-Admin-Actor") or "admin").strip()[:80] or "admin"
                record_audit("kick", "sessions", "count=%d" % int(data.get("count") or 0), actor)
                self.send_json(HTTPStatus.OK, {"ok": True, "data": data})
                return
            self.send_error_json(HTTPStatus.NOT_FOUND, "未知的 v2 DELETE 接口。")
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
