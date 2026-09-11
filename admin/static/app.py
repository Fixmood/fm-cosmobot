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
from concurrent.futures import ThreadPoolExecutor
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlencode, urlparse
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
        "tasks": {"ok": rpc_tasks["ok"], "running": len(entries), "entries": entries[:100], "reason": None if rpc_tasks["ok"] else rpc_tasks.get("error")},
        "recent_errors": errors,
        "audit": filtered_audit(query),
        "state": {name: len(read_state()[name]) for name in COLLECTIONS},
    }


def runtime_config_result(kind: str, payload: dict | None = None) -> dict:
    if kind not in {"persona", "trigger", "model", "image_model"}:
        return {"ok": False, "error": "运行时配置类型不存在。"}
    return fetch_rpc(f"config.{kind}", payload or {"action": "status"})


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
        elif request.path == "/api/overview":
            self.send_json(HTTPStatus.OK, {
                "ok": True,
                "generated_at": time.time(),
                "domain": fetch_domain("/health"),
                "stats": fetch_domain("/stats"),
                "archive": fetch_domain("/archive/status"),
                "state": {name: len(read_state()[name]) for name in COLLECTIONS},
            })
        elif request.path == "/api/observability/overview":
            self.send_json(HTTPStatus.OK, observability_overview(parse_qs(request.query)))
        elif request.path == "/api/logs":
            self.send_json(HTTPStatus.OK, {"ok": True, "items": filtered_audit(parse_qs(request.query))})
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
                self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.SERVICE_UNAVAILABLE, result)
        elif request.path == "/api/runtime/audit":
            result = fetch_rpc("audit.recent", {"limit": 50})
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
        if not self.require_admin():
            return
        if request.path == "/api/models/test":
            payload = self.read_json()
            result = test_model_connection(payload if isinstance(payload, dict) else {})
            self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_REQUEST, result)
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
