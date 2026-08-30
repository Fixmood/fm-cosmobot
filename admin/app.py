#!/usr/bin/env python3
"""Small FM Control Center API and static file server."""

from __future__ import annotations

import json
import os
import re
import secrets
import threading
import time
from concurrent.futures import ThreadPoolExecutor
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlparse
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parent
STATE_PATH = Path(os.environ.get("FM_ADMIN_STATE", "/data/fm-admin-state.json"))
DOMAIN_URL = os.environ.get("FM_DOMAIN_URL", "http://127.0.0.1:8077").rstrip("/")
ADMIN_TOKEN = os.environ.get("FM_ADMIN_TOKEN", "").strip()
RPC_ENABLED = os.environ.get("FM_RPC_ENABLED", "false").strip().lower() in {"1", "true", "yes", "on"}
RPC_HOST = os.environ.get("FM_RPC_HOST", "127.0.0.1")
RPC_PORT = int(os.environ.get("FM_RPC_PORT", "38765"))
RPC_TOKEN = os.environ.get("FM_RPC_TOKEN", "").strip()
RPC_TIMEOUT = float(os.environ.get("FM_RPC_TIMEOUT", "4"))
LOCK = threading.RLock()
COLLECTIONS = {"groups", "triggers", "personas", "models"}
VERSION_LIMIT = 100
ERROR_LIMIT = 100
DOMAIN_READS = {
    "library": "/library/search?limit=50",
    "contests": "/contest/search?limit=50",
    "scores": "/scores?limit=50",
    "stats": "/stats",
    "archive": "/archive/status",
}


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


def record_error(component: str, error: object, actor: str = "system") -> None:
    with LOCK:
        state = read_state()
        state["recent_errors"].append({
            "component": component,
            "error": sanitize_error(error),
            "actor": actor[:80] or "system",
            "at": time.time(),
        })
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


def fetch_domain(path: str) -> dict:
    request = Request(DOMAIN_URL + path, headers={"Accept": "application/json"})
    try:
        with urlopen(request, timeout=4) as response:
            return {"ok": True, "data": json.loads(response.read().decode("utf-8"))}
    except Exception as error:  # The dashboard must show partial outages instead of failing entirely.
        result = {"ok": False, "error": sanitize_error(error)}
        record_error("domain", result["error"])
        return result


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
    if kind not in {"persona", "trigger", "model"}:
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
    if kind == "model":
        action = request.get("action")
        if action in {"switch", "reset"}:
            previous = next((item for item in before.get("models", []) if item.get("current")), None)
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
        if not ADMIN_TOKEN:
            return True
        supplied = self.headers.get("X-FM-Admin-Token", "")
        return secrets.compare_digest(supplied, ADMIN_TOKEN)

    def send_json(self, status: int, payload: object) -> None:
        encoded = json.dumps(payload, ensure_ascii=False).encode("utf-8")
        self.send_response(status)
        self.send_header("Content-Type", "application/json; charset=utf-8")
        self.send_header("Cache-Control", "no-store")
        self.send_header("Content-Length", str(len(encoded)))
        self.end_headers()
        self.wfile.write(encoded)

    def send_error_json(self, status: int, message: str) -> None:
        self.send_json(status, {"ok": False, "error": message})

    def read_json(self) -> object:
        try:
            length = int(self.headers.get("Content-Length", "0"))
            return json.loads(self.rfile.read(length).decode("utf-8"))
        except (ValueError, json.JSONDecodeError, UnicodeDecodeError):
            return None

    def do_GET(self) -> None:
        if self.path.startswith("/api/") and not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        request = urlparse(self.path)
        if request.path == "/api/health":
            self.send_json(HTTPStatus.OK, {"ok": True, "service": "fm-control-center", "auth_required": bool(ADMIN_TOKEN)})
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
            path = DOMAIN_READS.get(name)
            if path is None:
                self.send_error_json(HTTPStatus.NOT_FOUND, "领域数据接口不存在。")
            else:
                result = fetch_domain(path)
                self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
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
        if not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        request = urlparse(self.path)
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
        if not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        request = urlparse(self.path)
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
        if not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        request = urlparse(self.path)
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

    def send_static(self, name: str) -> None:
        candidate = (ROOT / "static" / name).resolve()
        if candidate.parent != (ROOT / "static").resolve() or not candidate.is_file():
            self.send_error_json(HTTPStatus.NOT_FOUND, "页面不存在。")
            return
        body = candidate.read_bytes()
        content_type = "text/html; charset=utf-8" if candidate.suffix == ".html" else "text/plain; charset=utf-8"
        self.send_response(HTTPStatus.OK)
        self.send_header("Content-Type", content_type)
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)


def main() -> None:
    host = os.environ.get("FM_ADMIN_HOST", "127.0.0.1")
    port = int(os.environ.get("FM_ADMIN_PORT", "8090"))
    if host not in {"127.0.0.1", "localhost", "::1"} and not ADMIN_TOKEN:
        raise SystemExit("FM_ADMIN_TOKEN must be set when FM_ADMIN_HOST is not loopback")
    print(f"FM Control Center listening on http://{host}:{port}", flush=True)
    ThreadingHTTPServer((host, port), Api).serve_forever()


if __name__ == "__main__":
    main()
