#!/usr/bin/env python3
"""Small, dependency-free FM Control Center API and static file server."""

from __future__ import annotations

import json
import os
import secrets
import threading
import time
from http import HTTPStatus
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
from urllib.parse import parse_qs, urlparse
from urllib.request import Request, urlopen


ROOT = Path(__file__).resolve().parent
STATE_PATH = Path(os.environ.get("FM_ADMIN_STATE", "/data/fm-admin-state.json"))
DOMAIN_URL = os.environ.get("FM_DOMAIN_URL", "http://127.0.0.1:8077").rstrip("/")
ADMIN_TOKEN = os.environ.get("FM_ADMIN_TOKEN", "").strip()
LOCK = threading.RLock()
COLLECTIONS = {"groups", "triggers", "personas", "models"}
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
            return {"groups": {}, "triggers": {}, "personas": {}, "models": {}, "audit": []}
        try:
            value = json.loads(STATE_PATH.read_text(encoding="utf-8"))
        except (OSError, ValueError):
            value = {}
        return {
            **{name: value.get(name, {}) for name in COLLECTIONS},
            "audit": value.get("audit", [])[-200:],
        }


def write_state(state: dict) -> None:
    with LOCK:
        STATE_PATH.parent.mkdir(parents=True, exist_ok=True)
        temporary = STATE_PATH.with_suffix(STATE_PATH.suffix + ".tmp")
        temporary.write_text(json.dumps(state, ensure_ascii=False, indent=2), encoding="utf-8")
        temporary.replace(STATE_PATH)


def record_audit(action: str, collection: str, item_id: str) -> None:
    state = read_state()
    state["audit"].append({
        "action": action,
        "collection": collection,
        "item_id": item_id,
        "at": time.time(),
    })
    write_state(state)


def fetch_domain(path: str) -> dict:
    request = Request(DOMAIN_URL + path, headers={"Accept": "application/json"})
    try:
        with urlopen(request, timeout=4) as response:
            return {"ok": True, "data": json.loads(response.read().decode("utf-8"))}
    except Exception as error:  # The dashboard must show partial outages instead of failing entirely.
        return {"ok": False, "error": str(error)}


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
        elif request.path == "/api/logs":
            self.send_json(HTTPStatus.OK, {"ok": True, "items": list(reversed(read_state()["audit"]))})
        elif request.path.startswith("/api/domain/"):
            name = request.path.removeprefix("/api/domain/")
            path = DOMAIN_READS.get(name)
            if path is None:
                self.send_error_json(HTTPStatus.NOT_FOUND, "领域数据接口不存在。")
            else:
                result = fetch_domain(path)
                self.send_json(HTTPStatus.OK if result["ok"] else HTTPStatus.BAD_GATEWAY, result)
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
        if request.path.startswith("/api/collections/"):
            self.collection_write(request.path.removeprefix("/api/collections/"), replace=False)
        else:
            self.send_error_json(HTTPStatus.NOT_FOUND, "接口不存在。")

    def do_PUT(self) -> None:
        if not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        request = urlparse(self.path)
        if request.path.startswith("/api/collections/"):
            self.collection_write(request.path.removeprefix("/api/collections/"), replace=True)
        else:
            self.send_error_json(HTTPStatus.NOT_FOUND, "接口不存在。")

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
        record_audit("update" if replace else "create", collection, target_id)
        self.send_json(HTTPStatus.OK, {"ok": True, "item": item})

    def do_DELETE(self) -> None:
        if not self.authorized():
            self.send_error_json(HTTPStatus.UNAUTHORIZED, "需要有效的 FM_ADMIN_TOKEN。")
            return
        parts = [part for part in urlparse(self.path).path.removeprefix("/api/collections/").split("/") if part]
        if len(parts) != 2 or parts[0] not in COLLECTIONS:
            self.send_error_json(HTTPStatus.BAD_REQUEST, "删除需要提供资源名称和 id。")
            return
        state = read_state()
        if state[parts[0]].pop(parts[1], None) is None:
            self.send_error_json(HTTPStatus.NOT_FOUND, "记录不存在。")
            return
        write_state(state)
        record_audit("delete", parts[0], parts[1])
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
