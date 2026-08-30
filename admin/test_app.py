import json
import os
import tempfile
import threading
import unittest
from pathlib import Path
from unittest.mock import patch
from urllib.request import Request, urlopen

import app


class AdminApiTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.old_state = app.STATE_PATH
        self.old_token = app.ADMIN_TOKEN
        app.STATE_PATH = Path(self.tmp.name) / "state.json"
        app.ADMIN_TOKEN = "test-token"
        self.server = app.ThreadingHTTPServer(("127.0.0.1", 0), app.Api)
        threading.Thread(target=self.server.serve_forever, daemon=True).start()
        self.base = f"http://127.0.0.1:{self.server.server_port}"

    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        app.STATE_PATH = self.old_state
        app.ADMIN_TOKEN = self.old_token
        self.tmp.cleanup()

    def request(self, method, path, payload=None, token="test-token"):
        body = None if payload is None else json.dumps(payload).encode()
        headers = {"X-FM-Admin-Token": token}
        if body:
            headers["Content-Type"] = "application/json"
        request = Request(self.base + path, data=body, headers=headers, method=method)
        with urlopen(request) as response:
            return response.status, json.loads(response.read())

    def test_crud_is_authenticated_and_audited(self):
        with self.assertRaises(Exception):
            self.request("GET", "/api/collections/personas", token="bad")
        status, created = self.request("POST", "/api/collections/personas", {"id": "default", "text": "FM"})
        self.assertEqual(status, 200)
        self.assertEqual(created["item"]["id"], "default")
        _, listed = self.request("GET", "/api/collections/personas")
        self.assertEqual(len(listed["items"]), 1)
        self.request("PUT", "/api/collections/personas/default", {"id": "ignored", "text": "FM v2"})
        self.request("DELETE", "/api/collections/personas/default")
        _, logs = self.request("GET", "/api/logs")
        self.assertEqual([item["action"] for item in logs["items"]], ["delete", "update", "create"])

    def test_static_dashboard(self):
        with urlopen(self.base + "/") as response:
            self.assertEqual(response.status, 200)
            self.assertIn(b"FM Control Center", response.read())

    def test_domain_data_is_exposed_as_read_only_proxy(self):
        with patch.object(app, "fetch_domain", return_value={"ok": True, "data": {"items": []}}) as fetch:
            status, payload = self.request("GET", "/api/domain/contests")
        self.assertEqual(status, 200)
        self.assertTrue(payload["ok"])
        fetch.assert_called_once_with("/contest/search?limit=50")

    def test_runtime_rpc_is_gracefully_disabled_by_default(self):
        old_enabled = app.RPC_ENABLED
        app.RPC_ENABLED = False
        try:
            with self.assertRaises(Exception) as context:
                self.request("GET", "/api/runtime/audit")
            self.assertIn("502", str(context.exception))
        finally:
            app.RPC_ENABLED = old_enabled

    def test_runtime_rpc_returns_data_without_leaking_token(self):
        with patch.object(app, "fetch_rpc", return_value={"ok": True, "data": {"entries": []}}) as fetch:
            status, payload = self.request("GET", "/api/runtime/concurrency")
        self.assertEqual(status, 200)
        self.assertEqual(payload["data"], {"entries": []})
        fetch.assert_called_once_with("concurrency.list")

    def test_runtime_rpc_failure_is_bad_gateway(self):
        with patch.object(app, "fetch_rpc", return_value={"ok": False, "error": "timeout"}):
            with self.assertRaises(Exception) as context:
                self.request("GET", "/api/runtime/media")
        self.assertIn("502", str(context.exception))

    def test_runtime_config_write_returns_saved_and_applied(self):
        with patch.object(app, "runtime_config_result", return_value={
            "ok": True, "data": {"saved": True, "applied": True}
        }) as sync:
            status, payload = self.request("POST", "/api/runtime/config/persona", {
                "action": "set", "scope": "private_default", "content": "简洁。"
            })
        self.assertEqual(status, 200)
        self.assertEqual(payload["data"]["applied"], True)
        sync.assert_called_once_with("persona", {"action": "set", "scope": "private_default", "content": "简洁。"})

    def test_runtime_config_delete_maps_to_clear(self):
        with patch.object(app, "runtime_config_result", return_value={
            "ok": True, "data": {"saved": True, "applied": True}
        }) as sync:
            status, _ = self.request("DELETE", "/api/runtime/config/trigger")
        self.assertEqual(status, 200)
        sync.assert_called_once_with("trigger", {"action": "clear"})


if __name__ == "__main__":
    unittest.main()
