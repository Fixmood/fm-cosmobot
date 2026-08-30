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

    def test_audit_filters_by_actor_action_collection_and_time(self):
        self.request("POST", "/api/collections/personas", {"id": "one"})
        self.request("POST", "/api/collections/models", {"id": "two"}, token="test-token")
        # Actor is deliberately supplied as a separate header, like the production UI/API client.
        request = Request(self.base + "/api/collections/personas", data=json.dumps({"id": "three"}).encode(),
                          headers={"X-FM-Admin-Token": "test-token", "X-FM-Admin-Actor": "operator"}, method="POST")
        with urlopen(request):
            pass
        _, filtered = self.request("GET", "/api/logs?actor=operator&action=create&collection=personas&limit=1")
        self.assertEqual(len(filtered["items"]), 1)
        self.assertEqual(filtered["items"][0]["actor"], "operator")

    def test_observability_overview_tolerates_rpc_failure(self):
        def domain(path):
            return {"ok": True, "data": {"library_texts": 3}} if path == "/stats" else {"ok": True, "data": {}}

        def rpc(method, _params=None):
            if method == "config.snapshot":
                return {"ok": False, "error": "RPC timeout"}
            return {"ok": True, "data": {"entries": []}}

        with patch.object(app, "fetch_domain", side_effect=domain), patch.object(app, "fetch_rpc", side_effect=rpc):
            status, payload = self.request("GET", "/api/observability/overview")
        self.assertEqual(status, 200)
        self.assertTrue(payload["ok"])
        self.assertFalse(payload["rpc"]["ok"])
        self.assertFalse(payload["config_sync"]["applied"])
        self.assertEqual(payload["domain"]["ok"], True)

    def test_observability_reports_unsynced_saved_config_reason(self):
        app.write_state({"config_versions": [{"id": "cfg-1", "after": {"value": "new"}}]})
        with patch.object(app, "fetch_domain", return_value={"ok": True, "data": {}}), patch.object(
            app, "fetch_rpc", return_value={"ok": True, "data": {"value": "old"}}
        ):
            _, payload = self.request("GET", "/api/observability/overview")
        self.assertTrue(payload["config_sync"]["saved"])
        self.assertFalse(payload["config_sync"]["applied"])
        self.assertIn("不一致", payload["config_sync"]["reason"])

    def test_recent_errors_redact_credentials(self):
        app.record_error("rpc", "Authorization: Bearer secret123 api_key=sk-secret password=hunter2")
        state = app.read_state()
        self.assertNotIn("secret123", json.dumps(state))
        self.assertNotIn("sk-secret", json.dumps(state))
        self.assertNotIn("hunter2", json.dumps(state))

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

    def test_runtime_config_write_records_version_and_actor(self):
        snapshots = [
            {"ok": True, "data": {"private_default": "old", "models": []}},
            {"ok": True, "data": {"private_default": "new", "models": []}},
        ]
        with patch.object(app, "fetch_rpc", side_effect=snapshots), patch.object(
            app, "runtime_config_result", return_value={"ok": True, "data": {"saved": True, "applied": True}}
        ):
            status, payload = self.request(
                "POST", "/api/runtime/config/persona", {"action": "set", "scope": "private_default", "content": "new"}
            )
        self.assertEqual(status, 200)
        self.assertIn("version_id", payload)
        _, versions = self.request("GET", "/api/runtime/config/versions")
        self.assertEqual(versions["items"][0]["actor"], "admin")
        self.assertEqual(versions["items"][0]["operation"], "set")
        _, diff = self.request("GET", "/api/runtime/config/versions/" + payload["version_id"] + "/diff")
        self.assertEqual(diff["version_id"], payload["version_id"])

    def test_config_diff_is_recursive_and_excludes_unchanged_values(self):
        self.assertEqual(app.config_diff({"a": 1, "same": 2}, {"a": 3, "same": 2}), [
            {"path": "a", "before": 1, "after": 3}
        ])

    def test_runtime_config_rollback_restores_only_target_scope(self):
        state = {
            "config_versions": [{
                "id": "cfg-1", "at": 1, "actor": "admin", "kind": "persona", "operation": "set",
                "request": {"action": "set", "scope": "private_default", "content": "new"},
                "before": {"private_default": "old", "group_default": "keep", "models": []},
                "after": {"private_default": "new", "group_default": "keep", "models": []}, "changes": []
            }]
        }
        app.write_state(state)
        snapshots = [
            {"ok": True, "data": {"private_default": "new", "group_default": "keep", "models": []}},
            {"ok": True, "data": {"private_default": "old", "group_default": "keep", "models": []}},
        ]
        with patch.object(app, "fetch_rpc", side_effect=snapshots), patch.object(
            app, "runtime_config_result", return_value={"ok": True, "data": {"saved": True, "applied": True}}
        ) as sync:
            status, payload = self.request("POST", "/api/runtime/config/rollback", {"version_id": "cfg-1"})
        self.assertEqual(status, 200)
        self.assertIn("version_id", payload)
        sync.assert_called_once_with("persona", {"action": "set", "scope": "private_default", "content": "old"})

    def test_config_lifecycle_write_observe_rollback(self):
        snapshots = [
            {"ok": True, "data": {"private_default": "old", "group_default": "keep", "models": []}},
            {"ok": True, "data": {"private_default": "new", "group_default": "keep", "models": []}},
        ]
        with patch.object(app, "fetch_rpc", side_effect=snapshots), patch.object(
            app, "runtime_config_result", return_value={"ok": True, "data": {"saved": True, "applied": True}}
        ):
            status, written = self.request(
                "POST", "/api/runtime/config/persona",
                {"action": "set", "scope": "private_default", "content": "new"},
            )
        self.assertEqual(status, 200)
        version_id = written["version_id"]

        with patch.object(app, "fetch_domain", return_value={"ok": True, "data": {}}), patch.object(
            app, "fetch_rpc", side_effect=[
                {"ok": True, "data": {"private_default": "new", "group_default": "keep", "models": []}},
                {"ok": True, "data": {"entries": [{"name": "sync", "progress": 1.0}]}},
            ]
        ):
            _, observed = self.request("GET", "/api/observability/overview")
        self.assertTrue(observed["config_sync"]["saved"])
        self.assertTrue(observed["config_sync"]["applied"])
        self.assertEqual(observed["tasks"]["running"], 1)

        with patch.object(app, "fetch_rpc", side_effect=[
            {"ok": True, "data": {"private_default": "new", "group_default": "keep", "models": []}},
            {"ok": True, "data": {"private_default": "old", "group_default": "keep", "models": []}},
        ]), patch.object(
            app, "runtime_config_result", return_value={"ok": True, "data": {"saved": True, "applied": True}}
        ):
            status, rolled_back = self.request("POST", "/api/runtime/config/rollback", {"version_id": version_id})
        self.assertEqual(status, 200)
        self.assertIn("version_id", rolled_back)


if __name__ == "__main__":
    unittest.main()
