import json
import os
import tempfile
import threading
import unittest
from pathlib import Path
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


if __name__ == "__main__":
    unittest.main()
