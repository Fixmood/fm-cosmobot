#!/usr/bin/env python3
"""Standalone assertions for cosmobot_worker.py."""

from __future__ import annotations

import io
import json
import subprocess
import sys
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from collections.abc import Iterable

    import cosmobot_worker as worker
    from _typeshed import ReadableBuffer
else:
    import importlib.util

    worker_path = Path(__file__).with_name("cosmobot_worker.py")
    spec = importlib.util.spec_from_file_location("cosmobot_worker", worker_path)
    if spec is None or spec.loader is None:
        raise RuntimeError("failed to load cosmobot_worker.py")
    worker = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(worker)

WORKER_PATH = Path(__file__).with_name("cosmobot_worker.py")


class ShortWriter(io.BytesIO):
    """A stream that accepts at most three bytes per write."""

    def write(self, data: ReadableBuffer, /) -> int:
        """Perform an intentional short write."""
        return super().write(memoryview(data)[:3])


def frame(message: object) -> bytes:
    """Encode a host or worker protocol message."""
    stream = io.BytesIO()
    worker.write_frame(stream, message)
    return stream.getvalue()


def run_fixture(
    code: str,
    responses: Iterable[object] = (),
) -> tuple[bool, list[worker.JsonObject], str]:
    """Run one request against the worker in the current process."""
    request = {
        "jsonrpc": "2.0",
        "id": "host:run",
        "method": "python.run",
        "params": {"code": code},
    }
    stdin = io.BytesIO(frame(request) + b"".join(map(frame, responses)))
    stdout = io.BytesIO()
    stderr = io.BytesIO()
    completed = worker.serve_once(stdin, stdout, stderr)
    stdout.seek(0)
    messages: list[worker.JsonObject] = []
    while stdout.tell() < len(stdout.getvalue()):
        message = worker.read_frame(stdout)
        if not isinstance(message, dict):
            raise AssertionError("worker emitted a non-object frame")
        messages.append(message)
    return completed, messages, stderr.getvalue().decode()


def tool_response(request_id: int, results: list[worker.ToolResult]) -> object:
    """Build one successful tools.run host response."""
    return {
        "jsonrpc": "2.0",
        "id": request_id,
        "result": results,
    }


def success(content: str) -> worker.ToolSuccess:
    """Build one typed nested-tool success fixture."""
    return {"ok": True, "content": content}


def failed(failure: worker.Failure) -> worker.ToolFailure:
    """Build one typed nested-tool failure fixture."""
    return {"ok": False, "failure": failure}


def run_subprocess_fixture(code: str) -> tuple[int, worker.JsonObject, bytes]:
    """Run one request through a fresh worker process."""
    process = subprocess.Popen(
        [sys.executable, "-I", str(WORKER_PATH)],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    )
    if process.stdin is None or process.stdout is None or process.stderr is None:
        raise AssertionError("subprocess pipes were not created")
    request = frame(
        {
            "jsonrpc": "2.0",
            "id": "host:run",
            "method": "python.run",
            "params": {"code": code},
        }
    )
    stdout, stderr = process.communicate(request, timeout=5)
    response = worker.read_frame(io.BytesIO(stdout))
    if not isinstance(response, dict):
        raise AssertionError("worker emitted a non-object response")
    return process.returncode, response, stderr


def result_of(message: worker.JsonObject) -> worker.JsonObject:
    """Extract an asserted object-valued result field."""
    result = message.get("result")
    if not isinstance(result, dict):
        raise AssertionError("message has no object result")
    return result


def text_field(message: worker.JsonObject, key: str) -> str:
    """Extract an asserted string field."""
    value = message.get(key)
    if not isinstance(value, str):
        raise AssertionError(f"{key} is not a string")
    return value


def main() -> None:
    """Run the standalone worker assertions."""
    short = ShortWriter()
    worker.write_frame(short, {"message": "short writes still complete"})
    short.seek(0)
    assert worker.read_frame(short) == {"message": "short writes still complete"}

    failure: worker.Failure = {
        "category": "permission_denied",
        "message": "denied",
        "detail": "tool is hidden",
    }
    completed, messages, _ = run_fixture(
        """import cosmobot
try:
    cosmobot.run_tool('denied', {})
except cosmobot.RunToolException as error:
    assert error.name == 'denied' and error.index == 0
print(cosmobot.run_tool('allowed', {})['content'])
""",
        [
            tool_response(1, [failed(failure)]),
            tool_response(2, [success("continued")]),
        ],
    )
    assert completed and [message["id"] for message in messages] == [1, 2, "host:run"]
    assert result_of(messages[-1]) == {"kind": "completed", "content": "continued\n"}

    completed, messages, _ = run_fixture(
        """import cosmobot
cyclic = []
cyclic.append(cyclic)
try:
    cosmobot.run_tool('invalid', cyclic)
except ValueError:
    pass
print(cosmobot.run_tool('valid', {})['content'])
""",
        [tool_response(1, [success("id stayed one")])],
    )
    assert completed and [message["id"] for message in messages] == [1, "host:run"]

    completed, messages, _ = run_fixture(
        """import cosmobot
try:
    cosmobot.run_tool('rejected', {})
except cosmobot.RunToolException as error:
    assert error.name is None and error.index is None and error.results == []
print(cosmobot.run_tool('valid', {})['content'])
""",
        [
            {"jsonrpc": "2.0", "id": 1, "error": {"code": -32602, "message": "bad"}},
            tool_response(2, [success("continued")]),
        ],
    )
    assert completed and [message["id"] for message in messages] == [1, 2, "host:run"]

    completed, messages, _ = run_fixture(
        """import cosmobot
try:
    cosmobot.run_tools([{'name': 'ok', 'args': {}}, {'name': 'bad', 'args': {}}])
except cosmobot.RunToolException as error:
    assert error.name == 'bad' and error.index == 1
    assert len(error.results) == 2 and error.results[0]['content'] == 'first'
""",
        [
            tool_response(
                1,
                [
                    success("first"),
                    failed(failure),
                ],
            )
        ],
    )
    assert completed and result_of(messages[-1])["kind"] == "completed"

    for spelling in (
        "raise cosmobot.BackToAgent('next')",
        "cosmobot.complete('next')",
    ):
        completed, messages, _ = run_fixture(
            f"""import cosmobot
try:
    try:
        {spelling}
    except Exception:
        raise AssertionError('control was caught')
finally:
    cosmobot.run_tool('finally', {{}})
""",
            [tool_response(1, [success("ran finally")])],
        )
        assert completed and result_of(messages[-1]) == {
            "kind": "completed",
            "content": "next",
        }

    completed, messages, _ = run_fixture(
        """import cosmobot
try:
    try:
        cosmobot.fail('stop exactly')
    except Exception:
        raise AssertionError('control was caught')
finally:
    cosmobot.run_tool('finally', {})
""",
        [tool_response(1, [success("ran finally")])],
    )
    assert completed and result_of(messages[-1]) == {
        "kind": "failed",
        "message": "stop exactly",
    }

    completed, messages, _ = run_fixture(
        """import cosmobot
try:
    cosmobot.complete('caught')
except BaseException:
    print('ordinary execution resumed')
"""
    )
    assert completed
    assert (
        text_field(result_of(messages[-1]), "content") == "ordinary execution resumed\n"
    )

    completed, messages, _ = run_fixture(
        """import cosmobot
print('discard me')
cosmobot.complete('exact')
"""
    )
    assert completed and text_field(result_of(messages[-1]), "content") == "exact"

    capture = worker.Capture(1)
    capture.append("界".encode())
    capture.append(b"a")
    assert capture.text() == worker.TRUNCATION_MARKER[:1].decode()

    completed, messages, _ = run_fixture(
        """import os, sys
sys.__stdout__.buffer.write(b'dunder\\n')
sys.__stdout__.flush()
os.write(1, b'raw fd\\n')
"""
    )
    assert completed and len(messages) == 1
    assert text_field(result_of(messages[0]), "content") == "dunder\nraw fd\n"

    returncode, response, stderr = run_subprocess_fixture(
        """import os, sys
sys.__stdout__.buffer.write(b'dunder\\n')
sys.__stdout__.flush()
os.write(1, b'raw fd\\n')
"""
    )
    assert returncode == 0 and not stderr
    assert text_field(result_of(response), "content") == "dunder\nraw fd\n"

    returncode, response, stderr = run_subprocess_fixture(
        """import os
blocked = False
try:
    names = os.listdir(f'/proc/{os.getppid()}/fd')
except PermissionError:
    blocked = True
else:
    for name in names:
        try:
            os.open(f'/proc/{os.getppid()}/fd/{name}', os.O_RDWR)
        except OSError:
            blocked = True
assert blocked
print('parent fds protected')
"""
    )
    assert returncode == 0 and not stderr
    assert text_field(result_of(response), "content") == "parent fds protected\n"

    completed, messages, _ = run_fixture(
        """import os
assert os.read(0, 1) == b''
import cosmobot
print(cosmobot.run_tool('after-stdin', {})['content'])
""",
        [tool_response(1, [success("still works")])],
    )
    assert completed
    assert text_field(result_of(messages[-1]), "content") == "still works\n"

    completed, messages, _ = run_fixture("print('x' * 9000, end='')")
    assert completed and len(text_field(result_of(messages[-1]), "content")) == 9000
    completed, messages, _ = run_fixture(
        f"print('界' * {worker.MAX_STDOUT_BYTES}, end='')"
    )
    content = text_field(result_of(messages[-1]), "content")
    assert completed and content.endswith(worker.TRUNCATION_MARKER.decode())
    assert len(content.encode()) <= worker.MAX_STDOUT_BYTES

    completed, messages, _ = run_fixture(
        f"import os; os.write(1, b'\\0' * {worker.MAX_STDOUT_BYTES})"
    )
    assert completed and text_field(result_of(messages[-1]), "content").endswith(
        worker.TRUNCATION_MARKER.decode()
    )
    assert len(frame(messages[-1])) <= worker.MAX_RPC_BYTES + 1

    completed, messages, _ = run_fixture(
        """import urllib.request
try:
    urllib.request.urlopen('file:///etc/passwd')
except PermissionError:
    print('blocked')
"""
    )
    assert completed and text_field(result_of(messages[-1]), "content") == "blocked\n"

    for code in ("raise RuntimeError('boom')", "raise SystemExit(0)"):
        completed, messages, failure_stderr = run_fixture(code)
        assert not completed and not messages and failure_stderr

    completed, messages, _ = run_fixture('print(\'{\\"jsonrpc\\":\\"2.0\\"}\')')
    assert completed and len(messages) == 1
    assert text_field(result_of(messages[0]), "content") == '{"jsonrpc":"2.0"}\n'

    for invalid in (
        b"{}",
        b"{not json}\n",
        b" " * (worker.MAX_RPC_BYTES + 1) + b"\n",
    ):
        try:
            worker.read_frame(io.BytesIO(invalid))
        except (ValueError, json.JSONDecodeError):
            pass
        else:
            raise AssertionError("invalid frame was accepted")


if __name__ == "__main__":
    main()
