#!/usr/bin/env python3
"""Trusted standard-library supervisor for one py invocation."""

from __future__ import annotations

import contextlib
import ctypes
import json
import os
import selectors
import signal
import socket
import sys
import traceback
import types
from typing import TYPE_CHECKING, BinaryIO, NoReturn, TypeAlias, TypedDict, cast

if TYPE_CHECKING:
    from collections.abc import Callable

MAX_RPC_BYTES = 4 * 1024 * 1024
MAX_CONTROL_BYTES = 8 * 1024
MAX_STDOUT_BYTES = 1024 * 1024
MAX_STDERR_BYTES = 64 * 1024
TRUNCATION_MARKER = b"\n[output truncated]\n"
PR_SET_DUMPABLE = 4


JsonValue: TypeAlias = (
    bool | int | float | str | list["JsonValue"] | dict[str, "JsonValue"] | None
)
JsonObject: TypeAlias = dict[str, JsonValue]


class Failure(TypedDict):
    """A nested tool failure received from the host."""

    category: str
    message: str
    detail: str


class ToolCall(TypedDict):
    """One nested tool invocation sent to the host."""

    name: str
    args: JsonValue


class ToolSuccess(TypedDict):
    """A successful nested tool result."""

    ok: bool
    content: str


class ToolFailure(TypedDict):
    """A failed nested tool result."""

    ok: bool
    failure: Failure


ToolResult: TypeAlias = ToolSuccess | ToolFailure


def read_frame(stream: BinaryIO) -> JsonValue:
    """Read one newline-delimited, size-bounded JSON value."""
    frame = stream.readline(MAX_RPC_BYTES + 2)
    if not frame.endswith(b"\n"):
        raise ValueError("JSON-RPC frame is missing its terminating newline")
    payload = frame[:-1]
    if len(payload) > MAX_RPC_BYTES:
        raise ValueError("JSON-RPC frame exceeds 4 MiB")
    if b"\n" in payload:
        raise ValueError("JSON-RPC frame contains an unescaped newline")
    return cast("JsonValue", json.loads(payload))


def write_frame(stream: BinaryIO, message: object) -> None:
    """Write one newline-delimited, size-bounded JSON value."""
    payload = json.dumps(
        message,
        ensure_ascii=False,
        allow_nan=False,
        separators=(",", ":"),
    ).encode()
    if len(payload) > MAX_RPC_BYTES:
        raise ValueError("JSON-RPC frame exceeds 4 MiB")
    write_all(stream, payload + b"\n")
    stream.flush()


def write_all(stream: BinaryIO, data: bytes) -> None:
    """Write all bytes even when the stream performs short writes."""
    remaining = memoryview(data)
    while remaining:
        written = stream.write(remaining)
        if not written:
            raise EOFError("JSON-RPC stream closed during write")
        remaining = remaining[written:]


def bounded_text(value: object, label: str, *, allow_empty: bool = True) -> str:
    """Validate a control-plane string and its UTF-8 byte length."""
    if not isinstance(value, str):
        raise TypeError(f"{label} must be a string")
    if not allow_empty and not value:
        raise ValueError(f"{label} must not be empty")
    if len(value) > MAX_CONTROL_BYTES or len(value.encode()) > MAX_CONTROL_BYTES:
        raise ValueError(f"{label} exceeds 8 KiB")
    return value


class RunToolException(Exception):
    """Expose a nested tool failure to caller code."""

    def __init__(
        self,
        name: str | None,
        index: int | None,
        failure: Failure,
        results: list[ToolResult],
    ) -> None:
        """Store the failed call and all batch results."""
        self.name = name
        self.index = index
        self.failure = failure
        self.results = results
        super().__init__(failure["message"])


class BackToAgent(BaseException):
    """Unwind caller code with an ordinary completion."""

    def __init__(self, prompt: object) -> None:
        """Validate and store the completion text."""
        self.prompt = bounded_text(prompt, "BackToAgent prompt")
        super().__init__(self.prompt)


class _Fail(BaseException):
    def __init__(self, message: object) -> None:
        self.message = bounded_text(message, "failure message", allow_empty=False)
        super().__init__(self.message)


class ChildClient:
    """Relay nested tool calls over the private supervisor channel."""

    def __init__(self, channel: BinaryIO) -> None:
        """Keep the owned bidirectional child channel."""
        self.channel = channel

    def run(self, calls: list[ToolCall]) -> list[ToolResult]:
        """Send a validated batch and validate its host results."""
        write_frame(self.channel, {"kind": "tools", "calls": calls})
        response = require_object(read_frame(self.channel), "nested tool response")
        if "error" in response:
            failure = validate_failure(response["error"])
            raise RunToolException(None, None, failure, [])
        results = response.get("results")
        if not isinstance(results, list) or len(results) != len(calls):
            raise ValueError("nested tool result count does not match its request")
        return [validate_result(result) for result in results]


def validate_result(result: object) -> ToolResult:
    """Validate and narrow one nested tool result."""
    if not isinstance(result, dict):
        raise ValueError("invalid nested tool result envelope")
    envelope = cast("dict[str, object]", result)
    if type(envelope.get("ok")) is not bool:
        raise ValueError("invalid nested tool result envelope")
    if envelope["ok"]:
        if set(envelope) != {"ok", "content"} or not isinstance(
            envelope["content"], str
        ):
            raise ValueError("invalid nested tool success envelope")
        return cast("ToolSuccess", envelope)
    failure = envelope.get("failure")
    if set(envelope) != {"ok", "failure"} or not isinstance(failure, dict):
        raise ValueError("invalid nested tool failure envelope")
    validate_failure(failure)
    return cast("ToolFailure", envelope)


def validate_failure(failure: object) -> Failure:
    """Validate and narrow one nested failure detail."""
    if not isinstance(failure, dict):
        raise ValueError("invalid nested tool failure detail")
    detail = cast("dict[str, object]", failure)
    if set(detail) != {
        "category",
        "message",
        "detail",
    } or not all(isinstance(detail[key], str) for key in detail):
        raise ValueError("invalid nested tool failure detail")
    return cast("Failure", detail)


def validate_calls(calls: object) -> list[ToolCall]:
    """Validate and narrow a non-empty nested tool batch."""
    if not isinstance(calls, list) or not calls:
        raise ValueError("calls must be a non-empty list")
    if len(calls) > 16:
        raise ValueError("calls accepts at most 16 entries")
    for call in calls:
        if not isinstance(call, dict):
            raise ValueError("each call must contain exactly name and args")
        entry = cast("dict[str, object]", call)
        if set(entry) != {"name", "args"}:
            raise ValueError("each call must contain exactly name and args")
        if not isinstance(entry["name"], str) or not entry["name"]:
            raise ValueError("tool name must be a non-empty string")
    json.dumps(calls, ensure_ascii=False, allow_nan=False)
    return cast("list[ToolCall]", calls)


def require_object(value: JsonValue, label: str) -> JsonObject:
    """Require a decoded JSON object at a protocol boundary."""
    if not isinstance(value, dict):
        raise ValueError(f"{label} must be an object")
    return value


def install_module(client: ChildClient) -> None:
    """Install the in-memory API exposed to caller code."""
    module = types.ModuleType("cosmobot")

    def run_tools(calls: object) -> list[ToolResult]:
        checked_calls = validate_calls(calls)
        results = client.run(checked_calls)
        for index, result in enumerate(results):
            if not result["ok"]:
                raise RunToolException(
                    checked_calls[index]["name"],
                    index,
                    cast("ToolFailure", result)["failure"],
                    results,
                )
        return results

    def run_tool(name: str, args_json: JsonValue) -> ToolResult:
        return run_tools([{"name": name, "args": args_json}])[0]

    def complete(content: object = "") -> NoReturn:
        raise BackToAgent(content)

    def fail(message: object) -> NoReturn:
        raise _Fail(message)

    module.__dict__.update(
        {
            "RunToolException": RunToolException,
            "BackToAgent": BackToAgent,
            "run_tool": run_tool,
            "run_tools": run_tools,
            "complete": complete,
            "fail": fail,
        },
    )
    sys.modules["cosmobot"] = module


def reject_file_urls(event: str, args: tuple[object, ...]) -> None:
    """Reject urllib file URLs before it opens a local path."""
    if event == "urllib.Request" and str(args[0]).lower().startswith("file:"):
        raise PermissionError("file:// URLs are disabled in py")


def parse_run_request(request: JsonValue) -> str:
    """Validate the one accepted host JSON-RPC request."""
    if not isinstance(request, dict) or request.get("jsonrpc") != "2.0":
        raise ValueError("invalid python.run JSON-RPC request")
    if request.get("id") != "host:run" or request.get("method") != "python.run":
        raise ValueError("unexpected Python worker request")
    if set(request) != {"jsonrpc", "id", "method", "params"}:
        raise ValueError("invalid python.run request fields")
    params = request["params"]
    if not isinstance(params, dict) or set(params) != {"code"}:
        raise ValueError("python.run params must contain exactly code")
    if not isinstance(params["code"], str):
        raise ValueError("python.run code must be a string")
    return params["code"]


def child_main(code: str, channel_fd: int, stdout_fd: int, stderr_fd: int) -> None:
    """Execute caller code in the untrusted fork with redirected streams."""
    null_fd = os.open("/dev/null", os.O_RDONLY)
    os.dup2(null_fd, 0)
    os.close(null_fd)
    os.dup2(stdout_fd, 1)
    os.dup2(stderr_fd, 2)
    os.closerange(3, channel_fd)
    os.closerange(channel_fd + 1, os.sysconf("SC_OPEN_MAX"))
    channel = cast("BinaryIO", os.fdopen(channel_fd, "r+b", buffering=0))
    sys.stdout = sys.__stdout__ = os.fdopen(  # type: ignore[misc]
        os.dup(1), "w", encoding="utf-8"
    )
    sys.stderr = sys.__stderr__ = os.fdopen(  # type: ignore[misc]
        os.dup(2), "w", encoding="utf-8"
    )
    sys.addaudithook(reject_file_urls)
    install_module(ChildClient(channel))
    try:
        exec(compile(code, "<py>", "exec"), {})
    except BackToAgent as completed:
        write_frame(channel, {"kind": "completed", "content": completed.prompt})
    except _Fail as failed:
        write_frame(channel, {"kind": "failed", "message": failed.message})
    except BaseException as error:
        traceback.print_exception(error)
        sys.stderr.flush()
        raise SystemExit(1) from error
    else:
        sys.stdout.flush()
        write_frame(channel, {"kind": "fallthrough"})


class Capture:
    """Retain a bounded, valid-UTF-8 prefix of one output stream."""

    def __init__(self, limit: int) -> None:
        """Create an empty capture with a byte limit."""
        self.limit = limit
        self.data = bytearray()
        self.truncated = False

    def append(self, chunk: bytes) -> None:
        """Append bytes or finish the capture with a truncation marker."""
        if self.truncated:
            return
        if len(self.data) + len(chunk) <= self.limit:
            self.data.extend(chunk)
            return
        marker = TRUNCATION_MARKER[: self.limit]
        prefix_limit = self.limit - len(marker)
        self.data.extend(chunk[: max(0, prefix_limit - len(self.data))])
        del self.data[prefix_limit:]
        self.data[:] = bytes(self.data).decode(errors="ignore").encode()
        self.data.extend(marker)
        self.truncated = True

    def text(self) -> str:
        """Decode the retained UTF-8-compatible bytes."""
        return bytes(self.data).decode(errors="ignore")


def run_child(
    code: str,
    host_in: BinaryIO,
    host_out: BinaryIO,
    host_stderr: BinaryIO,
) -> JsonObject | None:
    """Fork, supervise, and reap one untrusted caller program."""
    prctl = cast(
        "Callable[[int, int, int, int, int], int]",
        ctypes.CDLL(None, use_errno=True).prctl,
    )
    if prctl(PR_SET_DUMPABLE, 0, 0, 0, 0) != 0:
        raise OSError(ctypes.get_errno(), "failed to protect Python supervisor fds")
    parent_channel, child_channel = socket.socketpair()
    stdout_read, stdout_write = os.pipe()
    stderr_read, stderr_write = os.pipe()
    pid = os.fork()
    if pid == 0:
        parent_channel.close()
        os.close(stdout_read)
        os.close(stderr_read)
        try:
            child_main(code, child_channel.detach(), stdout_write, stderr_write)
        except BaseException:
            traceback.print_exc()
            os._exit(1)
        else:
            os._exit(0)
    child_channel.close()
    os.close(stdout_write)
    os.close(stderr_write)
    try:
        return supervise(
            pid,
            parent_channel,
            stdout_read,
            stderr_read,
            host_in,
            host_out,
            host_stderr,
        )
    except BaseException:
        with contextlib.suppress(ProcessLookupError):
            os.kill(pid, signal.SIGKILL)
        with contextlib.suppress(ChildProcessError):
            os.waitpid(pid, 0)
        raise


def supervise(
    pid: int,
    channel: socket.socket,
    stdout_fd: int,
    stderr_fd: int,
    host_in: BinaryIO,
    host_out: BinaryIO,
    host_stderr: BinaryIO,
) -> JsonObject | None:
    """Relay control frames and bounded output until the child exits."""
    selector = selectors.DefaultSelector()
    channel_fd = channel.detach()
    channel_file = cast("BinaryIO", os.fdopen(channel_fd, "r+b", buffering=0))
    selector.register(channel_fd, selectors.EVENT_READ, "control")
    selector.register(stdout_fd, selectors.EVENT_READ, "stdout")
    selector.register(stderr_fd, selectors.EVENT_READ, "stderr")
    stdout = Capture(MAX_STDOUT_BYTES)
    stderr = Capture(MAX_STDERR_BYTES)
    terminal: JsonObject | None = None
    next_id = 1
    try:
        while selector.get_map():
            for key, _ in selector.select():
                terminal, next_id = consume_event(
                    key,
                    selector,
                    channel_fd,
                    channel_file,
                    stdout,
                    stderr,
                    host_in,
                    host_out,
                    terminal,
                    next_id,
                )
    finally:
        selector.close()
    _, status = os.waitpid(pid, 0)
    if terminal is None or status != 0:
        if stderr.data:
            host_stderr.write(bytes(stderr.data))
            host_stderr.flush()
        return None
    if terminal["kind"] == "fallthrough":
        return {
            "kind": "completed",
            "content": stdout.text() or "Python completed successfully.",
        }
    if terminal["kind"] == "completed":
        return {
            "kind": "completed",
            "content": bounded_text(terminal.get("content"), "completion"),
        }
    return {
        "kind": "failed",
        "message": bounded_text(
            terminal.get("message"),
            "failure message",
            allow_empty=False,
        ),
    }


def consume_event(
    key: selectors.SelectorKey,
    selector: selectors.BaseSelector,
    channel_fd: int,
    channel_file: BinaryIO,
    stdout: Capture,
    stderr: Capture,
    host_in: BinaryIO,
    host_out: BinaryIO,
    terminal: JsonObject | None,
    next_id: int,
) -> tuple[JsonObject | None, int]:
    """Consume one ready control or captured-output descriptor."""
    event = cast("str", key.data)
    if event == "control":
        try:
            message = require_object(read_frame(channel_file), "child message")
        except (EOFError, ValueError, json.JSONDecodeError):
            selector.unregister(channel_fd)
            channel_file.close()
            return terminal, next_id
        kind = message.get("kind")
        if kind == "tools" and terminal is None:
            calls = validate_calls(message.get("calls"))
            write_frame(
                host_out,
                {
                    "jsonrpc": "2.0",
                    "id": next_id,
                    "method": "tools.run",
                    "params": {"calls": calls},
                },
            )
            response = require_object(read_frame(host_in), "tools.run response")
            if response.get("jsonrpc") != "2.0" or response.get("id") != next_id:
                raise ValueError("invalid tools.run response")
            if "error" in response:
                write_frame(
                    channel_file,
                    {
                        "error": {
                            "category": "external_service_unavailable",
                            "message": "Host rejected the nested tool request.",
                            "detail": str(response["error"])[:500],
                        }
                    },
                )
                return terminal, next_id + 1
            results = response.get("result")
            if not isinstance(results, list):
                raise ValueError("tools.run response has no result array")
            write_frame(channel_file, {"results": results})
            return terminal, next_id + 1
        if kind in {"fallthrough", "completed", "failed"} and terminal is None:
            return message, next_id
        raise ValueError("invalid child protocol message")
    chunk = os.read(key.fd, 65536)
    if chunk:
        (stdout if event == "stdout" else stderr).append(chunk)
    else:
        selector.unregister(key.fileobj)
        os.close(key.fd)
    return terminal, next_id


def serve_once(
    stdin: BinaryIO | None = None,
    stdout: BinaryIO | None = None,
    stderr: BinaryIO | None = None,
) -> bool:
    """Serve exactly one host request."""
    if stdin is None:
        stdin = sys.stdin.buffer
    if stdout is None:
        stdout = sys.stdout.buffer
    if stderr is None:
        stderr = sys.stderr.buffer
    code = parse_run_request(read_frame(stdin))
    result = run_child(code, stdin, stdout, stderr)
    if result is None:
        return False
    response: JsonObject = {"jsonrpc": "2.0", "id": "host:run", "result": result}
    write_frame(stdout, fit_frame(response))
    return True


def fit_frame(response: JsonObject) -> JsonObject:
    """Truncate completed content until its response frame fits."""
    encoded_size = len(
        json.dumps(response, ensure_ascii=False, separators=(",", ":")).encode(),
    )
    if encoded_size <= MAX_RPC_BYTES:
        return response
    result = require_object(response.get("result", {}), "terminal result")
    if result.get("kind") != "completed":
        raise ValueError("terminal JSON-RPC frame exceeds 4 MiB")
    marker = TRUNCATION_MARKER.decode()
    empty = {**response, "result": {**result, "content": ""}}
    budget = MAX_RPC_BYTES - len(
        json.dumps(empty, ensure_ascii=False, separators=(",", ":")).encode()
    )
    marker_size = json_string_bytes(marker)
    used = 0
    prefix = []
    content = result.get("content")
    if not isinstance(content, str):
        raise ValueError("completed terminal result omitted content")
    for char in content:
        size = json_string_bytes(char)
        if used + size + marker_size > budget:
            break
        prefix.append(char)
        used += size
    return {
        **response,
        "result": {**result, "content": "".join(prefix) + marker},
    }


def json_string_bytes(text: str) -> int:
    """Count UTF-8 bytes occupied inside a JSON string literal."""
    size = 0
    for char in text:
        codepoint = ord(char)
        if char in {'"', "\\"}:
            size += 2
        elif codepoint < 0x20:
            size += 2 if char in "\b\f\n\r\t" else 6
        else:
            size += len(char.encode())
    return size


def main() -> int:
    """Reject arguments and serve one request."""
    if sys.argv[1:]:
        raise SystemExit("usage: cosmobot_worker.py")
    return 0 if serve_once() else 1


if __name__ == "__main__":
    raise SystemExit(main())
