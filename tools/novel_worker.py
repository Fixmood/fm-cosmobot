#!/usr/bin/env python3
"""Novel download worker (host cron, once a minute).

Layering: FM runs inside a container and cannot run host scripts, but its /data is
the host's /opt/fm-cosmobot/runtime -- so FM writes a job file and this consumes it,
then notifies the requester when the book is in.

Job file: /opt/fm-cosmobot/runtime/novel-jobs/<jobid>.txt, plain key=value
Status:   /opt/fm-cosmobot/runtime/novel-status.txt, human readable lines for FM
"""
from __future__ import annotations

import fcntl
import re
import subprocess
import time
from pathlib import Path

RUNTIME = Path("/opt/fm-cosmobot/runtime")
JOBS = RUNTIME / "novel-jobs"
DONE = RUNTIME / "novel-jobs-done"
STATUS = RUNTIME / "novel-status.txt"
RAW = Path("/opt/fm-domain/assets/novels_raw")
LOG = Path("/opt/fm-cosmobot/novel-worker.log")
DOWNLOADER = Path("/opt/fm-cosmobot/fanqie.py")
IMPORTER_IN_DOMAIN = "/data/import_novels.py"
TOKEN_FILE = RUNTIME / ".rpc-token"
MAX_STATUS_LINES = 200


def log(message: str) -> None:
    line = "[" + time.strftime("%Y-%m-%d %H:%M:%S") + "] " + message
    try:
        with LOG.open("a", encoding="utf-8") as handle:
            handle.write(line + "\n")
    except OSError:
        pass


def status_line(text: str) -> None:
    stamp = time.strftime("%m-%d %H:%M")
    try:
        lines = STATUS.read_text(encoding="utf-8").splitlines() if STATUS.exists() else []
        lines.append("[" + stamp + "] " + text)
        STATUS.write_text("\n".join(lines[-MAX_STATUS_LINES:]) + "\n", encoding="utf-8")
    except OSError as error:
        log("  ! write status failed: " + str(error))


def run(cmd: list[str], timeout: int = 1800) -> tuple[int, str]:
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        return result.returncode, (result.stdout or "") + (result.stderr or "")
    except subprocess.TimeoutExpired:
        return 124, "timeout"
    except Exception as error:  # noqa: BLE001
        return 1, type(error).__name__ + ": " + str(error)


def parse_job(path: Path) -> dict:
    job: dict[str, str] = {"job_id": path.stem}
    for line in path.read_text(encoding="utf-8", errors="ignore").splitlines():
        if "=" in line:
            key, _, value = line.partition("=")
            job[key.strip()] = value.strip()
    return job


def notify(job: dict, content: str) -> None:
    kind = job.get("chat_kind") or "group"
    chat_id = str(job.get("chat_id") or "")
    if not chat_id:
        log("  ! no chat_id, skip notify")
        return
    try:
        token = TOKEN_FILE.read_text(encoding="utf-8").strip()
    except OSError:
        log("  ! rpc token unreadable")
        return
    script = (
        "import json, os, socket, struct, base64\n"
        "def frame(payload):\n"
        "    n = len(payload); header = bytearray([0x81])\n"
        "    if n < 126: header.append(0x80 | n)\n"
        "    elif n < 65536: header.append(0x80 | 126); header += struct.pack('>H', n)\n"
        "    else: header.append(0x80 | 127); header += struct.pack('>Q', n)\n"
        "    mask = os.urandom(4)\n"
        "    return bytes(header + mask) + bytes(b ^ mask[i % 4] for i, b in enumerate(payload))\n"
        "token = " + repr(token) + "\n"
        "params = {'platform':'qq','target_type':" + repr(kind) + ",'target_id':" + repr(chat_id) + ",'content':" + repr(content) + "}\n"
        "sock = socket.create_connection(('127.0.0.1', 38765), timeout=30)\n"
        "key = base64.b64encode(os.urandom(16)).decode()\n"
        "sock.sendall(('GET /rpc HTTP/1.1\\r\\nHost: 127.0.0.1:38765\\r\\nUpgrade: websocket\\r\\n'\n"
        "              'Connection: Upgrade\\r\\nSec-WebSocket-Key: ' + key + '\\r\\n'\n"
        "              'Sec-WebSocket-Version: 13\\r\\nAuthorization: Bearer ' + token + '\\r\\n\\r\\n').encode())\n"
        "sock.recv(4096)\n"
        "sock.sendall(frame(json.dumps({'jsonrpc':'2.0','id':'w','method':'send_direct_message','params':params}).encode()))\n"
        "sock.recv(65536); sock.close(); print('ok')\n"
    )
    code, out = run(["docker", "exec", "-i", "fm-cosmobot", "python3", "-c", script], timeout=90)
    if code == 0:
        log("  ok notified " + kind + ":" + chat_id)
    else:
        log("  ! notify failed: " + out[:200])


def archive(job_path: Path) -> None:
    DONE.mkdir(parents=True, exist_ok=True)
    try:
        job_path.rename(DONE / (job_path.stem + ".txt"))
    except OSError as error:
        log("  ! archive failed: " + str(error))


def process(job_path: Path) -> None:
    job = parse_job(job_path)
    job_id = job.get("job_id") or job_path.stem
    book = job.get("book") or ""
    if not book:
        log("  ! " + job_id + " no book field")
        archive(job_path)
        return
    log("start " + job_id + ": " + book)
    status_line("[downloading] " + book)

    before = {p.name for p in RAW.glob("*.txt")} if RAW.exists() else set()

    code, out = run(["python3", str(DOWNLOADER), "--book", book, "--out", str(RAW)])
    if code != 0:
        tail = out.strip().splitlines()[-1][:120] if out.strip() else "unknown"
        log("  ! " + job_id + " download failed: " + tail)
        status_line("[failed] " + book + " - " + tail)
        notify(job, "Novel download failed: " + book + "\nReason: " + tail)
        archive(job_path)
        return

    after = {p.name for p in RAW.glob("*.txt")} if RAW.exists() else set()
    new_files = sorted(after - before)
    title = new_files[0].replace(".txt", "") if new_files else book
    log("  downloaded, new files: " + str(new_files))

    code, out = run(["docker", "exec", "fm-domain", "python3", IMPORTER_IN_DOMAIN, "/assets/novels_raw"])
    if code != 0:
        log("  ! import failed: " + out[-200:])
        status_line("[failed] " + title + " - import failed")
        notify(job, "Downloaded " + title + " but importing into the library failed.")
        archive(job_path)
        return
    match = re.search(r"chapters=(\d+)", out)
    chapters = int(match.group(1)) if match else 0

    status_line("[importing] " + title + " - " + str(chapters) + " chapters, ranking and classifying")
    run(["docker", "exec", "fm-domain", "python3", "/app/app.py", "rank-library", "--db", "/data/fm-domain.sqlite3"])
    run(["docker", "exec", "fm-domain", "python3", "/app/app.py", "classify-library", "--db", "/data/fm-domain.sqlite3"])

    short_title = title
    status_line("[done] " + short_title + " - " + str(chapters) + " chapters in library")
    log("  ok " + job_id + " done: " + title + " " + str(chapters))
    notify(job, "《" + short_title + "》下好了，" + str(chapters) + " 章已入库。\n现在可以说：fm 来篇" + short_title)
    archive(job_path)


def main() -> int:
    JOBS.mkdir(parents=True, exist_ok=True)
    # 文件锁：cron 每分钟一次，但下载一本要几分钟 —— 没有锁的话同一个任务会被
    # 并发重复拉起来（2026-11-05 实测：日志里 start 同一个任务每 60 秒刷一次）。
    lock_path = RUNTIME / "novel-worker.lock"
    with lock_path.open("w") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError:
            return 0
        pending = sorted(JOBS.glob("*.txt"))
        if not pending:
            return 0
        for job_path in pending:
            # 立刻改名取走，避免下一次 cron 看到它
            running = job_path.with_suffix(".processing")
            try:
                job_path.rename(running)
            except OSError:
                continue
            try:
                process(running)
            except Exception as error:  # noqa: BLE001
                log("! " + running.name + " crashed: " + type(error).__name__ + ": " + str(error))
                status_line("[failed] " + running.stem + " - internal error " + type(error).__name__)
                archive(running)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
