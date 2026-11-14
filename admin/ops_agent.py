#!/usr/bin/env python3
"""FM 主机运维代理（宿主机 root 运行，队列驱动，白名单 ops）。

为什么需要它：fm-admin 容器没有挂 docker.sock（也不应该挂），所以后台的
「部署面板」不能直接操作 docker。后台只把请求写成队列文件，宿主机上的本代理
负责执行，并把结果写回：

  /opt/fm-admin/data/ops-queue/<id>.json    {"id","op","target","created_at"}
  /opt/fm-admin/data/ops-results/<id>.json  {"id","op","ok","result",...}

白名单只有三类（其余一律拒绝，绝不执行 rm / rmi / run / exec / 任意命令）：
  images  -> docker images，只列 fm-cosmobot:runtime-* 镜像
  inspect -> docker inspect fm-cosmobot 的摘要（镜像/启动时间/健康状态）
  restart -> docker restart -t 5 fm-cosmobot（仅允许 target=bot）

安全约束：
  * docker 一律用固定 argv 调用，不经过 shell，不拼接队列内容；
  * 队列文件名/内容只取 id / op / target 三个字段，其余忽略；
  * 结果文件原子写入（tmp + os.replace），队列文件处理后删除；
  * 单轮最多处理 50 个请求，单个 docker 命令有超时。

用法：
  python3 ops_agent.py --once                        # 扫一轮（cron 默认）
  python3 ops_agent.py --loop 3 --interval 18        # 一分钟内扫 3 轮
  FM_OPS_BASE=/opt/fm-admin/data python3 ops_agent.py --once
"""
from __future__ import annotations

import argparse
import fcntl
import json
import os
import re
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Any

BASE = Path(os.environ.get("FM_OPS_BASE", "/opt/fm-admin/data"))
QUEUE = BASE / "ops-queue"
RESULTS = BASE / "ops-results"
STATE = BASE / "ops-agent-state.json"
LOG = BASE / "ops-agent.log"
LOCK = BASE / ".ops-agent.lock"

CONTAINER_NAME = os.environ.get("FM_OPS_CONTAINER", "fm-cosmobot")
IMAGE_REPO = "fm-cosmobot"
MAX_QUEUE_FILES = 50
REQUEST_MAX_AGE_SECONDS = 600
VERSION = "1.0"

ALLOWED_OPS = ("images", "inspect", "restart")
SAFE_ID = re.compile(r"^[0-9A-Za-z._-]+$")
DOCKER = shutil.which("docker") or "/usr/bin/docker"


def log(message: str) -> None:
    line = "[%s] %s" % (time.strftime("%Y-%m-%d %H:%M:%S"), message)
    print(line, flush=True)
    try:
        BASE.mkdir(parents=True, exist_ok=True)
        if LOG.exists() and LOG.stat().st_size > 1024 * 1024:
            LOG.replace(LOG.with_suffix(".log.1"))
        with LOG.open("a", encoding="utf-8") as handle:
            handle.write(line + "\n")
    except OSError:
        pass


def atomic_write_json(path: Path, payload: Any) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    with temporary.open("w", encoding="utf-8") as handle:
        json.dump(payload, handle, ensure_ascii=False)
    os.replace(temporary, path)


def run_docker(argv: list[str], timeout: int = 60) -> dict:
    """固定 argv 调 docker，不经过 shell。"""
    try:
        completed = subprocess.run(
            [DOCKER] + argv,
            capture_output=True,
            text=True,
            encoding="utf-8",
            errors="replace",
            timeout=timeout,
            cwd="/",
        )
        return {
            "ok": completed.returncode == 0,
            "rc": completed.returncode,
            "stdout": (completed.stdout or "")[:20000],
            "stderr": (completed.stderr or "")[:2000],
        }
    except subprocess.TimeoutExpired:
        return {"ok": False, "rc": -1, "stdout": "", "stderr": "docker 命令超时(%ss)" % timeout}
    except Exception as exc:  # noqa: BLE001
        return {"ok": False, "rc": -1, "stdout": "", "stderr": str(exc)[:2000]}


def op_images() -> dict:
    output = run_docker(["images", "--format", "{{json .}}"], timeout=30)
    if not output["ok"]:
        raise RuntimeError("docker images 失败：" + (output["stderr"] or output["stdout"]))
    images = []
    for line in output["stdout"].splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            row = json.loads(line)
        except ValueError:
            continue
        repository = str(row.get("Repository") or "")
        tag = str(row.get("Tag") or "")
        if repository != IMAGE_REPO or not tag.startswith("runtime-"):
            continue
        images.append({
            "image": repository + ":" + tag,
            "repository": repository,
            "tag": tag,
            "id": str(row.get("ID") or ""),
            "created": str(row.get("CreatedAt") or ""),
            "size": str(row.get("Size") or ""),
            "digest": str(row.get("Digest") or ""),
        })
    return {"images": images, "count": len(images)}


def op_inspect() -> dict:
    output = run_docker(["inspect", CONTAINER_NAME], timeout=30)
    if not output["ok"]:
        raise RuntimeError("docker inspect 失败：" + (output["stderr"] or "容器不存在？"))
    try:
        data = json.loads(output["stdout"])
        item = data[0] if isinstance(data, list) and data else data
    except ValueError:
        raise RuntimeError("docker inspect 返回不是 JSON")
    if not isinstance(item, dict):
        raise RuntimeError("docker inspect 返回结构异常")
    state = item.get("State") if isinstance(item.get("State"), dict) else {}
    health = state.get("Health") if isinstance(state.get("Health"), dict) else {}
    config = item.get("Config") if isinstance(item.get("Config"), dict) else {}
    container = {
        "name": str(item.get("Name") or "").lstrip("/"),
        "image": str(config.get("Image") or ""),
        "image_id": str(item.get("Image") or ""),
        "created": str(item.get("Created") or ""),
        "status": str(state.get("Status") or ""),
        "running": bool(state.get("Running")),
        "started_at": str(state.get("StartedAt") or ""),
        "finished_at": str(state.get("FinishedAt") or ""),
        "restart_count": int(state.get("RestartCount") or 0),
        "health": str(health.get("Status") or ""),
        "platform": "bot",
    }
    return {"container": container}


def op_restart(target: str) -> dict:
    if target != "bot":
        raise PermissionError("restart 只允许 target=bot，收到：" + (target or "(空)"))
    output = run_docker(["restart", "-t", "5", CONTAINER_NAME], timeout=90)
    if not output["ok"]:
        raise RuntimeError("docker restart 失败：" + (output["stderr"] or output["stdout"]))
    inspect_result = op_inspect()
    return {
        "target": "bot",
        "restarted": True,
        "docker_output": (output["stdout"] or "").strip(),
        "container": inspect_result.get("container"),
    }


def execute(request: dict) -> dict:
    """执行一个白名单请求，返回 result 字段内容（失败抛异常）。"""
    op = str(request.get("op") or "").strip().lower()
    target = str(request.get("target") or "").strip().lower()
    if op not in ALLOWED_OPS:
        raise PermissionError("op 不在白名单：" + (op or "(空)"))
    if op == "images":
        return op_images()
    if op == "inspect":
        return op_inspect()
    if op == "restart":
        return op_restart(target)
    raise PermissionError("op 不在白名单：" + op)


def result_payload(request: dict, ok: bool, result: Any = None, error: str = "",
                   started: float | None = None) -> dict:
    finished = time.time()
    payload = {
        "id": str(request.get("id") or ""),
        "op": str(request.get("op") or ""),
        "target": str(request.get("target") or ""),
        "ok": ok,
        "requested_at": request.get("created_at"),
        "started_at": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(started or finished)),
        "finished_at": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(finished)),
        "duration_ms": int((finished - started) * 1000) if started else 0,
        "agent": {"version": VERSION, "pid": os.getpid(), "container": CONTAINER_NAME},
    }
    if error:
        payload["error"] = error
    if result is not None:
        payload["result"] = result
    return payload


def process_queue(scan_id: int) -> dict:
    QUEUE.mkdir(parents=True, exist_ok=True)
    RESULTS.mkdir(parents=True, exist_ok=True)
    try:
        candidates = sorted(QUEUE.glob("*.json"))[:MAX_QUEUE_FILES]
    except OSError as exc:
        return {"processed": 0, "error": str(exc)}
    processed = 0
    failures = 0
    last_id = ""
    for path in candidates:
        stem = path.stem
        if not SAFE_ID.match(stem):
            log("跳过非法队列文件名：" + str(path))
            try:
                path.unlink()
            except OSError:
                pass
            continue
        request: dict = {"id": stem}
        started = time.time()
        try:
            raw = path.read_text(encoding="utf-8")
            parsed = json.loads(raw)
            if not isinstance(parsed, dict):
                raise ValueError("队列内容不是 JSON 对象")
            request = {
                "id": stem,
                "op": parsed.get("op"),
                "target": parsed.get("target"),
                "created_at": parsed.get("created_at"),
            }
            created = float(request.get("created_at") or time.time())
            if time.time() - created > REQUEST_MAX_AGE_SECONDS:
                raise TimeoutError("请求超过 %s 秒未处理，已过期" % REQUEST_MAX_AGE_SECONDS)
            result = execute(request)
            payload = result_payload(request, True, result=result, started=started)
        except Exception as exc:  # noqa: BLE001
            failures += 1
            payload = result_payload(request, False, started=started, error=str(exc)[:1000])
        try:
            atomic_write_json(RESULTS / (stem + ".json"), payload)
            path.unlink()
            processed += 1
            last_id = stem
            log("scan=%s op=%s target=%s id=%s -> %s" % (
                scan_id, payload.get("op"), payload.get("target"), stem,
                "ok" if payload.get("ok") else "FAIL " + str(payload.get("error"))[:200]))
        except OSError as exc:
            log("写结果失败 id=%s：%s" % (stem, exc))
    return {"processed": processed, "failures": failures, "last_id": last_id}


def main() -> int:
    parser = argparse.ArgumentParser(description="FM host ops agent (docker whitelist)")
    parser.add_argument("--once", action="store_true", help="只扫一轮（默认）")
    parser.add_argument("--loop", type=int, default=0, help="连续扫 N 轮后退出")
    parser.add_argument("--interval", type=float, default=18.0, help="每轮间隔秒数（默认 18）")
    args = parser.parse_args()
    loops = args.loop if args.loop and args.loop > 0 else 1

    if not BASE.is_dir():
        log("BASE 目录不存在：" + str(BASE))
        return 2
    BASE.mkdir(parents=True, exist_ok=True)
    lock_handle = None
    try:
        lock_handle = LOCK.open("w", encoding="utf-8")
        fcntl.flock(lock_handle.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
    except OSError:
        log("已有 ops_agent 实例在跑，本轮跳过")
        return 0

    scans = 0
    total = 0
    try:
        for index in range(loops):
            scans += 1
            started = time.time()
            summary = process_queue(scans)
            total += int(summary.get("processed") or 0)
            state = {
                "version": VERSION,
                "pid": os.getpid(),
                "last_scan": time.time(),
                "last_scan_text": time.strftime("%Y-%m-%d %H:%M:%S"),
                "scan_seconds": round(time.time() - started, 3),
                "scans": scans,
                "processed_total": total,
                "last_processed_id": summary.get("last_id") or "",
                "queue": str(QUEUE),
                "results": str(RESULTS),
                "container": CONTAINER_NAME,
                "allowed_ops": list(ALLOWED_OPS),
            }
            atomic_write_json(STATE, state)
            if index < loops - 1:
                time.sleep(max(1.0, args.interval))
    finally:
        try:
            fcntl.flock(lock_handle.fileno(), fcntl.LOCK_UN)
        except Exception:  # noqa: BLE001
            pass
        if lock_handle is not None:
            lock_handle.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
