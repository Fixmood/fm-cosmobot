#!/usr/bin/env python3
"""FM 主机运维代理（宿主机 root 运行，队列驱动，白名单 ops）。

为什么需要它：fm-admin 容器没有挂 docker.sock（也不应该挂），所以后台的
「部署面板 / 运维监控」不能直接操作 docker。后台只把请求写成队列文件，宿主机上的
本代理负责执行，并把结果写回：

  /opt/fm-admin/data/ops-queue/<id>.json    {"id","op","target","params","created_at"}
  /opt/fm-admin/data/ops-results/<id>.json  {"id","op","ok","result",...}

白名单（其余一律拒绝，绝不执行 rm / rmi / run / exec / 任意 shell）：
  images         -> docker images，只列 fm-cosmobot:runtime-* 镜像（后台部署面板用）
  inspect        -> docker inspect fm-cosmobot 的摘要（镜像/启动时间/健康状态）
  restart        -> docker restart -t 5 fm-cosmobot（仅允许 target=bot）
  images_detail  -> docker images fm-cosmobot 全量 tag/大小/创建时间（运维监控页）
  logs           -> docker logs fm-cosmobot --tail N [--since T]（只读）
  metrics        -> 宿主机 load/mem/disk/uptime + docker stats 摘要（只读）
  cron           -> crontab -l（只读）
  rollback       -> 只校验镜像存在 + 返回人工回滚计划；**不自动重建容器**（受保护）
  backup_domain  -> 用 sqlite3 backup API 备份 /opt/fm-domain/data/fm-domain.sqlite3

安全约束：
  * docker 一律用固定 argv 调用，不经过 shell，不拼接队列内容；
  * 队列只取 id / op / target / params 字段，params 只允许简单标量且长度受限；
  * 结果文件原子写入（tmp + os.replace），队列文件处理后删除；
  * 单轮最多处理 50 个请求，单个 docker 命令有超时；备份文件只写
    /opt/fm-admin/data/backups 且按数量保留。

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
import sqlite3
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
BACKUPS = BASE / "backups"

CONTAINER_NAME = os.environ.get("FM_OPS_CONTAINER", "fm-cosmobot")
IMAGE_REPO = "fm-cosmobot"
DOMAIN_DB = os.environ.get("FM_OPS_DOMAIN_DB", "/opt/fm-domain/data/fm-domain.sqlite3")
MAX_QUEUE_FILES = 50
REQUEST_MAX_AGE_SECONDS = 600
LOG_TAIL_MAX = 2000
LOG_BYTES_MAX = 400000
DOMAIN_BACKUP_KEEP = 3
VERSION = "1.1"

ALLOWED_OPS = ("images", "inspect", "restart", "images_detail", "logs",
               "metrics", "cron", "rollback", "backup_domain")
SAFE_ID = re.compile(r"^[0-9A-Za-z._-]+$")
SAFE_PARAM_KEY = re.compile(r"^[0-9A-Za-z_.-]{1,40}$")
SAFE_IMAGE = re.compile(r"^fm-cosmobot:runtime-[A-Za-z0-9._-]{1,90}$")
DOCKER = shutil.which("docker") or "/usr/bin/docker"
CRONTAB = shutil.which("crontab") or "/usr/bin/crontab"


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
            "stdout": (completed.stdout or "")[:LOG_BYTES_MAX],
            "stderr": (completed.stderr or "")[:20000],
        }
    except subprocess.TimeoutExpired:
        return {"ok": False, "rc": -1, "stdout": "", "stderr": "docker 命令超时(%ss)" % timeout}
    except Exception as exc:  # noqa: BLE001
        return {"ok": False, "rc": -1, "stdout": "", "stderr": str(exc)[:2000]}


def human_size(size: object) -> str:
    try:
        value = float(size)
    except (TypeError, ValueError):
        return "-"
    for unit in ("B", "KB", "MB", "GB", "TB"):
        if value < 1024 or unit == "TB":
            return ("%.0f %s" if unit == "B" else "%.1f %s") % (value, unit)
        value /= 1024.0
    return "-"


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


def op_images_detail() -> dict:
    """fm-cosmobot 全部镜像（含非 runtime tag）+ 当前容器镜像（运维监控用）。"""
    output = run_docker(["images", IMAGE_REPO, "--format", "{{json .}}"], timeout=40)
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
        if repository != IMAGE_REPO:
            continue
        images.append({
            "image": repository + ":" + tag,
            "repository": repository,
            "tag": tag,
            "id": str(row.get("ID") or ""),
            "created_at": str(row.get("CreatedAt") or ""),
            "created_since": str(row.get("CreatedSince") or ""),
            "size": str(row.get("Size") or ""),
            "unique_size": str(row.get("UniqueSize") or ""),
            "digest": str(row.get("Digest") or ""),
        })
    try:
        current = op_inspect().get("container") or {}
    except Exception:  # noqa: BLE001
        current = {}
    return {"images": images, "count": len(images), "current_container": current}


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


_TIMESTAMP_RE = re.compile(r"^(\d{4}-\d{2}-\d{2}T[0-9:.]+Z)\s?(.*)$")


def _parse_log_lines(text: str, stream: str) -> list[dict]:
    lines = []
    for raw in (text or "").splitlines():
        match = _TIMESTAMP_RE.match(raw)
        if match:
            lines.append({"ts": match.group(1), "text": match.group(2)[:4000], "stream": stream})
        elif raw.strip():
            lines.append({"ts": "", "text": raw[:4000], "stream": stream})
    return lines


def op_logs(params: dict) -> dict:
    try:
        tail = int(params.get("tail") or 200)
    except (TypeError, ValueError):
        tail = 200
    tail = max(1, min(LOG_TAIL_MAX, tail))
    since = str(params.get("since") or "").strip()[:64]
    if since and not re.fullmatch(r"[0-9A-Za-z:._+T-]{4,64}", since):
        raise ValueError("since 参数格式不正确：" + since)
    argv = ["logs", "--timestamps"]
    if since:
        argv += ["--since", since]
    argv += ["--tail", str(tail), CONTAINER_NAME]
    output = run_docker(argv, timeout=30)
    lines = _parse_log_lines(output["stdout"], "stdout")
    stderr_lines = _parse_log_lines(output["stderr"], "stderr")
    if stderr_lines and not output["ok"]:
        raise RuntimeError("docker logs 失败：" + (output["stderr"] or "")[:1000])
    lines.extend(stderr_lines)
    lines.sort(key=lambda item: item["ts"] or "9999")
    return {
        "container": CONTAINER_NAME,
        "tail": tail,
        "since": since,
        "lines": lines,
        "count": len(lines),
        "truncated": len(output["stdout"]) >= LOG_BYTES_MAX,
        "source": "docker logs",
    }


def _read_proc(path: str) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return handle.read()
    except OSError:
        return ""


def op_metrics(params: dict) -> dict:
    now = time.time()
    load1 = load5 = load15 = 0.0
    processes = ""
    try:
        parts = _read_proc("/proc/loadavg").split()
        if len(parts) >= 3:
            load1, load5, load15 = float(parts[0]), float(parts[1]), float(parts[2])
        if len(parts) >= 4:
            processes = parts[3]
    except (ValueError, IndexError):
        pass
    meminfo = {}
    for line in _read_proc("/proc/meminfo").splitlines():
        if ":" not in line:
            continue
        key, _, value = line.partition(":")
        try:
            meminfo[key.strip()] = int(value.strip().split()[0])
        except (ValueError, IndexError):
            continue
    total_kb = int(meminfo.get("MemTotal") or 0)
    available_kb = int(meminfo.get("MemAvailable") or meminfo.get("MemFree") or 0)
    used_kb = max(0, total_kb - available_kb)
    swap_total_kb = int(meminfo.get("SwapTotal") or 0)
    swap_free_kb = int(meminfo.get("SwapFree") or 0)
    used_percent = round(used_kb / total_kb * 100, 1) if total_kb else None
    try:
        uptime_seconds = float(_read_proc("/proc/uptime").split()[0])
    except (ValueError, IndexError):
        uptime_seconds = 0.0
    uptime_text = ""
    if uptime_seconds:
        days = int(uptime_seconds // 86400)
        hours = int((uptime_seconds % 86400) // 3600)
        minutes = int((uptime_seconds % 3600) // 60)
        uptime_text = ("%d 天 " % days if days else "") + "%d 小时 %d 分" % (hours, minutes)
    disks = {}
    for path in ("/", "/opt"):
        try:
            usage = shutil.disk_usage(path)
            disks[path] = {
                "total": usage.total,
                "used": usage.used,
                "free": usage.free,
                "percent": round(usage.used / usage.total * 100, 1) if usage.total else None,
                "total_human": human_size(usage.total),
                "free_human": human_size(usage.free),
            }
        except OSError:
            continue
    docker_stats = {}
    stats_output = run_docker(
        ["stats", "--no-stream", "--format", "{{json .}}", CONTAINER_NAME], timeout=25)
    if stats_output["ok"]:
        for line in stats_output["stdout"].splitlines():
            line = line.strip()
            if not line:
                continue
            try:
                row = json.loads(line)
            except ValueError:
                continue
            docker_stats = {
                "name": str(row.get("Name") or ""),
                "cpu_percent": str(row.get("CPUPerc") or ""),
                "mem_usage": str(row.get("MemUsage") or ""),
                "mem_percent": str(row.get("MemPerc") or ""),
                "net_io": str(row.get("NetIO") or ""),
                "block_io": str(row.get("BlockIO") or ""),
                "pids": str(row.get("PIDs") or ""),
            }
            break
    else:
        docker_stats = {"error": (stats_output["stderr"] or "docker stats 失败")[:300]}
    try:
        container = op_inspect().get("container") or {}
    except Exception as exc:  # noqa: BLE001
        container = {"error": str(exc)[:300]}
    try:
        cpu_count = os.cpu_count() or 0
    except Exception:  # noqa: BLE001
        cpu_count = 0
    return {
        "host": {
            "hostname": os.uname().nodename if hasattr(os, "uname") else "",
            "now": now,
            "now_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(now)),
            "uptime_seconds": round(uptime_seconds, 1),
            "uptime_text": uptime_text,
            "cpu_count": cpu_count,
            "load": {"1m": load1, "5m": load5, "15m": load15, "processes": processes},
            "mem": {
                "total_kb": total_kb, "available_kb": available_kb, "used_kb": used_kb,
                "used_percent": used_percent,
                "total_human": human_size(total_kb * 1024),
                "available_human": human_size(available_kb * 1024),
                "used_human": human_size(used_kb * 1024),
                "swap_total_kb": swap_total_kb,
                "swap_free_kb": swap_free_kb,
            },
            "disk": disks,
        },
        "docker": {"container": container, "stats": docker_stats},
        "source": "host:/proc + docker stats",
    }


def op_cron(params: dict) -> dict:
    try:
        completed = subprocess.run([CRONTAB, "-l"], capture_output=True, text=True,
                                   encoding="utf-8", errors="replace", timeout=20, cwd="/")
        raw = completed.stdout or ""
        error = ""
        if completed.returncode != 0:
            error = (completed.stderr or "").strip()[:500]
            raw = ""
    except subprocess.TimeoutExpired:
        raise RuntimeError("crontab -l 超时")
    lines = [line for line in raw.splitlines() if line.strip()]
    return {"lines": lines, "count": len(lines), "raw": raw[:20000],
            "error": error, "source": "host:crontab -l"}


def op_rollback(params: dict) -> dict:
    """只做只读校验 + 返回人工回滚计划；不自动重建容器（会中断 QQ 会话）。"""
    image = str(params.get("image") or "").strip()
    if not SAFE_IMAGE.match(image):
        raise PermissionError("image 必须是 fm-cosmobot:runtime-<tag>，收到：" + (image or "(空)"))
    probe = run_docker(["image", "inspect", image, "--format",
                        "{{.Id}}\t{{.Created}}\t{{.Size}}"], timeout=25)
    image_exists = bool(probe["ok"])
    image_id = image_created = image_size = ""
    if image_exists:
        parts = (probe["stdout"] or "").strip().split("\t")
        image_id = parts[0] if len(parts) > 0 else ""
        image_created = parts[1] if len(parts) > 1 else ""
        image_size = human_size(parts[2]) if len(parts) > 2 else ""
    current = {}
    try:
        current = op_inspect().get("container") or {}
    except Exception as exc:  # noqa: BLE001
        current = {"error": str(exc)[:300]}
    steps = []
    if image_exists:
        steps = [
            "1. 记录当前镜像：" + str(current.get("image") or "?"),
            "2. 停止并重命名旧容器：docker stop -t 10 fm-cosmobot && docker rename fm-cosmobot fm-cosmobot.bak-<时间>",
            "3. 用原 run 参数重建容器，镜像改成 " + image + "（参考 /opt/fm-cosmobot/ 部署脚本）",
            "4. docker inspect fm-cosmobot 确认 Healthy 后再删旧容器",
        ]
    return {
        "protected": True,
        "executed": False,
        "image": image,
        "image_exists": image_exists,
        "image_id": image_id,
        "image_created": image_created,
        "image_size": image_size,
        "current": current,
        "manual_steps": steps,
        "note": "回滚受保护：自动重建容器会中断 QQ 会话且需要完整 run 参数（网络/挂载/环境变量），"
                "本代理只做只读校验并给出人工步骤；不允许 rm/rmi/run。",
    }


def _backup_domain(params: dict) -> dict:
    started = time.time()
    source_path = Path(DOMAIN_DB)
    if not source_path.is_file():
        raise RuntimeError("domain 数据库不存在：" + str(source_path))
    BACKUPS.mkdir(parents=True, exist_ok=True)
    source_size = source_path.stat().st_size
    usage = shutil.disk_usage(str(BACKUPS))
    if usage.free < source_size * 1.3 + 100 * 1024 * 1024:
        raise RuntimeError("磁盘空间不足：剩余 %s，需要约 %s"
                           % (human_size(usage.free), human_size(source_size * 1.3)))
    name = "fm-domain-" + time.strftime("%Y%m%d-%H%M%S") + ".sqlite3"
    target_path = BACKUPS / name
    source = sqlite3.connect("file:%s?mode=ro" % source_path, uri=True, timeout=30)
    try:
        target = sqlite3.connect(str(target_path), timeout=30)
        try:
            source.backup(target, pages=8192, sleep=0.001)
        finally:
            target.close()
    finally:
        source.close()
    size = target_path.stat().st_size
    verify = {"ok": False, "quick_check": "", "tables": [], "error": ""}
    try:
        check = sqlite3.connect("file:%s?mode=ro" % target_path, uri=True, timeout=20)
        try:
            quick = check.execute("PRAGMA quick_check").fetchone()
            verify["quick_check"] = str(quick[0]) if quick else ""
            verify["tables"] = [str(row[0]) for row in check.execute(
                "SELECT name FROM sqlite_master WHERE type='table' ORDER BY name").fetchall()]
            verify["ok"] = verify["quick_check"] == "ok"
        finally:
            check.close()
    except sqlite3.Error as exc:
        verify["error"] = str(exc)
    pruned: list[str] = []
    try:
        candidates = sorted(
            [item for item in BACKUPS.glob("fm-domain-*.sqlite3")],
            key=lambda item: item.stat().st_mtime, reverse=True)
        for item in candidates[DOMAIN_BACKUP_KEEP:]:
            if item.name == name:
                continue
            try:
                item.unlink()
                pruned.append(item.name)
            except OSError:
                continue
    except OSError:
        pass
    return {
        "db": "domain",
        "source": str(source_path),
        "path": str(target_path),
        "file": name,
        "size": size,
        "size_human": human_size(size),
        "source_size": source_size,
        "duration_ms": round((time.time() - started) * 1000),
        "verify": verify,
        "verified": bool(verify.get("ok")),
        "pruned": pruned,
        "keep": DOMAIN_BACKUP_KEEP,
        "source_agent": "host-ops-agent",
    }


def execute(request: dict) -> dict:
    """执行一个白名单请求，返回 result 字段内容（失败抛异常）。"""
    op = str(request.get("op") or "").strip().lower()
    target = str(request.get("target") or "").strip().lower()
    params = request.get("params") if isinstance(request.get("params"), dict) else {}
    if op not in ALLOWED_OPS:
        raise PermissionError("op 不在白名单：" + (op or "(空)"))
    if op == "images":
        return op_images()
    if op == "images_detail":
        return op_images_detail()
    if op == "inspect":
        return op_inspect()
    if op == "restart":
        return op_restart(target)
    if op == "logs":
        return op_logs(params)
    if op == "metrics":
        return op_metrics(params)
    if op == "cron":
        return op_cron(params)
    if op == "rollback":
        return op_rollback(params)
    if op == "backup_domain":
        return _backup_domain(params)
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


def _clean_params(raw: object) -> dict:
    if not isinstance(raw, dict):
        return {}
    clean: dict[str, Any] = {}
    for key, value in raw.items():
        key_text = str(key).strip()
        if not SAFE_PARAM_KEY.match(key_text):
            continue
        if isinstance(value, bool):
            clean[key_text] = value
        elif isinstance(value, (int, float)):
            clean[key_text] = value
        elif isinstance(value, str) and len(value) <= 200 and "\x00" not in value and "\n" not in value:
            clean[key_text] = value
        if len(clean) >= 20:
            break
    return clean


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
                "params": _clean_params(parsed.get("params")),
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
                "backups": str(BACKUPS),
                "container": CONTAINER_NAME,
                "domain_db": DOMAIN_DB,
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
