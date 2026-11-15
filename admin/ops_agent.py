#!/usr/bin/env python3
"""FM 主机运维代理（宿主机 root 运行，队列驱动，白名单 ops）。

为什么需要它：fm-admin 容器没有挂 docker.sock（也不应该挂），所以后台的
「部署面板 / 运维监控」不能直接操作 docker。后台只把请求写成队列文件，宿主机上的
本代理负责执行，并把结果写回：

  /opt/fm-admin/data/ops-queue/<id>.json    {"id","op","target","params","created_at"}
  /opt/fm-admin/data/ops-results/<id>.json  {"id","op","ok","result",...}

白名单（其余一律拒绝，绝不执行任意 shell）：
  images         -> docker images，只列 fm-cosmobot:runtime-* 镜像（后台部署面板用）
  inspect        -> docker inspect fm-cosmobot 的摘要（镜像/启动时间/健康状态）
  restart        -> docker restart -t 5 fm-cosmobot（仅允许 target=bot）
  images_detail  -> docker images fm-cosmobot 全量 tag/大小/创建时间（运维监控页）
  logs           -> docker logs fm-cosmobot --tail N [--since T] + 可选 grep 子串过滤（只读，≤500 行）
  metrics        -> 宿主机 load/mem/disk/uptime + docker stats 摘要（只读）
  stats          -> v2：多容器 docker stats 快照（bot/domain/admin）+ 磁盘 + 负载 + 内存（只读）
  cron           -> crontab -l（只读，兼容旧调用）
  cron_list      -> v2：crontab -l + 解析成 schedule/command 条目（只读）
  cron_set       -> v2：写入未实现，固定返回 501（改 crontab 必须人工 + 留证据）
  rollback       -> v2：真实安全回滚到 fm-cosmobot:runtime-*：校验镜像 → docker inspect 导出
                    当前容器 run 参数 → stop + rename 旧容器 → run 新镜像 → 健康检查（≤70s）→
                    失败自动恢复旧容器；结果写 /opt/fm-admin/data/backups/rollback-*.json
  config_backup  -> v2：把 /opt/fm-cosmobot/runtime/config.toml 备份到 backups/config-*.toml
  backup_domain  -> 用 sqlite3 backup API 备份 /opt/fm-domain/data/fm-domain.sqlite3

另外每 5 分钟（FM_OPS_HEALTH_INTERVAL）往 /opt/fm-admin/data/health.jsonl 追加一条
健康检查点：docker 容器状态/health + 磁盘 + 内存 + 负载，供后台 /api/v2/health/history。

安全约束：
  * docker 一律用固定 argv 调用，不经过 shell，不拼接队列内容；
  * 队列只取 id / op / target / params 字段，params 只允许简单标量且长度受限；
  * 结果文件原子写入（tmp + os.replace），队列文件处理后删除；
  * 单轮最多处理 50 个请求，单个 docker 命令有超时；备份文件只写
    /opt/fm-admin/data/backups 且按数量保留；
  * rollback 只允许 fm-cosmobot:runtime-*，只 stop/rename/run/start 自己创建的容器，
    不 rmi、不删数据卷；失败必须把旧容器改名 + 启动回来才算完。

用法：
  python3 ops_agent.py --once                        # 扫一轮（cron 默认）
  python3 ops_agent.py --loop 28 --interval 2        # 一分钟内扫 28 轮
  python3 ops_agent.py --health-now                  # 立即写一条健康检查点（测试用）
  FM_OPS_BASE=/opt/fm-admin/data python3 ops_agent.py --once
"""
from __future__ import annotations

import argparse
import calendar
import fcntl
import hashlib
import json
import os
import re
import shutil
import sqlite3
import subprocess
import sys
import threading
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
HEALTH_FILE = BASE / "health.jsonl"

CONTAINER_NAME = os.environ.get("FM_OPS_CONTAINER", "fm-cosmobot")
IMAGE_REPO = "fm-cosmobot"
DOMAIN_DB = os.environ.get("FM_OPS_DOMAIN_DB", "/opt/fm-domain/data/fm-domain.sqlite3")
CONFIG_PATH = os.environ.get("FM_OPS_CONFIG", "/opt/fm-cosmobot/runtime/config.toml")
# ── QQ 登录链路（snowluma 桌面 QQ → OneBot WS 3001 → bot）──
SNOWLUMA_CONTAINER = os.environ.get("FM_OPS_SNOWLUMA", "fm-snowluma")
QQ_DATA_DIR = BASE / "qq"
QQ_SCREEN_PATH = QQ_DATA_DIR / "latest.png"
QQ_SCREEN_REMOTE = "/tmp/fm-qq-screen.png"
QQ_LOG_TAIL = int(os.environ.get("FM_OPS_QQ_LOG_TAIL", "60000") or 60000)
QQ_LOG_FACTS_TTL = float(os.environ.get("FM_OPS_QQ_LOG_TTL", "10") or 10)
# 日志抓取保留的字节数（从尾部保留）：run_docker 默认只留前 400KB，日志必须留尾部
QQ_LOG_BYTES = int(os.environ.get("FM_OPS_QQ_LOG_BYTES", "4000000") or 4000000)
QQ_LOG_SINCE = os.environ.get("FM_OPS_QQ_LOG_SINCE", "6h")
ONEBOT_WS_PORT = int(os.environ.get("FM_OPS_ONEBOT_PORT", "3001") or 3001)
ONEBOT_HTTP_PORT = int(os.environ.get("FM_OPS_ONEBOT_HTTP_PORT", "3000") or 3000)
SNOWLUMA_WEBUI_PORT = int(os.environ.get("FM_OPS_SNOWLUMA_WEBUI_PORT", "5099") or 5099)
SNOWLUMA_RESTART_WAIT = float(os.environ.get("FM_OPS_SNOWLUMA_RESTART_WAIT", "20") or 20)
SNOWLUMA_SCREEN_SIZE = os.environ.get("FM_OPS_SNOWLUMA_SCREEN", "1920x1080")

# v2：运维监控页一次看这三个容器
STATS_CONTAINERS = tuple(
    name.strip() for name in os.environ.get("FM_OPS_STATS_CONTAINERS",
                                            "fm-cosmobot,fm-domain,fm-admin").split(",")
    if name.strip())
HEALTH_CONTAINERS = tuple(dict.fromkeys(("fm-cosmobot", "fm-domain", "fm-admin") + STATS_CONTAINERS))
MAX_QUEUE_FILES = 50
REQUEST_MAX_AGE_SECONDS = 600
LOG_TAIL_MAX = 2000
LOG_TAIL_RETURN_MAX = 500
LOG_BYTES_MAX = 400000
DOMAIN_BACKUP_KEEP = 3
CONFIG_BACKUP_KEEP = 10
HEALTH_INTERVAL = int(os.environ.get("FM_OPS_HEALTH_INTERVAL", "300") or 300)
HEALTH_KEEP_SAMPLES = int(os.environ.get("FM_OPS_HEALTH_KEEP", "3000") or 3000)
ROLLBACK_HEALTH_TIMEOUT = float(os.environ.get("FM_OPS_ROLLBACK_HEALTH_TIMEOUT", "70") or 70)
# v2.1.6：默认 -1 = 常驻（空闲也不退出，cron 每分钟尝试拉起 → 进程崩了 60s 内自愈，
# 不会再有「驻留退出到下一次 cron」之间最长 60s 的空档，导致 ops 首包 20~60s）。
# FM_OPS_LINGER>0 时按「空闲 N 秒退出」的旧行为；FM_OPS_MAX_UPTIME 到点主动退出换新进程。
LINGER_SECONDS = float(os.environ.get("FM_OPS_LINGER", "-1") or -1)
MAX_UPTIME_SECONDS = float(os.environ.get("FM_OPS_MAX_UPTIME", "43200") or 43200)
VERSION = "2.2.3"
# 进程启动时间（写进 state 的 started_at，供后台展示/判断重启）
STARTED_AT = time.time()

ALLOWED_OPS = ("images", "inspect", "restart", "images_detail", "logs",
               "metrics", "cron", "rollback", "backup_domain",
               "stats", "cron_list", "cron_set", "config_backup",
               # v2.2.0 QQ 登录面板：snowluma 状态 / 扫码界面截图 / 重启
               "qq_status", "qq_screen", "qq_restart")
SAFE_ID = re.compile(r"^[0-9A-Za-z._-]+$")
SAFE_PARAM_KEY = re.compile(r"^[0-9A-Za-z_.-]{1,40}$")
SAFE_IMAGE = re.compile(r"^fm-cosmobot:runtime-[A-Za-z0-9._-]{1,90}$")
SAFE_CONTAINER_BACKUP = re.compile(r"^fm-cosmobot-bak-[0-9]{8}-[0-9]{6}$")
DOCKER = shutil.which("docker") or "/usr/bin/docker"
CRONTAB = shutil.which("crontab") or "/usr/bin/crontab"
SECRET_ENV = re.compile(r"(?i)(key|token|secret|password|passwd|pwd|auth|credential)")


class CodedError(Exception):
    """带 HTTP 状态码的错误：结果文件的 error_status 会透传给后台 API（如 cron_set→501）。"""

    def __init__(self, status: int, message: str):
        super().__init__(message)
        self.status = int(status)
        self.message = str(message)


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


_CMD_CACHE: dict[str, tuple[float, Any]] = {}
_CMD_CACHE_LOCK = threading.Lock()


def _cmd_cached(key: str, ttl: float, producer: Any) -> Any:
    """宿主机命令结果短 TTL 缓存。

    docker stats 一次要 2~3s；后台 selftest/运维页会在一瞬间pull metrics+stats+logs+cron，
    串行执行就会把后面真正用户的请求挤到十几秒。缓存 3~6s 可以让并发 ops 复用同一次采集。
    """
    now = time.time()
    with _CMD_CACHE_LOCK:
        entry = _CMD_CACHE.get(key)
        if entry and now - float(entry[0]) <= ttl:
            return entry[1]
    value = producer()
    with _CMD_CACHE_LOCK:
        _CMD_CACHE[key] = (now, value)
    return value


def _docker_stats_rows(names: list[str] | tuple[str, ...]) -> list[dict]:
    """docker stats --no-stream 快照（固定 argv；失败返回 [{'error': ...}]；缓存 6s）。"""
    containers = [str(name) for name in names if str(name)]
    if not containers:
        return []
    return _cmd_cached("stats:" + ",".join(containers), 6.0,
                       lambda: _docker_stats_rows_uncached(containers))


def _docker_stats_rows_uncached(containers: list[str]) -> list[dict]:
    output = run_docker(["stats", "--no-stream", "--format", "{{json .}}"] + containers, timeout=45)
    if not output["ok"]:
        error = (output["stderr"] or output["stdout"] or "docker stats 失败")
        return [{"error": str(error)[:300]}]
    rows: list[dict] = []
    for line in output["stdout"].splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            row = json.loads(line)
        except ValueError:
            continue
        rows.append({
            "name": str(row.get("Name") or ""),
            "cpu_percent": str(row.get("CPUPerc") or ""),
            "mem_usage": str(row.get("MemUsage") or ""),
            "mem_percent": str(row.get("MemPerc") or ""),
            "net_io": str(row.get("NetIO") or ""),
            "block_io": str(row.get("BlockIO") or ""),
            "pids": str(row.get("PIDs") or ""),
        })
    return rows


def _docker_ps_rows() -> list[dict]:
    """docker ps 摘要（name/image/status/state），失败返回 []；缓存 3s。"""
    return _cmd_cached("ps", 3.0, _docker_ps_rows_uncached)


def _docker_ps_rows_uncached() -> list[dict]:
    output = run_docker(["ps", "--format", "{{json .}}"], timeout=25)
    if not output["ok"]:
        return []
    rows: list[dict] = []
    for line in output["stdout"].splitlines():
        line = line.strip()
        if not line:
            continue
        try:
            row = json.loads(line)
        except ValueError:
            continue
        status = str(row.get("Status") or "")
        health = ""
        for candidate in ("healthy", "unhealthy", "health: starting"):
            if candidate in status:
                health = candidate.replace("health: ", "")
                break
        rows.append({
            "name": str(row.get("Names") or ""),
            "image": str(row.get("Image") or ""),
            "status": status,
            "state": str(row.get("State") or ""),
            "health": health,
            "running_for": str(row.get("RunningFor") or ""),
            "created_at": str(row.get("CreatedAt") or ""),
        })
    return rows


def op_images() -> dict:
    """fm-cosmobot:runtime-* 镜像清单（docker images 在 220 个镜像的机器上要 ~20s，缓存 60s）。"""
    return _cmd_cached("images:runtime", 60.0, _op_images_uncached)


def _op_images_uncached() -> dict:
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
    """fm-cosmobot 全部镜像（含非 runtime tag）+ 当前容器镜像（缓存 90s，docker images 很慢）。"""
    return _cmd_cached("images:detail", 90.0, _op_images_detail_uncached)


def _op_images_detail_uncached() -> dict:
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
    """docker inspect 摘要（缓存 3s：运维页 metrics/stats 会高频调用）。"""
    return _cmd_cached("inspect:" + CONTAINER_NAME, 3.0, _op_inspect_uncached)


def _op_inspect_uncached() -> dict:
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
    inspect_result = _op_inspect_uncached()
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
    grep_text = str(params.get("grep") or "").strip()[:120]
    if any(char in grep_text for char in "\x00\r\n"):
        raise ValueError("grep 不能包含换行/控制字符")
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
    total_lines = len(lines)
    matched = total_lines
    if grep_text:
        needle = grep_text.lower()
        lines = [line for line in lines if needle in str(line.get("text") or "").lower()]
        matched = len(lines)
    truncated = len(lines) > LOG_TAIL_RETURN_MAX
    lines = lines[-LOG_TAIL_RETURN_MAX:]
    return {
        "container": CONTAINER_NAME,
        "tail": tail,
        "since": since,
        "grep": grep_text,
        "lines": lines,
        "count": len(lines),
        "total_lines": total_lines,
        "grep_matched": matched,
        "truncated": truncated or len(output["stdout"]) >= LOG_BYTES_MAX,
        "source": "docker logs",
    }


def _read_proc(path: str) -> str:
    try:
        with open(path, "r", encoding="utf-8", errors="replace") as handle:
            return handle.read()
    except OSError:
        return ""


def _host_metrics_block() -> dict:
    """宿主机 load/mem/disk/uptime/cpu（/proc + shutil.disk_usage）。"""
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
    try:
        cpu_count = os.cpu_count() or 0
    except Exception:  # noqa: BLE001
        cpu_count = 0
    return {
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
    }


def op_metrics(params: dict) -> dict:
    """宿主机指标 + docker stats 摘要（docker stats 走 6s 缓存，重复请求不再慢）。"""
    host = _host_metrics_block()
    stats_rows = _docker_stats_rows([CONTAINER_NAME])
    if stats_rows and not stats_rows[0].get("error"):
        docker_stats = stats_rows[0]
    else:
        docker_stats = {"error": (stats_rows[0].get("error") if stats_rows else "docker stats 无返回")}
    try:
        container = op_inspect().get("container") or {}
    except Exception as exc:  # noqa: BLE001
        container = {"error": str(exc)[:300]}
    return {
        "host": host,
        "docker": {"container": container, "stats": docker_stats},
        "source": "host:/proc + docker stats",
    }


def op_stats(params: dict) -> dict:
    """v2 GET /api/v2/ops/stats：docker stats（bot/domain/admin）+ 磁盘 + 负载 + 内存。"""
    started = time.time()
    host = _host_metrics_block()
    ps_rows = {str(row.get("name")): row for row in _docker_ps_rows()}
    stats_rows = _docker_stats_rows(list(STATS_CONTAINERS))
    errors = [row.get("error") for row in stats_rows if row.get("error")]
    stats_by_name = {str(row.get("name")): row for row in stats_rows if row.get("name")}
    containers = []
    for name in STATS_CONTAINERS:
        stats = stats_by_name.get(name) or {}
        info = ps_rows.get(name) or {}
        state_text = str(info.get("state") or "")
        containers.append({
            "name": name,
            "image": info.get("image") or "",
            "status": info.get("status") or "",
            "state": state_text,
            "health": info.get("health") or "",
            "running": state_text.lower() == "running" if state_text else None,
            "running_for": info.get("running_for") or "",
            "cpu_percent": stats.get("cpu_percent") or "",
            "mem_usage": stats.get("mem_usage") or "",
            "mem_percent": stats.get("mem_percent") or "",
            "net_io": stats.get("net_io") or "",
            "block_io": stats.get("block_io") or "",
            "pids": stats.get("pids") or "",
        })
    running = sum(1 for item in containers if item.get("running"))
    return {
        "collected_at": time.time(),
        "collected_at_text": time.strftime("%Y-%m-%d %H:%M:%S"),
        "collect_ms": round((time.time() - started) * 1000),
        "host": host,
        "containers": containers,
        "summary": {"containers": len(containers), "running": running},
        "errors": [str(item)[:300] for item in errors],
        "source": "host:/proc + docker stats + docker ps",
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


def _cron_entries(raw: str) -> list[dict]:
    """把 crontab 文本解析成 {line, schedule, command, is_ops_agent}。"""
    entries: list[dict] = []
    for line in (raw or "").splitlines():
        text = line.strip()
        if not text or text.startswith("#"):
            continue
        parts = text.split()
        if text.startswith("@") and len(parts) >= 2:
            schedule = parts[0]
            command = " ".join(parts[1:])
        elif len(parts) >= 6:
            schedule = " ".join(parts[:5])
            command = " ".join(parts[5:])
        else:
            continue
        entries.append({
            "line": text[:1000],
            "schedule": schedule,
            "command": command[:500],
            "is_ops_agent": "ops_agent.py" in command,
            "env": "=" in schedule and not schedule.startswith("*") and not schedule[0].isdigit(),
        })
    return entries


def op_cron_list(params: dict) -> dict:
    """v2 GET /api/v2/ops/cron：crontab -l 只读 + 解析（cron_set 未实现）。"""
    payload = op_cron(params)
    return {
        **payload,
        "entries": _cron_entries(payload.get("raw") or ""),
        "writable": False,
        "can_set": False,
        "note": "只读：cron_set 未实现（固定 501）；改 crontab 必须人工在宿主机执行并留证据。",
    }


def op_cron_set(params: dict) -> dict:
    """v2：cron 写入未实现（白名单限制），明确返回 501。"""
    line = str(params.get("line") or "").strip()[:200]
    raise CodedError(501,
                     "cron_set 未实现（只读白名单）：拒绝写入 crontab。"
                     "如确需改 cron，请人工在宿主机 crontab -e 并记录变更；请求内容="
                     + (line or "(空)"))


def _inspect_container(name: str) -> dict:
    output = run_docker(["inspect", name], timeout=30)
    if not output["ok"]:
        raise RuntimeError("docker inspect %s 失败：%s" % (name, (output["stderr"] or "容器不存在？")[:500]))
    try:
        data = json.loads(output["stdout"])
    except ValueError:
        raise RuntimeError("docker inspect 返回不是 JSON")
    item = data[0] if isinstance(data, list) and data else data
    if not isinstance(item, dict):
        raise RuntimeError("docker inspect 返回结构异常")
    return item


def _container_state(name: str) -> dict:
    item = _inspect_container(name)
    state = item.get("State") if isinstance(item.get("State"), dict) else {}
    health = state.get("Health") if isinstance(state.get("Health"), dict) else {}
    config = item.get("Config") if isinstance(item.get("Config"), dict) else {}
    return {
        "name": str(item.get("Name") or "").lstrip("/"),
        "image": str(config.get("Image") or ""),
        "status": str(state.get("Status") or ""),
        "running": bool(state.get("Running")),
        "health": str(health.get("Status") or ""),
        "restart_count": int(state.get("RestartCount") or 0),
        "started_at": str(state.get("StartedAt") or ""),
    }


def _redact_env(entries: list[str]) -> list[str]:
    redacted = []
    for entry in entries:
        name, sep, _value = str(entry).partition("=")
        redacted.append(name + "=<redacted>" if sep and SECRET_ENV.search(name) else str(entry))
    return redacted


def _export_run_spec(item: dict, redact: bool = False) -> list[str]:
    """把 docker inspect 的现有容器导出成 docker run 参数（固定 argv 列表）。

    redact=True 只用于「结果文件/报告展示」，真正 run 时必须用 redact=False 的原始 env。
    """
    config = item.get("Config") if isinstance(item.get("Config"), dict) else {}
    host = item.get("HostConfig") if isinstance(item.get("HostConfig"), dict) else {}
    argv: list[str] = []
    working_dir = str(config.get("WorkingDir") or "")
    if working_dir:
        argv += ["-w", working_dir]
    user = str(config.get("User") or "")
    if user:
        argv += ["--user", user]
    env_entries = [str(value) for value in (config.get("Env") or [])]
    if redact:
        for entry in _redact_env(env_entries):
            argv += ["-e", entry]
    else:
        for entry in env_entries:
            argv += ["-e", entry]
    entrypoint = config.get("Entrypoint")
    if isinstance(entrypoint, list) and entrypoint:
        argv += ["--entrypoint", str(entrypoint[0])]
    elif isinstance(entrypoint, str) and entrypoint:
        argv += ["--entrypoint", entrypoint]
    for bind in host.get("Binds") or []:
        argv += ["-v", str(bind)]
    for cap in host.get("CapAdd") or []:
        argv += ["--cap-add", str(cap)]
    for cap in host.get("CapDrop") or []:
        argv += ["--cap-drop", str(cap)]
    network = str(host.get("NetworkMode") or "")
    if network and network not in ("default", "bridge"):
        argv += ["--network", network]
    policy = host.get("RestartPolicy") if isinstance(host.get("RestartPolicy"), dict) else {}
    policy_name = str(policy.get("Name") or "")
    if policy_name and policy_name != "no":
        try:
            maximum = int(policy.get("MaximumRetryCount") or 0)
        except (TypeError, ValueError):
            maximum = 0
        argv += ["--restart", policy_name + (":%d" % maximum if maximum else "")]
    log_config = host.get("LogConfig") if isinstance(host.get("LogConfig"), dict) else {}
    if log_config.get("Type"):
        argv += ["--log-driver", str(log_config["Type"])]
    for key, value in (log_config.get("Config") or {}).items():
        argv += ["--log-opt", "%s=%s" % (key, value)]
    ipc = str(host.get("IpcMode") or "")
    if ipc and ipc not in ("private",):
        argv += ["--ipc", ipc]
    for option in host.get("SecurityOpt") or []:
        argv += ["--security-opt", str(option)]
    for dns in host.get("Dns") or []:
        argv += ["--dns", str(dns)]
    for host_entry in host.get("ExtraHosts") or []:
        argv += ["--add-host", str(host_entry)]
    for container_port, bindings in (host.get("PortBindings") or {}).items():
        for binding in (bindings or []):
            if not isinstance(binding, dict):
                continue
            host_ip = str(binding.get("HostIp") or "")
            host_port = str(binding.get("HostPort") or "")
            spec = str(container_port)
            if host_port:
                spec = (host_ip + ":" if host_ip else "") + host_port + ":" + str(container_port)
            argv += ["-p", spec]
    try:
        shm_size = int(host.get("ShmSize") or 0)
    except (TypeError, ValueError):
        shm_size = 0
    if shm_size > 0:
        argv += ["--shm-size", str(shm_size)]
    # Healthcheck：本机 bot 的 healthcheck 是部署脚本用 docker run 参数加的（镜像里没有），
    # 不导出的话重建容器会丢掉健康检查。Test 形如 ["CMD-SHELL", "python3 -c ..."]。
    healthcheck = config.get("Healthcheck") if isinstance(config.get("Healthcheck"), dict) else {}
    test = healthcheck.get("Test")
    if isinstance(test, list) and test:
        first = str(test[0] or "")
        if first == "CMD-SHELL" and len(test) >= 2:
            argv += ["--health-cmd", str(test[1])]
        elif first == "CMD" and len(test) >= 2:
            argv += ["--health-cmd", " ".join(str(item) for item in test[1:])]

    def go_duration(value: object) -> str:
        try:
            ns = int(value or 0)
        except (TypeError, ValueError):
            return ""
        if ns <= 0:
            return ""
        if ns % 1000000000 == 0:
            return "%ds" % (ns // 1000000000)
        if ns % 1000000 == 0:
            return "%dms" % (ns // 1000000)
        return "%dns" % ns

    for flag, key in (("--health-interval", "Interval"),
                      ("--health-timeout", "Timeout"),
                      ("--health-start-period", "StartPeriod")):
        duration = go_duration(healthcheck.get(key))
        if duration:
            argv += [flag, duration]
    try:
        retries = int(healthcheck.get("Retries") or 0)
    except (TypeError, ValueError):
        retries = 0
    if retries > 0:
        argv += ["--health-retries", str(retries)]
    return argv


def _wait_container_healthy(name: str, timeout: float) -> tuple[bool, dict, list[dict], str]:
    """轮询 docker inspect 判断容器是否真的健康。

    判定标准（v2 修正版，避免「restarting 也算 healthy」的误判）：
      * 容器自己声明了 Healthcheck（Config.Healthcheck 非空）→ 必须 State.Health.Status == healthy；
      * 没有 Healthcheck → 必须 State.Status == running 且连续 10s 稳定；
      * 出现 restarting / exited 且 RestartCount 增加 → 快速失败（别白等 70s）；
      * health == unhealthy → 快速失败。
    返回 (是否健康, 最后一次 state, 采样历史, 失败原因)
    """
    deadline = time.time() + max(5.0, float(timeout))
    history: list[dict] = []
    running_since: float | None = None
    bad_since: float | None = None
    state: dict = {}
    reason = "健康检查超时"
    while time.time() < deadline:
        try:
            item = _inspect_container(name)
        except Exception as exc:  # noqa: BLE001  容器刚创建时可能短暂不可 inspect
            history.append({"at": time.strftime("%H:%M:%S"), "error": str(exc)[:160]})
            time.sleep(2.0)
            continue
        raw_state = item.get("State") if isinstance(item.get("State"), dict) else {}
        config = item.get("Config") if isinstance(item.get("Config"), dict) else {}
        health_block = raw_state.get("Health") if isinstance(raw_state.get("Health"), dict) else {}
        has_healthcheck = bool(config.get("Healthcheck"))
        state = {
            "name": str(item.get("Name") or "").lstrip("/"),
            "image": str(config.get("Image") or ""),
            "status": str(raw_state.get("Status") or ""),
            "running": bool(raw_state.get("Running")),
            "health": str(health_block.get("Status") or ""),
            "restart_count": int(raw_state.get("RestartCount") or 0),
            "has_healthcheck": has_healthcheck,
            "cmd": [str(value) for value in (config.get("Cmd") or [])],
            "entrypoint": [str(value) for value in (config.get("Entrypoint") or [])],
        }
        history.append({"at": time.strftime("%H:%M:%S"), "status": state["status"],
                        "running": state["running"], "health": state["health"],
                        "restart_count": state["restart_count"]})
        status = state["status"]
        health = state["health"]
        if health == "healthy" and state["running"] and status == "running":
            return True, state, history, ""
        if health == "unhealthy":
            return False, state, history, "容器 healthcheck 报 unhealthy"
        bad = status in ("restarting", "dead", "exited") or not state["running"] \
            or state["restart_count"] >= 1
        if bad:
            bad_since = bad_since or time.time()
            if state["restart_count"] >= 2 or time.time() - bad_since >= 8:
                return False, state, history, (
                    "容器启动异常：status=%s restart_count=%s（run 参数/Cmd/配置不兼容）"
                    % (status, state["restart_count"]))
            time.sleep(2.0)
            continue
        bad_since = None
        if not has_healthcheck and status == "running" and state["running"]:
            running_since = running_since or time.time()
            if time.time() - running_since >= 10:
                return True, state, history, ""
        else:
            running_since = None
        time.sleep(2.5)
    try:
        final = _container_state(name)
    except Exception as exc:  # noqa: BLE001
        final = {"error": str(exc)[:200], "running": False}
    return False, final, history, reason


def _preflight_container_check(original: dict, image: str, spec: list[str], cmd: list[str]) -> dict:
    """用 `docker create` 干跑一次 run 参数（不启动进程），和现有容器结构逐项比对。

    这是 v2 最重要的安全网：rollback 前就能发现「run 参数漏了 Cmd/Entrypoint/Env/Binds」
    这类致命差异，避免把 bot 重建进崩溃循环。
    """
    name = CONTAINER_NAME + "-preflight-" + time.strftime("%H%M%S")
    result: dict = {"name": name, "created": False, "ok": False, "diff": [],
                    "expected_cmd": cmd, "argv_tail": list(spec) + [image] + list(cmd)}
    created = run_docker(["create", "--name", name] + spec + [image] + cmd, timeout=120)
    if not created["ok"]:
        result["error"] = (created["stderr"] or created["stdout"] or "")[:500]
        return result
    result["created"] = True
    try:
        new = _inspect_container(name)
    finally:
        if name.startswith(CONTAINER_NAME + "-preflight-"):
            run_docker(["rm", "-f", name], timeout=60)
    old_config = original.get("Config") if isinstance(original.get("Config"), dict) else {}
    new_config = new.get("Config") if isinstance(new.get("Config"), dict) else {}
    old_host = original.get("HostConfig") if isinstance(original.get("HostConfig"), dict) else {}
    new_host = new.get("HostConfig") if isinstance(new.get("HostConfig"), dict) else {}

    def loose(value: object) -> object:
        if isinstance(value, dict):
            return {key: loose(item) for key, item in sorted(value.items())}
        if isinstance(value, list):
            return [loose(item) for item in value]
        return value

    comparisons = (
        ("image", str(new_config.get("Image") or ""), image),
        ("cmd", [str(value) for value in (new_config.get("Cmd") or [])],
         [str(value) for value in (old_config.get("Cmd") or [])]),
        ("entrypoint", [str(value) for value in (new_config.get("Entrypoint") or [])],
         [str(value) for value in (old_config.get("Entrypoint") or [])]),
        ("env", sorted(str(value) for value in (new_config.get("Env") or [])),
         sorted(str(value) for value in (old_config.get("Env") or []))),
        ("workingdir", str(new_config.get("WorkingDir") or ""), str(old_config.get("WorkingDir") or "")),
        ("binds", sorted(str(value) for value in (new_host.get("Binds") or [])),
         sorted(str(value) for value in (old_host.get("Binds") or []))),
        ("network", str(new_host.get("NetworkMode") or ""), str(old_host.get("NetworkMode") or "")),
        ("restart", json.dumps(loose(new_host.get("RestartPolicy") or {}), sort_keys=True),
         json.dumps(loose(old_host.get("RestartPolicy") or {}), sort_keys=True)),
        ("capadd", sorted(str(value) for value in (new_host.get("CapAdd") or [])),
         sorted(str(value) for value in (old_host.get("CapAdd") or []))),
        ("logconfig", json.dumps(loose(new_host.get("LogConfig") or {}), sort_keys=True),
         json.dumps(loose(old_host.get("LogConfig") or {}), sort_keys=True)),
        ("shm_size", int(new_host.get("ShmSize") or 0), int(old_host.get("ShmSize") or 0)),
        ("healthcheck", json.dumps(loose(new_config.get("Healthcheck") or {}), sort_keys=True),
         json.dumps(loose(old_config.get("Healthcheck") or {}), sort_keys=True)),
    )
    for field, actual, expected in comparisons:
        if actual != expected:
            result["diff"].append({"field": field, "expected": expected, "actual": actual})
    result["ok"] = not result["diff"]
    return result


def _write_rollback_result(payload: dict) -> str:
    """写 rollback 结果文件；同时在 payload 里补 result_file（自引用）后落盘。"""
    BACKUPS.mkdir(parents=True, exist_ok=True)
    path = BACKUPS / ("rollback-%s.json" % time.strftime("%Y%m%d-%H%M%S"))
    payload["result_file"] = str(path)
    atomic_write_json(path, payload)
    return str(path)


def op_rollback(params: dict) -> dict:
    """v2：真实安全回滚（高危，白名单 + confirm 双闸门）。

    1) 校验 image 形如 fm-cosmobot:runtime-* 且镜像存在、与当前容器镜像不同；
    2) docker inspect 当前容器 → 导出 run 参数（env/binds/network/restart/log/cap/ipc/shm/ports）；
    3) docker stop -t 10 → docker rename fm-cosmobot fm-cosmobot-bak-<时间>；
    4) docker run -d --name fm-cosmobot <导出的参数> <目标镜像>；
    5) 健康检查最多 ROLLBACK_HEALTH_TIMEOUT 秒（Docker healthcheck=healthy）；
    6) 失败自动恢复：删新容器 → 旧容器改回名字 → start → 再等健康；
    7) 结果写 /opt/fm-admin/data/backups/rollback-<时间>.json（队列结果文件里也有）。
    """
    image = str(params.get("image") or "").strip()
    if not SAFE_IMAGE.match(image):
        raise PermissionError("image 必须是 fm-cosmobot:runtime-<tag>，收到：" + (image or "(空)"))
    if str(params.get("confirm") or "").strip().lower() not in {"1", "true", "yes", "confirm"}:
        raise ValueError("rollback 需要 confirm=1（高危操作，避免误触）")
    keep_old = str(params.get("keep_old") or "1").strip().lower() not in {"0", "false", "no"}
    started = time.time()
    steps: list[dict] = []

    def step(name: str, detail: Any = None) -> None:
        steps.append({"step": name, "at": time.strftime("%H:%M:%S"),
                      "elapsed_ms": int((time.time() - started) * 1000), "detail": detail})

    probe = run_docker(["image", "inspect", image, "--format", "{{.Id}}"], timeout=30)
    if not probe["ok"]:
        raise RuntimeError("镜像不存在或不可用：" + image + "（" + (probe["stderr"] or "")[:300] + "）")
    target_id = (probe["stdout"] or "").strip().splitlines()[0][:80] if probe["stdout"] else ""
    step("verify-image", {"image": image, "image_id": target_id})
    current = _inspect_container(CONTAINER_NAME)
    previous_state = _container_state(CONTAINER_NAME)
    previous_image = previous_state.get("image") or ""
    if previous_image == image:
        raise ValueError("目标镜像与当前容器镜像相同（%s），无需回滚" % image)
    run_spec = _export_run_spec(current, redact=False)
    display_spec = _export_run_spec(current, redact=True)
    # 关键：镜像的 CMD（本机是 serve --config config.toml）必须原样带上，
    # 否则重建出来的容器会因为没带参数而立刻退出 → 崩溃循环（v2 首测就是踩了这个坑）。
    run_cmd = [str(value) for value in ((current.get("Config") or {}).get("Cmd") or [])]
    if not run_cmd:
        display_cmd: list[str] = []
    else:
        display_cmd = list(run_cmd)
    stamped = time.strftime("%Y%m%d-%H%M%S")
    old_name = CONTAINER_NAME + "-bak-" + stamped
    if not SAFE_CONTAINER_BACKUP.match(old_name):
        raise RuntimeError("备份容器名不符合白名单：" + old_name)
    result: dict = {
        "image": image,
        "previous_image": previous_image,
        "target_image_id": target_id,
        "previous_container": old_name,
        "executed": False,
        "rolled_back": False,
        "healthy": False,
        "started_at": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(started)),
        "agent": {"version": VERSION, "pid": os.getpid(), "container": CONTAINER_NAME},
    }
    step("export-run-spec", {"argv": display_spec + ["<image>"] + display_cmd,
                             "cmd": display_cmd,
                             "previous_image": previous_image,
                             "previous_state": previous_state,
                             "backup_container": old_name})

    # 预检：docker create 干跑一次（不启动），逐项比对 Cmd/Entrypoint/Env/Binds/Network/
    # RestartPolicy/CapAdd/LogConfig/ShmSize/Healthcheck，任何不一致直接拒绝回滚。
    preflight = _preflight_container_check(current, image, run_spec, run_cmd)
    step("preflight-create", preflight)
    if not preflight.get("ok"):
        result.update({
            "error": "预检失败：导出的 run 参数与现有容器不一致，已拒绝重建（没有动线上容器）："
                     + json.dumps(preflight.get("diff"), ensure_ascii=False)[:700],
            "steps": steps,
        })
        result["result_file"] = _write_rollback_result(result)
        raise RuntimeError(result["error"])

    stop = run_docker(["stop", "-t", "10", CONTAINER_NAME], timeout=90)
    step("stop-old-container", {"ok": stop["ok"], "rc": stop["rc"],
                                "stderr": (stop["stderr"] or "")[:300]})
    if not stop["ok"]:
        result.update({"error": "停止旧容器失败：" + (stop["stderr"] or "")[:300], "steps": steps})
        result["result_file"] = _write_rollback_result(result)
        raise RuntimeError(result["error"])

    rename = run_docker(["rename", CONTAINER_NAME, old_name], timeout=30)
    step("rename-old-container", {"ok": rename["ok"], "new_name": old_name,
                                  "stderr": (rename["stderr"] or "")[:300]})
    if not rename["ok"]:
        start_back = run_docker(["start", CONTAINER_NAME], timeout=120)
        result.update({"error": "重命名旧容器失败：" + (rename["stderr"] or "")[:300],
                       "steps": steps,
                       "recovery": [{"action": "start old container", "ok": start_back["ok"]}]})
        result["result_file"] = _write_rollback_result(result)
        raise RuntimeError(result["error"] + "（旧容器已 start 回来）")

    # 注意：镜像名后面必须跟上原来的 Cmd（serve --config config.toml），否则容器立即退出
    run_argv = ["run", "-d", "--name", CONTAINER_NAME] + run_spec + [image] + run_cmd
    launch_result = run_docker(run_argv, timeout=180)
    step("run-target-image", {"ok": launch_result["ok"], "rc": launch_result["rc"],
                              "stdout": (launch_result["stdout"] or "").strip()[:200],
                              "stderr": (launch_result["stderr"] or "")[:500]})
    if launch_result["ok"]:
        healthy, health_state, history, health_reason = _wait_container_healthy(
            CONTAINER_NAME, ROLLBACK_HEALTH_TIMEOUT)
        step("health-check", {"healthy": healthy, "timeout_seconds": ROLLBACK_HEALTH_TIMEOUT,
                              "reason": health_reason,
                              "state": health_state, "history": history[-10:]})
        result.update({"executed": True, "healthy": healthy, "health": health_state,
                       "health_history": history[-20:]})
        if healthy:
            result.update({
                "rolled_back": True,
                "note": "回滚成功：%s → %s；旧容器保留为 %s（已停止，未删除）"
                        % (previous_image, image, old_name),
                "steps": steps,
                "duration_ms": int((time.time() - started) * 1000),
            })
            if not keep_old:
                removed = run_docker(["rm", old_name], timeout=60)
                step("remove-old-container", {"ok": removed["ok"],
                                              "stderr": (removed["stderr"] or "")[:300]})
                result["note"] = "回滚成功：%s → %s；旧容器已删除（keep_old=0）" % (previous_image, image)
            result["result_file"] = _write_rollback_result(result)
            return result
        failure = health_reason or ("健康检查超时（%.0fs 内未 healthy）" % ROLLBACK_HEALTH_TIMEOUT)
    else:
        result["executed"] = True
        failure = "目标镜像启动失败：" + (launch_result["stderr"] or "")[:300]

    step("recover-old-container", {"reason": failure})
    recovery: list[dict] = []
    removed_new = run_docker(["rm", "-f", CONTAINER_NAME], timeout=90)
    recovery.append({"action": "rm -f new container", "ok": removed_new["ok"],
                     "stderr": (removed_new["stderr"] or "")[:300]})
    renamed_back = run_docker(["rename", old_name, CONTAINER_NAME], timeout=30)
    recovery.append({"action": "rename old back", "ok": renamed_back["ok"],
                     "stderr": (renamed_back["stderr"] or "")[:300]})
    if not renamed_back["ok"]:
        run_docker(["rm", "-f", CONTAINER_NAME], timeout=90)
        renamed_back = run_docker(["rename", old_name, CONTAINER_NAME], timeout=30)
        recovery.append({"action": "rename old back (retry)", "ok": renamed_back["ok"],
                         "stderr": (renamed_back["stderr"] or "")[:300]})
    started_old = run_docker(["start", CONTAINER_NAME], timeout=120)
    recovery.append({"action": "start old container", "ok": started_old["ok"],
                     "stderr": (started_old["stderr"] or "")[:300]})
    recovered_healthy, recovered_state, recovered_history, recovered_reason = _wait_container_healthy(
        CONTAINER_NAME, 150)
    recovery.append({"action": "old container health", "healthy": recovered_healthy,
                     "reason": recovered_reason,
                     "state": recovered_state, "history": recovered_history[-8:]})
    result.update({
        "rolled_back": False,
        "recovered": recovered_healthy,
        "health": recovered_state,
        "error": failure,
        "note": "回滚失败（%s）；已自动恢复旧容器（健康=%s）。请人工检查 docker ps / docker logs。"
                % (failure, recovered_healthy),
        "recovery": recovery,
        "steps": steps,
        "duration_ms": int((time.time() - started) * 1000),
    })
    result["result_file"] = _write_rollback_result(result)
    raise RuntimeError("%s；已自动恢复旧容器（健康=%s），结果文件：%s"
                       % (failure, recovered_healthy, result["result_file"]))


def op_config_backup(params: dict) -> dict:
    """v2 POST /api/v2/ops/config-backup：备份 /opt/fm-cosmobot/runtime/config.toml。

    写 /opt/fm-admin/data/backups/config-<时间>.toml，回读 sha256 校验，保留最近 N 份。
    """
    started = time.time()
    source = Path(CONFIG_PATH)
    if not source.is_file():
        raise RuntimeError("config.toml 不存在：" + str(source))
    try:
        data = source.read_bytes()
    except OSError as exc:
        raise RuntimeError("读取 config.toml 失败：" + str(exc))
    if not data:
        raise RuntimeError("config.toml 是空文件：" + str(source))
    sha256 = hashlib.sha256(data).hexdigest()
    BACKUPS.mkdir(parents=True, exist_ok=True)
    name = "config-" + time.strftime("%Y%m%d-%H%M%S") + ".toml"
    target = BACKUPS / name
    temporary = target.with_name(target.name + ".tmp")
    with temporary.open("wb") as handle:
        handle.write(data)
    os.replace(temporary, target)
    read_back = target.read_bytes()
    check_sha = hashlib.sha256(read_back).hexdigest()
    kept: list[str] = []
    pruned: list[str] = []
    try:
        candidates = sorted(BACKUPS.glob("config-*.toml"),
                            key=lambda item: item.stat().st_mtime, reverse=True)
        for item in candidates[:CONFIG_BACKUP_KEEP]:
            kept.append(item.name)
        for item in candidates[CONFIG_BACKUP_KEEP:]:
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
        "source": str(source),
        "path": str(target),
        "file": name,
        "size": len(read_back),
        "size_human": human_size(len(read_back)),
        "sha256": sha256,
        "check_sha256": check_sha,
        "sha_match": check_sha == sha256,
        "created_at_text": time.strftime("%Y-%m-%d %H:%M:%S"),
        "duration_ms": int((time.time() - started) * 1000),
        "kept": kept,
        "pruned": pruned,
        "keep": CONFIG_BACKUP_KEEP,
        "source_agent": "host-ops-agent",
    }


# ── v2：健康检查点（每 HEALTH_INTERVAL 秒一条，供后台 /api/v2/health/history）──
_LAST_HEALTH: dict[str, Any] = {"at": 0.0, "text": "", "samples": 0}


def _load_health_meta() -> None:
    """启动时从 state + health.jsonl 恢复「上次检查点时间 / 样本数」。"""
    try:
        data = json.loads(STATE.read_text(encoding="utf-8"))
    except (OSError, ValueError, TypeError):
        data = {}
    if isinstance(data, dict):
        try:
            _LAST_HEALTH["at"] = float(data.get("last_health_at") or 0.0)
        except (TypeError, ValueError):
            _LAST_HEALTH["at"] = 0.0
        _LAST_HEALTH["text"] = str(data.get("last_health_at_text") or "")
        try:
            _LAST_HEALTH["samples"] = int(data.get("health_samples") or 0)
        except (TypeError, ValueError):
            _LAST_HEALTH["samples"] = 0
    if HEALTH_FILE.is_file():
        try:
            with HEALTH_FILE.open("r", encoding="utf-8", errors="replace") as handle:
                _LAST_HEALTH["samples"] = sum(1 for line in handle if line.strip())
        except OSError:
            pass


def health_snapshot() -> dict:
    """采集一条健康检查点：docker 容器状态/health + 磁盘 + 内存 + 负载。"""
    started = time.time()
    metrics = op_metrics({})
    host = metrics.get("host") if isinstance(metrics.get("host"), dict) else {}
    ps_rows = {str(row.get("name")): row for row in _docker_ps_rows()}
    containers: list[dict] = []
    for name in HEALTH_CONTAINERS:
        row = ps_rows.get(name)
        if row:
            state_text = str(row.get("state") or "")
            containers.append({
                "name": name,
                "image": row.get("image") or "",
                "status": row.get("status") or "",
                "state": state_text,
                "health": row.get("health") or "",
                "running": state_text.lower() == "running",
            })
        else:
            containers.append({"name": name, "image": "", "status": "not-found",
                               "state": "missing", "health": "", "running": False})
    return {
        "at": time.time(),
        "at_text": time.strftime("%Y-%m-%d %H:%M:%S"),
        "load": host.get("load") or {},
        "mem": host.get("mem") or {},
        "disk": host.get("disk") or {},
        "host": {"uptime_text": host.get("uptime_text") or "",
                 "cpu_count": host.get("cpu_count") or 0},
        "containers": containers,
        "agent": {"version": VERSION, "pid": os.getpid()},
        "collect_ms": round((time.time() - started) * 1000),
    }


def write_health_checkpoint(force: bool = False) -> dict | None:
    """按 HEALTH_INTERVAL 节流追加一条 health.jsonl；返回本次快照（未到期返回 None）。"""
    now = time.time()
    if not force and float(_LAST_HEALTH.get("at") or 0) and now - float(_LAST_HEALTH["at"]) < HEALTH_INTERVAL:
        return None
    snapshot = health_snapshot()
    HEALTH_FILE.parent.mkdir(parents=True, exist_ok=True)
    with HEALTH_FILE.open("a", encoding="utf-8") as handle:
        handle.write(json.dumps(snapshot, ensure_ascii=False) + "\n")
    _LAST_HEALTH["at"] = snapshot["at"]
    _LAST_HEALTH["text"] = snapshot["at_text"]
    _LAST_HEALTH["samples"] = int(_LAST_HEALTH.get("samples") or 0) + 1
    try:
        if HEALTH_FILE.stat().st_size > 512 * 1024:
            lines = HEALTH_FILE.read_text(encoding="utf-8", errors="replace").splitlines()
            if len(lines) > HEALTH_KEEP_SAMPLES:
                temporary = HEALTH_FILE.with_name(HEALTH_FILE.name + ".tmp")
                temporary.write_text("\n".join(lines[-HEALTH_KEEP_SAMPLES:]) + "\n", encoding="utf-8")
                os.replace(temporary, HEALTH_FILE)
    except OSError:
        pass
    return snapshot


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


# ══════════════════════════════════════════════════════════════════════════
# v2.2.0：QQ 登录面板（snowluma 状态 / 扫码界面截图 / 重启）
#   链路：手机QQ扫码 → snowluma 容器桌面 QQ → snowluma OneBot WS 0.0.0.0:3001 → fm-cosmobot 连接
# ══════════════════════════════════════════════════════════════════════════
_QQ_BOT_CONN_SCRIPT = r"""
import socket
def _addr(text):
    ip, port = text.split(":")
    return socket.inet_ntoa(bytes.fromhex(ip)[::-1]), int(port, 16)
hits = []
for path in ("/proc/net/tcp", "/proc/net/tcp6"):
    try:
        lines = open(path, encoding="utf-8").read().splitlines()[1:]
    except OSError:
        continue
    for line in lines:
        parts = line.split()
        if len(parts) < 4 or parts[3] != "01":
            continue
        try:
            local_ip, local_port = _addr(parts[1])
            remote_ip, remote_port = _addr(parts[2])
        except Exception:
            continue
        if remote_port == __ONEBOT_PORT__:
            hits.append("%s:%s->%s:%s" % (local_ip, local_port, remote_ip, remote_port))
print("\n".join(hits))
""".replace("__ONEBOT_PORT__", str(ONEBOT_WS_PORT))


def _rfc3339_local(value: object) -> str:
    """docker logs --timestamps 的 UTC 时间（2026-11-15T05:12:49.44Z）→ 宿主机本地时间文本。"""
    text = str(value or "").strip()
    match = re.match(r"^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})", text)
    if not match:
        return text[:32]
    try:
        epoch = calendar.timegm(time.strptime(match.group(1), "%Y-%m-%dT%H:%M:%S"))
    except ValueError:
        return text[:32]
    return time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(epoch))


def _container_brief(name: str) -> dict:
    """容器状态摘要（inspect，不启动任何东西）。"""
    try:
        item = _inspect_container(name)
    except Exception as exc:  # noqa: BLE001
        return {"name": name, "state": "missing", "running": False, "error": str(exc)[:200]}
    state = item.get("State") if isinstance(item.get("State"), dict) else {}
    config = item.get("Config") if isinstance(item.get("Config"), dict) else {}
    health_block = state.get("Health") if isinstance(state.get("Health"), dict) else {}
    return {
        "name": str(item.get("Name") or "").lstrip("/") or name,
        "state": str(state.get("Status") or "unknown"),
        "running": bool(state.get("Running")),
        "health": str(health_block.get("Status") or ""),
        "started_at": str(state.get("StartedAt") or ""),
        "started_at_text": _rfc3339_local(state.get("StartedAt")),
        "restart_count": int(state.get("RestartCount") or 0),
        "image": str(config.get("Image") or ""),
    }


def _docker_exec_output(container: str, argv: list[str], timeout: int = 30) -> dict:
    """固定 argv 的 docker exec（不经过 shell）。"""
    return run_docker(["exec", container] + [str(item) for item in argv], timeout=timeout)


def _onebot_listeners() -> dict:
    """snowluma 容器内 OneBot 端口监听情况（ONE bot WS 3001 + HTTP 3000）。"""
    output = _docker_exec_output(SNOWLUMA_CONTAINER, ["ss", "-tln"], timeout=20)
    lines = (output.get("stdout") or "").splitlines()
    ports: dict[str, str] = {}
    for line in lines:
        parts = line.split()
        if len(parts) >= 4 and parts[0] == "LISTEN":
            local = parts[3]
            if ":" in local:
                ports[local.rsplit(":", 1)[1]] = local
    return {
        "ok": bool(output.get("ok")),
        "listening": str(ONEBOT_WS_PORT) in ports,
        "port": ONEBOT_WS_PORT,
        "http": str(ONEBOT_HTTP_PORT) in ports,
        "http_port": ONEBOT_HTTP_PORT,
        "ws_bind": ports.get(str(ONEBOT_WS_PORT), ""),
        "ports": sorted(ports.keys()),
        "error": ((output.get("stderr") or output.get("stdout") or "")[:300]
                  if not output.get("ok") else ""),
    }


def _onebot_peers() -> list[str]:
    """谁连着 OneBot 3001（snowluma 侧 ss -tn 的 ESTAB 对端，正常应看到 bot 的 IP:port）。"""
    output = _docker_exec_output(SNOWLUMA_CONTAINER, ["ss", "-tn"], timeout=20)
    peers: list[str] = []
    for line in (output.get("stdout") or "").splitlines():
        parts = line.split()
        if len(parts) >= 5 and parts[0] == "ESTAB":
            local, peer = parts[3], parts[4]
            if local.endswith(":" + str(ONEBOT_WS_PORT)):
                peers.append(peer)
    return peers


def _bot_onebot_connections() -> list[str]:
    """bot 容器 /proc/net/tcp 里连到 3001 的 ESTABLISHED 连接（用容器自带 python3 解析）。"""
    output = _docker_exec_output(CONTAINER_NAME, ["python3", "-c", _QQ_BOT_CONN_SCRIPT], timeout=25)
    if not output.get("ok"):
        return []
    return [line.strip() for line in (output.get("stdout") or "").splitlines() if line.strip()]


def _png_dimensions(path: Path) -> tuple[int | None, int | None]:
    try:
        with path.open("rb") as handle:
            head = handle.read(33)
    except OSError:
        return None, None
    if len(head) < 24 or head[:8] != b"\x89PNG\r\n\x1a\n":
        return None, None
    return int.from_bytes(head[16:20], "big"), int.from_bytes(head[20:24], "big")


def _qq_screen_meta() -> dict:
    """现有截图文件的信息（不抓图）。"""
    path = QQ_SCREEN_PATH
    if not path.is_file():
        return {"available": False, "path": str(path)}
    try:
        stat = path.stat()
    except OSError as exc:
        return {"available": False, "path": str(path), "error": str(exc)[:200]}
    width, height = _png_dimensions(path)
    return {
        "available": True,
        "path": str(path),
        "bytes": stat.st_size,
        "mtime": stat.st_mtime,
        "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
        "age_seconds": round(max(0.0, time.time() - stat.st_mtime), 1),
        "width": width,
        "height": height,
    }


_QQ_LOGIN_RE = re.compile(
    r"^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})\S*\s+.*\[Hook\]\s+login detected:\s+PID=(\d+)\s+UIN=(\d+)")
_QQ_GROUPS_RE = re.compile(
    r"^(\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2})\S*\s+.*\[OneBot\]\s+groups loaded:\s+UIN=(\d+)\s+count=(\d+)")
_QQ_OFFLINE_HINTS = ("logout", "kicked", "kickout", "offline", "登录失效", "下线", "重新登录", "token expired")


def _docker_logs_text(container: str, tail: int = QQ_LOG_TAIL, since: str = "") -> str:
    """抓 docker logs 文本，**保留最后 QQ_LOG_BYTES 字节**。

    注意：不能复用 run_docker（它只保留前 LOG_BYTES_MAX=400KB），否则默认拿不到最近的
    登录/群信息（实测 6 万行日志的 login 行在第 3.3 万行之后，直接被截掉）。
    """
    argv = [DOCKER, "logs", "--timestamps"]
    if since:
        argv += ["--since", str(since)]
    else:
        argv += ["--tail", str(int(tail))]
    argv.append(container)
    try:
        completed = subprocess.run(argv, capture_output=True, text=True, encoding="utf-8",
                                   errors="replace", timeout=120, cwd="/")
    except subprocess.TimeoutExpired:
        return ""
    except Exception:  # noqa: BLE001
        return ""
    text = (completed.stdout or "") + "\n" + (completed.stderr or "")
    if len(text.encode("utf-8", "replace")) > QQ_LOG_BYTES:
        # 按字符截尾（近似字节数，够用）
        text = text[-QQ_LOG_BYTES:]
    return text


def _qq_log_lines(tail: int = QQ_LOG_TAIL) -> list[str]:
    """snowluma 日志（带 docker 时间戳）：默认取最近 6h、仅保留尾部 4MB，再解析。"""
    text = _docker_logs_text(SNOWLUMA_CONTAINER, tail=tail, since=QQ_LOG_SINCE)
    if not text.strip():
        text = _docker_logs_text(SNOWLUMA_CONTAINER, tail=tail, since="")
    return [line for line in text.splitlines() if line.strip()]


def _qq_log_facts_cached() -> dict:
    """日志解析结果缓存 30s（6 万行每次解析 ~0.2s；status 10s 缓存 + 这里 30s 足够）。"""
    facts = _cmd_cached("qq:logfacts", QQ_LOG_FACTS_TTL, lambda: _qq_log_facts(_qq_log_lines()))
    return dict(facts or {})


def _qq_log_facts(lines: list[str]) -> dict:
    """从 snowluma 日志里提取：最近登录时间 / UIN / 群数量 / 掉线类告警。"""
    login_at = ""
    uin = ""
    groups: int | None = None
    groups_at = ""
    offline_hits: list[str] = []
    for line in lines:
        match = _QQ_LOGIN_RE.search(line)
        if match:
            login_at = _rfc3339_local(match.group(1))
            uin = match.group(3)
            continue
        match = _QQ_GROUPS_RE.search(line)
        if match:
            groups = int(match.group(3))
            groups_at = _rfc3339_local(match.group(1))
            continue
        if "[Event]" in line:
            # 群聊事件日志里可能出现用户聊天内容（例如「QQ又掉线了」），不是系统状态
            continue
        lowered = line.lower()
        if any(hint in lowered for hint in _QQ_OFFLINE_HINTS):
            offline_hits.append(line[:300])
    return {"last_login_at": login_at, "uin": uin, "groups": groups,
            "groups_loaded_at": groups_at, "offline_hints": offline_hits[-3:],
            "log_lines": len(lines)}


def op_qq_status(params: dict) -> dict:
    """GET /api/v2/qq/status：snowluma + OneBot 3001/3000 + bot 连接 + 群数/最近登录。"""
    checked = time.time()
    snow = _container_brief(SNOWLUMA_CONTAINER)
    bot = _container_brief(CONTAINER_NAME)
    onebot = _onebot_listeners()
    peers = _onebot_peers() if onebot.get("ok") else []
    connections = _bot_onebot_connections()
    facts = _qq_log_facts_cached()
    connected = bool(connections) or bool(peers)
    # 判定：OneBot 3001 在监听 + 有连接（bot 或其它客户端）== 已登录；
    # groups / last_login_at 只是展示信息（日志轮转时可能拿不到，不影响判定）。
    logged_in = bool(onebot.get("listening")) and connected
    return {
        "snowluma": snow,
        "onebot": {**onebot, "peers": peers, "peer_count": len(peers)},
        "bot": {**bot, "connected_to_onebot": connected,
                "connections": connections, "connection_count": len(connections)},
        "groups": facts.get("groups"),
        "groups_loaded_at": facts.get("groups_loaded_at") or "",
        "last_login_at": facts.get("last_login_at") or "",
        "uin": facts.get("uin") or "",
        "logged_in": logged_in,
        "offline_hints": facts.get("offline_hints") or [],
        "log_lines_scanned": facts.get("log_lines"),
        "log_tail": QQ_LOG_TAIL,
        "log_facts_ttl": QQ_LOG_FACTS_TTL,
        "screen": _qq_screen_meta(),
        "webui": {"port": SNOWLUMA_WEBUI_PORT,
                  "url": "http://127.0.0.1:%d" % SNOWLUMA_WEBUI_PORT},
        "checked_at": checked,
        "checked_at_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(checked)),
        "source": "host-ops-agent:docker inspect/exec/logs",
    }


def op_qq_screen(params: dict) -> dict:
    """GET /api/v2/qq/screen：snowluma 桌面截图（ffmpeg x11grab → docker cp → data/qq/latest.png）。"""
    started = time.time()
    QQ_DATA_DIR.mkdir(parents=True, exist_ok=True)
    capture = _docker_exec_output(
        SNOWLUMA_CONTAINER,
        ["sh", "-c", "DISPLAY=:1 ffmpeg -loglevel error -f x11grab -video_size %s -i :1 "
                     "-frames:v 1 -y %s" % (SNOWLUMA_SCREEN_SIZE, QQ_SCREEN_REMOTE)],
        timeout=40,
    )
    if not capture.get("ok"):
        raise RuntimeError("snowluma 截图失败（ffmpeg x11grab）："
                           + ((capture.get("stderr") or capture.get("stdout") or "")[:400]))
    temp_path = QQ_DATA_DIR / ("latest.%d.png" % int(started * 1000))
    copy = run_docker(["cp", SNOWLUMA_CONTAINER + ":" + QQ_SCREEN_REMOTE, str(temp_path)], timeout=40)
    run_docker(["exec", SNOWLUMA_CONTAINER, "rm", "-f", QQ_SCREEN_REMOTE], timeout=15)
    if not copy.get("ok"):
        raise RuntimeError("docker cp 截图失败：" + ((copy.get("stderr") or copy.get("stdout") or "")[:400]))
    try:
        os.replace(temp_path, QQ_SCREEN_PATH)
    except OSError as exc:
        raise RuntimeError("截图落盘失败：" + str(exc)[:300])
    width, height = _png_dimensions(QQ_SCREEN_PATH)
    try:
        digest = hashlib.md5(QQ_SCREEN_PATH.read_bytes()).hexdigest()
        stat = QQ_SCREEN_PATH.stat()
    except OSError as exc:
        raise RuntimeError("读取截图失败：" + str(exc)[:300])
    return {
        "path": str(QQ_SCREEN_PATH),
        "bytes": stat.st_size,
        "md5": digest,
        "width": width,
        "height": height,
        "mtime": stat.st_mtime,
        "mtime_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(stat.st_mtime)),
        "captured_at": time.time(),
        "duration_ms": int((time.time() - started) * 1000),
        "container": SNOWLUMA_CONTAINER,
        "source": "docker exec snowluma:ffmpeg x11grab",
    }


def _wait_onebot_listening(timeout: float) -> tuple[bool, dict, int]:
    deadline = time.time() + max(5.0, float(timeout))
    attempts = 0
    last: dict = {}
    while time.time() < deadline:
        attempts += 1
        last = _onebot_listeners()
        if last.get("listening"):
            return True, last, attempts
        time.sleep(2.0)
    return False, last, attempts


def _wait_bot_connected(timeout: float) -> tuple[bool, list[str], int]:
    deadline = time.time() + max(5.0, float(timeout))
    attempts = 0
    last: list[str] = []
    while time.time() < deadline:
        attempts += 1
        last = _bot_onebot_connections()
        if last:
            return True, last, attempts
        time.sleep(2.0)
    return False, last, attempts


def op_qq_restart(params: dict) -> dict:
    """POST /api/v2/qq/restart：重启 snowluma 或 bot（白名单 target；支持 dry_run）。

    snowluma 真重启会退出 QQ 登录态、需要手机 QQ 重新扫码，所以支持 dry_run 只做校验。
    """
    target = str(params.get("target") or "").strip().lower()
    if target not in ("snowluma", "bot"):
        raise PermissionError("qq_restart 只允许 target=snowluma|bot，收到：" + (target or "(空)"))
    dry_run = str(params.get("dry_run") or "").strip().lower() in {"1", "true", "yes", "on"}
    confirm = str(params.get("confirm") or "").strip().lower() in {"1", "true", "yes", "on"}
    if target == "snowluma" and not dry_run and not confirm:
        # 事故教训：snowluma 重启会丢掉已扫码的 QQ 登录态，必须显式 confirm 才允许执行。
        raise CodedError(409, "拒绝重启 snowluma：它会退出 QQ 登录态并要求手机QQ重新扫码。"
                              "确需重启请带 confirm=true；只想校验请用 dry_run=true。")
    container = SNOWLUMA_CONTAINER if target == "snowluma" else CONTAINER_NAME
    before = _container_brief(container)
    if dry_run:
        plan = (["docker restart -t 10 " + container] if target == "snowluma"
                else ["docker restart -t 5 " + container,
                      "等待容器 healthy（≤120s）", "等待 bot 重新连上 OneBot 3001（≤90s）"])
        return {
            "target": target, "container": container, "dry_run": True, "restarted": False,
            "before": before,
            "onebot_before": _onebot_listeners() if target == "snowluma" else {},
            "plan": plan,
            "confirm_required": target == "snowluma",
            "note": "dry-run：只做参数与前置状态校验，没有重启任何容器。"
                    + ("（注意：snowluma 真重启会退出 QQ 登录态，需要手机 QQ 重新扫码；"
                       "请只在确认要重新登录时执行。）" if target == "snowluma" else ""),
            "checked_at": time.time(),
        }
    started = time.time()
    wait_seconds = "10" if target == "snowluma" else "5"
    output = run_docker(["restart", "-t", wait_seconds, container], timeout=180)
    if not output.get("ok"):
        raise RuntimeError("docker restart %s 失败：%s"
                           % (container, (output.get("stderr") or output.get("stdout") or "")[:400]))
    result = {
        "target": target,
        "container": container,
        "dry_run": False,
        "restarted": True,
        "step": "docker restart -t %s %s" % (wait_seconds, container),
        "docker_output": (output.get("stdout") or "").strip()[:200],
        "restarted_at": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(started)),
        "before": before,
    }
    if target == "snowluma":
        ok, onebot, attempts = _wait_onebot_listening(SNOWLUMA_RESTART_WAIT)
        result.update({
            "onebot": onebot, "onebot_listening": ok, "onebot_attempts": attempts,
            "healthy_after_s": round(time.time() - started, 1),
            "note": "snowluma 已重启：" + ("OneBot %d 已恢复监听；" % ONEBOT_WS_PORT if ok
                                          else "%ss 内未恢复监听，请看容器日志；" % SNOWLUMA_RESTART_WAIT)
                    + "QQ 掉登录后需要手机 QQ 重新扫码（后台 QQ 登录页可看二维码）。",
        })
    else:
        healthy, state, history, reason = _wait_container_healthy(CONTAINER_NAME, 120)
        connected, connections, attempts = _wait_bot_connected(90)
        result.update({
            "healthy": healthy, "health": state, "health_history": history[-8:],
            "health_reason": reason,
            "connected_to_onebot": connected, "connections": connections,
            "connect_attempts": attempts,
            "healthy_after_s": round(time.time() - started, 1),
            "note": "bot 已重启：" + ("已重新连上 OneBot %d。" % ONEBOT_WS_PORT if connected
                                    else "但 90s 内未观察到到 %d 的连接（可能稍后重连，看 bot 日志）。"
                                         % ONEBOT_WS_PORT),
        })
    result["duration_ms"] = int((time.time() - started) * 1000)
    return result


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
    # ── v2 新增 ──
    if op == "stats":
        return op_stats(params)
    if op == "cron_list":
        return op_cron_list(params)
    if op == "cron_set":
        return op_cron_set(params)
    if op == "config_backup":
        return op_config_backup(params)
    # ── v2.2.0 QQ 登录面板 ──
    if op == "qq_status":
        return op_qq_status(params)
    if op == "qq_screen":
        return op_qq_screen(params)
    if op == "qq_restart":
        return op_qq_restart(params)
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


HEAVY_OPS = ("images", "images_detail", "rollback", "restart", "backup_domain", "qq_restart")
_HEAVY_STOP = threading.Event()


def _read_queue_entries() -> list[tuple[Path, Any]]:
    """读队列目录（最多 MAX_QUEUE_FILES），返回 [(path, parsed_or_None)]。"""
    entries: list[tuple[Path, Any]] = []
    try:
        candidates = sorted(QUEUE.glob("*.json"))[:MAX_QUEUE_FILES]
    except OSError:
        return entries
    for path in candidates:
        if not SAFE_ID.match(path.stem):
            log("跳过非法队列文件名：" + str(path))
            try:
                path.unlink()
            except OSError:
                pass
            continue
        try:
            parsed = json.loads(path.read_text(encoding="utf-8"))
        except Exception:  # noqa: BLE001  解析错误交给 _process_request_file 统一报错
            parsed = None
        entries.append((path, parsed))
    return entries


def _is_heavy(parsed: Any) -> bool:
    if not isinstance(parsed, dict):
        return False
    op = str(parsed.get("op") or "").strip().lower()
    if op == "qq_restart":
        # dry-run 只做参数/状态校验，不重启容器 → 走快 op，避免排重 op 队列
        params = parsed.get("params") if isinstance(parsed.get("params"), dict) else {}
        dry = str(params.get("dry_run") or "").strip().lower() in {"1", "true", "yes", "on"}
        return not dry
    return op in HEAVY_OPS


def _process_request_file(path: Path, scan_id: int, parsed: Any = None) -> dict:
    """处理一个队列文件：执行 op → 写结果文件 → 删队列文件。不抛异常。"""
    stem = path.stem
    request: dict = {"id": stem}
    started = time.time()
    try:
        if not isinstance(parsed, dict):
            if parsed is None:
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
        payload = result_payload(request, False, started=started, error=str(exc)[:1000])
        status = getattr(exc, "status", None)
        if isinstance(status, int) and 400 <= status <= 599:
            payload["error_status"] = status
    try:
        atomic_write_json(RESULTS / (stem + ".json"), payload)
        path.unlink()
        log("scan=%s op=%s target=%s id=%s -> %s" % (
            scan_id, payload.get("op"), payload.get("target"), stem,
            "ok" if payload.get("ok") else "FAIL " + str(payload.get("error"))[:200]))
    except OSError as exc:
        log("写结果失败 id=%s：%s" % (stem, exc))
        return {"stem": stem, "ok": False, "error": str(exc)[:200]}
    return {"stem": stem, "ok": bool(payload.get("ok")),
            "duration_ms": payload.get("duration_ms"), "op": payload.get("op")}


def process_queue(scan_id: int) -> dict:
    """处理「快」op（metrics/logs/stats/cron/inspect/config_backup…）。

    重 op（images/images_detail/rollback/restart/backup_domain）交给 _heavy_worker_loop，
    避免 docker images（~20s）把高频的 ops/metrics、ops/logs 堵在队列后面
    （QA P1-2 复测里 force=1 曾因它排队 18.6s）。
    """
    QUEUE.mkdir(parents=True, exist_ok=True)
    RESULTS.mkdir(parents=True, exist_ok=True)
    processed = 0
    failures = 0
    last_id = ""
    skipped_heavy = 0
    for path, parsed in _read_queue_entries():
        if _is_heavy(parsed):
            skipped_heavy += 1
            continue
        outcome = _process_request_file(path, scan_id, parsed)
        if outcome.get("stem"):
            processed += 1
            last_id = outcome["stem"]
            if not outcome.get("ok"):
                failures += 1
    return {"processed": processed, "failures": failures, "last_id": last_id,
            "skipped_heavy": skipped_heavy}


def process_heavy_sync(scan_id: int = 0) -> int:
    """单轮模式（--once）下同步处理重 op，保持旧行为。"""
    count = 0
    for path, parsed in _read_queue_entries():
        if not _is_heavy(parsed):
            continue
        _process_request_file(path, scan_id, parsed)
        count += 1
    return count


def _heavy_worker_loop() -> None:
    """重 op 专用线程：串行执行 images/images_detail/rollback/restart/backup_domain。

    这些 op 单次 5~180s，串行保证 rollback/restart 不会互相打架；同时不影响快 op。
    """
    while not _HEAVY_STOP.wait(1.0):
        try:
            heavy = [(path, parsed) for path, parsed in _read_queue_entries() if _is_heavy(parsed)]
        except Exception as exc:  # noqa: BLE001
            log("重 op 扫描失败：%s" % str(exc)[:200])
            continue
        for path, parsed in heavy:
            if _HEAVY_STOP.is_set():
                return
            try:
                _process_request_file(path, 0, parsed)
            except Exception as exc:  # noqa: BLE001
                log("重 op 处理异常：%s" % str(exc)[:300])


def _queue_pending() -> int:
    try:
        return len(list(QUEUE.glob("*.json")))
    except OSError:
        return 0


_STATE_LOCK = threading.Lock()
# 心跳线程共享的「最近一轮扫描」信息（长时间 op 期间也能刷新心跳，避免后台误判离线）
_SCAN_INFO: dict[str, Any] = {"scans": 0, "total": 0, "last_id": "", "scan_seconds": 0.0, "mode": "loop"}


def _write_agent_state(scan_seconds: float, scans: int, total: int, last_id: str,
                       extra: dict | None = None) -> dict:
    now = time.time()
    now_text = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(now))
    state = {
        "version": VERSION,
        "pid": os.getpid(),
        # v2.1.2：统一 state schema（Lead/QA 反馈「读 updated_at 得到 None」）：
        # updated_at / started_at 用 epoch 秒 + 对应的 *_text 本地时间；
        # last_scan / last_scan_text 保留为兼容旧读取方。
        "schema_version": 2,
        "updated_at": now,
        "updated_at_text": now_text,
        "started_at": STARTED_AT,
        "started_at_text": time.strftime("%Y-%m-%d %H:%M:%S", time.localtime(STARTED_AT)),
        "last_scan": now,
        "last_scan_text": now_text,
        "scan_seconds": round(scan_seconds, 3),
        "scans": scans,
        "processed_total": total,
        "last_processed_id": last_id or "",
        "queue": str(QUEUE),
        "results": str(RESULTS),
        "backups": str(BACKUPS),
        "container": CONTAINER_NAME,
        "domain_db": DOMAIN_DB,
        "allowed_ops": list(ALLOWED_OPS),
        "stats_containers": list(STATS_CONTAINERS),
        "config_path": CONFIG_PATH,
        "health_file": str(HEALTH_FILE),
        "health_interval": HEALTH_INTERVAL,
        "last_health_at": _LAST_HEALTH.get("at") or 0,
        "last_health_at_text": _LAST_HEALTH.get("text") or "",
        "health_samples": _LAST_HEALTH.get("samples") or 0,
        "linger_seconds": LINGER_SECONDS,
        "mode": "loop",
        "uptime_seconds": round(max(0.0, now - STARTED_AT), 1),
    }
    if isinstance(extra, dict):
        state.update(extra)
    with _STATE_LOCK:
        atomic_write_json(STATE, state)
    return state


def _heartbeat_loop(stop_event: "threading.Event") -> None:
    """每 5s 刷一次心跳（独立线程）：长 op（rollback/images）或驻留空闲时后台仍显示在线。"""
    while not stop_event.wait(5.0):
        try:
            _write_agent_state(_SCAN_INFO.get("scan_seconds") or 0.0,
                               int(_SCAN_INFO.get("scans") or 0),
                               int(_SCAN_INFO.get("total") or 0),
                               str(_SCAN_INFO.get("last_id") or ""),
                               {"mode": _SCAN_INFO.get("mode") or "loop",
                                "heartbeat": True,
                                "pending": _queue_pending()})
        except Exception:  # noqa: BLE001  心跳失败不影响主循环
            pass


def _run_scan(scans: int) -> dict:
    """扫一轮队列 + （按需）写健康检查点；返回 process_queue 的摘要。"""
    started = time.time()
    _SCAN_INFO["mode"] = "resident" if int(_SCAN_INFO.get("scans") or 0) >= 1 else "loop"
    summary = process_queue(scans)
    # v2：每 HEALTH_INTERVAL 秒追加一条健康检查点（节流在函数内部判断）
    try:
        snapshot = write_health_checkpoint()
        if snapshot:
            log("health checkpoint：%s containers=%s samples=%s"
                % (snapshot["at_text"], len(snapshot.get("containers") or []),
                   _LAST_HEALTH.get("samples")))
    except Exception as exc:  # noqa: BLE001  健康检查点失败不能拖垮队列处理
        log("健康检查点写入失败：%s" % str(exc)[:300])
    summary = dict(summary or {})
    summary["scan_seconds"] = round(time.time() - started, 3)
    return summary


def main() -> int:
    parser = argparse.ArgumentParser(description="FM host ops agent (docker whitelist)")
    parser.add_argument("--once", action="store_true", help="只扫一轮（默认）")
    parser.add_argument("--loop", type=int, default=0, help="连续扫 N 轮后进入驻留等待")
    parser.add_argument("--interval", type=float, default=18.0, help="每轮间隔秒数（默认 18）")
    parser.add_argument("--linger", type=float, default=None,
                        help="扫完 --loop 后继续驻留等队列的秒数（默认 90；0=不驻留立即退出）")
    parser.add_argument("--health-now", action="store_true",
                        help="立即写一条健康检查点（忽略 5 分钟节流），然后退出")
    args = parser.parse_args()
    loops = args.loop if args.loop and args.loop > 0 else 1
    # v2.1：cron 每分钟才拉起一次，进程扫完 --loop 就退出会留下最长 ~60s 空档，
    # 造成后台 ops 接口首包 20~30s（QA P1-3 的根因）。所以 --loop 模式扫完后「驻留」
    # 继续盯队列（1s 轮询），空闲超过 LINGER 秒才退出（下一分钟 cron 再拉起 = 自愈）。
    if args.linger is not None:
        linger_seconds = max(0.0, float(args.linger))
    elif args.once or not args.loop:
        linger_seconds = 0.0
    else:
        linger_seconds = LINGER_SECONDS

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

    _load_health_meta()
    if args.health_now:
        try:
            snapshot = write_health_checkpoint(force=True)
            log("health checkpoint（--health-now）：%s samples=%s"
                % (snapshot.get("at_text") if snapshot else "skip", _LAST_HEALTH.get("samples")))
        except Exception as exc:  # noqa: BLE001
            log("健康检查点写入失败：%s" % str(exc)[:300])
            return 3
        try:
            fcntl.flock(lock_handle.fileno(), fcntl.LOCK_UN)
        except Exception:  # noqa: BLE001
            pass
        lock_handle.close()
        return 0

    scans = 0
    total = 0
    # v2.1.1：独立心跳线程，长 op（rollback/images）或驻留空闲时后台也能看到 agent 在线
    stop_heartbeat = threading.Event()
    heartbeat_thread = None
    heavy_thread = None
    if linger_seconds != 0 or loops > 1:
        heartbeat_thread = threading.Thread(target=_heartbeat_loop, args=(stop_heartbeat,),
                                           name="fm-ops-heartbeat", daemon=True)
        heartbeat_thread.start()
        # v2.1.4：重 op 独立线程，避免 docker images(~20s)/rollback 堵住高频快 op
        _HEAVY_STOP.clear()
        heavy_thread = threading.Thread(target=_heavy_worker_loop, name="fm-ops-heavy", daemon=True)
        heavy_thread.start()
    try:
        for index in range(loops):
            scans += 1
            summary = _run_scan(scans)
            total += int(summary.get("processed") or 0)
            _SCAN_INFO.update({"scans": scans, "total": total,
                               "last_id": summary.get("last_id") or "",
                               "scan_seconds": summary.get("scan_seconds") or 0.0,
                               "mode": "loop"})
            _write_agent_state(summary.get("scan_seconds") or 0.0, scans, total,
                               summary.get("last_id") or "",
                               {"mode": "loop", "linger_seconds": linger_seconds})
            if linger_seconds <= 0 and loops == 1:
                # 单轮模式：同步把重 op 也处理掉（保持 --once 的旧行为）
                process_heavy_sync(scans)
                _write_agent_state(summary.get("scan_seconds") or 0.0, scans, total,
                                   summary.get("last_id") or "",
                                   {"mode": "once", "linger_seconds": 0.0})
            if index < loops - 1:
                time.sleep(max(1.0, args.interval))

        # ── 驻留等待：队列一来就处理；linger_seconds<0 = 永不因空闲退出（常驻）──
        if linger_seconds != 0:
            idle_since = time.time()
            last_state_at = 0.0
            if linger_seconds > 0:
                log("进入驻留模式：轮询队列（0.5s），空闲 %.0fs 后退出" % linger_seconds)
            else:
                log("进入常驻模式：轮询队列（0.5s），不因空闲退出；"
                    "最长运行 %.0fs 后主动退出换新进程" % MAX_UPTIME_SECONDS)
            while True:
                pending = _queue_pending()
                if pending:
                    summary = _run_scan(scans + 1)
                    processed = int(summary.get("processed") or 0)
                    if processed == 0:
                        # v2.1.5：队列里只剩「重 op」（images/rollback 由 fm-ops-heavy 线程处理），
                        # 本轮没处理任何请求 → 别空转、也别把 scans 计数刷爆（曾刷到 6 万+）。
                        time.sleep(0.5)
                        continue
                    scans += 1
                    total += processed
                    idle_since = time.time()
                    _SCAN_INFO.update({"scans": scans, "total": total,
                                       "last_id": summary.get("last_id") or "",
                                       "scan_seconds": summary.get("scan_seconds") or 0.0,
                                       "mode": "resident"})
                    _write_agent_state(summary.get("scan_seconds") or 0.0, scans, total,
                                       summary.get("last_id") or "",
                                       {"mode": "resident", "linger_seconds": linger_seconds,
                                        "pending": _queue_pending(),
                                        "idle_seconds": 0.0})
                    continue
                now = time.time()
                idle = now - idle_since
                if MAX_UPTIME_SECONDS > 0 and now - STARTED_AT >= MAX_UPTIME_SECONDS:
                    log("已连续运行 %.0fs（上限 %.0fs），主动退出由 cron 拉起新进程"
                        % (now - STARTED_AT, MAX_UPTIME_SECONDS))
                    _write_agent_state(0.0, scans, total, "",
                                       {"mode": "max-uptime-exit", "linger_seconds": linger_seconds,
                                        "idle_seconds": round(idle, 1)})
                    break
                if linger_seconds > 0 and idle >= linger_seconds:
                    log("空闲 %.0fs，退出驻留（cron 下一分钟会重新拉起）" % idle)
                    _write_agent_state(0.0, scans, total, "",
                                       {"mode": "exiting", "linger_seconds": linger_seconds,
                                        "idle_seconds": round(idle, 1)})
                    break
                if now - last_state_at >= 4.0:
                    _write_agent_state(0.0, scans, total, "",
                                       {"mode": "resident-idle", "linger_seconds": linger_seconds,
                                        "pending": 0, "idle_seconds": round(idle, 1)})
                    last_state_at = now
                time.sleep(0.5)
    finally:
        stop_heartbeat.set()
        _HEAVY_STOP.set()
        if heartbeat_thread is not None:
            heartbeat_thread.join(timeout=2.0)
        if heavy_thread is not None:
            heavy_thread.join(timeout=3.0)
        try:
            fcntl.flock(lock_handle.fileno(), fcntl.LOCK_UN)
        except Exception:  # noqa: BLE001
            pass
        if lock_handle is not None:
            lock_handle.close()
    return 0


if __name__ == "__main__":
    sys.exit(main())
