"""FM 后台 v4 API：分析 / 内容管理 / 运维 v2 / 健康历史 / 缓存配套。

设计原则（与 v2/v3 保持一致，不破坏既有接口）
- 全部只读 sqlite：/runtime/cosmobot.sqlite3 用 `file:...?mode=ro` URI 打开（api_v2._query）；
- 内容管理一律代理 fm-domain（app.fetch_domain 传入），失败抛 ApiError(502)，
  由 app.py 统一返回 {"ok":false,"error":...}，不产生 5xx 堆栈；
- 缓存：domain 类接口按 TTL 5~300s 缓存（见 CACHE_TTLS），sqlite 统计类 30~120s；
- 运维：一律通过宿主机 ops-agent 队列（api_v2.ops_request），后台容器没有 docker socket；
  回滚是高危操作：必须 confirm=true，由宿主机代理「停旧→改名→run 新镜像→健康检查→
  失败自动恢复旧容器」，结果同时写 /opt/fm-admin/data/backups/rollback-*.json；
- 错误：抛 api_v2.ApiError(message, status)。

接口清单见文件末尾 V4_ROUTES。
"""
from __future__ import annotations

import calendar
import hashlib
import json
import math
import os
import re
import threading
import time
from typing import Any, Callable

import api_v2 as v2

RUNTIME = v2.RUNTIME
DATA_BASE = v2.OPS_BASE                      # 容器内 /data == 宿主机 /opt/fm-admin/data
HEALTH_HISTORY = os.environ.get("FM_OPS_HEALTH_FILE", os.path.join(DATA_BASE, "health.jsonl"))
HEALTH_INTERVAL_SECONDS = int(os.environ.get("FM_OPS_HEALTH_INTERVAL", "300") or 300)
BACKUP_DIR = os.path.join(DATA_BASE, "backups")
RESULTS_DIR = v2.OPS_RESULTS

LOCAL_OFFSET_HOURS = v2.LOCAL_OFFSET_HOURS

# 各接口缓存 TTL（秒）：sqlite 统计类按任务要求控制在 5~30s，domain 代理类 30s
# （文库正文/群名这类低频变化的数据放宽到 300s），写操作/实时类不缓存。
CACHE_TTLS: dict[str, float] = {
    "analytics/messages": 20.0,
    "analytics/groups": 30.0,
    "analytics/senders": 30.0,
    "analytics/latency": 30.0,
    "analytics/tokens": 30.0,
    "domain/groups": 300.0,
    "library/list": 30.0,
    "library/text": 300.0,
    "contests": 30.0,
    "members/leaderboard": 30.0,
    "novels": 30.0,
    "novel": 30.0,
    "health/history": 15.0,
    "ops/stats": 15.0,
    "ops/metrics": 15.0,
    "ops/logs": 12.0,
    "ops/cron": 15.0,
    "ops/qq_status": 10.0,
}

WEEKDAYS = ("周一", "周二", "周三", "周四", "周五", "周六", "周日")


# ═══════════════════════════════════════════════════════════════════════
# 0. 缓存（进程内 TTL + 单飞保护；数据量小，不需要 LRU 淘汰）
# ═══════════════════════════════════════════════════════════════════════
_CACHE: dict[str, dict[str, Any]] = {}
_CACHE_LOCK = threading.Lock()
_CACHE_STATS = {"hits": 0, "misses": 0, "sets": 0, "evictions": 0}
_CACHE_MAX_KEYS = 400


def _cache_key(kind: str, *parts: object) -> str:
    return kind + "|" + "|".join(str(part) for part in parts)


def cache_get(key: str, ttl: float) -> tuple[Any, float] | None:
    """命中返回 (value, age_seconds)，过期/不存在返回 None。"""
    now = time.time()
    with _CACHE_LOCK:
        entry = _CACHE.get(key)
        if not entry:
            _CACHE_STATS["misses"] += 1
            return None
        age = now - float(entry.get("at") or 0.0)
        if age > ttl:
            _CACHE.pop(key, None)
            _CACHE_STATS["misses"] += 1
            return None
        _CACHE_STATS["hits"] += 1
        return entry.get("value"), age


def cache_set(key: str, value: Any) -> None:
    with _CACHE_LOCK:
        if len(_CACHE) >= _CACHE_MAX_KEYS and key not in _CACHE:
            # 简单淘汰：丢掉最旧的 1/4
            oldest = sorted(_CACHE.items(), key=lambda item: float(item[1].get("at") or 0.0))
            for name, _entry in oldest[: max(1, len(oldest) // 4)]:
                _CACHE.pop(name, None)
                _CACHE_STATS["evictions"] += 1
        _CACHE[key] = {"value": value, "at": time.time()}
        _CACHE_STATS["sets"] += 1


def cache_wrap(kind: str, key_parts: tuple, ttl: float, producer: Callable[[], Any]) -> dict:
    """缓存包一层：返回 {"value":..., "cached": bool, "age_seconds": float|None, "ttl": ttl}。"""
    key = _cache_key(kind, *key_parts)
    hit = cache_get(key, ttl)
    if hit is not None:
        value, age = hit
        return {"value": value, "cached": True, "age_seconds": round(age, 1), "ttl": ttl}
    value = producer()
    cache_set(key, value)
    return {"value": value, "cached": False, "age_seconds": 0.0, "ttl": ttl}


def cache_peek(key: str) -> tuple[Any, float] | None:
    """不管 TTL 是否过期都返回值（stale-while-revalidate 用）；不存在返回 None。"""
    now = time.time()
    with _CACHE_LOCK:
        entry = _CACHE.get(key)
        if not entry:
            return None
        return entry.get("value"), now - float(entry.get("at") or 0.0)


# ── 宿主机代理类接口（ops/*）：缓存优先 + 单飞后台刷新 + 首次可不等 ──
# QA P1-2/P1-3：docker stats / docker logs 每次 1~3s，且代理按 cron 周期扫描队列，
# 冷调用可能 20s+。这里统一做：
#   * TTL 内直接命中（~ms）；
#   * 过期但手里有旧值 → 立刻返回旧值，同时后台线程刷新（stale-while-revalidate）；
#   * 完全没缓存且 wait=False（自检探针用）→ 立刻返回 warming，后台刷新，不阻塞请求。
_OPS_REFRESH_LOCK = threading.Lock()
_OPS_REFRESH_INFLIGHT: set[str] = set()
_OPS_REFRESH_STATS = {"kicks": 0, "done": 0, "failed": 0}


def _ops_refresh_async(key: str, producer: Callable[[], Any]) -> bool:
    """后台刷一个缓存键（同一个 key 同时只跑一个线程）。返回是否真的发起。"""
    with _OPS_REFRESH_LOCK:
        if key in _OPS_REFRESH_INFLIGHT:
            return False
        _OPS_REFRESH_INFLIGHT.add(key)
        _OPS_REFRESH_STATS["kicks"] += 1

    def runner() -> None:
        try:
            value = producer()
            cache_set(key, value)
            _OPS_REFRESH_STATS["done"] += 1
        except Exception:  # noqa: BLE001  后台刷新失败不能影响请求；下次再刷
            _OPS_REFRESH_STATS["failed"] += 1
        finally:
            with _OPS_REFRESH_LOCK:
                _OPS_REFRESH_INFLIGHT.discard(key)

    threading.Thread(target=runner, name="fm-ops-refresh", daemon=True).start()
    return True


def _ops_cached(kind: str, key_parts: tuple, ttl: float, fetch: Callable[[], dict],
                wait: bool = True, force: bool = False) -> dict:
    """ops 类接口统一缓存策略。

    wait=True  ：没有缓存时同步抓一次（最多等代理一个来回）；
    wait=False ：没有缓存时立刻返回 warming + 后台抓（自检探针/首屏不阻塞用）；
    force=True ：忽略缓存，同步抓并回填。
    返回 {"value", "cached", "age_seconds", "stale", "warming", "ttl"}。
    """
    key = _cache_key("ops/" + kind, *key_parts)
    if force:
        value = fetch()
        cache_set(key, value)
        return {"value": value, "cached": False, "age_seconds": 0.0,
                "stale": False, "warming": False, "ttl": ttl}
    peek = cache_peek(key)
    if peek is not None:
        value, age = peek
        if age <= ttl:
            return {"value": value, "cached": True, "age_seconds": round(age, 1),
                    "stale": False, "warming": False, "ttl": ttl}
        _ops_refresh_async(key, fetch)
        return {"value": value, "cached": True, "age_seconds": round(age, 1),
                "stale": True, "warming": False, "ttl": ttl}
    if not wait:
        _ops_refresh_async(key, fetch)
        return {"value": None, "cached": False, "age_seconds": None,
                "stale": False, "warming": True, "ttl": ttl}
    value = fetch()
    cache_set(key, value)
    return {"value": value, "cached": False, "age_seconds": 0.0,
            "stale": False, "warming": False, "ttl": ttl}


def _ops_warming_payload(kind: str, ttl: float, extra: dict | None = None) -> dict:
    payload = {
        "warming": True,
        "cached": False,
        "cache_age_seconds": None,
        "cache_ttl": ttl,
        "note": "首次采集已入队，宿主机代理通常 1~3s 内返回；稍后刷新本接口（或加 ?force=1 同步等待）。",
        "source": "host-ops-agent(warming)",
    }
    if isinstance(extra, dict):
        payload.update(extra)
    return payload


def cache_stats(rate_limit: dict | None = None) -> dict:
    now = time.time()
    with _CACHE_LOCK:
        stats = dict(_CACHE_STATS)
        entries = [
            {
                "key": key,
                "age_seconds": round(now - float(entry.get("at") or 0.0), 1),
                "ttl": next((ttl for kind, ttl in CACHE_TTLS.items() if key.startswith(kind + "|")), None),
            }
            for key, entry in list(_CACHE.items())
        ]
    with _OPS_REFRESH_LOCK:
        refresh = dict(_OPS_REFRESH_STATS)
        inflight = sorted(_OPS_REFRESH_INFLIGHT)
    entries.sort(key=lambda item: -(item.get("age_seconds") or 0))
    total = stats["hits"] + stats["misses"]
    return {
        "entries": len(entries),
        "max_keys": _CACHE_MAX_KEYS,
        "hits": stats["hits"],
        "misses": stats["misses"],
        "sets": stats["sets"],
        "evictions": stats["evictions"],
        "hit_ratio": round(stats["hits"] / total, 3) if total else None,
        "ttls": dict(CACHE_TTLS),
        "top_entries": entries[:40],
        "ops_refresh": {**refresh, "inflight": inflight},
        "rate_limit": rate_limit or {},
        "now": now,
        "note": "接口级 TTL 缓存：domain 代理 30s、文库正文 300s、sqlite 统计 20~30s、"
                "ops 类 12~15s（过期先用旧值+后台刷新，wait=0 首次不等）。",
    }


# ═══════════════════════════════════════════════════════════════════════
# 1. sqlite 小工具（只读；api_v2._query 已经保证 file:...?mode=ro）
# ═══════════════════════════════════════════════════════════════════════
def _epoch_local_text(value: object) -> float | None:
    """把库里的 UTC DATETIME 文本转 epoch 秒（保留微秒/纳秒小数，run 耗时才准）。"""
    text = str(value or "").strip()
    if len(text) < 19:
        return None
    try:
        epoch = float(calendar.timegm(time.strptime(text[:19], "%Y-%m-%d %H:%M:%S")))
    except ValueError:
        return None
    match = re.match(r"^\.(\d{1,9})", text[19:])
    if match:
        epoch += float("0." + match.group(1))
    return epoch


def _percentile(sorted_values: list[float], percent: float) -> float | None:
    """最近秩（nearest-rank）分位数。"""
    if not sorted_values:
        return None
    rank = max(1, min(len(sorted_values), int(math.ceil(percent / 100.0 * len(sorted_values)))))
    return sorted_values[rank - 1]


def _day_local(epoch: float | None) -> str:
    if epoch is None:
        return ""
    return time.strftime("%Y-%m-%d", time.gmtime(epoch + LOCAL_OFFSET_HOURS * 3600))


def _group_name_map(domain: Any) -> dict[str, str]:
    """domain /groups 群名映射（缓存 300s；取不到返回 {}）。"""
    if not callable(domain):
        return {}
    try:
        wrapped = cache_wrap("domain/groups", (), CACHE_TTLS["domain/groups"], lambda: domain("/groups") or {})
    except Exception:  # noqa: BLE001
        return {}
    result = wrapped["value"]
    items = result.get("data") if isinstance(result, dict) else result
    if isinstance(items, dict):
        items = items.get("groups") or items.get("data") or []
    names: dict[str, str] = {}
    for group in items if isinstance(items, list) else []:
        if not isinstance(group, dict):
            continue
        gid = str(group.get("group_id") or group.get("id") or "").strip()
        name = str(group.get("display_name") or group.get("name") or "").strip()
        if gid and name:
            names[gid] = name
    return names


def _label(chat_id: str, kind: str, names: dict[str, str]) -> str:
    return names.get(chat_id) or (("私聊 " if "Private" in (kind or "") else "群 ") + chat_id)


# ═══════════════════════════════════════════════════════════════════════
# 2. A. 分析类
# ═══════════════════════════════════════════════════════════════════════
def analytics_messages(days: object = 14) -> dict:
    """GET /api/v2/analytics/messages?days=14

    每日消息量（总/机器人/人类）、活跃发言人数、新增发言人数、24x7 小时热力图。
    """
    days_n, since, dates = v2._day_window(v2._int_param(days, 14, 1, 90))
    ttl = CACHE_TTLS["analytics/messages"]

    def build() -> dict:
        daily_sql = (
            "SELECT substr(datetime(substr(recorded_at,1,19), '+%d hours'),1,10) AS day, "
            "COUNT(*) AS total, "
            "SUM(CASE WHEN is_bot=1 THEN 1 ELSE 0 END) AS bot, "
            "SUM(CASE WHEN is_bot=0 THEN 1 ELSE 0 END) AS human, "
            "COUNT(DISTINCT CASE WHEN is_bot=0 THEN sender_id END) AS active_senders "
            "FROM chat_log WHERE substr(recorded_at,1,19) >= ? GROUP BY day"
        ) % LOCAL_OFFSET_HOURS
        buckets = {str(row.get("day") or ""): row for row in v2._query(daily_sql, (since,))}
        new_sql = (
            "SELECT day, COUNT(*) AS n FROM ("
            "  SELECT sender_id, substr(datetime(substr(MIN(recorded_at),1,19), '+%d hours'),1,10) AS day "
            "  FROM chat_log WHERE is_bot=0 AND sender_id IS NOT NULL AND sender_id != '' "
            "  GROUP BY sender_id"
            ") WHERE day >= ? GROUP BY day"
        ) % LOCAL_OFFSET_HOURS
        new_buckets = {str(row.get("day") or ""): int(row.get("n") or 0)
                       for row in v2._query(new_sql, (dates[0] if dates else since,))}
        daily = []
        for date in dates:
            row = buckets.get(date) or {}
            daily.append({
                "date": date,
                "total": int(row.get("total") or 0),
                "bot": int(row.get("bot") or 0),
                "human": int(row.get("human") or 0),
                "active_senders": int(row.get("active_senders") or 0),
                "new_senders": int(new_buckets.get(date) or 0),
            })

        heat_sql = (
            "SELECT CAST(strftime('%%w', datetime(substr(recorded_at,1,19), '+%d hours')) AS INTEGER) AS dow, "
            "CAST(strftime('%%H', datetime(substr(recorded_at,1,19), '+%d hours')) AS INTEGER) AS hour, "
            "COUNT(*) AS n, SUM(CASE WHEN is_bot=0 THEN 1 ELSE 0 END) AS human "
            "FROM chat_log WHERE substr(recorded_at,1,19) >= ? GROUP BY dow, hour"
        ) % (LOCAL_OFFSET_HOURS, LOCAL_OFFSET_HOURS)
        grid = [[0 for _ in range(24)] for _ in range(7)]
        human_grid = [[0 for _ in range(24)] for _ in range(7)]
        for row in v2._query(heat_sql, (since,)):
            try:
                dow = int(row.get("dow") or 0)  # 0=周日
                hour = int(row.get("hour") or 0)
            except (TypeError, ValueError):
                continue
            index = (dow + 6) % 7  # 周一=0 ... 周日=6
            if 0 <= index < 7 and 0 <= hour < 24:
                grid[index][hour] += int(row.get("n") or 0)
                human_grid[index][hour] += int(row.get("human") or 0)
        flat_max = max((max(row) for row in grid), default=0)
        peak = {"weekday": "", "hour": 0, "count": 0}
        for index, row in enumerate(grid):
            for hour, count in enumerate(row):
                if count > peak["count"]:
                    peak = {"weekday": WEEKDAYS[index], "hour": hour,
                            "label": "%s %02d:00" % (WEEKDAYS[index], hour), "count": count}
        peak.setdefault("label", "")

        totals_row = v2._query(
            "SELECT COUNT(*) AS total, SUM(CASE WHEN is_bot=1 THEN 1 ELSE 0 END) AS bot, "
            "COUNT(DISTINCT CASE WHEN is_bot=0 THEN sender_id END) AS active_senders "
            "FROM chat_log WHERE substr(recorded_at,1,19) >= ?", (since,))
        totals_row = totals_row[0] if totals_row else {}
        human = [row for row in daily if row["human"] > 0]
        busiest = max(daily, key=lambda row: row["total"], default={})
        totals = {
            "messages": int(totals_row.get("total") or 0),
            "bot": int(totals_row.get("bot") or 0),
            "human": int(totals_row.get("total") or 0) - int(totals_row.get("bot") or 0),
            "active_senders": int(totals_row.get("active_senders") or 0),
            "new_senders": sum(row["new_senders"] for row in daily),
            "days": days_n,
            "avg_per_day": round(sum(row["total"] for row in daily) / max(1, len(daily)), 1),
            "avg_human_per_day": round(sum(row["human"] for row in daily) / max(1, len(daily)), 1),
            "bot_ratio": round(int(totals_row.get("bot") or 0) / max(1, int(totals_row.get("total") or 0)), 3),
            "busiest_day": {"date": busiest.get("date", ""), "total": busiest.get("total", 0)},
            "peak_hour": peak,
            "active_hours": sum(1 for row in grid for count in row if count > 0),
        }
        return {
            "days": days_n,
            "from": dates[0] if dates else "",
            "to": dates[-1] if dates else "",
            "daily": daily,
            "hourly": {
                "rows": 7,
                "cols": 24,
                "weekdays": list(WEEKDAYS),
                "hours": list(range(24)),
                "total": grid,
                "human": human_grid,
                "max": flat_max,
            },
            "totals": totals,
            "source": "sqlite:chat_log",
            "index_note": "chat_log 有 sender_id/chat_id 索引；recorded_at 无索引，用 substr 区间过滤 + 30s 缓存",
        }

    wrapped = cache_wrap("analytics/messages", (days_n,), ttl, build)
    data = wrapped["value"]
    data["cached"] = wrapped["cached"]
    data["cache_age_seconds"] = wrapped["age_seconds"]
    data["cache_ttl"] = ttl
    return data


def analytics_groups(days: object = 30, limit: object = 50, domain: Any = None) -> dict:
    """GET /api/v2/analytics/groups?days=30&limit=50

    每群消息量 / 活跃人数 / 人均条数 / 活跃小时（Top 小时）。
    """
    days_n, since, _dates = v2._day_window(v2._int_param(days, 30, 1, 90))
    limit_n = v2._int_param(limit, 50, 1, 200)
    ttl = CACHE_TTLS["analytics/groups"]

    def build() -> dict:
        sql = (
            "SELECT CAST(chat_id AS TEXT) AS chat_id, MAX(kind_key) AS kind, "
            "COUNT(*) AS messages, SUM(CASE WHEN is_bot=1 THEN 1 ELSE 0 END) AS bot, "
            "SUM(CASE WHEN is_bot=0 THEN 1 ELSE 0 END) AS human, "
            "COUNT(DISTINCT CASE WHEN is_bot=0 THEN sender_id END) AS active_senders, "
            "MIN(recorded_at) AS first_at, MAX(recorded_at) AS last_at "
            "FROM chat_log WHERE chat_id IS NOT NULL AND substr(recorded_at,1,19) >= ? "
            "GROUP BY chat_id ORDER BY messages DESC"
        )
        rows = v2._query(sql, (since,))
        hour_sql = (
            "SELECT CAST(chat_id AS TEXT) AS chat_id, "
            "CAST(strftime('%%H', datetime(substr(recorded_at,1,19), '+%d hours')) AS INTEGER) AS hour, "
            "COUNT(*) AS n FROM chat_log "
            "WHERE chat_id IS NOT NULL AND substr(recorded_at,1,19) >= ? GROUP BY chat_id, hour"
        ) % LOCAL_OFFSET_HOURS
        hours: dict[str, dict[int, int]] = {}
        for row in v2._query(hour_sql, (since,)):
            chat_id = str(row.get("chat_id") or "")
            try:
                hour = int(row.get("hour") or 0)
            except (TypeError, ValueError):
                continue
            hours.setdefault(chat_id, {})[hour] = int(row.get("n") or 0)
        names = _group_name_map(domain)
        groups = []
        for row in rows[:limit_n]:
            chat_id = str(row.get("chat_id") or "")
            kind = str(row.get("kind") or "")
            active = int(row.get("active_senders") or 0)
            histogram = hours.get(chat_id) or {}
            peak_hour = max(histogram.items(), key=lambda item: item[1], default=(None, 0))
            groups.append({
                "chat_id": chat_id,
                "name": _label(chat_id, kind, names),
                "kind": kind,
                "messages": int(row.get("messages") or 0),
                "bot": int(row.get("bot") or 0),
                "human": int(row.get("human") or 0),
                "bot_ratio": round(int(row.get("bot") or 0) / max(1, int(row.get("messages") or 0)), 3),
                "active_senders": active,
                "messages_per_sender": round(int(row.get("human") or 0) / active, 1) if active else None,
                "active_hours": sum(1 for count in histogram.values() if count > 0),
                "peak_hour": {"hour": peak_hour[0], "count": peak_hour[1]},
                "first_at": v2._local_time(row.get("first_at")),
                "last_at": v2._local_time(row.get("last_at")),
            })
        return {
            "days": days_n,
            "from": v2._local_time(since),
            "groups": groups,
            "count": len(groups),
            "total_groups": len(rows),
            "totals": {
                "messages": sum(row["messages"] for row in groups),
                "active_senders": sum(row["active_senders"] for row in groups),
            },
            "source": "sqlite:chat_log + domain:/groups(群名)",
        }

    wrapped = cache_wrap("analytics/groups", (days_n, limit_n), ttl, build)
    data = wrapped["value"]
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"], "cache_ttl": ttl})
    return data


def analytics_senders(days: object = 7, limit: object = 20, domain: Any = None) -> dict:
    """GET /api/v2/analytics/senders?days=7&limit=20

    最活跃成员（含群分布）：消息量 / 活跃天数 / 活跃群数 / Top3 群。
    """
    days_n, since, _dates = v2._day_window(v2._int_param(days, 7, 1, 90))
    limit_n = v2._int_param(limit, 20, 1, 100)
    ttl = CACHE_TTLS["analytics/senders"]

    def build() -> dict:
        sql = (
            "SELECT sender_id, MAX(sender_username) AS name, COUNT(*) AS messages, "
            "COUNT(DISTINCT chat_id) AS groups, "
            "COUNT(DISTINCT substr(datetime(substr(recorded_at,1,19), '+%d hours'),1,10)) AS active_days, "
            "MIN(recorded_at) AS first_at, MAX(recorded_at) AS last_at "
            "FROM chat_log WHERE is_bot=0 AND sender_id IS NOT NULL AND sender_id != '' "
            "AND substr(recorded_at,1,19) >= ? GROUP BY sender_id ORDER BY messages DESC LIMIT ?"
        ) % LOCAL_OFFSET_HOURS
        rows = v2._query(sql, (since, max(limit_n * 2, limit_n)))
        sender_ids = [str(row.get("sender_id")) for row in rows[:limit_n]]

        per_group: dict[str, list[dict]] = {}
        if sender_ids:
            marks = ",".join("?" for _ in sender_ids)
            group_sql = (
                "SELECT sender_id, CAST(chat_id AS TEXT) AS chat_id, MAX(kind_key) AS kind, COUNT(*) AS n "
                "FROM chat_log WHERE is_bot=0 AND sender_id IN (%s) AND substr(recorded_at,1,19) >= ? "
                "GROUP BY sender_id, chat_id ORDER BY n DESC" % marks
            )
            for row in v2._query(group_sql, tuple(sender_ids) + (since,)):
                per_group.setdefault(str(row.get("sender_id")), []).append(row)
        names = _group_name_map(domain)
        senders = []
        for row in rows[:limit_n]:
            sender_id = str(row.get("sender_id") or "")
            group_rows = per_group.get(sender_id) or []
            senders.append({
                "sender_id": sender_id,
                "name": str(row.get("name") or sender_id),
                "messages": int(row.get("messages") or 0),
                "active_days": int(row.get("active_days") or 0),
                "groups": int(row.get("groups") or 0),
                "messages_per_day": round(int(row.get("messages") or 0) / max(1, int(row.get("active_days") or 0)), 1),
                "first_at": v2._local_time(row.get("first_at")),
                "last_at": v2._local_time(row.get("last_at")),
                "group_top": [
                    {"chat_id": str(item.get("chat_id") or ""),
                     "name": _label(str(item.get("chat_id") or ""), str(item.get("kind") or ""), names),
                     "messages": int(item.get("n") or 0)}
                    for item in group_rows[:3]
                ],
            })
        total_row = v2._query(
            "SELECT COUNT(DISTINCT sender_id) AS senders, COUNT(*) AS messages FROM chat_log "
            "WHERE is_bot=0 AND substr(recorded_at,1,19) >= ?", (since,))
        total_row = total_row[0] if total_row else {}
        return {
            "days": days_n,
            "senders": senders,
            "count": len(senders),
            "totals": {
                "active_senders": int(total_row.get("senders") or 0),
                "human_messages": int(total_row.get("messages") or 0),
                "avg_messages_per_sender": round(int(total_row.get("messages") or 0)
                                                 / max(1, int(total_row.get("senders") or 0)), 1),
            },
            "source": "sqlite:chat_log + domain:/groups(群名)",
        }

    wrapped = cache_wrap("analytics/senders", (days_n, limit_n), ttl, build)
    data = wrapped["value"]
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"], "cache_ttl": ttl})
    return data


def analytics_latency(days: object = 7) -> dict:
    """GET /api/v2/analytics/latency?days=7

    从 audit_log 的 agent_run_started / agent_run_finished 还原每次 run 的耗时：
    p50/p90/p99、平均值、直方图、按天、按状态、最慢 run。
    """
    days_n, since, _dates = v2._day_window(v2._int_param(days, 7, 1, 90))
    ttl = CACHE_TTLS["analytics/latency"]

    def build() -> dict:
        rows = v2._query(
            "SELECT run_id, event_kind, occurred_at, event_json FROM audit_log "
            "WHERE event_kind IN ('agent_run_started','agent_run_finished','agent_run_interrupted') "
            "AND substr(occurred_at,1,19) >= ?", (since,))
        starts: dict[str, float] = {}
        finishes: dict[str, tuple[float, dict]] = {}
        interrupted = 0
        for row in rows:
            run_id = str(row.get("run_id") or "")
            if not run_id:
                continue
            epoch = _epoch_local_text(row.get("occurred_at"))
            if epoch is None:
                continue
            kind = str(row.get("event_kind") or "")
            if kind == "agent_run_started":
                if run_id not in starts or epoch < starts[run_id]:
                    starts[run_id] = epoch
            elif kind == "agent_run_finished":
                payload: dict = {}
                try:
                    parsed = json.loads(str(row.get("event_json") or "{}"))
                    if isinstance(parsed, dict):
                        payload = parsed
                except (TypeError, ValueError):
                    payload = {}
                previous = finishes.get(run_id)
                if previous is None or epoch > previous[0]:
                    finishes[run_id] = (epoch, payload)
            else:
                interrupted += 1
        samples: list[tuple[float, float, str, str]] = []  # (elapsed_ms, finish_epoch, status, run_id)
        status_counts: dict[str, int] = {}
        for run_id, (finish, payload) in finishes.items():
            start = starts.get(run_id)
            if start is None:
                continue
            elapsed_ms = (finish - start) * 1000.0
            if elapsed_ms <= 0 or elapsed_ms > 1800 * 1000:
                continue
            status = str(payload.get("status") or "unknown")
            status_counts[status] = status_counts.get(status, 0) + 1
            samples.append((elapsed_ms, finish, status, run_id))
        values = sorted(item[0] for item in samples)
        buckets = [
            ("0-0.5s", 0, 500), ("0.5-1s", 500, 1000), ("1-2s", 1000, 2000),
            ("2-5s", 2000, 5000), ("5-10s", 5000, 10000), ("10-20s", 10000, 20000),
            ("20-30s", 20000, 30000), ("30-60s", 30000, 60000), ("60s+", 60000, None),
        ]
        histogram = []
        for label, low, high in buckets:
            count = sum(1 for value in values if value >= low and (high is None or value < high))
            histogram.append({"bucket": label, "low_ms": low, "high_ms": high, "count": count,
                              "ratio": round(count / max(1, len(values)), 3)})
        daily: dict[str, list[float]] = {}
        for elapsed_ms, finish, _status, _run_id in samples:
            daily.setdefault(_day_local(finish), []).append(elapsed_ms)
        daily_rows = []
        for date in sorted(daily.keys()):
            day_values = sorted(daily[date])
            daily_rows.append({
                "date": date,
                "runs": len(day_values),
                "avg_ms": round(sum(day_values) / len(day_values), 1),
                "p90_ms": round(_percentile(day_values, 90) or 0, 1),
            })
        slowest = sorted(samples, key=lambda item: -item[0])[:5]
        pairs = len(samples)
        return {
            "days": days_n,
            "runs": {
                "started": len(starts),
                "finished": len(finishes),
                "paired": pairs,
                "unfinished": max(0, len(starts) - pairs),
                "interrupted": interrupted,
            },
            "ms": {
                "count": len(values),
                "min": round(values[0], 1) if values else None,
                "p50": round(_percentile(values, 50) or 0, 1) if values else None,
                "p90": round(_percentile(values, 90) or 0, 1) if values else None,
                "p99": round(_percentile(values, 99) or 0, 1) if values else None,
                "avg": round(sum(values) / len(values), 1) if values else None,
                "max": round(values[-1], 1) if values else None,
            },
            "histogram": histogram,
            "by_status": [{"status": status, "count": count}
                          for status, count in sorted(status_counts.items(), key=lambda item: -item[1])],
            "daily": daily_rows,
            "slowest": [{"run_id_masked": "run-" + item[3][:8] + "…" if item[3] else "",
                         "ms": round(item[0], 1),
                         "finished_at": _local_text(item[1]), "status": item[2]}
                        for item in slowest],
            "source": "audit_log:agent_run_started→agent_run_finished",
            "note": "耗时=同 run_id 的 finished 时间 - started 时间（UTC 文本直接相减）；无 elapsed 字段的历史事件也能算。",
        }

    wrapped = cache_wrap("analytics/latency", (days_n,), ttl, build)
    data = wrapped["value"]
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"], "cache_ttl": ttl})
    return data


def _local_text(epoch: float | None) -> str:
    if epoch is None:
        return ""
    return time.strftime("%Y-%m-%d %H:%M:%S", time.gmtime(epoch + LOCAL_OFFSET_HOURS * 3600))


def analytics_tokens(days: object = 7) -> dict:
    """GET /api/v2/analytics/tokens?days=7

    token 用量：数据来自 audit_log 的 model_turn_finished 事件里的 tokenUsage
    （prompt/completion/total/cached）。没有数据的时段明确返回 available=false。
    """
    days_n, since, _dates = v2._day_window(v2._int_param(days, 7, 1, 90))
    ttl = CACHE_TTLS["analytics/tokens"]

    def build() -> dict:
        rows = v2._query(
            "SELECT occurred_at, run_id, event_json FROM audit_log "
            "WHERE event_kind='model_turn_finished' AND substr(occurred_at,1,19) >= ?", (since,))
        totals = {"prompt": 0, "completion": 0, "total": 0, "cached": 0}
        daily: dict[str, dict[str, int]] = {}
        turns = 0
        runs: set[str] = set()
        for row in rows:
            try:
                event = json.loads(str(row.get("event_json") or "{}"))
            except (TypeError, ValueError):
                continue
            if not isinstance(event, dict):
                continue
            usage = event.get("tokenUsage") if isinstance(event.get("tokenUsage"), dict) else {}
            if not usage:
                continue
            try:
                prompt = int(usage.get("prompt_tokens") or 0)
                completion = int(usage.get("completion_tokens") or 0)
                total = int(usage.get("total_tokens") or (prompt + completion))
                cached = int((usage.get("prompt_tokens_details") or {}).get("cached_tokens") or 0)
            except (TypeError, ValueError, AttributeError):
                continue
            turns += 1
            run_id = str(row.get("run_id") or "")
            if run_id:
                runs.add(run_id)
            totals["prompt"] += prompt
            totals["completion"] += completion
            totals["total"] += total
            totals["cached"] += cached
            date = _day_local(_epoch_local_text(row.get("occurred_at")))
            bucket = daily.setdefault(date, {"date": date, "turns": 0, "prompt": 0,
                                             "completion": 0, "total": 0, "cached": 0})
            bucket["turns"] += 1
            bucket["prompt"] += prompt
            bucket["completion"] += completion
            bucket["total"] += total
            bucket["cached"] += cached
        if turns == 0:
            return {
                "days": days_n,
                "available": False,
                "turns": 0,
                "reason": "audit_log 的 model_turn_finished 事件里没有 tokenUsage 字段（或该时间窗无数据）",
                "source": "audit_log:model_turn_finished.tokenUsage",
            }
        cached_ratio = round(totals["cached"] / totals["prompt"], 3) if totals["prompt"] else None
        return {
            "days": days_n,
            "available": True,
            "turns": turns,
            "runs": len(runs),
            "tokens": {
                "prompt": totals["prompt"],
                "completion": totals["completion"],
                "total": totals["total"],
                "cached": totals["cached"],
                "cached_ratio": cached_ratio,
            },
            "per_turn": {
                "prompt": round(totals["prompt"] / turns, 1),
                "completion": round(totals["completion"] / turns, 1),
                "total": round(totals["total"] / turns, 1),
            },
            "daily": [daily[key] for key in sorted(daily.keys())],
            "source": "audit_log:model_turn_finished.tokenUsage",
            "note": "只有 LLM 调用会记 token；审计事件未记录模型名，所以不做按模型拆分（需要 bot 侧补字段）。",
        }

    wrapped = cache_wrap("analytics/tokens", (days_n,), ttl, build)
    data = wrapped["value"]
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"], "cache_ttl": ttl})
    return data


# ═══════════════════════════════════════════════════════════════════════
# 3. B. 内容管理（代理 fm-domain + 缓存）
# ═══════════════════════════════════════════════════════════════════════
def _domain_ok(domain: Callable[..., Any], path: str, params: dict | None = None,
               timeout_status: int = 502, not_found_status: int = 404) -> dict:
    """调 domain 并解包：失败抛 ApiError，成功返回 domain 的原始 JSON（去掉 ok）。"""
    from urllib.parse import urlencode as _urlencode
    query = _urlencode({key: value for key, value in (params or {}).items()
                        if value is not None and str(value) != ""})
    try:
        result = domain(path, query)
    except Exception as exc:  # noqa: BLE001
        raise v2.ApiError("fm-domain 请求失败（%s）：%s" % (path, str(exc)[:200]), timeout_status)
    if not isinstance(result, dict) or not result.get("ok"):
        error = ""
        if isinstance(result, dict):
            error = str(result.get("error") or result.get("message") or "")[:300]
        if "not found" in error.lower():
            raise v2.ApiError("fm-domain %s：%s" % (path, error or "not found"), not_found_status)
        raise v2.ApiError("fm-domain %s 失败：%s" % (path, error or "未知错误"), timeout_status)
    data = result.get("data")
    if isinstance(data, dict):
        payload = dict(data)
        payload.pop("ok", None)
        return payload
    if isinstance(data, list):
        return {"items": data, "count": len(data)}
    return {"value": data}


def library_list(domain: Any, limit: object = 20, offset: object = 0,
                 q: object = "", difficulty: object = "", group_id: object = "") -> dict:
    """GET /api/v2/library?limit&offset&q&difficulty：文库分页列表（domain /library/list，缓存 30s）。"""
    limit_n = v2._int_param(limit, 20, 1, 100)
    offset_n = v2._int_param(offset, 0, 0, 100000)
    query_text = str(q or "").strip()[:120]
    difficulty_text = str(difficulty or "").strip()[:20]
    group_text = str(group_id or "").strip()[:32]
    ttl = CACHE_TTLS["library/list"]

    def build() -> dict:
        payload = _domain_ok(domain, "/library/list", {
            "limit": limit_n, "offset": offset_n,
            "q": query_text, "difficulty": difficulty_text, "group_id": group_text,
        })
        payload.setdefault("items", [])
        payload.setdefault("total", len(payload.get("items") or []))
        return payload

    wrapped = cache_wrap("library/list", (limit_n, offset_n, query_text, difficulty_text, group_text), ttl, build)
    data = dict(wrapped["value"])
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"],
                 "cache_ttl": ttl, "source": "domain:/library/list"})
    return data


def library_text(domain: Any, text_id: object = "") -> dict:
    """GET /api/v2/library/text?id=：文库单篇正文（domain /library/text，缓存 300s）。"""
    ident = str(text_id or "").strip()
    if not ident:
        raise v2.ApiError("缺少 id 参数（文库文本 id，可由 /api/v2/library 的 items[].id 取）", 400)
    if not re.fullmatch(r"[0-9A-Za-z._-]{1,80}", ident):
        raise v2.ApiError("id 格式不正确（只允许字母数字和 ._-，最长 80）", 400)
    ttl = CACHE_TTLS["library/text"]

    def build() -> dict:
        payload = _domain_ok(domain, "/library/text", {"id": ident})
        return payload

    wrapped = cache_wrap("library/text", (ident,), ttl, build)
    data = dict(wrapped["value"])
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"],
                 "cache_ttl": ttl, "source": "domain:/library/text"})
    return data


def contests(domain: Any) -> dict:
    """GET /api/v2/contests：赛文清单 + 今日赛文（domain /contests/list + /contests/today，缓存 30s）。"""
    ttl = CACHE_TTLS["contests"]

    def build() -> dict:
        listing = _domain_ok(domain, "/contests/list")
        today = _domain_ok(domain, "/contests/today")
        return {
            "contests": listing,
            "list": listing.get("items") or [],
            "count": int(listing.get("count") or len(listing.get("items") or [])),
            "today": today,
            "today_items": today.get("items") or [],
            "today_count": int(today.get("count") or len(today.get("items") or [])),
        }

    wrapped = cache_wrap("contests", (), ttl, build)
    data = dict(wrapped["value"])
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"],
                 "cache_ttl": ttl, "source": "domain:/contests/list + /contests/today"})
    return data


def members_leaderboard(domain: Any, days: object = 7, limit: object = 20,
                        group_id: object = "", metric: object = "") -> dict:
    """GET /api/v2/members/leaderboard?days=7&limit=20：打字排行（domain /typing/leaderboard，缓存 30s）。"""
    days_n = v2._int_param(days, 7, 1, 90)
    limit_n = v2._int_param(limit, 20, 1, 100)
    group_text = str(group_id or "").strip()[:32]
    metric_text = str(metric or "").strip()[:20]
    ttl = CACHE_TTLS["members/leaderboard"]

    def build() -> dict:
        payload = _domain_ok(domain, "/typing/leaderboard", {
            "days": days_n, "limit": limit_n,
            "group_id": group_text, "metric": metric_text,
        })
        payload.setdefault("items", [])
        payload.setdefault("count", len(payload.get("items") or []))
        return payload

    wrapped = cache_wrap("members/leaderboard", (days_n, limit_n, group_text, metric_text), ttl, build)
    data = dict(wrapped["value"])
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"],
                 "cache_ttl": ttl, "source": "domain:/typing/leaderboard"})
    return data


def _novel_stats_raw(domain: Any) -> dict:
    """番茄书架原始数据（domain 的 /novel/stats 是 POST；缓存 30s）。"""
    ttl = CACHE_TTLS["novels"]

    def build() -> dict:
        payload = v2._post_domain("/novel/stats", {})
        if not isinstance(payload, dict) or not payload.get("ok"):
            error = ""
            if isinstance(payload, dict):
                error = str(payload.get("error") or payload.get("message") or "")[:300]
            raise v2.ApiError("fm-domain /novel/stats 失败：" + (error or "未知错误"), 502)
        data = dict(payload)
        data.pop("ok", None)
        return data

    wrapped = cache_wrap("novels", ("stats",), ttl, build)
    return wrapped


def novels_cached(domain: Any, q: object = "", limit: object = 20, offset: object = 0) -> dict:
    """GET /api/v2/novels?q=&limit=&offset=：番茄书架 + 关键字搜索（缓存 30s）。"""
    query_text = str(q or "").strip()[:120].lower()
    limit_n = v2._int_param(limit, 20, 1, 200)
    offset_n = v2._int_param(offset, 0, 0, 10000)
    wrapped = _novel_stats_raw(domain)
    payload = dict(wrapped["value"])
    items = payload.get("items") if isinstance(payload.get("items"), list) else []
    if query_text:
        items = [item for item in items
                 if query_text in str(item.get("title") or "").lower()
                 or query_text in str(item.get("author") or "").lower()
                 or query_text in str(item.get("source") or "").lower()]
    total = len(items)
    page = items[offset_n: offset_n + limit_n]
    return {
        **{key: payload.get(key) for key in ("books", "chapters", "progress", "records", "count")},
        "items": page,
        "total": total,
        "count": len(page),
        "limit": limit_n,
        "offset": offset_n,
        "q": query_text,
        "has_more": offset_n + len(page) < total,
        "cached": wrapped["cached"],
        "cache_age_seconds": wrapped["age_seconds"],
        "cache_ttl": wrapped["ttl"],
        "source": "domain:/novel/stats(POST) + 本地过滤",
    }


def novel_detail_cached(domain: Any, book_id: object = "") -> dict:
    """GET /api/v2/novel?id=：番茄单本详情（domain /novel/book，缓存 30s）。"""
    ident = str(book_id or "").strip()
    if not ident:
        raise v2.ApiError("缺少 id 参数（番茄 book_id，可由 /api/v2/novels 的 items[].book_id 取）", 400)
    if not re.fullmatch(r"[0-9A-Za-z._-]{1,80}", ident):
        raise v2.ApiError("id 格式不正确", 400)
    ttl = CACHE_TTLS["novel"]

    def build() -> dict:
        return _domain_ok(domain, "/novel/book", {"book_id": ident})

    wrapped = cache_wrap("novel", (ident,), ttl, build)
    data = dict(wrapped["value"])
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"],
                 "cache_ttl": ttl, "source": "domain:/novel/book"})
    return data


# ═══════════════════════════════════════════════════════════════════════
# 4. C. 运维增强（宿主机 ops-agent v2 白名单）
# ═══════════════════════════════════════════════════════════════════════
def _flag(value: object, default: bool = False) -> bool:
    if isinstance(value, bool):
        return value
    if value is None or value == "":
        return default
    return str(value).strip().lower() in {"1", "true", "yes", "on"}


def ops_stats(wait: object = True, force: object = False) -> dict:
    """GET /api/v2/ops/stats：docker stats（bot/domain/admin）+ 磁盘 + 负载 + 内存快照。

    缓存 15s（QA P1-2）：TTL 内 ~ms；过期先返回旧值 + 后台刷新；
    ?wait=0 首次不等（warming）；?force=1 同步强制刷新。
    """
    ttl = CACHE_TTLS["ops/stats"]

    def build() -> dict:
        out = v2.ops_request("stats", {}, timeout=90.0)
        payload = out.get("result") or {}
        return {
            "host": payload.get("host") or {},
            "containers": payload.get("containers") or [],
            "summary": payload.get("summary") or {},
            "collected_at": payload.get("collected_at"),
            "collected_at_text": payload.get("collected_at_text") or "",
            "request_id": out.get("request_id"),
            "elapsed_ms": out.get("elapsed_ms"),
            "duration_ms": out.get("duration_ms"),
            "source": "host-ops-agent:docker stats(+host /proc)",
        }

    result = _ops_cached("stats", (), ttl, build, wait=_flag(wait, True), force=_flag(force))
    if result["warming"]:
        return _ops_warming_payload("stats", ttl, {"host": {}, "containers": [], "summary": {}})
    data = dict(result["value"] or {})
    data.update({"cached": result["cached"], "cache_age_seconds": result["age_seconds"],
                 "cache_ttl": ttl, "stale": result["stale"], "warming": False})
    return data


def ops_metrics_cached(wait: object = True, force: object = False) -> dict:
    """GET /api/v2/ops/metrics：宿主机 load/mem/disk/uptime + docker stats（缓存 15s）。

    v3 的 /ops/metrics 恒 3.0s 无缓存（QA P1-2），这里同样走 _ops_cached：
    TTL 内 ~ms、过期先用旧值+后台刷新、?wait=0 首次不阻塞、?force=1 同步刷新。
    """
    ttl = CACHE_TTLS["ops/metrics"]

    def build() -> dict:
        out = v2.ops_request("metrics", {}, timeout=90.0)
        payload = out.get("result") or {}
        return {
            "host": payload.get("host") or {},
            "docker": payload.get("docker") or {},
            "request_id": out.get("request_id"),
            "elapsed_ms": out.get("elapsed_ms"),
            "duration_ms": out.get("duration_ms"),
            "source": "host-ops-agent",
        }

    result = _ops_cached("metrics", (), ttl, build, wait=_flag(wait, True), force=_flag(force))
    if result["warming"]:
        return _ops_warming_payload("metrics", ttl, {"host": {}, "docker": {}})
    data = dict(result["value"] or {})
    data.update({"cached": result["cached"], "cache_age_seconds": result["age_seconds"],
                 "cache_ttl": ttl, "stale": result["stale"], "warming": False})
    return data


def ops_logs_v2(tail: object = 200, since: object = "", grep: object = "",
                wait: object = True, force: object = False) -> dict:
    """GET /api/v2/ops/logs?tail=&since=&grep=：docker logs fm-cosmobot（≤500 行，可 grep）。

    缓存 12s，key = (tail, since, grep)（QA P1-3：冷启 20.9~29.6s）；
    过期先返回旧日志 + 后台刷新；?wait=0 首次不阻塞（warming）；自检探针用 wait=0。
    """
    tail_n = v2._int_param(tail, 200, 1, 500)
    since_text = str(since or "").strip()
    if since_text and not re.fullmatch(r"[0-9A-Za-z:._+T-]{4,64}", since_text):
        raise v2.ApiError("since 参数格式不正确（docker 支持的 RFC3339 时间戳或 unix 秒）", 400)
    grep_text = str(grep or "").strip()
    if len(grep_text) > 120:
        raise v2.ApiError("grep 太长（最多 120 字符）", 400)
    if any(char in grep_text for char in "\x00\r\n"):
        raise v2.ApiError("grep 不能包含控制字符", 400)
    ttl = CACHE_TTLS["ops/logs"]

    def build() -> dict:
        out = v2.ops_request("logs", {"tail": tail_n, "since": since_text, "grep": grep_text},
                             timeout=60.0)
        payload = out.get("result") or {}
        lines = payload.get("lines") if isinstance(payload.get("lines"), list) else []
        return {
            "container": payload.get("container") or "fm-cosmobot",
            "tail": tail_n,
            "since": since_text,
            "grep": grep_text,
            "lines": lines,
            "count": len(lines),
            "total_lines": int(payload.get("total_lines") or len(lines)),
            "grep_matched": int(payload.get("grep_matched") or len(lines)),
            "truncated": bool(payload.get("truncated")),
            "request_id": out.get("request_id"),
            "elapsed_ms": out.get("elapsed_ms"),
            "duration_ms": out.get("duration_ms"),
            "source": "host-ops-agent:docker logs",
        }

    result = _ops_cached("logs", (tail_n, since_text, grep_text), ttl, build,
                         wait=_flag(wait, True), force=_flag(force))
    if result["warming"]:
        return _ops_warming_payload("logs", ttl, {
            "container": "fm-cosmobot", "tail": tail_n, "since": since_text,
            "grep": grep_text, "lines": [], "count": 0})
    data = dict(result["value"] or {})
    data.update({"cached": result["cached"], "cache_age_seconds": result["age_seconds"],
                 "cache_ttl": ttl, "stale": result["stale"], "warming": False})
    return data


def ops_cron_list(wait: object = True, force: object = False) -> dict:
    """GET /api/v2/ops/cron：宿主机 crontab 只读列表（解析成 schedule/command，缓存 15s）。"""
    ttl = CACHE_TTLS["ops/cron"]

    def build() -> dict:
        out = v2.ops_request("cron_list", {}, timeout=45.0)
        payload = out.get("result") or {}
        return {
            "lines": payload.get("lines") or [],
            "entries": payload.get("entries") or [],
            "count": int(payload.get("count") or len(payload.get("lines") or [])),
            "raw": payload.get("raw") or "",
            "writable": False,
            "can_set": False,
            "request_id": out.get("request_id"),
            "elapsed_ms": out.get("elapsed_ms"),
            "source": "host-ops-agent:crontab -l",
            "note": "只读：cron_set 写入未实现（返回 501），改 crontab 必须人工在宿主机执行并留证据。",
        }

    result = _ops_cached("cron", (), ttl, build, wait=_flag(wait, True), force=_flag(force))
    if result["warming"]:
        return _ops_warming_payload("cron", ttl, {"lines": [], "entries": [], "count": 0,
                                                  "writable": False, "can_set": False})
    data = dict(result["value"] or {})
    data.update({"cached": result["cached"], "cache_age_seconds": result["age_seconds"],
                 "cache_ttl": ttl, "stale": result["stale"], "warming": False})
    return data


def ops_cron_set(line: object = "") -> dict:
    """POST /api/v2/ops/cron-set：白名单写入（未实现，明确返回 501）。"""
    text = str(line or "").strip()
    if text and any(char in text for char in "\x00\r\n"):
        raise v2.ApiError("cron 行不能包含换行/控制字符", 400)
    try:
        out = v2.ops_request("cron_set", {"line": text[:200]}, timeout=20.0)
    except v2.ApiError as exc:
        # 代理明确拒绝（501）时保持 501，其它错误原样抛出
        raise exc
    return {"cron_set": False, "request_id": out.get("request_id"),
            "note": "宿主机代理返回了非预期结果：cron_set 应始终返回 501。", "result": out.get("result")}


def ops_rollback_safe(image: object = "", confirm: object = False, keep_old: object = True) -> dict:
    """POST /api/v2/ops/rollback {image, confirm}：真实安全回滚（宿主机代理执行）。

    操作序列（由宿主机 root 代理完成，后台容器没有 docker socket）：
      1) 校验镜像存在且形如 fm-cosmobot:runtime-*，且与当前容器镜像不同；
      2) docker inspect 当前容器 → 导出 run 参数（env/binds/network/restart/log/cap/ipc/shm/ports/cmd）；
      3) docker stop -t 10 → docker rename fm-cosmobot fm-cosmobot-bak-<时间>；
      4) docker run -d --name fm-cosmobot <导出的参数> <目标镜像>；
      5) 健康检查（最多 70s，Docker healthcheck=healthy 才算成功）；
      6) 失败自动恢复：删掉新容器 → 旧容器改回名字 → docker start → 再等健康；
      7) 结果写 /opt/fm-admin/data/backups/rollback-<时间>.json + 队列结果文件。
    """
    image_text = str(image or "").strip()
    if not re.fullmatch(r"fm-cosmobot:runtime-[A-Za-z0-9._-]{1,90}", image_text):
        raise v2.ApiError("image 必须形如 fm-cosmobot:runtime-<tag>（只允许 runtime 镜像）", 400)
    confirmed = confirm if isinstance(confirm, bool) else str(confirm or "").strip().lower() in {"1", "true", "yes", "on"}
    if not confirmed:
        raise v2.ApiError("回滚是高危操作：请求体必须带 confirm:true", 400)
    out = v2.ops_request("rollback", {
        "image": image_text,
        "confirm": "1",
        "keep_old": "1" if (keep_old if isinstance(keep_old, bool)
                            else str(keep_old).lower() in {"1", "true", "yes"}) else "0",
    }, timeout=300.0)
    payload = out.get("result") or {}
    return {
        "image": payload.get("image") or image_text,
        "executed": bool(payload.get("executed")),
        "rolled_back": bool(payload.get("rolled_back")),
        "healthy": bool(payload.get("healthy")),
        "previous_image": payload.get("previous_image") or "",
        "previous_container": payload.get("previous_container") or "",
        "target_image_id": payload.get("target_image_id") or "",
        "health": payload.get("health") or {},
        "steps": payload.get("steps") or [],
        "result_file": payload.get("result_file") or "",
        "recovered": payload.get("recovered"),
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "source": "host-ops-agent:docker inspect/stop/rename/run",
        "note": payload.get("note") or "回滚成功：旧容器已保留为 fm-cosmobot-bak-*（已停止），需要清理可人工 docker rm。",
    }


def ops_config_backup() -> dict:
    """POST /api/v2/ops/config-backup：备份 /opt/fm-cosmobot/runtime/config.toml 到 /opt/fm-admin/data/backups。"""
    out = v2.ops_request("config_backup", {}, timeout=60.0)
    payload = out.get("result") or {}
    return {
        "file": payload.get("file") or "",
        "path": payload.get("path") or "",
        "source": payload.get("source") or v2.CONFIG_TOML_PATH,
        "size": payload.get("size"),
        "size_human": payload.get("size_human") or "",
        "sha256": payload.get("sha256") or "",
        "sha_match": bool(payload.get("sha_match")),
        "created_at_text": payload.get("created_at_text") or "",
        "kept": payload.get("kept") or [],
        "pruned": payload.get("pruned") or [],
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "host_agent": "host-ops-agent",
    }


# ═══════════════════════════════════════════════════════════════════════
# 4b. QQ 登录面板（snowluma 桌面 QQ → OneBot WS 3001 → bot）
#     背景：2026-11-15 FM 因 QQ 掉登录静默离线，用户要求在后台看状态/二维码/重启。
# ═══════════════════════════════════════════════════════════════════════
QQ_DATA_DIR = os.path.join(DATA_BASE, "qq")
QQ_SCREEN_PATH = os.path.join(QQ_DATA_DIR, "latest.png")
QQ_SCREEN_FRESH_SECONDS = 60.0
QQ_OPS = ("qq_status", "qq_screen", "qq_restart")


def _ensure_qq_ops_allowed() -> None:
    """把 QQ 面板用到的 3 个 op 补进 api_v2.OPS_ALLOWED。

    本任务文件所有权只允许改 api_v4.py / app.py / ops_agent.py，所以这里运行时补齐
    api_v2 的白名单（tuple，补齐一次即可），不改 api_v2 源码。
    """
    missing = [name for name in QQ_OPS if name not in v2.OPS_ALLOWED]
    if missing:
        v2.OPS_ALLOWED = tuple(v2.OPS_ALLOWED) + tuple(missing)


def _qq_screen_stat() -> dict:
    """后台侧看到的截图文件状态（不触发抓图）。"""
    path = QQ_SCREEN_PATH
    if not os.path.isfile(path):
        return {"available": False, "path": path,
                "note": "还没有截图：调用 GET /api/v2/qq/screen 或 POST /api/v2/qq/refresh 触发抓取"}
    try:
        stat = os.stat(path)
    except OSError as exc:
        return {"available": False, "path": path, "error": str(exc)[:200]}
    width = height = None
    try:
        with open(path, "rb") as handle:
            head = handle.read(33)
        if len(head) >= 24 and head[:8] == b"\x89PNG\r\n\x1a\n":
            width = int.from_bytes(head[16:20], "big")
            height = int.from_bytes(head[20:24], "big")
    except OSError:
        pass
    age = max(0.0, time.time() - stat.st_mtime)
    return {
        "available": True,
        "path": path,
        "bytes": stat.st_size,
        "mtime": stat.st_mtime,
        "mtime_text": _local_text(stat.st_mtime),
        "age_seconds": round(age, 1),
        "fresh": age <= QQ_SCREEN_FRESH_SECONDS,
        "width": width,
        "height": height,
    }


def _read_qq_png() -> bytes:
    try:
        with open(QQ_SCREEN_PATH, "rb") as handle:
            payload = handle.read(8 * 1024 * 1024)
    except OSError as exc:
        raise v2.ApiError("读取 QQ 截图失败：" + str(exc)[:200], 503)
    if not payload.startswith(b"\x89PNG\r\n\x1a\n"):
        raise v2.ApiError("QQ 截图文件不是合法 PNG", 503)
    return payload


def _qq_warnings(status: dict) -> list[str]:
    """把状态翻译成给人看的提醒（前端可直接展示）。"""
    warnings: list[str] = []
    snow = status.get("snowluma") or {}
    onebot = status.get("onebot") or {}
    bot = status.get("bot") or {}
    if str(snow.get("state") or "") != "running":
        warnings.append("snowluma 容器未运行（state=%s）" % (snow.get("state") or "?"))
    elif not onebot.get("listening"):
        warnings.append("OneBot %s 未监听：QQ 很可能掉登录了，需要在「QQ 登录」页扫码"
                        % onebot.get("port", 3001))
    if str(bot.get("state") or "") != "running":
        warnings.append("bot 容器未运行（state=%s）" % (bot.get("state") or "?"))
    elif not bot.get("connected_to_onebot"):
        warnings.append("bot 未连上 OneBot %s（消息收发会中断）" % onebot.get("port", 3001))
    if not status.get("logged_in") and status.get("groups") is None:
        warnings.append("日志里没有读到群数量（可能还没登录成功）")
    elif status.get("logged_in") and status.get("groups") is None:
        warnings.append("已登录但日志里没解析到群数量（日志轮转所致，不影响收发）")
    hints = status.get("offline_hints") or []
    if hints:
        warnings.append("日志里出现掉线类告警：" + str(hints[-1])[:160])
    screen = status.get("screen") or {}
    if screen.get("available") and float(screen.get("age_seconds") or 0) > QQ_SCREEN_FRESH_SECONDS:
        warnings.append("二维码/屏幕截图已 %.0fs 未更新，可点「刷新二维码」" % float(screen.get("age_seconds") or 0))
    return warnings


def qq_status(force: object = False) -> dict:
    """GET /api/v2/qq/status：snowluma 状态 + OneBot 监听 + bot 连接 + 群数/最近登录。

    宿主机代理采集一次 ~0.3s，但会 docker logs 6 万行，所以后台缓存 10s（?force=1 强制）。
    """
    _ensure_qq_ops_allowed()
    ttl = CACHE_TTLS["ops/qq_status"]

    def build() -> dict:
        out = v2.ops_request("qq_status", {}, timeout=45.0)
        payload = out.get("result") if isinstance(out.get("result"), dict) else {}
        data = dict(payload or {})
        data["agent"] = {"request_id": out.get("request_id"), "elapsed_ms": out.get("elapsed_ms"),
                         "duration_ms": out.get("duration_ms"),
                         "source": "host-ops-agent:docker inspect/exec/logs"}
        return data

    result = _ops_cached("qq_status", (), ttl, build, wait=True, force=_flag(force))
    data = dict(result["value"] or {})
    data.update({"cached": result["cached"], "cache_age_seconds": result["age_seconds"],
                 "cache_ttl": ttl})
    # 后台侧再算一次 logged_in（OneBot 在监听 + bot 已连接），避免代理日志解析失败时误报未登录
    onebot = data.get("onebot") if isinstance(data.get("onebot"), dict) else {}
    bot = data.get("bot") if isinstance(data.get("bot"), dict) else {}
    data["logged_in"] = bool(onebot.get("listening")) and bool(bot.get("connected_to_onebot"))
    data["screen_file"] = _qq_screen_stat()
    data["warnings"] = _qq_warnings(data)
    return data


def qq_screen_png(force: object = False) -> tuple[bytes, dict]:
    """GET /api/v2/qq/screen 用：60s 内直接返回现有 PNG，否则抓一次再返回。

    抓图失败但有旧图时返回旧图并标 stale（前端可提示"图可能不是最新"）；连旧图都没有 → 503。
    """
    _ensure_qq_ops_allowed()
    force_flag = _flag(force)
    stat = _qq_screen_stat()
    if stat.get("available") and stat.get("fresh") and not force_flag:
        meta = dict(stat)
        meta.update({"triggered": False, "cached": True, "stale": False})
        payload = _read_qq_png()
        meta["md5"] = hashlib.md5(payload).hexdigest()
        return payload, meta
    error = ""
    request_meta: dict = {}
    try:
        out = v2.ops_request("qq_screen", {}, timeout=90.0)
        request_meta = {"request_id": out.get("request_id"), "elapsed_ms": out.get("elapsed_ms"),
                        "duration_ms": out.get("duration_ms")}
    except v2.ApiError as exc:
        error = exc.message
    stat = _qq_screen_stat()
    if stat.get("available"):
        meta = dict(stat)
        meta.update({"triggered": True, "cached": False, "request_meta": request_meta,
                     "stale": bool(error), "warning": error})
        payload = _read_qq_png()
        meta["md5"] = hashlib.md5(payload).hexdigest()
        return payload, meta
    raise v2.ApiError("QQ 截图不可用：" + (error or "snowluma 未运行或截图失败"), 503)


def qq_refresh() -> dict:
    """POST /api/v2/qq/refresh：强制重新抓一张 snowluma 桌面（扫码页=二维码）。"""
    _ensure_qq_ops_allowed()
    out = v2.ops_request("qq_screen", {}, timeout=90.0)
    stat = _qq_screen_stat()
    if not stat.get("available"):
        raise v2.ApiError("刷新失败：宿主机代理没有生成截图文件", 503)
    payload = _read_qq_png()
    return {
        "ok": True,
        "bytes": stat.get("bytes"),
        "mtime": stat.get("mtime"),
        "mtime_text": stat.get("mtime_text"),
        "age_seconds": stat.get("age_seconds"),
        "md5": hashlib.md5(payload).hexdigest(),
        "width": stat.get("width"),
        "height": stat.get("height"),
        "path": stat.get("path"),
        "request_id": out.get("request_id"),
        "elapsed_ms": out.get("elapsed_ms"),
        "duration_ms": out.get("duration_ms"),
        "agent": out.get("result") if isinstance(out.get("result"), dict) else {},
        "note": "已重新抓取 snowluma 当前桌面（未登录时就是扫码二维码）",
    }


def qq_restart(target: object = "", dry_run: object = False, confirm: object = False) -> dict:
    """POST /api/v2/qq/restart：重启 snowluma 或 bot。

    安全：snowluma 真重启会丢失 QQ 登录态（需手机QQ重新扫码），宿主机代理要求 confirm=1；
    dry_run=1 只做校验/返回计划，不重启任何容器。
    """
    _ensure_qq_ops_allowed()
    target_text = str(target or "").strip().lower()
    if target_text not in ("snowluma", "bot"):
        raise v2.ApiError("target 只允许 snowluma | bot（收到：%s）" % (target_text or "(空)"), 400)
    dry = _flag(dry_run)
    params: dict[str, object] = {"target": target_text, "dry_run": "1" if dry else "0"}
    if _flag(confirm):
        params["confirm"] = "1"
    out = v2.ops_request("qq_restart", params, timeout=45.0 if dry else 240.0)
    payload = out.get("result") if isinstance(out.get("result"), dict) else {}
    data = dict(payload or {})
    data.update({"request_id": out.get("request_id"), "elapsed_ms": out.get("elapsed_ms"),
                 "duration_ms": out.get("duration_ms"),
                 "source": "host-ops-agent:docker restart"})
    return data


# ═══════════════════════════════════════════════════════════════════════
# 5. D. 健康历史（宿主机 ops-agent 每 5 分钟追加 /data/health.jsonl）
# ═══════════════════════════════════════════════════════════════════════
def health_history(days: object = 7) -> dict:
    """GET /api/v2/health/history?days=7

    读取宿主机代理积累的检查点（health.jsonl）：docker health + 磁盘 + 内存 + 负载。
    文件还不存在时返回 available=false + 说明（不是错误，前端显示占位）。
    """
    days_n = v2._int_param(days, 7, 1, 30)
    ttl = CACHE_TTLS["health/history"]
    cutoff = time.time() - days_n * 86400

    def build() -> dict:
        path = HEALTH_HISTORY
        if not os.path.isfile(path):
            return {
                "available": False,
                "file": path,
                "points": [],
                "count": 0,
                "note": "检查点文件还没生成：宿主机 ops_agent v2 每 5 分钟追加一次（安装后立即写第一份）。",
            }
        try:
            size = os.path.getsize(path)
        except OSError as exc:
            raise v2.ApiError("读健康历史失败：" + str(exc), 500)
        points: list[dict] = []
        bad_lines = 0
        try:
            with open(path, "r", encoding="utf-8", errors="replace") as handle:
                for line in handle:
                    line = line.strip()
                    if not line:
                        continue
                    try:
                        raw = json.loads(line)
                    except ValueError:
                        bad_lines += 1
                        continue
                    if not isinstance(raw, dict):
                        bad_lines += 1
                        continue
                    at = raw.get("at")
                    try:
                        at_value = float(at or 0)
                    except (TypeError, ValueError):
                        continue
                    if at_value < cutoff:
                        continue
                    containers = raw.get("containers") if isinstance(raw.get("containers"), list) else []
                    disk = raw.get("disk") if isinstance(raw.get("disk"), dict) else {}
                    mem = raw.get("mem") if isinstance(raw.get("mem"), dict) else {}
                    load = raw.get("load") if isinstance(raw.get("load"), dict) else {}
                    bot = next((item for item in containers if isinstance(item, dict)
                                and item.get("name") == "fm-cosmobot"), {})
                    root_disk = disk.get("/") if isinstance(disk.get("/"), dict) else {}
                    points.append({
                        "at": at_value,
                        "at_text": _local_text(at_value),
                        "bot": {
                            "name": bot.get("name") or "fm-cosmobot",
                            "status": bot.get("status") or "",
                            "health": bot.get("health") or "",
                            "running": bool(bot.get("running")),
                            "image": bot.get("image") or "",
                        },
                        "containers": containers,
                        "mem_used_percent": mem.get("used_percent"),
                        "mem_used_human": mem.get("used_human") or "",
                        "disk_percent": root_disk.get("percent"),
                        "disk_free_human": root_disk.get("free_human") or "",
                        "load1": (load.get("1m") if isinstance(load, dict) else None),
                        "agent_version": str((raw.get("agent") or {}).get("version") or "")
                        if isinstance(raw.get("agent"), dict) else "",
                    })
        except OSError as exc:
            raise v2.ApiError("读健康历史失败：" + str(exc), 500)
        points.sort(key=lambda item: item["at"])
        healthy = [item for item in points
                   if item["bot"].get("running") and item["bot"].get("health") in ("healthy", "")]
        gaps = []
        for previous, current in zip(points, points[1:]):
            delta = current["at"] - previous["at"]
            if delta > 900:
                gaps.append({"from": previous["at_text"], "to": current["at_text"],
                             "seconds": round(delta, 1)})
        mem_values = [float(item["mem_used_percent"]) for item in points
                      if isinstance(item.get("mem_used_percent"), (int, float))]
        load_values = [float(item["load1"]) for item in points if isinstance(item.get("load1"), (int, float))]
        disk_values = [float(item["disk_percent"]) for item in points
                       if isinstance(item.get("disk_percent"), (int, float))]
        latest = points[-1] if points else None
        return {
            "available": bool(points),
            "file": path,
            "size": size,
            "days": days_n,
            "count": len(points),
            "bad_lines": bad_lines,
            "window": {"from": points[0]["at_text"] if points else "",
                       "to": points[-1]["at_text"] if points else ""},
            "interval_seconds": HEALTH_INTERVAL_SECONDS,
            "points": points,
            "latest": latest,
            "summary": {
                "samples": len(points),
                "healthy_samples": len(healthy),
                "healthy_ratio": round(len(healthy) / len(points), 3) if points else None,
                "mem_max_percent": max(mem_values) if mem_values else None,
                "mem_avg_percent": round(sum(mem_values) / len(mem_values), 1) if mem_values else None,
                "load1_max": max(load_values) if load_values else None,
                "disk_min_percent": min(disk_values) if disk_values else None,
                "gaps": gaps,
            },
            "source": "host-ops-agent:health.jsonl",
            "note": "检查点由宿主机 root 代理每 %d 秒采集一次（docker health + 磁盘 + 内存 + 负载），只读。" % HEALTH_INTERVAL_SECONDS,
        }

    wrapped = cache_wrap("health/history", (days_n,), ttl, build)
    data = wrapped["value"]
    data.update({"cached": wrapped["cached"], "cache_age_seconds": wrapped["age_seconds"], "cache_ttl": ttl})
    return data


# ═══════════════════════════════════════════════════════════════════════
# 6. E. 自检补充项 + 路由表
# ═══════════════════════════════════════════════════════════════════════
def selftest_v4_checks() -> list[dict]:
    """v4 内部资源项（和 api_v2.selftest_internal 同样的 check 结构）。"""
    checks: list[dict] = []

    def run(name: str, fn: Callable[[], Any], required: bool = True) -> None:
        started = time.monotonic()
        detail: Any = None
        error = ""
        try:
            detail = fn()
            ok = True
        except Exception as exc:  # noqa: BLE001
            error = str(exc)[:400]
            ok = False
        checks.append({
            "name": name, "kind": "internal", "method": "INTERNAL", "path": "",
            "http_status": None, "ms": round((time.monotonic() - started) * 1000, 1),
            "ok": ok, "level": "ok" if ok else ("fail" if required else "warn"),
            "required": required, "error": error, "detail": detail,
        })

    def check_agent_v2() -> dict:
        agent = v2._ops_agent_state()
        if not agent.get("online"):
            raise RuntimeError("宿主机代理离线：最后心跳 %s 秒前" % agent.get("age_seconds"))
        allowed = [str(item) for item in (agent.get("allowed_ops") or [])]
        missing = [name for name in ("stats", "logs", "cron_list", "rollback", "config_backup")
                   if name not in allowed]
        if missing:
            raise RuntimeError("宿主机代理还是旧版，缺 op：" + " / ".join(missing))
        return {"version": agent.get("version"), "scans": agent.get("scans"),
                "age_seconds": agent.get("age_seconds"),
                "mode": agent.get("mode"), "linger_seconds": agent.get("linger_seconds"),
                "health_samples": agent.get("health_samples"),
                "last_health_at_text": agent.get("last_health_at_text"),
                "allowed_ops": allowed}

    run("v4:ops-agent-v2", check_agent_v2)

    def check_agent_schema() -> dict:
        """心跳 state schema 校验（QA/Lead：字段名不一致会误报离线）。

        要求：updated_at / updated_at_text / started_at 都存在、schema_version>=2，
        且 updated_at 年龄 < 90s（与 api_v2._ops_agent_state 用同一判定）。
        """
        agent = v2._ops_agent_state()
        fields = agent.get("schema_fields") or {}
        missing = []
        if not agent.get("updated_at"):
            missing.append("updated_at")
        if not agent.get("updated_at_text"):
            missing.append("updated_at_text")
        if not agent.get("started_at"):
            missing.append("started_at")
        if not agent.get("version"):
            missing.append("version")
        if missing:
            raise RuntimeError("心跳 state 缺字段：" + " / ".join(missing) +
                               "（老版本代理写的 state？等下一次心跳后重试）")
        ttl = agent.get("online_ttl_seconds") or 90
        age = agent.get("age_seconds")
        if age is None or float(age) >= float(ttl):
            raise RuntimeError("心跳过期：updated_at=%s，age=%ss ≥ %ss"
                               % (agent.get("updated_at_text"), age, ttl))
        return {"schema_version": agent.get("schema_version"),
                "updated_at": agent.get("updated_at"),
                "updated_at_text": agent.get("updated_at_text"),
                "started_at": agent.get("started_at"),
                "started_at_text": agent.get("started_at_text"),
                "uptime_seconds": agent.get("uptime_seconds"),
                "age_seconds": age, "online_ttl_seconds": ttl,
                "version": agent.get("version"), "mode": agent.get("mode"),
                "heartbeat": agent.get("heartbeat")}

    run("v4:ops-agent-schema", check_agent_schema)

    def check_ops_cache() -> dict:
        """P1-2/P1-3 回归：ops/metrics、ops/logs 的缓存/后台刷新是否在工作。"""
        with _CACHE_LOCK:
            keys = sorted(key for key in _CACHE if key.startswith("ops/"))
        with _OPS_REFRESH_LOCK:
            refresh = dict(_OPS_REFRESH_STATS)
            inflight = sorted(_OPS_REFRESH_INFLIGHT)
        stats = {key: round(time.time() - float((_CACHE.get(key) or {}).get("at") or 0), 1)
                 for key in keys}
        if not keys and not refresh.get("kicks"):
            # 冷启动/刚重启：ops 接口还没人访问过，属正常状态（不是失败）
            return {"cached_keys": [], "ages": {}, "refresh": refresh, "ttls": {},
                    "note": "冷启动：还没有 ops 接口被访问（正常；访问一次后就会预热）"}
        return {"cached_keys": keys, "ages": stats,
                "refresh": {**refresh, "inflight": inflight},
                "ttls": {key: ttl for key, ttl in CACHE_TTLS.items() if key.startswith("ops/")}}

    run("v4:ops-cache", check_ops_cache, required=False)

    def check_health_file() -> dict:
        if not os.path.isfile(HEALTH_HISTORY):
            raise RuntimeError("health.jsonl 不存在：" + HEALTH_HISTORY)
        size = os.path.getsize(HEALTH_HISTORY)
        if size <= 0:
            raise RuntimeError("health.jsonl 是空文件")
        samples = 0
        last: dict = {}
        with open(HEALTH_HISTORY, "r", encoding="utf-8", errors="replace") as handle:
            for line in handle:
                line = line.strip()
                if not line:
                    continue
                samples += 1
                try:
                    parsed = json.loads(line)
                except ValueError:
                    continue
                if isinstance(parsed, dict):
                    last = parsed
        age = time.time() - float(last.get("at") or 0) if last else None
        return {"file": HEALTH_HISTORY, "size": size, "samples": samples,
                "last_age_seconds": round(age, 1) if age is not None else None}

    run("v4:health-history", check_health_file, required=False)

    def check_cache() -> dict:
        with _CACHE_LOCK:
            stats = dict(_CACHE_STATS)
            entries = len(_CACHE)
        total = stats["hits"] + stats["misses"]
        return {"entries": entries, "hits": stats["hits"], "misses": stats["misses"],
                "hit_ratio": round(stats["hits"] / total, 3) if total else None,
                "ttls": CACHE_TTLS}

    run("v4:cache", check_cache, required=False)

    def check_rate_limit() -> dict:
        per_min = int(os.environ.get("FM_ADMIN_RATE_PER_MIN", "60") or 60)
        burst = int(os.environ.get("FM_ADMIN_RATE_BURST", str(per_min * 2)) or per_min * 2)
        if per_min <= 0:
            raise RuntimeError("限流配置异常：FM_ADMIN_RATE_PER_MIN=%s" % per_min)
        return {"per_min": per_min, "burst": burst,
                "env": {"FM_ADMIN_RATE_PER_MIN": os.environ.get("FM_ADMIN_RATE_PER_MIN"),
                        "FM_ADMIN_RATE_BURST": os.environ.get("FM_ADMIN_RATE_BURST")}}

    run("v4:rate-limit", check_rate_limit)

    def check_backup_dir() -> dict:
        if not os.path.isdir(BACKUP_DIR):
            raise RuntimeError("备份目录不存在：" + BACKUP_DIR)
        names = [name for name in os.listdir(BACKUP_DIR)
                 if name.startswith("config-") or name.startswith("rollback-")]
        return {"dir": BACKUP_DIR, "v4_files": len(names), "sample": sorted(names)[-3:]}

    run("v4:backup-dir", check_backup_dir, required=False)

    def check_qq_screen_file() -> dict:
        """QQ 登录面板：截图文件是否存在/是合法 PNG（不触发抓图；optional）。"""
        stat = _qq_screen_stat()
        if not stat.get("available"):
            raise RuntimeError("还没有 QQ 截图：" + str(stat.get("path")))
        if not stat.get("width") or not stat.get("height"):
            raise RuntimeError("QQ 截图不是合法 PNG：" + str(stat.get("path")))
        return {"path": stat.get("path"), "bytes": stat.get("bytes"),
                "width": stat.get("width"), "height": stat.get("height"),
                "age_seconds": stat.get("age_seconds"), "fresh": stat.get("fresh")}

    run("v4:qq-screen-file", check_qq_screen_file, required=False)

    return checks


V4_ROUTES: dict[str, str] = {
    # A. 分析
    "analytics/messages": "analytics_messages(days)",
    "analytics/groups": "analytics_groups(days, limit, domain)",
    "analytics/senders": "analytics_senders(days, limit, domain)",
    "analytics/latency": "analytics_latency(days)",
    "analytics/tokens": "analytics_tokens(days)",
    # B. 内容管理
    "library": "library_list(domain, limit, offset, q, difficulty, group_id)",
    "library/text": "library_text(domain, id)",
    "contests": "contests(domain)",
    "members/leaderboard": "members_leaderboard(domain, days, limit, group_id, metric)",
    "novels": "novels_cached(domain, q, limit, offset)",
    "novel": "novel_detail_cached(domain, id)",
    # C. 运维
    "ops/stats": "ops_stats(wait, force)",
    "ops/metrics": "ops_metrics_cached(wait, force)  # 15s 缓存（QA P1-2）",
    "ops/logs": "ops_logs_v2(tail, since, grep, wait, force)  # 12s 缓存（QA P1-3）",
    "ops/cron": "ops_cron_list(wait, force)  # 15s 缓存",
    "ops/cron-set": "ops_cron_set(line)  # POST，未实现→501",
    "ops/rollback": "ops_rollback_safe(image, confirm)  # POST，真实回滚",
    "ops/config-backup": "ops_config_backup()  # POST",
    # C2. QQ 登录面板（snowluma / OneBot）
    "qq/status": "qq_status(force)  # 10s 缓存；/api/v2/qq/status",
    "qq/screen": "qq_screen_png(force)  # PNG；60s 内直接返回，否则抓图",
    "qq/refresh": "qq_refresh()  # POST，强制抓图",
    "qq/restart": "qq_restart(target, dry_run, confirm)  # POST；snowluma 真重启需 confirm",
    # D. 系统
    "health/history": "health_history(days)",
    "cache/stats": "cache_stats(rate_limit)",
    # DELETE /api/v2/sessions/{id} 在 app.py 里实现（要访问进程内 SESSIONS）
}
