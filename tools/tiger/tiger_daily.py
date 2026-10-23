#!/usr/bin/env python3
"""虎杯（虎码杯）成绩采集 —— 直接走 race.tiger-code.com 的官方接口。

背景（2026-10-24）：
    competition_scores 里虎杯数据停在 2026-08-27（15 个比赛日、172 条），此后全空。
    原来是从旧系统 astrbot_plugin_outputpro 一次性快照导入的（快照停在 08-22），
    而那个系统已经不在了，机器人侧也没有任何写入路径 —— 所以 8-27 之后没人采集。

数据源：
    GET https://race.tiger-code.com/api/leaderboard/date/<YYYY-MM-DD>
    {"success":true,"data":{"leaderboard":[
        {"rank":1,"username":"玖柒","speed":182.23,"hit_rate":6.95,"kpw":2.29,
         "accuracy":92.66,"word_ratio":0.6101,"correction_count":29,
         "total_keys":1479,"input_method":"虎码","tier":"🏆大师·2"}, ...]}}

字段映射（写进 competition_scores，保持和旧数据同样的形状，这样现有图表/榜单不用改）：
    record_id        "tiger:<date>:<username>"   ← 稳定、可重复运行
    competition_date <date>
    user_id          ""        （接口没有 QQ 号；下游查询按 user_name 也能匹配）
    user_name        username
    group_id         "21037015"（沿用旧数据的群号，避免按群过滤的地方失配）
    source_group     "虎杯"
    speed            speed
    occurred_at      <date 当天 0 点的 unix 时间>
    source_json      完整的接口记录 + 派生的 key/acc/raw，供现有渲染代码读取

用法（在 fm-domain 容器里跑，库在 /data）：
    python3 tiger_daily.py --date 2026-10-24
    python3 tiger_daily.py --from 2026-08-28 --to 2026-10-24
    python3 tiger_daily.py              # 默认补最近 3 天
"""
from __future__ import annotations

import argparse
import json
import sqlite3
import sys
import time
import urllib.error
import urllib.request
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

API = "https://race.tiger-code.com/api/leaderboard/date/{day}"
DB_DEFAULT = "/data/fm-domain.sqlite3"
GROUP_ID = "21037015"
SOURCE_GROUP = "虎杯"
CHINA = timezone(timedelta(hours=8))


def fetch_day(day: str, retries: int = 3) -> list[dict] | None:
    url = API.format(day=day)
    for attempt in range(retries):
        try:
            request = urllib.request.Request(
                url, headers={"User-Agent": "fm-cosmobot/0.1 (+tiger-daily)"}
            )
            with urllib.request.urlopen(request, timeout=30) as response:
                payload = json.load(response)
        except Exception as error:  # noqa: BLE001 - 网络问题都归到这里
            if attempt + 1 == retries:
                print(f"  {day} 获取失败: {type(error).__name__}: {error}", file=sys.stderr)
                return None
            time.sleep(2 * (attempt + 1))
            continue
        if not isinstance(payload, dict) or not payload.get("success"):
            print(f"  {day} 接口返回异常: {str(payload)[:120]}", file=sys.stderr)
            return None
        data = payload.get("data") or {}
        board = data.get("leaderboard")
        return board if isinstance(board, list) else []
    return None


def normalize_name(value: str) -> str:
    """去掉「地区」/『地区』/[…] 前缀和空白，用于跨系统比对人名。

    群里叫「云南」Fixmood，跟打器上叫 Fixmood —— 不去前缀永远对不上。
    """
    text = str(value or "").strip()
    for left, right in (("「", "」"), ("『", "』"), ("[", "]"), ("【", "】")):
        if text.startswith(left):
            end = text.find(right)
            if end > 0:
                text = text[end + 1:].strip()
    return text.replace(" ", "").replace("\u3000", "").casefold()


def load_name_to_qq(db: sqlite3.Connection) -> dict[str, str]:
    """用户名 -> QQ 号。两个来源：旧虎杯记录、机器人的聊天日志。"""
    mapping: dict[str, str] = {}

    def remember(name: str, user_id: str) -> None:
        key = normalize_name(name)
        value = str(user_id or "").strip()
        if key and value and key not in mapping:
            mapping[key] = value

    # 来源一：库里已有的虎杯记录（user_id 是 QQ 号）
    for name, user_id in db.execute(
        "SELECT DISTINCT user_name, user_id FROM competition_scores "
        "WHERE user_id IS NOT NULL AND user_id != ''"
    ):
        remember(name, user_id)

    # 来源二：机器人的聊天日志（昵称 -> 发送者 QQ 号）
    bot_db = Path("/botdata/cosmobot.sqlite3")
    if bot_db.exists():
        try:
            bot = sqlite3.connect(f"file:{bot_db}?mode=ro", uri=True)
            try:
                for name, sender in bot.execute(
                    "SELECT DISTINCT sender_username, sender_id FROM chat_log "
                    "WHERE sender_id IS NOT NULL AND sender_id != '' "
                    "AND sender_username IS NOT NULL AND sender_username != ''"
                ):
                    remember(name, sender)
            finally:
                bot.close()
        except Exception as error:  # noqa: BLE001
            print(f"  读取聊天日志做映射失败（忽略）: {error}", file=sys.stderr)
    return mapping


def occurred_at_for(day: str) -> float:
    parsed = datetime.strptime(day, "%Y-%m-%d").replace(tzinfo=CHINA)
    return parsed.timestamp()


def build_record(day: str, entry: dict, name_to_qq: dict[str, str]) -> tuple | None:
    username = str(entry.get("username") or "").strip()
    if not username:
        return None
    speed = float(entry.get("speed") or 0)
    if speed <= 0:
        # 榜上偶尔有 0.01 这种没真正跑完的，和旧逻辑一致：速度<=0 不入库。
        # 0.01 大于 0 所以仍会进；这里只挡掉真正的 0。
        return None
    source_json = {
        # 下游渲染读的是这几个键（和旧数据一致）
        "speed": speed,
        "key": float(entry.get("hit_rate") or 0),
        "acc": float(entry.get("accuracy") or 0),
        # 新增可用的指标
        "code_length": float(entry.get("kpw") or 0),
        "word_rate": float(entry.get("word_ratio") or 0),
        "correction": int(entry.get("correction_count") or 0),
        "total_keys": int(entry.get("total_keys") or 0),
        "input_method": str(entry.get("input_method") or ""),
        "tier": str(entry.get("tier") or ""),
        "vip_level": int(entry.get("vip_level") or 0),
        "rank": int(entry.get("rank") or 0),
        "competition_date": day,
        "source_group": SOURCE_GROUP,
        # 保留原始接口记录，将来要补字段不用重新抓
        "tiger_raw": entry,
    }
    return (
        f"tiger:{day}:{username}",
        day,
        name_to_qq.get(normalize_name(username), ""),  # 映射回 QQ 号；映射不到就留空
        username,
        GROUP_ID,
        SOURCE_GROUP,
        speed,
        occurred_at_for(day),
        json.dumps(source_json, ensure_ascii=False),
    )


def import_day(db: sqlite3.Connection, day: str, name_to_qq: dict[str, str], limit: int = 0) -> tuple[int, int]:
    board = fetch_day(day)
    if board is None:
        return (0, -1)
    if limit:
        board = board[:limit]
    written = 0
    for entry in board:
        if not isinstance(entry, dict):
            continue
        record = build_record(day, entry, name_to_qq)
        if record is None:
            continue
        db.execute(
            "INSERT INTO competition_scores VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?) "
            "ON CONFLICT(record_id) DO UPDATE SET "
            "speed=excluded.speed, source_json=excluded.source_json, "
            "occurred_at=excluded.occurred_at, user_name=excluded.user_name, "
            "user_id=excluded.user_id",
            record,
        )
        written += 1
    db.commit()
    return (written, len(board))


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default=DB_DEFAULT)
    parser.add_argument("--date", default="")
    parser.add_argument("--from", dest="date_from", default="")
    parser.add_argument("--to", dest="date_to", default="")
    parser.add_argument("--limit", type=int, default=0)
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    today = datetime.now(CHINA).date()
    if args.date:
        days = [args.date]
    elif args.date_from:
        start = datetime.strptime(args.date_from, "%Y-%m-%d").date()
        end = (
            datetime.strptime(args.date_to, "%Y-%m-%d").date()
            if args.date_to
            else today
        )
        if end < start:
            print("--to 早于 --from", file=sys.stderr)
            return 2
        days = [(start + timedelta(days=offset)).isoformat()
                for offset in range((end - start).days + 1)]
    else:
        days = [(today - timedelta(days=offset)).isoformat() for offset in (0, 1, 2)]

    db = sqlite3.connect(args.db, timeout=30)
    db.row_factory = sqlite3.Row
    name_to_qq = load_name_to_qq(db)
    print(f"  用户名映射: {len(name_to_qq)} 条")
    total_written = 0
    failed = 0
    try:
        for index, day in enumerate(days, 1):
            written, reported = import_day(db, day, name_to_qq, args.limit)
            if reported < 0:
                failed += 1
                print(f"  [{index}/{len(days)}] {day}: 获取失败")
            else:
                total_written += written
                print(f"  [{index}/{len(days)}] {day}: 写入 {written} 条（接口返回 {reported} 条）")
            if len(days) > 3:
                time.sleep(0.6)  # 回填时对站点温柔一点
        latest = db.execute(
            "SELECT max(competition_date), count(*) FROM competition_scores"
        ).fetchone()
        print(f"  完成：本次写入 {total_written} 条，失败 {failed} 天；"
              f"库内最新 {latest[0]}，共 {latest[1]} 条")
    finally:
        db.close()
    return 1 if failed else 0


if __name__ == "__main__":
    raise SystemExit(main())
