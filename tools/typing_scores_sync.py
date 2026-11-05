#!/usr/bin/env python3
"""把 chat_log 里的打字成绩解析进 domain 的 typing_scores 表。

为什么宿主机跑：成绩消息全在 bot 的 chat_log 里（SQLite），要写的是 domain 的库
（另一个 SQLite）—— 两个都在宿主机路径下，一个 cron 脚本就能两边都读。

支持「增量」：记住上次处理到 chat_log 的哪一行 id，下次只处理更新的。
也可以 --backfill 全量重扫（首次入库用）。

成绩消息实测格式（字段顺序会变，所以用**关键词锚定**而不是死板的正则整串匹配）：
    第777段 速度172.32 击键6.36 码长2.22 键准94.41% 字数241 时间01:23.913
    打词60.58% 重打1 普1.62 选重3 [词提开] 【fix】 虎魄v1.5.4
"""
from __future__ import annotations

import argparse
import json
import re
import sqlite3
import sys
import time

BOT_DB = "/opt/fm-cosmobot/runtime/cosmobot.sqlite3"
DOMAIN_DB = "/opt/fm-domain/data/fm-domain.sqlite3"

SCORE_HINT = re.compile(r"速度\s*[\d.]+")
SEGMENT = re.compile(r"第\s*([0-9]+|groupmatch)\s*段")


def number(pattern: str, text: str) -> float | None:
    match = re.search(pattern, text)
    if not match:
        return None
    try:
        return float(match.group(1))
    except (TypeError, ValueError):
        return None


def duration_ms(text: str) -> int | None:
    """用时格式：01:23.913（mm:ss.mmm）或 00:03.85。"""
    match = re.search(r"时间\s*(\d+):(\d+)\.(\d+)", text)
    if not match:
        return None
    minutes, seconds, fraction = match.groups()
    fraction = (fraction + "000")[:3]
    return int(minutes) * 60000 + int(seconds) * 1000 + int(fraction)


def parse_score(text: str) -> dict | None:
    if not SCORE_HINT.search(text):
        return None
    segment = SEGMENT.search(text)
    if not segment:
        return None
    meta: dict[str, str] = {}
    for key in ("慢字", "慢字词", "凭证", "首打认证", "词提开"):
        if key in text:
            meta[key] = "1"
    device_match = re.search(
        r"(虎魄|大帅比打器|大帅比|极速|晴窗|五笔|仓颉|拼音|小鹤|音形)\s*v?([\d]+\.?[\d]*)", text)
    difficulty_match = re.search(r"(淼|水|易|普|难|虐)\s*([\d.]+)", text)
    if not text:
        return None
    score = {
        "segment_label": segment.group(0).replace(" ", ""),
        "speed": number(r"速度\s*([\d.]+)", text),
        "key_rate": number(r"击键\s*([\d.]+)", text),
        "code_length": number(r"码长\s*([\d.]+)", text),
        "accuracy": number(r"键准\s*([\d.]+)", text),
        "char_count": number(r"字数\s*(\d+)", text),
        "duration_ms": duration_ms(text),
        "word_rate": number(r"打词\s*([\d.]+)", text),
        "backspace": number(r"回改\s*(\d+)", text),
        "delete_count": number(r"退格\s*(\d+)", text),
        "retype": number(r"重打\s*(\d+)", text),
        "choice_count": number(r"选重\s*(\d+)", text),
        "difficulty": (difficulty_match.group(1) + difficulty_match.group(2)) if difficulty_match else "",
        "device": (device_match.group(1) + "v" + (device_match.group(2) or "")) if device_match else "unknown",
        "meta": json.dumps(meta, ensure_ascii=False),
        "raw_text": text,
    }
    if score["speed"] is None:
        return None
    # 明显的坏数据直接丢：2026-11-06 回填时出现过 speed=5.98e+25 的解析事故，
    # 让一个人的均速统计变得毫无意义。宁可少一条，也不要污染分析结果。
    def sane(value, low, high):
        return value is None or low <= value <= high
    if not sane(score["speed"], 0, 1200):
        return None
    if not sane(score["key_rate"], 0, 60):
        return None
    if not sane(score["accuracy"], 0, 100):
        return None
    if not sane(score["char_count"], 1, 200000):
        return None
    if not sane(score["duration_ms"], 100, 3600000):
        return None
    if not sane(score["word_rate"], 0, 100):
        return None
    return score


def to_epoch(value) -> float:
    """"chat_log.recorded_at 存的是 '2026-08-22 11:18:15.785783726+0000' 这种字符串。"""
    if value is None:
        return 0.0
    if isinstance(value, (int, float)):
        return float(value)
    text = str(value).strip()
    try:
        from datetime import datetime, timezone
        # 9 位纳秒 -> 6 位微秒
        cleaned = re.sub(r"\.(\d{6})\d+", r".\1", text)
        # +0000 -> +00:00
        if re.search(r"[+-]\d{4}$", cleaned):
            cleaned = cleaned[:-2] + ":" + cleaned[-2:]
        parsed = datetime.fromisoformat(cleaned)
        if parsed.tzinfo is None:
            parsed = parsed.replace(tzinfo=timezone.utc)
        return parsed.timestamp()
    except Exception:
        return 0.0

def state_path() -> str:
    return "/opt/fm-cosmobot/typing-scores-state.json"


def load_state() -> int:
    try:
        with open(state_path(), encoding="utf-8") as handle:
            return int(json.load(handle).get("last_row_id") or 0)
    except (OSError, ValueError, TypeError):
        return 0


def save_state(row_id: int) -> None:
    with open(state_path(), "w", encoding="utf-8") as handle:
        json.dump({"last_row_id": row_id, "updated_at": time.time()}, handle)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--backfill", action="store_true", help="从头全量扫一遍")
    parser.add_argument("--limit", type=int, default=0, help="本次最多处理多少条（0=不限）")
    args = parser.parse_args()

    bot = sqlite3.connect(f"file:{BOT_DB}?mode=ro", uri=True, timeout=30)
    domain = sqlite3.connect(DOMAIN_DB, timeout=60)
    domain.execute("PRAGMA journal_mode=WAL")

    since = 0 if args.backfill else load_state()
    rows = bot.execute(
        "SELECT rowid, chat_id, sender_id, sender_username, body_text, recorded_at "
        "FROM chat_log WHERE rowid > ? AND body_text LIKE '%速度%' AND body_text LIKE '%键准%' "
        "ORDER BY rowid" + (f" LIMIT {args.limit}" if args.limit else ""),
        (since,),
    ).fetchall()

    inserted = 0
    last_row = since
    for row_id, chat_id, sender_id, sender_name, body, recorded in rows:
        last_row = row_id
        parsed = parse_score(body or "")
        if not parsed:
            continue
        # 去重：同一群人同一时间同一条原文只存一次
        exists = domain.execute(
            "SELECT 1 FROM typing_scores WHERE chat_id=? AND user_id=? AND recorded_at=? AND raw_text=? LIMIT 1",
            (str(chat_id), str(sender_id), to_epoch(recorded), body),
        ).fetchone()
        if exists:
            continue
        domain.execute(
            "INSERT INTO typing_scores (platform,chat_id,user_id,user_name,segment_label,"
            "speed,key_rate,code_length,accuracy,char_count,duration_ms,word_rate,"
            "backspace,delete_count,retype,choice_count,difficulty,device,meta,raw_text,recorded_at) "
            "VALUES ('qq',?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)",
            (str(chat_id), str(sender_id), str(sender_name or ""), parsed["segment_label"],
             parsed["speed"], parsed["key_rate"], parsed["code_length"], parsed["accuracy"],
             int(parsed["char_count"] or 0), parsed["duration_ms"], parsed["word_rate"],
             int(parsed["backspace"] or 0), int(parsed["delete_count"] or 0),
             int(parsed["retype"] or 0), int(parsed["choice_count"] or 0),
             parsed["difficulty"], parsed["device"], parsed["meta"], parsed["raw_text"],
             to_epoch(recorded)),
        )
        inserted += 1
    # 补名字：很多 chat_log 行的 sender_username 是空的，
    # 但同一个 sender_id 在别的行里有名字 —— 用有名字的那条回填。
    domain.execute(
        "UPDATE typing_scores SET user_name=("
        "  SELECT t2.user_name FROM typing_scores t2 "
        "  WHERE t2.user_id=typing_scores.user_id AND t2.user_name<>'' LIMIT 1) "
        "WHERE user_name='' AND EXISTS ("
        "  SELECT 1 FROM typing_scores t3 WHERE t3.user_id=typing_scores.user_id AND t3.user_name<>'')"
    )
    domain.commit()
    save_state(last_row)
    total = domain.execute("SELECT count(*) FROM typing_scores").fetchone()[0]
    print(f"[{time.strftime('%H:%M:%S')}] 本次新增 {inserted} 条，表里共 {total} 条（扫描到 rowid {last_row}）")
    bot.close()
    domain.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
