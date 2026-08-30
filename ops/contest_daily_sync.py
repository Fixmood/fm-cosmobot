#!/usr/bin/env python3
"""Archive the current public typing-contest texts into FM's contest library."""

import hashlib
import json
import sqlite3
import sys
import argparse
from datetime import date, datetime, timezone, timedelta
from pathlib import Path

sys.path.insert(0, "/app")
from app import (  # noqa: E402
    PUBLIC_COMPETITION_GROUPS,
    get_public_competition,
)

DB = Path("/data/fm-domain.sqlite3")
CHINA = timezone(timedelta(hours=8))


def ensure_metadata_table(db: sqlite3.Connection) -> None:
    db.execute(
        "CREATE TABLE IF NOT EXISTS contest_text_metadata ("
        "text_id TEXT PRIMARY KEY, source_type TEXT NOT NULL, archive_status TEXT NOT NULL, "
        "source_url TEXT NOT NULL, fetched_at TEXT NOT NULL)"
    )
    db.execute(
        "INSERT OR IGNORE INTO contest_text_metadata "
        "SELECT text_id, '历史赛文', '已归档', '', '' FROM contest_texts"
    )


def clean_content(value: str) -> str:
    lines = [line.strip() for line in str(value or "").splitlines()]
    while lines and (not lines[-1] or lines[-1].startswith("-----")):
        lines.pop()
    return "\n".join(lines).strip()


def archive(db: sqlite3.Connection, result: dict) -> bool:
    content = clean_content(result.get("content"))
    if not content:
        return False
    source = str(result.get("source") or "").strip() or "未知赛文"
    date = str(result.get("date") or "").strip()
    title = str(result.get("title") or "").strip() or f"{source} {date}"
    digest = hashlib.sha256(f"{source}\n{date}\n{content}".encode("utf-8")).hexdigest()
    text_id = f"auto-{digest[:24]}"
    relative = f"auto/{source}/{date}-{digest[:12]}.txt"
    db.execute(
        "INSERT INTO contest_texts VALUES (?, ?, ?, ?, ?, ?, ?) "
        "ON CONFLICT(text_id) DO UPDATE SET title=excluded.title,content=excluded.content,char_count=excluded.char_count",
        (text_id, title, source, date, relative, content, len(content)),
    )
    source_type = "极速杯" if source == "极速杯" else "锦标赛" if source == "锦标赛" else "群赛文"
    source_url = "https://www.jsxiaoshi.com/competition_rank.html" if source_type == "极速杯" else "https://www.jsxiaoshi.com/championships_rank.html" if source_type == "锦标赛" else "https://www.dazi.club/"
    db.execute(
        "INSERT INTO contest_text_metadata VALUES (?, ?, ?, ?, ?) "
        "ON CONFLICT(text_id) DO UPDATE SET archive_status=excluded.archive_status,source_url=excluded.source_url,fetched_at=excluded.fetched_at",
        (text_id, source_type, "已归档", source_url, datetime.now(CHINA).isoformat()),
    )
    return True


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--start-date", default="")
    parser.add_argument("--end-date", default="")
    parser.add_argument("--fill-gaps", action="store_true")
    parser.add_argument("--sources", default="", help="Comma-separated source names to sync")
    args = parser.parse_args()
    if args.fill_gaps and not args.start_date:
        earliest = db_min_date()
        start = date.fromisoformat(earliest) if earliest else datetime.now(CHINA).date()
    else:
        start = date.fromisoformat(args.start_date) if args.start_date else datetime.now(CHINA).date()
    end = date.fromisoformat(args.end_date) if args.end_date else datetime.now(CHINA).date()
    if end < start:
        raise SystemExit("end date must not precede start date")
    db = sqlite3.connect(DB, timeout=30)
    ensure_metadata_table(db)
    saved, skipped, errors = [], [], []
    available_sources = list(PUBLIC_COMPETITION_GROUPS.values()) + ["极速杯", "锦标赛"]
    requested_sources = [value.strip() for value in args.sources.split(",") if value.strip()]
    unknown_sources = [value for value in requested_sources if value not in available_sources]
    if unknown_sources:
        raise SystemExit(f"unsupported sources: {', '.join(unknown_sources)}")
    sources = requested_sources or available_sources
    existing = {
        (source, day)
        for source, day in db.execute("SELECT source_group,competition_date FROM contest_texts")
    }
    current = start
    while current <= end:
        day = current.isoformat()
        for source in sources:
            if args.fill_gaps and (source, day) in existing:
                skipped.append(f"{day} {source} (已有)")
                continue
            try:
                result = get_public_competition(source, "", day, refresh=True)
                if archive(db, result):
                    saved.append(f"{day} {source}")
                    existing.add((source, day))
                else:
                    skipped.append(f"{day} {source}")
            except Exception as exc:
                errors.append({"date": day, "source": source, "error": str(exc)[:200]})
            finally:
                # Never hold a write transaction while waiting on the next network request.
                db.commit()
        current += timedelta(days=1)
    print(json.dumps({"start_date": start.isoformat(), "end_date": end.isoformat(), "saved": saved, "skipped": skipped, "errors": errors}, ensure_ascii=False))
    return 0 if saved or not errors else 1


def db_min_date() -> str:
    db = sqlite3.connect(DB)
    row = db.execute("SELECT MIN(competition_date) FROM contest_texts WHERE competition_date GLOB '20??-??-??'").fetchone()
    db.close()
    return str(row[0] or "")


if __name__ == "__main__":
    raise SystemExit(main())
