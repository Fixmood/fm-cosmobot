#!/usr/bin/env python3
"""Dry-run or apply deterministic cleanup to FM's indexed typing library."""

import argparse
import json
import sqlite3
import time
from collections import Counter
from pathlib import Path

from app import clean_library_content, library_content_quality, normalize_library_content


def backup_database(database: Path, backup_dir: Path) -> Path:
    backup_dir.mkdir(parents=True, exist_ok=True)
    target = backup_dir / f"{database.stem}-before-library-clean-{time.strftime('%Y%m%d-%H%M%S')}.sqlite3"
    source_db = sqlite3.connect(database)
    target_db = sqlite3.connect(target)
    try:
        source_db.backup(target_db)
    finally:
        target_db.close()
        source_db.close()
    return target


def audit(database: Path, apply_changes: bool = False, example_limit: int = 20, backup_dir: Path | None = None) -> dict:
    backup = None
    if apply_changes:
        backup = backup_database(database, backup_dir or database.parent / "backups")

    db = sqlite3.connect(database)
    db.row_factory = sqlite3.Row
    rows = db.execute(
        "SELECT text_id,category,relative_path,title,content,char_count FROM library_texts "
        "WHERE category<>'fm_single_chars' ORDER BY category,relative_path"
    ).fetchall()
    result = {
        "mode": "apply" if apply_changes else "dry-run",
        "database": str(database), "backup": str(backup) if backup else None,
        "scanned": len(rows), "unchanged": 0, "cleaned": 0, "deleted": 0,
        "clean_reasons": Counter(), "delete_reasons": Counter(),
        "cleaned_examples": [], "deleted_examples": [],
    }
    changed_rows = []
    deleted_ids = []
    try:
        for row in rows:
            cleaned, reasons = clean_library_content(row["title"], row["content"])
            accepted, quality_reason = library_content_quality(row["title"], cleaned, row["category"])
            if not accepted:
                result["deleted"] += 1
                result["delete_reasons"][quality_reason] += 1
                deleted_ids.append(row["text_id"])
                if len(result["deleted_examples"]) < example_limit:
                    result["deleted_examples"].append({
                        "text_id": row["text_id"], "category": row["category"],
                        "title": row["title"], "reason": quality_reason,
                        "before": row["content"][:300], "after": cleaned[:300],
                    })
                continue
            compact_count = len(normalize_library_content(cleaned))
            if cleaned != row["content"].strip() or compact_count != int(row["char_count"]):
                result["cleaned"] += 1
                result["clean_reasons"].update(reasons or ["char_count"])
                changed_rows.append((cleaned, compact_count, row["text_id"]))
                if len(result["cleaned_examples"]) < example_limit:
                    result["cleaned_examples"].append({
                        "text_id": row["text_id"], "category": row["category"],
                        "title": row["title"], "reasons": reasons,
                        "before": row["content"][:300], "after": cleaned[:300],
                    })
            else:
                result["unchanged"] += 1

        if apply_changes:
            db.execute("BEGIN IMMEDIATE")
            db.executemany("UPDATE library_texts SET content=?,char_count=? WHERE text_id=?", changed_rows)
            affected_ids = [row[2] for row in changed_rows] + deleted_ids
            db.executemany("DELETE FROM library_rankings WHERE text_id=?", ((value,) for value in affected_ids))
            db.executemany("DELETE FROM library_sessions WHERE text_id=?", ((value,) for value in deleted_ids))
            db.executemany("DELETE FROM library_texts WHERE text_id=?", ((value,) for value in deleted_ids))
            db.commit()
    except Exception:
        db.rollback()
        raise
    finally:
        db.close()

    result["clean_reasons"] = dict(result["clean_reasons"].most_common())
    result["delete_reasons"] = dict(result["delete_reasons"].most_common())
    return result


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--database", required=True, type=Path)
    parser.add_argument("--apply", action="store_true")
    parser.add_argument("--backup-dir", type=Path)
    parser.add_argument("--examples", type=int, default=20)
    parser.add_argument("--report", type=Path)
    args = parser.parse_args()
    result = audit(args.database, args.apply, args.examples, args.backup_dir)
    payload = json.dumps(result, ensure_ascii=False, indent=2)
    if args.report:
        args.report.write_text(payload + "\n", encoding="utf-8")
    print(payload)


if __name__ == "__main__":
    main()
