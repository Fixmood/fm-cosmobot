#!/usr/bin/env python3
"""Export contest_texts正文 to server-side UTF-8 TXT files and verify the export."""

import argparse
import hashlib
import json
import sqlite3
from pathlib import Path


def digest(value: str) -> str:
    return hashlib.sha256(value.encode("utf-8")).hexdigest()


def safe_path(root: Path, relative: str) -> Path:
    path = Path(relative)
    if path.is_absolute() or ".." in path.parts:
        raise ValueError(f"unsafe relative_path: {relative!r}")
    return root / path


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", default="/data/fm-domain.sqlite3")
    parser.add_argument("--root", default="/data/contest-library")
    parser.add_argument("--verify-only", action="store_true")
    args = parser.parse_args()
    root = Path(args.root)
    db = sqlite3.connect(args.db)
    db.row_factory = sqlite3.Row
    rows = db.execute("SELECT text_id,relative_path,content,char_count FROM contest_texts ORDER BY text_id").fetchall()
    exported = missing = mismatched = 0
    manifest = []
    for row in rows:
        content = str(row["content"] or "")
        path = safe_path(root, row["relative_path"])
        if not args.verify_only:
            path.parent.mkdir(parents=True, exist_ok=True)
            tmp = path.with_suffix(path.suffix + ".tmp")
            tmp.write_text(content.rstrip() + "\n", encoding="utf-8")
            tmp.replace(path)
            exported += 1
        try:
            file_content = path.read_text(encoding="utf-8").rstrip()
        except (OSError, UnicodeError):
            missing += 1
            continue
        if file_content != content.rstrip() or len(file_content) != int(row["char_count"]):
            mismatched += 1
            continue
        manifest.append({"text_id": row["text_id"], "relative_path": row["relative_path"], "char_count": len(file_content), "sha256": digest(file_content)})
    if not args.verify_only and not missing and not mismatched:
        (root / "manifest.json").write_text(json.dumps({"version": 1, "count": len(manifest), "files": manifest}, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps({"database_rows": len(rows), "exported": exported, "verified": len(manifest), "missing": missing, "mismatched": mismatched, "root": str(root)}, ensure_ascii=False))
    return 0 if not missing and not mismatched else 1


if __name__ == "__main__":
    raise SystemExit(main())
