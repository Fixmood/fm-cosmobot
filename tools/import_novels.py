#!/usr/bin/env python3
"""把下载好的小说 TXT 导入 FM 文库。

文库表结构（app.py 里定的）：
    library_texts(text_id, category, relative_path, title, content, char_count)

导入策略：
  · 一个**章节** = 一篇文库记录（不按段落切）—— 「上一篇/这篇接着发」按记录走，
    而发送时已经由 segment_length 控制每次发多少字，所以章节粒度刚好。
  · text_id 用 md5(书名+章节名) 前 24 位，和现有数据同格式，且重跑幂等。
  · category 统一 fm_texts（fm_single_chars 是单字练习表，不要混）。
  · relative_path 必须唯一 → 用 fanqie:<书名>/<章节名>。
导入后要跑：
    python3 /app/app.py rank-library --db /data/fm-domain.sqlite3      # 难度
    python3 /app/app.py classify-library --db /data/fm-domain.sqlite3  # 题材
"""
from __future__ import annotations

import argparse
import hashlib
import re
import sqlite3
import sys
import time
from pathlib import Path

DB = "/data/fm-domain.sqlite3"
CHAPTER_HEAD = re.compile(r"^(.*第\s*[0-9一二三四五六七八九十百千万]+\s*[章节回卷].*)$", re.M)


def log(message: str) -> None:
    print(f"[{time.strftime('%H:%M:%S')}] {message}", flush=True)


def split_chapters(text: str) -> list[tuple[str, str]]:
    """按「第N章」切分。切不出来就整篇当一章。"""
    lines = text.splitlines()
    chapters: list[tuple[str, list[str]]] = []
    current_title = ""
    current_lines = [""]
    for line in lines:
        stripped = line.strip()
        if CHAPTER_HEAD.match(stripped) and len(stripped) <= 60:
            if current_lines and current_title:
                chapters.append((current_title, current_lines))
            current_title = stripped
            current_lines = [""]
        else:
            current_lines.append(line)
    if current_title:
        chapters.append((current_title, current_lines))
    if not chapters:
        return [("全文", text)]
    return [(title, "\n".join(lines).strip()) for title, lines in chapters]


def clean_content(content: str) -> str:
    content = content.replace("\u3000", " ")
    content = re.sub(r"[ \t]+\n", "\n", content)
    content = re.sub(r"\n{3,}", "\n\n", content)
    return content.strip()


def text_id_for(book: str, chapter: str) -> str:
    return hashlib.md5(f"{book}|{chapter}".encode("utf-8")).hexdigest()[:24]


def import_file(db: sqlite3.Connection, path: Path, book: str | None = None) -> int:
    raw = path.read_text(encoding="utf-8", errors="ignore")
    book_name = book or path.stem
    chapters = split_chapters(raw)
    inserted = 0
    for chapter_title, content in chapters:
        body = clean_content(content)
        if len(body) < 120:
            continue
        title = f"{book_name}·{chapter_title}" if chapter_title != "全文" else book_name
        text_id = text_id_for(book_name, chapter_title)
        try:
            db.execute(
                "INSERT INTO library_texts (text_id,category,relative_path,title,content,char_count) "
                "VALUES (?,?,?,?,?,?) ON CONFLICT(text_id) DO UPDATE SET "
                "title=excluded.title,content=excluded.content,char_count=excluded.char_count",
                (text_id, "fm_texts", f"fanqie:{book_name}/{chapter_title}", title, body, len(body)),
            )
            inserted += 1
        except sqlite3.Error as error:
            log(f"  ✗ {title}: {error}")
    db.commit()
    log(f"  {book_name}: {inserted} 章入库（共切出 {len(chapters)} 段）")
    log(f"  chapters={inserted}")
    return inserted


def main() -> int:
    parser = argparse.ArgumentParser(description="把小说 TXT 导入 FM 文库")
    parser.add_argument("paths", nargs="+", help="TXT 文件或目录")
    parser.add_argument("--db", default=DB)
    args = parser.parse_args()

    files: list[Path] = []
    for item in args.paths:
        path = Path(item)
        if path.is_dir():
            files.extend(sorted(path.glob("*.txt")))
        elif path.is_file():
            files.append(path)
    if not files:
        log("没有找到 txt 文件")
        return 1

    db = sqlite3.connect(args.db, timeout=30)
    db.row_factory = sqlite3.Row
    total = 0
    for path in files:
        total += import_file(db, path)
    remaining = db.execute("SELECT count(*) FROM library_texts WHERE category='fm_texts'").fetchone()[0]
    log(f"✓ 本次导入 {total} 章；文库现有 {remaining} 篇")
    db.close()
    log("下一步：")
    log("  python3 /app/app.py rank-library --db /data/fm-domain.sqlite3")
    log("  python3 /app/app.py classify-library --db /data/fm-domain.sqlite3")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        raise SystemExit(130)
