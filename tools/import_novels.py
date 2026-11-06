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


def read_text_any(path: Path) -> str:
    """按顺序尝试几种编码读文件。

    2026-11-06：所有者传的 28 本里有两本（照金枝 / 国医圣手）正文是 GBK，
    用 UTF-8 + errors="ignore" 读会得到一堆空字符，章节一行都切不出来（导进去 0 章）。
    """
    raw = path.read_bytes()
    # 有 BOM 的直接按 BOM 走
    if raw.startswith(b"\xef\xbb\xbf"):
        return raw.decode("utf-8-sig", errors="ignore")
    if raw.startswith(b"\xff\xfe") or raw.startswith(b"\xfe\xff"):
        return raw.decode("utf-16", errors="ignore")
    for encoding in ("utf-8", "gb18030", "gbk", "big5"):
        try:
            text = raw.decode(encoding)
            # 粗判：解码结果里汉字占比太低说明猜错编码了
            if text and sum(1 for ch in text[:5000] if "\u4e00" <= ch <= "\u9fff") >= 20:
                return text
        except (UnicodeDecodeError, LookupError):
            continue
    return raw.decode("utf-8", errors="ignore")


def parse_book_header(path: Path, raw: str) -> dict:
    """从番茄导出的 TXT 头部读书名/作者/书籍ID/章节数。读不到就退回文件名。

    头部只在开头 60 行内，不要全文扫（253,703 行的书扫全文太慢）。
    """
    info = {"title": path.stem, "author": "", "book_id": "", "total_chapters": 0}
    head = "\n".join(raw.splitlines()[:60])
    for line in head.splitlines():
        text = line.strip()
        for key, field in (("书名：", "title"), ("作者：", "author"), ("书籍 ID：", "book_id")):
            if text.startswith(key):
                value = text[len(key):].strip()
                if value:
                    info[field] = value
        if text.startswith("章节数："):
            digits = "".join(ch for ch in text if ch.isdigit())
            if digits:
                info["total_chapters"] = int(digits)
    # 文件名形如「书名 - 作者」，没有头部信息时用它兜底
    if info["author"] == "" and " - " in path.stem:
        parts = path.stem.split(" - ", 1)
        info["title"] = parts[0].strip()
        info["author"] = parts[1].strip()
    if not info["book_id"]:
        info["book_id"] = path.stem
    return info


def clean_content(content: str) -> str:
    content = content.replace("\u3000", " ")
    content = re.sub(r"[ \t]+\n", "\n", content)
    content = re.sub(r"\n{3,}", "\n\n", content)
    return content.strip()


def text_id_for(book: str, chapter: str) -> str:
    return hashlib.md5(f"{book}|{chapter}".encode("utf-8")).hexdigest()[:24]


def import_file(db: sqlite3.Connection, path: Path, book: str | None = None) -> int:
    raw = read_text_any(path)
    info = parse_book_header(path, raw)
    book_name = book or info["title"]
    book_id = info["book_id"]
    book_author = info["author"]
    chapters = split_chapters(raw)
    # 下载器是「追加写」：重跑同一本书会在文件里留下重复的章节段。
    # 这里按章节标题去重，保留第一次出现的（2026-11-05 实测：重跑后章号从 24 开始）。
    # 重复的章节标题要**保留正文最长的那条**，不能简单留第一条。
    # 2026-11-06 实测：《照金枝》《国医圣手》两本书每个「第N章」都出现两次，
    # 正文在第二次后面 —— 留第一条会得到空正文，整本书 0 章入库。
    best: dict[str, str] = {}
    for chapter_title, content in chapters:
        if len(content) > len(best.get(chapter_title, "")):
            best[chapter_title] = content
    chapters = list(best.items())
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
                (text_id, "fanqie", f"fanqie:{book_name}/{chapter_title}", title, body, len(body)),
            )
            inserted += 1
        except sqlite3.Error as error:
            log(f"  ✗ {title}: {error}")
    # 顺便登记书和章节映射（番茄专区靠这两张表按书/按章取文）
    db.execute(
        "INSERT INTO novel_books (book_id,title,author,total_chapters,word_count,source,imported_at) "
        "VALUES (?,?,?,?,?,'fanqie',?) ON CONFLICT(book_id) DO UPDATE SET "
        "title=excluded.title,total_chapters=excluded.total_chapters,"
        "word_count=excluded.word_count,imported_at=excluded.imported_at",
        (book_id, book_name, book_author, inserted,
         sum(len(clean_content(c)) for _, c in chapters), time.time()),
    )
    for index, (chapter_title, content) in enumerate(chapters, start=1):
        body = clean_content(content)
        if len(body) < 120:
            continue
        text_id = text_id_for(book_name, chapter_title)
        db.execute(
            "INSERT INTO novel_chapters (text_id,book_id,chapter_index,title,word_count) "
            "VALUES (?,?,?,?,?) ON CONFLICT(text_id) DO UPDATE SET "
            "book_id=excluded.book_id,chapter_index=excluded.chapter_index,"
            "title=excluded.title,word_count=excluded.word_count",
            (text_id, book_id, index, f"{book_name}·{chapter_title}", len(body)),
        )
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
    db.execute("UPDATE library_texts SET category='fanqie' WHERE relative_path LIKE 'fanqie:%' AND category<>'fanqie'")
    db.commit()
    remaining = db.execute("SELECT count(*) FROM library_texts WHERE category IN ('fm_texts','fanqie')").fetchone()[0]
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
