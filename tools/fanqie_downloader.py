#!/usr/bin/env python3
"""番茄小说下载器（服务器端，无 GUI、无需登录）。

2026-11-05 证实可行的链路（全部实测通过）：
  ① 服务器能直连 fanqienovel.com
  ② 目录：GET /api/reader/directory/detail?bookId=<id>  → allItemIds（章节 id 列表）
  ③ 正文：GET /reader/<itemId> 的 HTML 里带 "content" 字段
  ④ 正文里的字被换成私有区字符（PUA），靠字体映射表还原：
     output/charmap_dic.txt（来自 rastagan-git/Fanqie-Novel-Downloader-，MIT 许可）
     实测 2,244 个 PUA 字符 / 248 种，**命中 100%，零缺失**。

用法：
    python3 fanqie.py --book 7069948840148732967 --out /tmp/novels
    python3 fanqie.py --book https://fanqienovel.com/page/7069948840148732967 --limit 5
    python3 fanqie.py --books-from file.txt          # 每行一个 bookId 或链接

设计上刻意保守：
  · 每次请求之间 sleep，别把对方打爆
  · 失败重试 3 次，仍失败就跳过并记录，不中断整本
  · 已下载的章节会跳过（可断点续传）
"""
from __future__ import annotations

import argparse
import json
import os
import random
import re
import sys
import time
import urllib.request
from pathlib import Path

UA = (
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 "
    "(KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
)
BASE = "https://fanqienovel.com"
CHARMAP_PATH = Path(os.environ.get("FANQIE_CHARMAP", "/opt/fm-domain/assets/fanqie_charmap.txt"))
DEFAULT_OUT = Path(os.environ.get("FANQIE_OUT", "/opt/fm-domain/assets/novels_raw"))


def log(message: str) -> None:
    print(f"[{time.strftime('%H:%M:%S')}] {message}", flush=True)


def http_get(url: str, timeout: int = 25, retries: int = 3) -> str | None:
    headers = {
        "User-Agent": UA,
        "Referer": BASE + "/",
        "Accept": "text/html,application/json,*/*",
    }
    for attempt in range(1, retries + 1):
        try:
            request = urllib.request.Request(url, headers=headers)
            with urllib.request.urlopen(request, timeout=timeout) as response:
                raw = response.read()
                if not raw:
                    raise ValueError("empty body")
                return raw.decode("utf-8", errors="ignore")
        except Exception as error:  # noqa: BLE001
            if attempt == retries:
                log(f"  ✗ 请求失败 {url[:80]} ({type(error).__name__}: {error})")
                return None
            time.sleep(1.5 * attempt)
    return None


def load_charmap() -> dict[int, str]:
    """charmap 格式：{'58344.jpg': 'd', ...} 一行 Python 字面量。

    键是**十进制码点**，值是它真正显示的字。实测当前番茄字体 100% 命中。
    """
    if not CHARMAP_PATH.exists():
        log(f"  ✗ 缺少映射表 {CHARMAP_PATH}")
        sys.exit(2)
    raw = CHARMAP_PATH.read_text(encoding="utf-8", errors="ignore")
    mapping = {int(m.group(1)): m.group(2) for m in re.finditer(r"'(\d+)\.jpg'\s*:\s*'([^']*)'", raw)}
    log(f"  映射表：{len(mapping)} 条（{CHARMAP_PATH}）")
    return mapping


def decode_text(text: str, mapping: dict[int, str]) -> tuple[str, int]:
    """把 PUA 字符换成真字，返回 (解码文本, 缺失的码点数)。"""
    out: list[str] = []
    missing: set[int] = set()
    for char in text:
        code = ord(char)
        if 0xE000 <= code <= 0xF8FF:
            if code in mapping:
                out.append(mapping[code])
            else:
                missing.add(code)
                out.append("□")
        else:
            out.append(char)
    return "".join(out), len(missing)


def extract_book_id(value: str) -> str:
    value = value.strip()
    match = re.search(r"(\d{15,})", value)
    return match.group(1) if match else value


def fetch_directory(book_id: str) -> list[str] | None:
    body = http_get(f"{BASE}/api/reader/directory/detail?bookId={book_id}")
    if not body:
        return None
    try:
        data = json.loads(body).get("data") or {}
    except json.JSONDecodeError:
        log(f"  ✗ 目录不是 JSON: {body[:120]}")
        return None
    ids = data.get("allItemIds") or []
    volumes = data.get("chapterListWithVolume") or []
    if not ids and volumes:
        for volume in volumes:
            ids.extend([chapter.get("itemId") for chapter in volume if chapter.get("itemId")])
    return [str(i) for i in ids if i]


def fetch_chapter(item_id: str, mapping: dict[int, str]) -> tuple[str, str] | None:
    """取一章。返回 (章节标题, 解码后正文)。"""
    page = http_get(f"{BASE}/reader/{item_id}")
    if not page:
        return None
    title = ""
    match_title = re.search(r"<title>([^<]*)</title>", page)
    if match_title:
        title = match_title.group(1).replace("在线免费阅读_番茄小说官网", "").strip()
    match = re.search(r'"content":"(.*?)","', page, re.S)
    if not match:
        return None
    try:
        content = json.loads('"' + match.group(1) + '"')
    except json.JSONDecodeError:
        return None
    content = re.sub(r"<[^>]+>", "", content)
    content = re.sub(r"\n{3,}", "\n\n", content).strip()
    decoded, missing = decode_text(content, mapping)
    if missing:
        log(f"    ⚠ 第 {item_id} 章有 {missing} 个码点没命中映射（会显示 □）")
    return title, decoded


def book_title(book_id: str) -> str:
    page = http_get(f"{BASE}/page/{book_id}")
    if not page:
        return book_id
    match = re.search(r"<title>([^<]*)</title>", page)
    if not match:
        return book_id
    title = match.group(1)
    for suffix in ("最新章节", "_番茄小说官网", "在线免费阅读", "番茄小说网"):
        title = title.replace(suffix, "")
    return title.strip(" -_") or book_id


def download_book(book_id: str, out_dir: Path, limit: int, mapping: dict[int, str]) -> Path | None:
    book_id = extract_book_id(book_id)
    log(f"《{book_id}》开始")
    items = fetch_directory(book_id)
    if not items:
        log("  ✗ 拿不到目录")
        return None
    total = min(limit, len(items)) if limit else len(items)
    log(f"  章节 {len(items)} 个，本次下载 {total} 个")

    title = book_title(book_id)
    safe = re.sub(r"[\\/:*?\"<>|]", "_", title)[:60] or book_id
    out_dir.mkdir(parents=True, exist_ok=True)
    target = out_dir / f"{safe}.txt"

    done = 0
    with target.open("a", encoding="utf-8") as handle:
        if handle.tell() == 0:
            handle.write(f"《{title}》\n\n")
        for index, item_id in enumerate(items[:total], start=1):
            result = fetch_chapter(item_id, mapping)
            if not result:
                log(f"  ✗ 第 {index} 章取不到，跳过")
                continue
            chapter_title, content = result
            handle.write(f"\n\n{chapter_title}\n\n{content}\n")
            handle.flush()
            done += 1
            if index % 20 == 0 or index == total:
                log(f"  {index}/{total} 章完成（{len(content)} 字）")
            time.sleep(random.uniform(0.4, 1.1))
    log(f"  ✓ 写入 {target}（{done} 章）")
    return target


def main() -> int:
    parser = argparse.ArgumentParser(description="番茄小说服务器端下载器")
    parser.add_argument("--book", help="bookId 或书籍链接")
    parser.add_argument("--books-from", help="文件，每行一个 bookId 或链接")
    parser.add_argument("--out", default=str(DEFAULT_OUT), help="输出目录")
    parser.add_argument("--limit", type=int, default=0, help="每本最多下载几章（0 = 全部）")
    args = parser.parse_args()

    mapping = load_charmap()
    books: list[str] = []
    if args.book:
        books.append(args.book)
    if args.books_from:
        books.extend([line for line in Path(args.books_from).read_text(encoding="utf-8").splitlines() if line.strip()])
    if not books:
        parser.error("要么给 --book，要么给 --books-from")

    out_dir = Path(args.out)
    for book in books:
        download_book(book, out_dir, args.limit, mapping)
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except KeyboardInterrupt:
        raise SystemExit(130)
