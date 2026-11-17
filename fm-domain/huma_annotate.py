#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""虎码文本标注图渲染（2026-11-17）。

输入一段文字，按虎码码表做「词组优先最长匹配」编码，输出一张分析图：
  · 标题统计（字数 / 码长 / 选重 / 缺字 + 码长分布）
  · 颜色图例：一码=绿、二简=蓝、三简=紫、四码=橙、缺字=红
  · QWERTY 键盘热力图（每个键的击键次数与占比）
  · 正文逐字带码（字在上、码在下，按码长着色；选重加下划线）

码表来源：/botdata/huma/huma_index.tsv（fm-domain 容器把 bot 的 runtime 只读挂到 /botdata），
格式：`编码<TAB>候选1 候选2 ...`，编码为 a-z，长度 1-4。

设计要点：
  · 反向索引（字词 -> 候选码列表，按码长排序）进程级缓存，码表 mtime 变化自动重载；
  · 编码是确定性的：同一段文字永远得到同一张图、同一组统计；
  · 空白字符不进入正文渲染、不计字数，但空格键次数单独统计；
  · 码表里没有的字符记为「缺字」，渲染成红色，不计键数。
"""

from __future__ import annotations

import collections
import os
import re
import threading
import time
from typing import Dict, List, Optional, Tuple

from PIL import Image, ImageDraw, ImageFont

# ── 码表 ────────────────────────────────────────────────────────────────────

TABLE_CANDIDATES = [
    "/botdata/huma/huma_index.tsv",
    "/data/huma_index.tsv",
]

MAX_TEXT_CHARS = 5000
MAX_RENDER_CHARS = 1200

_index_lock = threading.Lock()
_index_cache: Dict[str, object] = {"signature": None, "reverse": None}


def _table_path() -> Optional[str]:
    for path in TABLE_CANDIDATES:
        if os.path.exists(path):
            return path
    return None


def _signature(path: str) -> Tuple[float, int]:
    stat = os.stat(path)
    return (stat.st_mtime, stat.st_size)


def load_reverse_index() -> Dict[str, List[Tuple[str, int]]]:
    """text -> [(码, 该码里这个字词是第几候选)]，排序：首选候选优先，再按码长。

    选「该字词是首选候选」的码，才不用按 ; 去选重 —— 这是用户软件里实际的打法
    （例如「还是」不是 1 键的 c，而是 4 键的 cuot，因为 c 的首选是「不」）。"""
    path = _table_path()
    if path is None:
        raise RuntimeError("huma 码表不存在（/botdata/huma/huma_index.tsv）")
    signature = _signature(path)
    with _index_lock:
        if _index_cache.get("signature") == signature and _index_cache.get("reverse"):
            return _index_cache["reverse"]  # type: ignore[return-value]
    reverse: Dict[str, List[Tuple[str, int]]] = collections.defaultdict(list)
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if "\t" not in line:
                continue
            code, _, candidates = line.partition("\t")
            code = code.strip()
            if not code:
                continue
            for position, candidate in enumerate(c.strip() for c in candidates.split(" ")):
                if candidate:
                    reverse[candidate].append((code, position))
    for key, codes in reverse.items():
        codes.sort(key=lambda item: (item[1] != 0, len(item[0]), item[0]))
    result = dict(reverse)
    with _index_lock:
        _index_cache["signature"] = signature
        _index_cache["reverse"] = result
    return result


# ── 编码 ────────────────────────────────────────────────────────────────────


PUNCTUATION = set("，。、！？；：（）“”‘’《》〈〉【】—…·‘’〝〞,.;:!?()[]{}<>\"'`~@#$%^&*+-=_/\\|")


class Entry:
    __slots__ = ("text", "code", "code_len", "selected", "missing", "punctuation")

    def __init__(self, text: str, code: Optional[str], selected: bool):
        self.text = text
        self.code = code
        self.code_len = len(code) if code else 0
        self.selected = selected
        self.missing = code is None and text not in PUNCTUATION
        self.punctuation = code is None and text in PUNCTUATION




def clean_article(text: str) -> str:
    """从引用内容里取出真正的正文（2026-11-17 所有者要求）。

    群里引用一条赛文消息时，原文形如：
        [FM/AI赛文·虐]《标题》[字数259] 天未晞，盐场尽头的风车……
        ……任海风把新木的辛香吹进渠网深处。----- 第5 5段 2026-11-17-FM赛文
    去掉开头 `[...]《...》[字数N]` 与结尾 `----- 第N段 …FM赛文` 之类的元信息，
    只留下真正要标注的文章正文。
    """
    cleaned = text.replace("\r\n", "\n").replace("\r", "\n")
    match = re.search(r"字数\s*\d+\s*[\]】]", cleaned)
    if match:
        cleaned = cleaned[match.end():]
    else:
        match = re.search(r"[\]】]\s*《[^》]*》", cleaned)
        if match:
            cleaned = cleaned[match.end():]
    kept = []
    for line in cleaned.split("\n"):
        stripped = line.strip()
        if re.match(r"^-{3,}", stripped):
            break
        if "FM赛文" in stripped:
            break
        if "赛文" in stripped and "字数" in stripped:
            break
        if re.match(r"^第\s*\d+\s*段", stripped) or re.match(r"^\d+\s*段\s*20\d\d", stripped):
            break
        kept.append(line)
    body = "\n".join(kept).strip()
    return body or text.strip()


def encode(text: str) -> Tuple[List[Entry], Dict[str, object]]:
    text = clean_article(text)
    reverse = load_reverse_index()
    entries: List[Entry] = []
    index = 0
    length = len(text)
    while index < length:
        char = text[index]
        if char.isspace():
            index += 1
            continue
        matched: Optional[Entry] = None
        for size in (4, 3, 2, 1):
            if index + size > length:
                continue
            piece = text[index:index + size]
            codes = reverse.get(piece)
            if not codes:
                continue
            code, position = codes[0]
            matched = Entry(piece, code, position != 0)
            break
        if matched is None:
            entries.append(Entry(char, None, False))
            index += 1
        else:
            entries.append(matched)
            index += len(matched.text)

    chars = len(entries)
    key_count = sum(entry.code_len for entry in entries if not entry.missing)
    selected = sum(1 for entry in entries if entry.selected)
    missing = sum(1 for entry in entries if entry.missing)
    length_dist = collections.Counter(
        entry.code_len for entry in entries if not entry.missing and not entry.punctuation
    )
    key_counts = collections.Counter()
    for entry in entries:
        if entry.code:
            for ch in entry.code:
                key_counts[ch] += 1
    spaces = sum(1 for ch in text if ch.isspace())
    stats = {
        "chars": chars,
        "keys": key_count,
        "code_len": (key_count / chars) if chars else 0.0,
        "selected": selected,
        "missing": missing,
        "length_dist": dict(sorted(length_dist.items())),
        "key_counts": dict(key_counts),
        "spaces": spaces,
    }
    return entries, stats


_candidates_lock = threading.Lock()
_candidates_cache: Dict[str, object] = {"signature": None, "map": None}


def _candidates_by_code() -> Dict[str, List[str]]:
    path = _table_path()
    if path is None:
        return {}
    signature = _signature(path)
    with _candidates_lock:
        if _candidates_cache.get("signature") == signature and _candidates_cache.get("map"):
            return _candidates_cache["map"]  # type: ignore[return-value]
    mapping: Dict[str, List[str]] = {}
    with open(path, "r", encoding="utf-8", errors="replace") as handle:
        for line in handle:
            if "\t" not in line:
                continue
            code, _, candidates = line.partition("\t")
            code = code.strip()
            if not code:
                continue
            mapping[code] = [c.strip() for c in candidates.split(" ") if c.strip()]
    with _candidates_lock:
        _candidates_cache["signature"] = signature
        _candidates_cache["map"] = mapping
    return mapping


# ── 渲染 ────────────────────────────────────────────────────────────────────

BG = (13, 20, 31)
PANEL = (20, 30, 45)
TEXT = (232, 238, 246)
DIM = (130, 145, 165)
ACCENT = (255, 190, 90)

COLOR_BY_LEN = {
    1: (84, 189, 112),   # 一码 绿
    2: (86, 140, 240),   # 二简 蓝
    3: (168, 104, 226),  # 三简 紫
    4: (226, 132, 58),   # 四码 橙
}
COLOR_MISSING = (226, 74, 74)  # 缺字 红
COLOR_SELECTED = (255, 214, 102)

KEY_ROWS = [
    list("qwertyuiop"),
    list("asdfghjkl;"),
    list("zxcvbnm,./"),
]

WIDTH = 1400
MARGIN = 48

FONT_DIR = "/assets/msyh.ttc"


def _font(size: int) -> ImageFont.FreeTypeFont:
    for path in ("/assets/msyh.ttc", "/msyh.ttc", FONT_DIR):
        if os.path.exists(path):
            try:
                return ImageFont.truetype(path, size)
            except Exception:
                continue
    return ImageFont.load_default()


def _text_size(draw: ImageDraw.ImageDraw, text: str, font) -> Tuple[int, int]:
    if not text:
        return (0, 0)
    box = draw.textbbox((0, 0), text, font=font)
    return (box[2] - box[0], box[3] - box[1])


def render_png(text: str) -> bytes:
    text = text.strip()
    if not text:
        raise ValueError("text 不能为空")
    if len(text) > MAX_TEXT_CHARS:
        text = text[:MAX_TEXT_CHARS]

    entries, stats = encode(text)
    truncated = len(entries) > MAX_RENDER_CHARS
    if truncated:
        entries = entries[:MAX_RENDER_CHARS]

    probe = Image.new("RGB", (WIDTH, 10))
    probe_draw = ImageDraw.Draw(probe)
    char_font = _font(30)
    code_font = _font(17)
    char_h = 34
    code_h = 18
    pair_h = char_h + code_h + 16
    pair_gap = 12

    lines: List[List[Entry]] = []
    line: List[Entry] = []
    line_width = 0
    usable = WIDTH - MARGIN * 2
    for entry in entries:
        cw, _ = _text_size(probe_draw, entry.text, char_font)
        kw, _ = _text_size(probe_draw, entry.code or "??", code_font)
        block = max(cw, kw) + pair_gap
        if line and line_width + block > usable:
            lines.append(line)
            line = []
            line_width = 0
        line.append(entry)
        line_width += block
    if line:
        lines.append(line)

    key_size = 92
    key_gap = 16
    row_gap = 14
    keyboard_h = 10 + len(KEY_ROWS) * (key_size + row_gap) + 10 + 46 + 16
    header_h = 210
    legend_h = 54
    body_h = len(lines) * pair_h + 24 + (34 if truncated else 0)
    height = header_h + legend_h + keyboard_h + body_h + MARGIN
    image = Image.new("RGB", (WIDTH, height), BG)
    draw = ImageDraw.Draw(image)

    title_font = _font(40)
    stats_font = _font(24)
    small_font = _font(20)
    tiny_font = _font(16)

    # 标题与统计
    draw.text((MARGIN, MARGIN - 8), "【虎码】", font=title_font, fill=ACCENT)
    stat_line = (
        f"字数 {stats['chars']}    码长 {stats['code_len']:.2f}    "
        f"选重 {stats['selected']}    缺字 {stats['missing']}"
        f"    空格 {stats['spaces']}    总键数 {stats['keys']}"
    )
    draw.text((MARGIN, MARGIN + 52), stat_line, font=stats_font, fill=TEXT)
    dist = stats["length_dist"]
    dist_line = "  ".join(
        f"{label} {dist.get(length, 0)}"
        for length, label in ((1, "一码"), (2, "二码"), (3, "三码"), (4, "四码"))
    )
    more = sum(count for length, count in dist.items() if length > 4)
    dist_line += f"  >4码 {more}"
    dist_w, _ = _text_size(draw, dist_line, small_font)
    draw.text((WIDTH - MARGIN - dist_w, MARGIN + 8), dist_line, font=small_font, fill=DIM)

    # 图例
    legend_y = header_h - 18
    lx = MARGIN
    for label, color in (
        ("一码", COLOR_BY_LEN[1]),
        ("二简", COLOR_BY_LEN[2]),
        ("三简", COLOR_BY_LEN[3]),
        ("四码", COLOR_BY_LEN[4]),
        ("缺字", COLOR_MISSING),
    ):
        draw.rectangle([lx, legend_y, lx + 18, legend_y + 18], fill=color)
        draw.text((lx + 26, legend_y - 3), label, font=tiny_font, fill=DIM)
        lx += 96
    draw.text((lx + 8, legend_y - 3), "（只有词着色；选重：码下方黄线）", font=tiny_font, fill=DIM)

    # 键盘热力图
    key_counts = stats["key_counts"]
    total_keys = max(1, stats["keys"])
    kb_top = header_h + legend_h
    for row_index, keys in enumerate(KEY_ROWS):
        row_width = len(keys) * (key_size + key_gap) - key_gap
        start_x = (WIDTH - row_width) // 2
        y = kb_top + 10 + row_index * (key_size + row_gap)
        for col, key in enumerate(keys):
            x = start_x + col * (key_size + key_gap)
            count = key_counts.get(key, 0)
            ratio = count / total_keys
            intensity = min(1.0, ratio / 0.09)
            fill = (
                int(PANEL[0] + (226 - PANEL[0]) * intensity),
                int(PANEL[1] + (118 - PANEL[1]) * intensity),
                int(PANEL[2] + (44 - PANEL[2]) * intensity),
            )
            draw.rounded_rectangle([x, y, x + key_size, y + key_size], radius=10, fill=fill)
            label = key.upper()
            lw, _ = _text_size(draw, label, stats_font)
            draw.text((x + (key_size - lw) / 2, y + 12), label, font=stats_font, fill=TEXT)
            pct = f"{ratio * 100:.1f}%"
            pw, _ = _text_size(draw, pct, tiny_font)
            draw.text((x + (key_size - pw) / 2, y + 44), pct, font=tiny_font, fill=TEXT)
            cw2, _ = _text_size(draw, str(count), tiny_font)
            draw.text((x + (key_size - cw2) / 2, y + 66), str(count), font=tiny_font, fill=DIM)

    # 空格键
    space_y = kb_top + 10 + len(KEY_ROWS) * (key_size + row_gap) + 4
    space_w = 420
    space_x = (WIDTH - space_w) // 2
    space_count = stats["spaces"]
    space_ratio = space_count / max(1, space_count + total_keys)
    draw.rounded_rectangle(
        [space_x, space_y, space_x + space_w, space_y + 46], radius=10, fill=PANEL
    )
    space_text = f"Space  {space_ratio * 100:.1f}%   {space_count} 次"
    sw, _ = _text_size(draw, space_text, tiny_font)
    draw.text((space_x + (space_w - sw) / 2, space_y + 12), space_text, font=tiny_font, fill=TEXT)

    # 正文逐字带码（只有词着色）
    y = kb_top + keyboard_h
    for line_entries in lines:
        x = MARGIN
        for entry in line_entries:
            if entry.punctuation:
                color = DIM
                code_text = ""
            elif entry.missing:
                color = COLOR_MISSING
                code_text = ""
            elif len(entry.text) <= 1:
                color = TEXT
                code_text = entry.code or ""
            else:
                color = COLOR_BY_LEN.get(entry.code_len, COLOR_BY_LEN[4])
                code_text = entry.code or ""
            cw, _ = _text_size(draw, entry.text, char_font)
            kw, _ = _text_size(draw, code_text or "?", code_font)
            block = max(cw, kw)
            draw.text((x + (block - cw) / 2, y), entry.text, font=char_font, fill=color)
            if code_text:
                draw.text((x + (block - kw) / 2, y + char_h + 2), code_text, font=code_font, fill=color)
            if entry.selected and len(entry.text) > 1:
                draw.line(
                    [(x + (block - kw) / 2, y + char_h + code_h + 6),
                     (x + (block + kw) / 2, y + char_h + code_h + 6)],
                    fill=COLOR_SELECTED,
                    width=2,
                )
            x += block + pair_gap
        y += pair_h

    if truncated:
        draw.text((MARGIN, y + 6), f"（正文过长，仅渲染前 {MAX_RENDER_CHARS} 字）", font=tiny_font, fill=DIM)

    buffer = __import__("io").BytesIO()
    image.save(buffer, format="PNG", optimize=True)
    return buffer.getvalue()


if __name__ == "__main__":
    import sys

    sample = sys.argv[1] if len(sys.argv) > 1 else "还是那句话，证据，你得拿出证据来。"
    data = render_png(sample)
    out = sys.argv[2] if len(sys.argv) > 2 else "/tmp/huma-sample.png"
    with open(out, "wb") as handle:
        handle.write(data)
    entries, stats = encode(sample)
    print("stats:", stats)
    print("first entries:", [(e.text, e.code, e.selected) for e in entries[:12]])
    print("wrote", out, len(data), "bytes")
