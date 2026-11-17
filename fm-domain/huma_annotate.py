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
    # 2026-11-17 按「码表文件顺序」取码（等价于昙花赛码器的 --stable 口径）：
    # 码表里先出现的那个码就是真实输入码，这样算出的码长/选重和输入法实际打的一致
    # （实测同一段文字：文件顺序 89 键/码长 1.82，而「最短码优先」只要 83 键/1.69）。
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
    # 2026-11-17 所有者最终口径：显示**最简码**（有简码就用简码），
    # 因此按码长排序；需要选重时由码后的 ; / 数字表示。
    for key, codes in reverse.items():
        codes.sort(key=lambda item: (len(item[0]), item[0]))
    result = dict(reverse)
    with _index_lock:
        _index_cache["signature"] = signature
        _index_cache["reverse"] = result
    return result


# ── 编码 ────────────────────────────────────────────────────────────────────


PUNCTUATION = set("，。、！？；：（）“”‘’《》〈〉【】—…·‘’〝〞,.;:!?()[]{}<>\"'`~@#$%^&*+-=_/\\|")

PUNCT_COMMIT_CHARS = set("，。！？：、,.!?:（）“”’《》〈〉【】「」『』—…·")


class Entry:
    __slots__ = (
        "text", "code", "code_len", "position", "selected",
        "missing", "punctuation", "next_punct", "plain",
    )

    def __init__(self, text: str, code: Optional[str], position: int,
                 next_punct: str = "", plain: bool = False):
        self.text = text
        self.code = code
        self.code_len = len(code) if code else 0
        self.position = position
        self.selected = position > 0
        self.plain = plain
        self.missing = code is None and not plain and text not in PUNCTUATION
        self.punctuation = code is None and text in PUNCTUATION
        # 下一个字符若是标点，可以用「码 + 标点」直接上屏（句末常见）
        self.next_punct = next_punct


def selector_suffix(position: int) -> str:
    """选重按键：第 2 候选用 ';'，第 3..9 用数字，第 10 用 '0'。"""
    if position <= 0:
        return ""
    if position == 1:
        return ";"
    index = position + 1
    return str(index) if index <= 9 else "0"


def display_code(entry: "Entry") -> str:
    if not entry.code:
        return ""
    return entry.code + selector_suffix(entry.position)




def clean_article(text: str) -> str:
    """从引用内容里取出真正的正文（2026-11-17 所有者要求）。

    去掉开头 `[...]《...》[字数N]` 与结尾 `----- 第N段 …FM赛文` 的元信息。
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
    """词优先编码（2026-11-17 所有者最终口径）。

    · 词组优先最长匹配（4→2 字），命中词组就用该词的码（优先最短简码）；
    · 没有词组匹配时按单字取码（简码优先）；
    · 字数 = 非空白字符数（不是匹配后的条目数），码长 = 总键数 / 字数；
    · 选重 = 该码的首选候选不是这个词/字，需要按 ; 或数字去选。
    """
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
        # 标点一律按「标点上屏」处理（不查码表、不占空格），顿号/逗号/句号都一样
        if char in PUNCT_COMMIT_CHARS:
            entries.append(Entry(char, None, 0))
            index += 1
            continue
        # 2026-11-17 所有者：阿拉伯数字直接用数字键打，不走码表、不算缺字
        if char.isascii() and char.isdigit():
            entries.append(Entry(char, None, 0, plain=True))
            index += 1
            continue
        matched = None
        for size in (4, 3, 2, 1):
            if index + size > length:
                continue
            piece = text[index:index + size]
            candidates = reverse.get(piece)
            if not candidates:
                continue
            code, position = candidates[0]
            after = text[index + size] if index + size < length else ""
            next_punct = after if after in PUNCT_COMMIT_CHARS else ""
            matched = Entry(piece, code, position, next_punct)
            break
        if matched is None:
            entries.append(Entry(char, None, 0))
            index += 1
        else:
            entries.append(matched)
            index += len(matched.text)

    # 2026-11-17 所有者口径：
    #   · 码长要算上「上屏空格」；
    #   · 每打完一个字/词都要按空格上屏（白色单字、橙/蓝/紫词都一样）；
    #   · 例外：带选重（; 或数字）的条目由选择键直接上屏，不再按空格；
    #   · 选择键（; / 数字）本身也是击键，计入键数与键盘热力。
    chars = sum(1 for ch in text if not ch.isspace())
    selected = sum(1 for entry in entries if entry.selected)
    missing = sum(1 for entry in entries if entry.missing)
    commit_entries = [e for e in entries if not e.missing and not e.punctuation]
    # 上屏方式：带选重 → 选择键上屏；后面紧跟标点 → 那个标点上屏；否则按空格上屏。
    spaces = sum(1 for e in commit_entries if not e.selected and not e.next_punct and not e.plain)
    selector_keys = sum(1 for e in commit_entries if e.selected)
    punct_keys = sum(1 for e in commit_entries if not e.selected and e.next_punct)
    letter_keys = sum(e.code_len for e in commit_entries)
    plain_keys = sum(1 for e in commit_entries if e.plain)
    key_count = letter_keys + selector_keys + punct_keys + spaces + plain_keys
    length_dist = collections.Counter(e.code_len for e in commit_entries if not e.plain)
    key_counts = collections.Counter()
    for entry in commit_entries:
        if entry.code:
            for ch in entry.code:
                key_counts[ch] += 1
            if entry.selected:
                suffix = selector_suffix(entry.position)
                if suffix:
                    key_counts[suffix] += 1
    for entry in commit_entries:
        if entry.plain:
            key_counts[entry.text] += 1
        elif not entry.selected and entry.next_punct:
            key_counts[entry.next_punct] += 1
    key_counts["space"] += spaces
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

KEYBOARD_LAYOUT = [
    # (按键标签, 宽度, 是否字符键)  —— 字符键参与热力统计
    [("`", 1, 1), ("1", 1, 1), ("2", 1, 1), ("3", 1, 1), ("4", 1, 1), ("5", 1, 1),
     ("6", 1, 1), ("7", 1, 1), ("8", 1, 1), ("9", 1, 1), ("0", 1, 1), ("-", 1, 1),
     ("=", 1, 1), ("Bksp", 2, 0)],
    [("Tab", 1.5, 0), ("q", 1, 1), ("w", 1, 1), ("e", 1, 1), ("r", 1, 1), ("t", 1, 1),
     ("y", 1, 1), ("u", 1, 1), ("i", 1, 1), ("o", 1, 1), ("p", 1, 1),
     ("[", 1, 1), ("]", 1, 1), ("\\", 1.5, 1)],
    [("Caps", 1.75, 0), ("a", 1, 1), ("s", 1, 1), ("d", 1, 1), ("f", 1, 1), ("g", 1, 1),
     ("h", 1, 1), ("j", 1, 1), ("k", 1, 1), ("l", 1, 1), (";", 1, 1),
     ("'", 1, 1), ("Enter", 2.25, 0)],
    [("Shift", 2.25, 0), ("z", 1, 1), ("x", 1, 1), ("c", 1, 1), ("v", 1, 1), ("b", 1, 1),
     ("n", 1, 1), ("m", 1, 1), (",", 1, 1), (".", 1, 1), ("/", 1, 1), ("Shift", 2.75, 0)],
    [("Ctrl", 1.25, 0), ("Win", 1.25, 0), ("Alt", 1.25, 0), ("Space", 6.25, 2),
     ("Alt", 1.25, 0), ("Win", 1.25, 0), ("Menu", 1.25, 0), ("Ctrl", 1.25, 0)],
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

    key_size = 56
    key_gap = 10
    row_gap = 8
    space_height = 28
    keyboard_h = 10 + 4 * (key_size + row_gap) + (space_height + row_gap) + 16
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
    draw.text((MARGIN, MARGIN - 8), "fix", font=title_font, fill=ACCENT)
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
    draw.text((lx + 8, legend_y - 3), "（词着色；选重码附选择键 ;/数字；码长含上屏空格）", font=tiny_font, fill=DIM)

    # 键盘热力图（真实 QWERTY 布局：修饰键 + 字母 + 数字 + 空格）
    key_counts = stats["key_counts"]
    total_keys = max(1, stats["keys"])
    kb_top = header_h + legend_h
    unit_total = 15.0
    kb_width = min(WIDTH - MARGIN * 2, 860)
    unit = (kb_width - key_gap * 14) / unit_total
    rows = KEYBOARD_LAYOUT
    for row_index, row in enumerate(rows):
        y = kb_top + 10 + row_index * (key_size + row_gap)
        row_units = sum(w for _, w, _ in row)
        row_width = unit * row_units + key_gap * (row_units - 1)
        x = (WIDTH - row_width) / 2
        for label, width_units, kind in row:
            width = unit * width_units + key_gap * (width_units - 1)
            height = key_size if row_index < 4 else 46
            if kind == 0:
                fill = (24, 34, 50)
                label_color = DIM
                percent = None
                count_text = None
            else:
                count = key_counts.get(label.lower(), 0)
                ratio = count / total_keys
                intensity = min(1.0, ratio / 0.09)
                fill = (
                    int(PANEL[0] + (226 - PANEL[0]) * intensity),
                    int(PANEL[1] + (118 - PANEL[1]) * intensity),
                    int(PANEL[2] + (44 - PANEL[2]) * intensity),
                )
                label_color = TEXT
                percent = f"{ratio * 100:.1f}%"
                count_text = str(count)
            draw.rounded_rectangle([x, y, x + width, y + height], radius=8, fill=fill)
            if kind == 2:
                # 空格键：一行显示「Space  x%  n次」
                shown = f"Space  {percent}  {count_text} 次"
                space_font = _font(14)
                tw, th = _text_size(draw, shown, space_font)
                draw.text((x + (width - tw) / 2, y + (height - th) / 2 - 1), shown, font=space_font, fill=TEXT)
            else:
                shown = label.upper() if len(label) == 1 else label
                key_font = _font(15 if len(shown) <= 2 else 11)
                tw, th = _text_size(draw, shown, key_font)
                draw.text((x + (width - tw) / 2, y + 2), shown, font=key_font, fill=label_color)
                if percent is not None:
                    micro_font = _font(11)
                    pw, _ = _text_size(draw, percent, micro_font)
                    draw.text((x + (width - pw) / 2, y + 20), percent, font=micro_font, fill=TEXT)
                    cw2, _ = _text_size(draw, count_text or "0", micro_font)
                    draw.text((x + (width - cw2) / 2, y + 34), count_text or "0", font=micro_font, fill=DIM)
            x += width + key_gap

    # 正文逐字带码（只有词着色）
    y = kb_top + keyboard_h
    for line_entries in lines:
        x = MARGIN
        for entry in line_entries:
            if entry.punctuation:
                color = DIM
                code_text = ""
            elif entry.plain:
                color = TEXT
                code_text = ""
            elif entry.missing:
                color = COLOR_MISSING
                code_text = ""
            elif len(entry.text) <= 1:
                color = TEXT
                code_text = display_code(entry)
            else:
                color = COLOR_BY_LEN.get(entry.code_len, COLOR_BY_LEN[4])
                code_text = display_code(entry)
            cw, _ = _text_size(draw, entry.text, char_font)
            kw, _ = _text_size(draw, code_text or "?", code_font)
            block = max(cw, kw)
            draw.text((x + (block - cw) / 2, y), entry.text, font=char_font, fill=color)
            if code_text:
                draw.text((x + (block - kw) / 2, y + char_h + 2), code_text, font=code_font, fill=color)
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
