#!/usr/bin/env python3
"""更新 *fix 的码表：下载/读取 -> 校验 -> 原子替换。

用法：
    update_table.py <url|本地路径>
    update_table.py --check <路径>      # 只校验，不替换

设计要点（每一条都有理由）：
  · **换行必须归一**：主人上传的表是 CRLF，旧表是 LF。如果按 `\\t` 切完不 strip，
    编码会带上尾随 `\\r`，整张表**静默失效**（不报错，是全查不到）—— 比崩溃更糟。
  · **校验不过就一个字都不动**：这是破坏性操作，坏表会让 *fix 整体失效或悄悄解错字。
  · **原子替换**：先写 .new 再 os.replace，避免 *fix 读到半截文件。
  · **留备份**：旧索引存成 huma_index.tsv.<时间戳>.bak。
  · **锚点校验**：最高频的几个字（的一是不了）在词码表里极其稳定，任何一版都该是
    u/f/o/c/r。对不上说明格式读错了（列序反了、编码被截了、文件根本不是码表）。
"""
import collections
import datetime as dt
import json
import os
import pathlib
import shutil
import sys
import urllib.request

HERE = pathlib.Path("/data/huma")
INDEX = HERE / "huma_index.tsv"
INCOMING = HERE / "incoming"

# 锚点：字 -> 编码。最高频字，跨版本稳定。
ANCHORS = {"的": "u", "一": "f", "是": "o", "不": "c", "了": "r"}
MIN_ENTRIES = 1000


def fetch(source: str) -> bytes:
    if source.startswith(("http://", "https://")):
        req = urllib.request.Request(source, headers={"User-Agent": "Mozilla/5.0"})
        with urllib.request.urlopen(req, timeout=90) as r:
            return r.read()
    return pathlib.Path(source).read_bytes()


def parse(raw: bytes):
    text = raw.decode("utf-8")            # 明确要求 UTF-8；解不开就报错而不是猜
    lines = text.splitlines()             # splitlines 同时处理 \n 和 \r\n
    pairs, bad = [], []
    for l in lines:
        if not l.strip():
            continue
        parts = l.split("\t")
        if len(parts) != 2:
            bad.append(l[:80])
            continue
        ch, code = parts[0].strip(), parts[1].strip()
        if not ch or not code:
            bad.append(l[:80])
            continue
        pairs.append((ch, code))
    return pairs, bad


def validate(pairs, bad, old_first: dict):
    """返回 (ok, [问题...], 统计)"""
    problems = []
    if len(pairs) < MIN_ENTRIES:
        problems.append(f"条目太少：{len(pairs)} < {MIN_ENTRIES}")
    if bad:
        problems.append(f"有 {len(bad)} 行解析不出两列，例：{bad[:2]}")

    alphabet = sorted({c for _, code in pairs for c in code})
    non_az = sorted(set(alphabet) - set("abcdefghijklmnopqrstuvwxyz"))
    if non_az:
        problems.append(f"编码里出现非 a-z 字符：{non_az[:10]}（解码器只认 a-z）")
    if any(c in ("\r", "\n", " ") for _, code in pairs for c in code):
        problems.append("编码里含空白字符 —— 换行没归一干净")

    # 锚点
    entry2code = {}
    for ch, code in pairs:
        entry2code.setdefault(ch, code)
    for ch, want in ANCHORS.items():
        got = entry2code.get(ch)
        if got != want:
            problems.append(f"锚点不符：「{ch}」应为 {want}，实为 {got}")

    # 与旧表的延续性：抽样看「同一个码的首选」是否还对得上
    rev = collections.defaultdict(list)
    for ch, code in pairs:
        rev[code].append(ch)
    same = total = 0
    for code, first in list(old_first.items())[:20000]:
        total += 1
        cands = rev.get(code)
        if cands and cands[0] == first:
            same += 1
    keep = same / total if total else 1.0
    if keep < 0.90:
        problems.append(f"与旧表延续性过低：{keep*100:.1f}% 的首选没变（阈值 90%）")

    stats = {
        "entries": len(pairs),
        "codes": len(rev),
        "single": sum(1 for ch, _ in pairs if len(ch) == 1),
        "phrase": sum(1 for ch, _ in pairs if len(ch) > 1),
        "lengths": dict(sorted(collections.Counter(len(c) for _, c in pairs).items())),
        "avg_candidates": round(len(pairs) / len(rev), 2) if rev else 0,
        "anchor_keep": round(keep * 100, 1),
    }
    return (not problems), problems, stats, rev


def load_old_first():
    first = {}
    if INDEX.exists():
        for line in INDEX.read_text(encoding="utf-8").splitlines():
            p = line.split("\t")
            if len(p) == 2:
                c = p[1].split(" ")
                if c and c[0]:
                    first[p[0]] = c[0]
    return first


def main():
    args = [a for a in sys.argv[1:] if a != "--check"]
    check_only = "--check" in sys.argv
    if not args:
        print("用法: update_table.py <url|路径>")
        return 2
    source = args[0]

    raw = fetch(source)
    print(f"取到 {len(raw)} 字节")

    pairs, bad = parse(raw)
    old_first = load_old_first()
    ok, problems, stats, rev = validate(pairs, bad, old_first)

    print(f"条目 {stats['entries']}  编码 {stats['codes']}  单字 {stats['single']}  词组 {stats['phrase']}")
    print(f"长度分布 {stats['lengths']}  平均 {stats['avg_candidates']} 条/编码")
    print(f"与旧表首选一致率 {stats['anchor_keep']}%")

    if not ok:
        print()
        print("❌ 校验不通过，**没有动任何文件**：")
        for p in problems:
            print(f"   · {p}")
        return 1

    if check_only:
        print("✅ 校验通过（--check，未替换）")
        return 0

    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S")
    INCOMING.mkdir(parents=True, exist_ok=True)
    shutil.copy2(source, INCOMING / f"{stamp}-raw.txt") if not source.startswith("http") else None

    tmp = INDEX.with_suffix(".tsv.new")
    with tmp.open("w", encoding="utf-8") as f:
        for code in sorted(rev):
            f.write(code + "\t" + " ".join(rev[code]) + "\n")

    old_stats = None
    if INDEX.exists():
        backup = INDEX.with_name(f"huma_index.tsv.{stamp}.bak")
        old_lines = INDEX.read_text(encoding="utf-8").splitlines()
        old_stats = len(old_lines)
        shutil.copy2(INDEX, backup)
        print(f"旧索引备份 -> {backup.name}（{old_stats} 个编码）")

    os.replace(tmp, INDEX)      # 原子
    print()
    print("✅ 已替换")
    if old_stats:
        print(f"   编码数 {old_stats} -> {len(rev)}（{len(rev)-old_stats:+d}）")
    print(f"   现在的索引: {INDEX}  {INDEX.stat().st_size} 字节")
    return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except SystemExit:
        raise
    except Exception as exc:  # noqa: BLE001
        # 别把 Python 堆栈甩给聊天窗口：一行说清是什么毛病就够了。
        # （2026-10-03 第一次实机失败时，聊天里出现的是一整段 Traceback
        #   —— 对排错有用，但那是给我的，不是给所有者的。）
        print(f"内部错误（{type(exc).__name__}）：{exc}")
        print("现有码表没动。")
        sys.exit(1)
