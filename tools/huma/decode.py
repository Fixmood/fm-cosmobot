#!/usr/bin/env python3
"""虎码解码器：把编码串解回句子；引号里的内容原样输出。

用法：
    decode.py "ko3 sp;"                          # 纯编码
    decode.py 'rd wj "（笑）" r'                  # 引号里原样，外面的码照解 → 回家了（笑）
    decode.py "ko3sp;"                           # 连着打（推测）
    decode.py --verbose "..."                    # 附带模式/候选/选择键，供排错

选择键（表里编码只含 a-z，所以选择键无歧义）：
    ;  -> 2选      （主人习惯：「;」表示 2 选）
    1-9 -> 对应序号（主人：其它选用数字就行）
    0  -> 10选

引号规则（2026-10-03 所有者要求：*fix 支持「引号里原样 + 后面编码照用」）：
  · `"..."` 里的内容**原封不动**输出，不参与解码
  · 引号外的编码照常解，**引号前后都可以有码**
  · ASCII `"` 和全角 `“ ”` 都认 —— 中文输入法打出来的是全角，只认 ASCII 会让人白试
  · 引号没闭合 → 报错退出（不猜），路由会显示成「解码失败：引号没有闭合」

默认**只输出句子本身**，不带模式、不带候选、不带任何多余文字。
"""
import pathlib
import sys

HERE = pathlib.Path(__file__).resolve().parent
INDEX = HERE / "huma_index.tsv"
MAX_CODE_LEN = 4
SELECTOR_KEYS = {";": 2, "0": 10}
SELECTOR_CHARS = set(SELECTOR_KEYS) | set("123456789")
QUOTE_CHARS = ('"', "\u201c", "\u201d")     # ASCII 双引号 + 全角左右引号
UNKNOWN = "〓"

_SELECTOR_LOG = []


def load_index(path):
    rev = {}
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        parts = line.split("\t")
        if len(parts) == 2:
            rev[parts[0]] = parts[1].split(" ")
    return rev


def split_literals(raw):
    """切成 [(是否字面量, 文本)]。任何引号字符都能开，下一个引号字符关。"""
    parts, buf, in_quote = [], "", False
    for ch in raw:
        if ch in QUOTE_CHARS:
            parts.append((in_quote, buf))
            buf, in_quote = "", not in_quote
        else:
            buf += ch
    if in_quote:
        raise ValueError("引号没有闭合")
    parts.append((in_quote, buf))
    return [(is_lit, text) for is_lit, text in parts if text != ""]


def selector_index(ch):
    if ch in SELECTOR_KEYS:
        return SELECTOR_KEYS[ch]
    if ch.isdigit() and ch != "0":
        return int(ch)
    return None


def split_selector(token):
    """把 `ko3` / `sp;` 拆成 (code, index)。没有选择键则 index=None。"""
    t = token.strip().lower()
    if not t:
        return "", None
    last = t[-1]
    if last in SELECTOR_CHARS:
        idx = selector_index(last)
        if idx is not None:
            return t[:-1], idx
    return t, None


def resolve(rev, code, idx):
    """查表取字。idx=None 取字频第一；越界或查不到返回 None。"""
    cands = rev.get(code)
    if not cands:
        return None
    if idx is None:
        return cands[0]
    if 1 <= idx <= len(cands):
        return cands[idx - 1]
    return None


def decode_spaced(rev, text):
    """分隔符分开的 token：每个 = 码 + 可选选择键。"""
    out, amb, miss = [], [], []
    for raw_tok in text.replace(",", " ").replace("/", " ").split():
        code, idx = split_selector(raw_tok)
        if not code:
            continue
        ch = resolve(rev, code, idx)
        if ch is None:
            out.append(UNKNOWN)
            miss.append(raw_tok.strip())
            continue
        out.append(ch)
        if idx is not None:
            _SELECTOR_LOG.append((raw_tok.strip(), f"第{idx}选 -> {ch}"))
        cands = rev.get(code) or []
        if idx is None and len(cands) > 1:
            amb.append((code, cands[:6]))
    return "".join(out), amb, miss


def decode_continuous(rev, stream):
    """连着打：动态规划切分，每个片段是 code + 可选选择键。仍是推测。"""
    n = len(stream)
    dp = [(float("inf"), []) for _ in range(n + 1)]
    dp[0] = (0.0, [])
    for i in range(n):
        if dp[i][0] == float("inf"):
            continue
        for length in range(1, MAX_CODE_LEN + 1):
            if i + length > n:
                break
            code = stream[i:i + length]
            cands = rev.get(code)
            if not cands:
                continue
            j = i + length
            idx = None
            if j < n and stream[j] in SELECTOR_CHARS and selector_index(stream[j]) is not None:
                cand = selector_index(stream[j])
                if 1 <= cand <= len(cands):
                    idx, j = cand, j + 1
            gain = 0.6 * length + (0.0 if idx is not None else (0.6 if len(cands) > 1 else 0.0))
            cost = dp[i][0] + gain
            if cost < dp[j][0]:
                dp[j] = (cost, dp[i][1] + [(code, idx, cands)])
    if dp[n][0] == float("inf"):
        return None, None, None
    out, amb = [], []
    for code, idx, cands in dp[n][1]:
        out.append(cands[idx - 1] if idx is not None else cands[0])
        if idx is not None:
            _SELECTOR_LOG.append((code, f"第{idx}选 -> {cands[idx - 1]}"))
        if idx is None and len(cands) > 1:
            amb.append((code, cands[:6]))
    return "".join(out), amb, []


def decode_codes(rev, text):
    """一段纯编码：自动判「空格分隔」还是「连着打」。返回 (句子, 模式, amb, miss)。"""
    tokens = text.replace(",", " ").replace("/", " ").split()
    has_sep = any(c.isspace() or c in ",/" for c in text)
    if has_sep:
        spaced = True
    elif len(tokens) == 1:
        # 单个 token：去掉选择键后能查到才算精确，否则可能是「码+选择键」连着的多段
        code, _ = split_selector(tokens[0])
        spaced = code in rev
    else:
        spaced = False

    if spaced:
        sentence, amb, miss = decode_spaced(rev, text)
        return sentence, "空格分隔（精确）", amb, miss
    sentence, amb, _ = decode_continuous(rev, text.lower())
    if sentence is None:
        sys.stderr.write(f"无法切分：{text}\n")
        return UNKNOWN, "连续输入（推测）", [], []
    return sentence, "连续输入（推测）", amb, []


def main():
    argv = list(sys.argv[1:])
    verbose = "--verbose" in argv
    argv = [a for a in argv if a != "--verbose"]
    raw = " ".join(argv).strip()
    if not raw:
        print('用法: decode.py "<编码串>"', file=sys.stderr)
        return 2

    rev = load_index(INDEX)
    try:
        parts = split_literals(raw)
    except ValueError as exc:
        print(str(exc), file=sys.stderr)
        return 1

    out, modes, amb_all, miss_all = [], [], [], []
    for is_literal, text in parts:
        if is_literal:
            out.append(text)                      # 引号内：原封不动
            modes.append("引号内原样")
            continue
        sentence, mode, amb, miss = decode_codes(rev, text)
        out.append(sentence)
        modes.append(mode)
        amb_all += amb
        miss_all += miss

    sys.stdout.write("".join(out) + "\n")
    if verbose:
        sys.stderr.write("模式：" + " + ".join(modes) + "\n")
        for tok, note in _SELECTOR_LOG:
            sys.stderr.write(f"选择键 {tok}: {note}\n")
        if amb_all:
            notes = " · ".join(f"{c}→{'/'.join(cs)}" for c, cs in amb_all[:10])
            sys.stderr.write(f"多候选（已取字频第一个）：{notes}\n")
        if miss_all:
            sys.stderr.write(f"查不到：{' '.join(miss_all)}\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
