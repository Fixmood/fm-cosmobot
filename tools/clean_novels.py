#!/usr/bin/env python3
"""清洗番茄小说正文里的占位符垃圾（所有者 2026-11-06）。

现象：发出来的段落里出现「［插图］」这种东西。

清洗规则刻意保守 —— 只删**确定的占位符**，绝不动可能是正文的内容：

  删：
    ［插图］ ［图片］（全角，独立占位）
    ［视频］［音乐］［音频］ 等媒体占位
    行首独立的 [图片] / [视频] / [音乐]（半角，且不属于「昵称：[图片]」这种聊天体）
    （未完待续）及其后面的整段（作者催更/求关注的话）
    http(s) 链接

  不删：
    [图片] 出现在「昵称：[图片]」里 —— 聊天体小说的剧情内容
    [比心] [表情包] [照片] [6] —— 表情/动作，属于正文
    「本站」「加更」「求推荐」—— 实测大多是正文里的普通词，删了会伤内容
"""
import pathlib
import re

DB = "/data/fm-domain.sqlite3"

# 媒体占位符（全角/半角都覆盖，但要排除「昵称：」前缀的聊天体）
MEDIA_WORDS = ("插图", "图片", "视频", "音乐", "音频", "语音", "表情包图片")

TAIL_MARKERS = ("（未完待续）", "(未完待续)", "（本章完）")


def clean_novel_text(text: str) -> str:
    original = text

    # ① 全角占位符：［插图］［图片］…（全角方括号在这批书里唯一用途就是占位）
    for word in MEDIA_WORDS:
        # 只动全角**方**括号：2026-11-06 实测【图片】是聊天体小说的剧情
        # （「[crush]：【图片】」），删了会破坏正文，所以【…】一律不碰。
        text = text.replace("［" + word + "］", "")

    # ② 半角 [图片] 之类：只删「整行只有它」的，保留「昵称：[图片]」这种剧情
    text = re.sub(r"(?m)^\s*\[(?:图片|视频|音乐|插图)\]\s*$", "", text)

    # ③ （未完待续）/（本章完）之后的整段都是作者的话
    for marker in TAIL_MARKERS:
        index = text.find(marker)
        if index != -1:
            text = text[:index]
            break

    # ④ 链接
    text = re.sub(r"https?://\S+", "", text)

    # ⑤ 收尾：占位符删掉后容易留下空行和行尾空格
    text = re.sub(r"[ \t]+\n", "\n", text)
    text = re.sub(r"\n{3,}", "\n\n", text)
    lines = [line.strip() for line in text.splitlines()]
    return "\n".join(lines).strip() if text != original else original


def main() -> int:
    import sqlite3

    connection = sqlite3.connect(DB, timeout=120)
    rows = connection.execute(
        "SELECT text_id, content FROM library_texts WHERE category='fanqie'"
    ).fetchall()
    changed = 0
    for text_id, content in rows:
        cleaned = clean_novel_text(content or "")
        if cleaned != content:
            connection.execute(
                "UPDATE library_texts SET content=?, char_count=? WHERE text_id=?",
                (cleaned, len(cleaned), text_id),
            )
            changed += 1
    connection.commit()
    print(f"  清洗完成：检查 {len(rows)} 篇，修改 {changed} 篇")
    connection.close()
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
