#!/usr/bin/env python3
"""FM 的行为记分卡 —— 把散文自省变成可比数字。

## 为什么

自省笔记是散文（「略失耐心」），没法比较。**没有数字就判断不了「改好了没有」**，
而那是控制回路的前提。

## 只算确定能算的

有几类问题要靠判断（「说了没做的事」），那留在自省笔记里。
这里只放**纯数字**，每条都能从 audit_log / chat_log 直接数出来：

  成功率        answered / 总运行
  撞上限        撞满 agent_max_turns 被截断的次数
  工具过重      单次运行工具调用 > 15 次
  响应过慢      单次运行 > 30 秒
  回复长度      中位 / p90
  空回复        正文为空且无图（真正什么都没说）

## 输出

  memory/self/metrics.md      今天的数字 + 与上次的对比
  memory/self/metrics.json    历史序列（供趋势与自动判断用）
"""
import json
import os
import sqlite3
import urllib.request
import statistics
import sys
from datetime import datetime, timedelta, timedelta, timezone


# ── 时区：显式 UTC+8，不依赖环境 ────────────────────────────────────────
#
# 原来这里用 time.localtime()，它随环境变：
#   TZ=Asia/Shanghai 时给北京时间；TZ 缺失时可能给 UTC。
# 而 /etc/localtime 在这些容器里指向 Etc/UTC，与 TZ 矛盾 —— 实测现在
# 恰好一致，但那是环境凑巧，容器重建时 TZ 没传进去就会静默差 8 小时。
#
# 改成显式 UTC+8：读到同一份 Unix 时间戳，永远得到同一个结果。
from datetime import timezone as _tz, timedelta as _td
BJ_TZ = _tz(_td(hours=8))


def bj_now():
    """有时区信息的当前时间。要格式化就用它。"""
    return datetime.now(BJ_TZ)


def bj_str(unix_ts, fmt="%Y-%m-%d %H:%M:%S"):
    """把 Unix 时间戳按北京时间格式化。"""
    return datetime.fromtimestamp(float(unix_ts), BJ_TZ).strftime(fmt)


def bj_day(unix_ts):
    """按北京时间取日期（YYYY-MM-DD）。"""
    return datetime.fromtimestamp(float(unix_ts), BJ_TZ).strftime("%Y-%m-%d")


BJ = timezone(timedelta(hours=8))


def notes_root() -> str:
    for candidate in ("/botrw/memory", "/data/memory"):
        if os.path.isdir(candidate):
            return candidate
    return "/data/memory"


def bot_db() -> str:
    for candidate in ("/botrw/cosmobot.sqlite3", "/botdata/cosmobot.sqlite3",
                      "/data/cosmobot.sqlite3"):
        if os.path.isfile(candidate):
            return candidate
    return "/botdata/cosmobot.sqlite3"


WINDOW_DAYS = 7
OUT_MD = os.path.join(notes_root(), "self", "metrics.md")
OUT_JSON = os.path.join(notes_root(), "self", "metrics.json")


def iso_utc(dt: datetime) -> str:
    return dt.astimezone(timezone.utc).strftime("%Y-%m-%d %H:%M:%S")


def parse_ts(value):
    """audit_log.occurred_at 是带 +0000 的纳秒字符串。

    datetime 既不吃 +0000 也不吃 9 位纳秒，所以手工规整。
    这段与 fm-domain 的 parse_bot_time 一致。
    """
    if value is None:
        return None
    text = str(value).strip()
    if not text:
        return None
    if len(text) >= 5 and text[-5] in "+-" and text[-3] != ":":
        text = f"{text[:-2]}:{text[-2:]}"
    head, sep, rest = text.partition(" ")
    if sep:
        text = f"{head}T{rest}"
    main, dot, frac = text.partition(".")
    if dot:
        digits = "".join(ch for ch in frac if ch.isdigit())[:6]
        tail = ""
        for marker in ("+", "-", "Z"):
            idx = frac.find(marker)
            if idx >= 0:
                tail = frac[idx:]
                break
        if tail and len(tail) >= 5 and tail[-3] != ":":
            tail = f"{tail[:-2]}:{tail[-2:]}"
        text = f"{main}.{digits}{tail}" if digits else f"{main}{tail}"
    try:
        return datetime.fromisoformat(text)
    except (TypeError, ValueError):
        return None


def compute() -> dict:
    con = sqlite3.connect(f"file:{bot_db()}?mode=ro", uri=True, timeout=20)
    con.row_factory = sqlite3.Row
    try:
        now = datetime.now(BJ)
        since = iso_utc(now - timedelta(days=WINDOW_DAYS))

        # 运行：状态、工具调用数、耗时
        #
        # 耗时不能用事件的 elapsedMs —— agent_run_finished 里没有这个字段
        # （第一版就是这么写的，结果 slow_runs 恒为 0）。
        # 改成取该 run 的首尾 occurred_at 相减，和 /bot/runs 的做法一致。
        runs = []
        for r in con.execute("""
            SELECT run_id, event_json FROM audit_log
            WHERE event_kind='agent_run_finished' AND occurred_at >= ?
        """, (since,)):
            rid = r["run_id"]
            try:
                finished = json.loads(r["event_json"] or "{}")
            except (TypeError, ValueError):
                finished = {}
            evs = con.execute("""
                SELECT event_kind, event_json, occurred_at FROM audit_log
                WHERE run_id=? ORDER BY id
            """, (rid,)).fetchall()
            kinds = [e["event_kind"] for e in evs]
            tool_names = set()
            for e in evs:
                if e["event_kind"] == "tool_call_started":
                    try:
                        d = json.loads(e["event_json"] or "{}")
                    except (TypeError, ValueError):
                        continue
                    n = (d.get("toolCall") or {}).get("name")
                    if n:
                        tool_names.add(n)
            elapsed_ms = None
            if evs:
                t1 = parse_ts(evs[0]["occurred_at"])
                t2 = parse_ts(evs[-1]["occurred_at"])
                if t1 and t2 and t2 >= t1:
                    elapsed_ms = (t2 - t1).total_seconds() * 1000
            runs.append({
                # 用完整 run_id：实测同名 run_id 会出现多条 agent_run_finished
                # （96 个 run_id 重复出现），续跑共享它。所以按它分组就能
                # 看出「撞了上限但后来答上了」。
                "run_key": rid,
                "status": str(finished.get("status")),
                "turns": finished.get("turnsUsed"),
                "tool_calls": kinds.count("tool_call_started"),
                "elapsed_ms": elapsed_ms,
                "tools": tool_names,
            })

        total = len(runs)
        answered = sum(1 for x in runs if x["status"] == "answered")
        tool_limit = sum(1 for x in runs if x["status"] == "tool_limit")

        # 「撞上限」不等于失败。
        #
        # autoContinuingAgentStream（AgentRun.hs:487）会在 status=tool_limit 时
        # 自动续跑，最多 2 次；实测同一 run_id 前面是 tool_limit、后面是 answered。
        # 所以真正失败的是「同一 run_id 撞了上限且从未 answered」的那些。
        #
        # 第一版把这个混为一谈，结论是「30 次撞上限」，其中还有 2 次被我
        # 当成「彻底没回消息」——那两次其实也被续跑救回来了
        # （linked_message_id 为空不代表没回答，续跑那次才有）。
        by_run = {}
        for x in runs:
            key = x["run_key"]
            prev = by_run.get(key)
            if prev is None:
                by_run[key] = dict(x, tool_limit_count=0, answered_count=0)
                prev = by_run[key]
            if x["status"] == "tool_limit":
                prev["tool_limit_count"] += 1
            elif x["status"] == "answered":
                prev["answered_count"] += 1
                prev["status"] = "answered"        # 后来答上了，就算答上了
                prev["tools"] |= x["tools"]
                prev["tool_calls"] += x["tool_calls"]

        # 「撞了上限但最终答上」是浪费（多花了轮数和 token），不是失败；
        # 「撞了上限且始终没答」才是真失败。
        #
        # 注意：这里只算总数。**分开开发/生产要等下面把 runs 分成两组后再算**——
        # 第一版就在这里直接报总数，结果把开发自测混进去，
        # 报出的「9 次失败」里真实对话只有 2 次。
        recovered_total = sum(1 for v in by_run.values()
                              if v["tool_limit_count"] > 0 and v["answered_count"] > 0)
        lost_total = sum(1 for v in by_run.values()
                         if v["tool_limit_count"] > 0 and v["answered_count"] == 0)

        heavy = sum(1 for x in runs if x["tool_calls"] > 15)
        slow = sum(1 for x in runs if isinstance(x["elapsed_ms"], (int, float))
                   and x["elapsed_ms"] > 30000)
        top_turns = max((x["turns"] or 0) for x in runs) if runs else 0

        # 分开「开发/自测」与「真实对话」。
        #
        # 为什么必须分：run_bash / sandbox / terminal 是 agent 自改自测用的，
        # 群友不会触发。实测 168 次撞上限里 133 次用了这三个工具——
        # 混在一起报，会把开发噪音当成生产问题，指标就失去意义。
        dev_tools = {"run_bash", "sandbox", "terminal", "workspace"}
        dev_runs = [x for x in runs if x["tools"] & dev_tools]
        prod_runs = [x for x in runs if not (x["tools"] & dev_tools)]
        prod_tool_limit = sum(1 for x in prod_runs if x["status"] == "tool_limit")
        prod_slow = sum(1 for x in prod_runs
                        if isinstance(x["elapsed_ms"], (int, float)) and x["elapsed_ms"] > 30000)
        prod_total = len(prod_runs)
        prod_answered = sum(1 for x in prod_runs if x["status"] == "answered")

        # 真实对话里的「救回 / 始终没答」——只统计不含开发工具的运行
        prod_by_run = {}
        for x in prod_runs:
            v = prod_by_run.setdefault(x["run_key"], {"tl": 0, "ans": 0})
            if x["status"] == "tool_limit":
                v["tl"] += 1
            elif x["status"] == "answered":
                v["ans"] += 1
        prod_recovered = sum(1 for v in prod_by_run.values() if v["tl"] > 0 and v["ans"] > 0)
        prod_lost = sum(1 for v in prod_by_run.values() if v["tl"] > 0 and v["ans"] == 0)

        # 回复长度与空回复
        lens = []
        empty = 0
        for r in con.execute("""
            SELECT body_text, image_urls FROM chat_log
            WHERE is_bot=1 AND recorded_at >= ?
        """, (since,)):
            body = r["body_text"] or ""
            has_img = bool((r["image_urls"] or "").strip())
            if not body.strip():
                if not has_img:
                    empty += 1
                continue
            lens.append(len(body))
        lens.sort()

        def pct(v):
            return lens[min(len(lens) - 1, int(len(lens) * v / 100))] if lens else 0

        return {
            "date": now.strftime("%Y-%m-%d"),
            "window_days": WINDOW_DAYS,
            "runs_total": total,
            "answered": answered,
            "success_rate": round(answered / total * 100, 1) if total else None,
            "tool_limit": tool_limit,
            "heavy_runs": heavy,
            "slow_runs": slow,
            "max_turns_seen": top_turns,
            "dev_runs": len(dev_runs),
            "prod_runs": prod_total,
            "prod_success_rate": round(prod_answered / prod_total * 100, 1) if prod_total else None,
            "prod_tool_limit": prod_tool_limit,
            "recovered_total": recovered_total,
            "lost_total": lost_total,
            "prod_recovered": prod_recovered,
            "prod_lost": prod_lost,
            "prod_slow": prod_slow,
            "replies": len(lens),
            "reply_p50": pct(50),
            "reply_p90": pct(90),
            "reply_max": lens[-1] if lens else 0,
            "empty_replies": empty,
        }
    finally:
        con.close()


def load_history() -> list:
    try:
        with open(OUT_JSON, encoding="utf-8") as handle:
            data = json.load(handle)
        return data if isinstance(data, list) else []
    except (OSError, ValueError):
        return []


def delta(cur, prev, key):
    if prev is None:
        return ""
    a, b = prev.get(key), cur.get(key)
    if not isinstance(a, (int, float)) or not isinstance(b, (int, float)):
        return ""
    d = b - a
    if d == 0:
        return "（持平）"
    return f"（{'↑' if d > 0 else '↓'}{abs(d):g}）"


DOMAIN_BASE = os.environ.get("FM_DOMAIN_BASE", "http://172.20.0.4:8077")


def contest_lines() -> list:
    """赛文预生成的健康检查：未来 7 天是否齐备、难度是否都是「虐」。

    为什么放进记分卡：这套自动化的失败是**静默的** —— ai_contest_daily.py 只写自己的日志，
    没有任何告警；而它提前 7 天备文，所以能悄悄坏掉一整周才被人发现。

    **必须走 HTTP 而不是读库**：机器人和 domain 是两个容器、各挂各的，
    机器人容器里的 /data/fm-domain.sqlite3 是个 **0 字节的残留文件**（2026-08-26 留下），
    连上去只会得到 "no such table" —— 我第一版就是这么写错的，「文件存在」不等于「是对的库」。
    """
    today = bj_now().date()
    missing, wrong, unreachable = [], [], []
    for offset in range(7):
        day = (today + timedelta(days=offset)).isoformat()
        try:
            with urllib.request.urlopen(
                f"{DOMAIN_BASE}/ai-contest/text?date={day}", timeout=10
            ) as response:
                payload = json.loads(response.read().decode("utf-8"))
        except Exception as exc:  # noqa: BLE001
            unreachable.append(f"{day}（{type(exc).__name__}）")
            continue
        body = payload.get("text") or payload.get("body") or payload.get("content") or ""
        if payload.get("status") not in (None, "ok") or not body:
            missing.append(day)
            continue
        difficulty = payload.get("difficulty") or payload.get("level") or "?"
        if difficulty != "虐":
            wrong.append(f"{day}（{difficulty}）")

    if unreachable:
        return ["", f"**赛文预生成**：⚠️ 查不到：" + "、".join(unreachable) + " —— domain 服务或网络有问题。", ""]
    problems = []
    if missing:
        problems.append("缺备文：" + "、".join(missing))
    if wrong:
        problems.append("难度不是「虐」的：" + "、".join(wrong))
    if problems:
        return (
            ["", "### ⚠️ 赛文预生成异常", ""]
            + [f"- {x}" for x in problems]
            + ["", "修复办法见 `tools/contest/README.md`（删掉对应日期的行，cron 会按新规则重生成）。", ""]
        )
    return ["", "**赛文预生成**：未来 7 天齐备，难度全部「虐」。", ""]


def main() -> int:
    if "--dry" in sys.argv:
        print(json.dumps(compute(), ensure_ascii=False, indent=1))
        return 0

    cur = compute()
    history = load_history()
    prev = history[-1] if history else None

    lines = [
        f"# 行为记分卡 · {cur['date']}",
        "",
        f"窗口：最近 {cur['window_days']} 天。数字由 `self_metrics.py` 自动算出，不是自述。",
        "",
        "**先看这一组——只算真实对话**（排除了用 run_bash / sandbox / terminal 的",
        "开发自测运行，那些是 agent 自改自测触发的，群友不会触发）：",
        "",
        "| 指标 | 值 | 与上次 |",
        "|---|---|---|",
        f"| 真实对话运行 | {cur['prod_runs']} 次 | |",
        f"| 成功率 | {cur['prod_success_rate']}% {delta(cur, prev, 'prod_success_rate')} | |",
        f"| 撞上限后**救回** | {cur['prod_recovered']} 次（全体 {cur['recovered_total']}） | 多花了轮数，最终答上了 |",
        f"| 撞上限且**始终没答** | {cur['prod_lost']} 次（全体 {cur['lost_total']}）{delta(cur, prev, 'prod_lost')} | **这才是失败** |",
        f"| 响应过慢（>30 秒） | {cur['prod_slow']} 次 {delta(cur, prev, 'prod_slow')} | |",
        "",
        "**参考——含开发自测的全部运行**：",
        "",
        "| 指标 | 值 |",
        "|---|---|",
        f"| 运行总数 | {cur['runs_total']}（其中开发自测 {cur['dev_runs']}） |",
        f"| 成功率 | {cur['success_rate']}% |",
        f"| 撞轮数上限 | {cur['tool_limit']} 次 |",
        f"| 工具过重（>15 次） | {cur['heavy_runs']} 次 |",
        f"| 响应过慢（>30 秒） | {cur['slow_runs']} 次 |",
        "",
        "**回复形态**（只统计有正文的回复）：",
        "",
        "| 指标 | 值 |",
        "|---|---|",
        f"| 回复条数 | {cur['replies']} |",
        f"| 中位长度 | {cur['reply_p50']} 字 |",
        f"| p90 长度 | {cur['reply_p90']} 字 |",
        f"| 空回复（什么都没说） | {cur['empty_replies']} 次 {delta(cur, prev, 'empty_replies')} |",
        "",
        "**怎么读**：",
        "",
        "- 「撞上限后救回」是**浪费**（多花轮数和 token），不是失败——",
        "  `autoContinuingAgentStream` 会自动续跑最多 2 次把它救回来。",
        "- 「撞上限且始终没答」才是**失败**：那是要盯着归零的。",
        "- 「空回复」同样要归零。",
        "- 回复长度只做参考，不是越短越好。",
        "- 开发自测那组波动大、不代表对外表现，只看趋势不看绝对值。",
        "",
        *contest_lines(),
        "<!-- 由 self_metrics.py 自动生成 -->",
    ]
    text = "\n".join(lines) + "\n"
    os.makedirs(os.path.dirname(OUT_MD), exist_ok=True)
    with open(OUT_MD, "w", encoding="utf-8") as handle:
        handle.write(text)

    history = [h for h in history if h.get("date") != cur["date"]]
    history.append(cur)
    history = history[-30:]
    with open(OUT_JSON, "w", encoding="utf-8") as handle:
        json.dump(history, handle, ensure_ascii=False, indent=1)

    print(f"  已写入 {OUT_MD}")
    print(f"  已更新 {OUT_JSON}（{len(history)} 天）")
    print(f"  成功率 {cur['success_rate']}%  撞上限 {cur['tool_limit']}  空回复 {cur['empty_replies']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
