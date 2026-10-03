# FM 555 赛文（AI 赛文）日生成

每天练的那篇「555 赛文」不是模型现写的，是**这个脚本预先批量生成的**。

## 它怎么跑

宿主机 crontab（**不在容器里，也不在机器人代码里**）：

```cron
0 */6 * * * flock -n /var/lock/fm-ai-contest-daily.lock docker run --rm --network fm-runtime \
  -v /opt/fm-cosmobot/ai_contest_daily.py:/tmp/ai_contest_daily.py:ro \
  -v /opt/fm-cosmobot/runtime/config.toml:/opt/fm-cosmobot/runtime/config.toml:ro \
  --entrypoint python3 fm-domain:local /tmp/ai_contest_daily.py \
  >> /opt/fm-cosmobot/ai_contest_daily.log 2>&1
```

**每 6 小时一次**，把**未来 7 天**（`FM_AI_CONTEST_PREFILL_DAYS`，默认 7、上限 14）的赛文补齐。
已存在的日期会 skip（日志里 `"status": "existing"`），所以**光改脚本不会改已有的文章**。

结果落在 domain 库的 `ai_contest_texts` 表（容器内 `/data/fm-domain.sqlite3`，
宿主机 `/opt/fm-domain/data/fm-domain.sqlite3`）。

## 难度

```python
def pick_difficulty():
    return "虐"
```

**2026-10-03 所有者定的：一律「虐」**（原来是 `random.choices(["难","虐"], weights=[7,3])`，
70% 出「难」，实测太简单没挑战性）。

⚠️ **难度有两套来源，只改一边等于没改**：

| 路径 | 用在哪 | 改哪 |
|---|---|---|
| `ai_contest_daily.py`（本脚本） | **日常预生成**（每天实际练的那篇） | 本文件 |
| `Bot/Handler/Ask/AgentRun.hs` 的 `aiContestGenerationSystemPrompt` | 有人当场说「生成一篇 555 赛文」 | 那个 prompt |

2026-10-03 就踩过这个：先改了 agent 那条，**对每天练的文章毫无影响** ✗。

## 想重生成某几天

脚本对已存在的日期是 skip，所以要先把行删掉：

```bash
# 先备份（务必）
python3 - <<'EOF'
import json, sqlite3, pathlib
con = sqlite3.connect("/opt/fm-domain/data/fm-domain.sqlite3", timeout=10)
cols = [d[1] for d in con.execute("pragma table_info(ai_contest_texts)")]
rows = con.execute("select * from ai_contest_texts where competition_date > '2026-10-10'").fetchall()
pathlib.Path("/opt/fm-cosmobot/ai_contest_texts.bak.json").write_text(
    json.dumps([dict(zip(cols, r)) for r in rows], ensure_ascii=False, indent=1), encoding="utf-8")
print(f"备份 {len(rows)} 篇")
EOF

# 再删，然后等下一次 cron（或手动跑一次上面那条 docker run）
```

## 注意

- **crontab 不在 git 里** —— 换机器/重装要手动加回上面那一行（本 README 就是它的出处）
- 生成失败只写进 `ai_contest_daily.log`，**没有人会被告警**；由于提前 7 天，持续失败最多能掩盖一周
- 该脚本原先只活在服务器上，2026-10-04 才纳入仓库 —— 之前丢了就没人再生成赛文了
