# 虎杯成绩采集

数据源：`https://race.tiger-code.com/api/leaderboard/date/<YYYY-MM-DD>`
（虎码杯官方榜，返回 JSON，字段含 speed / hit_rate / kpw / accuracy / word_ratio / correction_count）

## 为什么有这个脚本

`competition_scores` 的虎杯数据停在 2026-08-27。原来靠旧系统
`astrbot_plugin_outputpro` 的一次性快照导入（快照停在 08-22），那个系统已经不在了，
机器人侧也没有任何写入路径 —— 于是 8-27 之后两个月没有任何虎杯数据，
虎杯曲线一直是空图。

现在直接走官方接口，不再依赖任何第三方机器人或快照。

## 用法

在 fm-domain 容器里跑（库在 `/data`，脚本通过 `/botdata` 挂进去）：

    docker exec fm-domain python3 /botdata/tiger_daily.py                 # 最近 3 天
    docker exec fm-domain python3 /botdata/tiger_daily.py --date 2026-10-24
    docker exec fm-domain python3 /botdata/tiger_daily.py --from 2026-08-28 --to 2026-10-24

定时任务（每 2 小时，幂等重写最近 3 天）：

    20 */2 * * * docker exec fm-domain python3 /botdata/tiger_daily.py >> /opt/fm-cosmobot/tiger_daily.log 2>&1

## 用户名 -> QQ 号

官方接口只给「跟打器用户名」，没有 QQ 号，而现有图表/榜单按 `user_id`（QQ 号）查。
脚本会从两个来源建映射（去掉「地区」/『地区』前缀后比对）：

1. 库里已有的虎杯记录（`user_name` 与 `user_id` 都有）
2. 机器人的聊天日志 `/botdata/cosmobot.sqlite3` 的 `chat_log`（昵称 -> 发送者 QQ）

映射不到的记录 `user_id` 留空，仍可按 `user_name` 查询。
2026-10-24 实测：新数据 2193 条中 656 条（29%）映射成功；所有者可正常按 QQ 号查到。
