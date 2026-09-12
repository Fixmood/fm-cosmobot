# FM 部署回滚操作

## 触发条件

出现以下任一情况，应暂停继续发布并评估回滚：容器不健康、RPC/Domain 无法连接、配置无法生效、关键消息链路失败、媒体发送异常、任务持续失败或日志出现致命错误。

## 先读这段：当前生产形态

生产容器 `fm-cosmobot` 是**手工 `docker run` 启动的**，不带 Compose 标签：

```bash
docker inspect fm-cosmobot --format '{{index .Config.Labels "com.docker.compose.project"}}'
# 输出为空 -> 不是 Compose 管的容器
```

- `/opt/fm-cosmobot/compose.yaml`、`deploy/cosmobot.compose.yaml` 以及
  `ops/deploy_*.sh` 里的 `FM_STABLE_BOT_IMAGE` 默认值都指向
  `fm-cosmobot:runtime-fm-tools`，那是**历史镜像**，比线上落后很多代。
- 因此**不要**用 `docker compose ... up -d --force-recreate fm-cosmobot` 或
  `ops/deploy_production.sh` 去"回滚"生产：那会把线上换成十几代前的版本，
  而且容器名冲突会让 Compose 直接报错。

## 回滚阶梯（都是停着的容器，改名即可启用）

| 容器名 | 镜像 | 状态 |
| --- | --- | --- |
| `fm-cosmobot-prev-wiring-20260912-194138` | `fm-cosmobot:runtime-prefixfix-20260912` | Exited (0) |
| `fm-cosmobot-prev-prefixfix-20260912-191344` | `fm-cosmobot:runtime-flushfix-20260912` | Exited (0) |
| `fm-cosmobot-prev-flushfix-20260912-180646` | `fm-cosmobot:runtime-latency-20260912` | Exited (0) |
| `fm-cosmobot-prev-latency-20260912-173215` | `fm-cosmobot:runtime-notices-20260912` | Exited (0) |
| `fm-cosmobot-prev-notices-20260912-171124` | `fm-cosmobot:runtime-roster-20260912` | Exited (0) |
| `fm-cosmobot-prev-roster-20260912-163712` | `fm-cosmobot:runtime-baregate-20260912` | Exited (0) |
| `fm-cosmobot-prev-baregate-20260912-155422` | `fm-cosmobot:runtime-opening-guard-20260912` | Exited (0) |
| `fm-cosmobot-prev-opening-20260912-152244` | `fm-cosmobot:runtime-mention-body-20260912` | Exited (0) |
| `fm-cosmobot-prev-mention-20260912-145822` | `fm-cosmobot:runtime-bare-prefix-20260912` | Exited (0) |
| `fm-cosmobot-prev-bare-20260912-140226` | `fm-cosmobot:runtime-context-20260912` | Exited (0) |
| `fm-cosmobot-prev-context-20260912-130118` | `fm-cosmobot:runtime-seedream-20260912` | Exited (0) |
| `fm-cosmobot-prev-seedream-20260912-055215` | `fm-cosmobot:runtime-retryfix-20260912` | Exited (0) |
| `fm-cosmobot-prev-retryfix-20260912-011727` | `fm-cosmobot:runtime-restored-20260912` | Exited (0) |
| `fm-cosmobot-context-bad` | `fm-cosmobot:runtime-context-20260911` | Exited (0) - known-bad, parked for reference only, do not start |
| `fm-cosmobot-prev-20260912-002912` | `fm-cosmobot:runtime-direct-image-search-20260910` | Exited (0) |

实时查看当前线上与所有回滚点：

```bash
docker inspect fm-cosmobot --format '{{.Config.Image}}'
docker ps -a --filter name=fm-cosmobot --format '{{.Names}} | {{.Image}} | {{.Status}}'
```

## 手动回滚（已验证的容器级步骤）

```bash
TARGET=fm-cosmobot-prev-seedream-20260912-055215   # 换成上表里要回到的那一行
STAMP=$(date +%Y%m%d-%H%M%S)

docker stop fm-cosmobot
docker rename fm-cosmobot "fm-cosmobot-rolled-back-$STAMP"   # 保留现场，不要删
docker rename "$TARGET" fm-cosmobot
docker start fm-cosmobot
sleep 20
docker inspect fm-cosmobot --format 'image={{.Config.Image}} running={{.State.Running}} restarts={{.RestartCount}}'
docker logs --tail 20 fm-cosmobot
```

回滚的容器与线上共用同一份挂载（`/opt/fm-cosmobot/runtime:/data`），
所以配置、数据库、记忆目录都会跟着走，不需要额外恢复。

## 回滚后验证

```bash
docker inspect fm-cosmobot --format 'running={{.State.Running}} restarts={{.RestartCount}}'
docker exec fm-cosmobot md5sum /opt/cosmobot/cosmobot
docker logs --tail 80 fm-cosmobot | grep -iE 'error|failed'
```

然后确认 QQ 群、私聊和 Matrix 房间的基本消息链路。容器 healthy 不等于消息能送达。

## 自动回滚

`ops/deploy_production.sh`、`ops/deploy_prefix_tmp.sh` 里的自动回滚属于
historic compose 流水线（见上），当前生产不走它们。如果需要自动化回滚脚本，
以 `docs/` 与 `AGENTS.md` 的 Deployment And Runtime 一节为准。

失败后先收集现场：

```bash
docker ps -a --filter name=fm-cosmobot
docker logs --tail 200 fm-cosmobot
ls -la /opt/fm-cosmobot/tool-output/ | tail -20
```

## 配置回滚

应用配置回滚使用后台的 `POST /api/runtime/config/rollback`，它只恢复目标配置范围；
如果 RPC 不可达或无法确认快照，接口返回失败并保留当前配置，不应手工覆盖无关配置。

不要删除 `/opt/fm-admin/data`、`/opt/fm-domain/data`、`/opt/fm-cosmobot/runtime`
或 `/opt/fm-cosmobot/backups`。

## 回滚后记录

- 记录故障开始时间、触发条件、旧/新镜像标签和镜像 ID；
- 保留构建、容器和 Admin 最近错误日志；
- 记录验证脚本结果和受影响的业务链路；
- 修复原因并通过完整回归后，才能重新发布。
