# FM 部署回滚操作

## 触发条件

出现以下任一情况，应暂停继续发布并评估回滚：容器不健康、RPC/Domain 无法连接、配置无法生效、关键消息链路失败、媒体发送异常、任务持续失败或日志出现致命错误。

## 自动回滚

使用 `ops/deploy_production.sh` 发布时，镜像切换后的健康检查或命令失败会触发脚本的回滚处理。脚本会：

1. 删除候选容器；
2. 将上一次稳定镜像 ID 重新标记到 `FM_STABLE_BOT_IMAGE` 和 `FM_STABLE_DOMAIN_IMAGE`；
3. 强制重建 Domain 和 Cosmobot 容器；
4. 保留失败构建日志，退出非零状态。

部署失败后执行：

```bash
bash ops/verify_production.sh
docker compose -f /opt/fm-domain/compose.yaml ps
docker compose -f /opt/fm-cosmobot/compose.yaml ps
```

确认旧版本健康后，再检查测试群、私聊和 Matrix 房间的基本消息链路。

## 手动回滚

如果自动回滚未能完成，先从部署 manifest 或上一次发布记录中找到旧镜像标签，然后执行：

```bash
export FM_STABLE_BOT_IMAGE=fm-cosmobot:runtime-fm-tools
export FM_STABLE_DOMAIN_IMAGE=fm-domain:local
docker compose -f /opt/fm-domain/compose.yaml up -d --force-recreate fm-domain
docker compose -f /opt/fm-cosmobot/compose.yaml up -d --force-recreate fm-cosmobot
bash /opt/fm-cosmobot/source/ops/verify_production.sh
```

不要删除 `/opt/fm-admin/data`、`/opt/fm-domain/data`、运行目录或备份。应用配置回滚使用后台的 `POST /api/runtime/config/rollback`，它只恢复目标配置范围；如果 RPC 不可达或无法确认快照，接口返回失败并保留当前配置，不应手工覆盖无关配置。

## 回滚后记录

- 记录故障开始时间、触发条件、旧/新镜像标签和镜像 ID；
- 保留构建、容器和 Admin 最近错误日志；
- 记录验证脚本结果和受影响的业务链路；
- 修复原因并通过完整回归后，才能重新发布。
