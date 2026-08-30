# FM 生产部署检查清单

## 发布前

- [ ] 已确认发布提交、目标分支和变更范围；工作区无未提交或未跟踪文件。
- [ ] 已运行 `bash ops/check_secrets.py`，未发现 API Key、Token、密码、私钥或生产数据。
- [ ] 已运行 `bash ops/fm_regression.sh`；若本机没有 Cabal/GHC，已在 CI 或专用构建环境完成 Haskell 构建和测试。
- [ ] 已运行 `bash ops/stage7_verify.sh`；Docker 主机上已通过 Compose 语法和镜像构建检查。
- [ ] 已确认 `FM_ADMIN_TOKEN`、`FM_RPC_TOKEN` 使用独立高强度值，未写入仓库或日志。
- [ ] 已确认 Domain、Cosmobot 和 Admin 使用正确的持久化目录、网络和镜像标签。
- [ ] 已确认数据库、赛文文件、配置文件和媒体目录已有可恢复备份。

## 发布中

- [ ] 构建候选 Domain 与 Cosmobot 镜像，并记录提交 SHA、镜像 ID 和构建日志。
- [ ] 先启动候选服务，确认容器健康检查通过，再切换稳定标签。
- [ ] 使用 `ops/verify_production.sh` 检查容器健康、稳定镜像一致性、Domain `/health` 和最近致命日志。
- [ ] 在测试 QQ 群、QQ 私聊和 Matrix 房间分别验证触发、回复、赛文发文、成绩处理和媒体发送。
- [ ] 访问 Admin 总览，确认配置同步、RPC、Domain、任务和最近错误状态均符合预期。

## 发布后

- [ ] 保存部署 manifest、验证输出和发布时间。
- [ ] 观察至少一个完整业务周期，确认没有异常主动回复、工具误调用、媒体类型错误或任务中断。
- [ ] 若失败，停止继续切换，按 `docs/ROLLBACK.md` 回滚并保留失败日志。

生产验证脚本只读检查当前容器和日志，不会替换镜像。部署脚本负责候选构建、切换和失败时恢复上一个稳定镜像。
