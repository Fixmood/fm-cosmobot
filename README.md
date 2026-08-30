# FM

FM 是基于 Cosmobot 的多平台智能机器人实现，面向 QQ 和 Matrix 提供对话、上下文记忆、中文跟打训练、赛事数据、媒体处理、消息桥接和运行管理能力。

本仓库包含 FM 使用的 Cosmobot 运行时、领域服务、Agent 工具、测试和部署脚本。生产凭据、用户数据、聊天记录、成绩库、媒体缓存和数据库由部署环境管理，不提交到仓库。

## 能力概览

### 对话与 Agent

- 支持 QQ 群、QQ 私聊和 Matrix 房间。
- 按平台、会话和用户隔离上下文、记忆、权限及个性化设置。
- 支持 OpenAI 兼容的聊天、图像和音频模型。
- 支持模型添加、编辑、删除、切换和默认模型恢复。
- 支持网页检索、文件处理、图片分析与生成、语音生成、定时任务和后台任务。
- 提供工具调用审计、结果压缩、任务续行和受控执行环境。

### 跟打与赛文

- 从练习文库按长度、关键词、题材和计算难度选择文章。
- 对文章正文执行清洗、去重、索引、难度计算和题材/文体分类。
- 支持连续发文、续段、恢复上一篇、停止、撤回、顺序单字和乱序单字练习。
- 获取并保存历史赛文，定时同步每日赛文并补齐缺失日期。
- 支持历史赛文独立练习会话及成绩提交后的续发。
- 收录普通跟打、公开赛事和每日 555 赛文成绩。
- 提供排行榜、历史查询、成绩汇总和个人成绩趋势图。

### 消息与管理

- 将不同平台消息标准化，并支持 QQ 与 Matrix 之间的桥接。
- 支持长消息分段、QQ 合并转发、媒体发送和消息撤回。
- 支持明确授权下的传话、跨会话转发和双向对话接管。
- 支持群能力、房间访问、触发规则、循环保护和服务状态管理。
- 支持全局默认人设、群级人设、私聊人设和用户级回复风格覆盖。

### Web 管理后台

- `admin/` 提供 FM Control Center 的无依赖管理 API 和网页界面。
- 总览页读取 FM Domain 的健康、统计和归档状态。
- 管理 API 提供文库、赛文、成绩和归档数据的真实只读查询。
- 支持触发规则、人设、模型和群配置的增删改查，并记录操作审计。
- 后台控制平面与 FM Domain 业务数据库分开保存；生产运行时接入通过受认证 API 或 RPC 完成。

## 工具系统

FM 默认注册 83 个聊天 Agent 工具，覆盖模型、消息、文库、赛事、桥接、媒体、记忆、执行环境和后台任务；ACP 客户端另有 3 个专用工具。

工具是否可用取决于运行配置、平台、会话类型、消息意图、群能力和管理员权限。完整工具名称、用途和注册范围见 [`docs/FM_TOOLS.md`](docs/FM_TOOLS.md)。

## 架构

```text
QQ / Matrix
    |
    v
平台驱动 -> 标准消息 -> 路由与处理器 -> Agent 运行时
                                      |       |       |
                                      v       v       v
                                     LLM     工具    记忆
                                              |
                                              v
                                        FM 领域服务
                                   文库 / 成绩 / 赛事 / 报表
```

主要模块：

| 路径 | 职责 |
| --- | --- |
| `cosmobot/lib/Bot/Core/` | 平台无关的消息、路由、会话和回复类型 |
| `cosmobot/lib/Bot/Chat/Driver/` | QQ、Matrix 等平台驱动与消息标准化 |
| `cosmobot/lib/Bot/Handler/` | 用户可见行为、命令和准入策略 |
| `cosmobot/lib/Bot/Agent/` | Agent 循环、中间件、工具和审计 |
| `cosmobot/lib/Bot/Effect/` | 运行时能力接口及解释器边界 |
| `cosmobot/lib/Bot/LLM/` | OpenAI 兼容模型配置与传输 |
| `cosmobot/lib/Bot/Storage/` | SQLite 等持久化存储 |
| `fm-domain/` | 文库、赛文、成绩、排行榜和报表服务 |
| `admin/` | FM Control Center 管理 API、网页和测试 |
| `deploy/fm-admin.compose.yaml` | FM 管理后台容器模板 |
| `ops/` | 同步、回归检查和生产部署脚本 |
| `docs/` | 工具参考、部署和运维说明 |

## 环境要求

- GHC 9.10.3
- Cabal 3.14.2.0 或兼容版本
- Python 3
- Linux 生产环境
- Docker Compose v2
- 启用沙盒时需要 `bubblewrap`
- QQ/OneBot、Matrix 和 OpenAI 兼容模型服务
- 启用跟打与赛事能力时需要 FM Domain 服务

Typst、ImageMagick、对象存储、搜索服务以及图像/音频模型仅在启用对应功能时需要。

## 构建与测试

```bash
cabal update
cabal build -j all --enable-tests --enable-benchmarks
cabal test -j all --test-options=--hide-successes
python3 -m unittest discover -s fm-domain -p 'test_*.py' -v
```

生产发布前执行：

```bash
bash ops/fm_regression.sh
bash ops/deploy_production.sh
bash ops/verify_production.sh
```

部署脚本会检查源码状态和凭据特征，构建并测试候选镜像，验证服务健康状态和镜像版本，并在验证失败时恢复上一版本。容器健康不等于消息链路验证通过，发布后仍应在测试 QQ 群和 Matrix 房间验证实际流程。

## 配置与数据边界

以 [`cosmobot/config.example.toml`](cosmobot/config.example.toml) 为配置起点。运行配置按需包含：

- QQ/OneBot 和 Matrix 连接信息；
- 聊天、图像和音频模型；
- 用户权限、管理员和会话访问范围；
- 媒体存储、公开访问地址、记忆、Skill、沙盒和定时任务；
- 搜索及其他第三方服务。

以下内容不得提交到 Git：

- API Key、Token、密码、Cookie、代理订阅和私钥；
- 生产配置、SQLite 数据库、运行日志和媒体缓存；
- 消息历史、用户记忆、成绩数据和其他用户数据。

生产数据通过服务器持久化目录和容器挂载管理，源码仓库只保存可复现的程序、模板、测试和部署逻辑。

## 开发约定

- 遵循 [`AGENTS.md`](AGENTS.md) 中的模块边界和实现约定。
- 跨平台或用户状态必须使用完整的平台、会话和用户身份键隔离。
- 功能修改应添加与影响范围匹配的回归测试。
- 提交前运行 `git diff --check` 和相关测试。
- 需要协作审核的修改使用独立分支和 Pull Request。

## 上游

FM 使用 [Cosmobot](https://github.com/ksqsf/cosmobot) 的运行时架构，并在平台适配、Agent 工具、中文跟打、赛事数据、消息桥接和运维流程上提供定制实现。

引入上游更新前，应评估模块差异、第三方依赖和授权条件，并完成构建、回归和生产流程验证。
