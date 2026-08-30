# FM 工具参考

本文档记录 `Bot.Agent.Tools.defaultToolsWith` 注册的 83 个聊天 Agent 工具，以及 3 个仅在 ACP 会话中提供的工具。

工具是否对某条消息可用，由运行配置、工具标签、平台、会话类型、显式意图、群能力和管理员权限共同决定。工具出现在注册表中，不代表所有用户或所有消息均可调用。

## 工具控制与触发

| 工具 | 用途 |
| --- | --- |
| `tool_enable` | 为当前 Agent 线程启用额外工具标签。 |
| `trigger_manage` | 查看、设置、增加、移除或重置当前及指定会话的触发方式。 |

## 仓库、模型与配额

| 工具 | 用途 |
| --- | --- |
| `fm_repository_pr` | 查看 FM 仓库状态，创建工作分支，或在授权后提交 Pull Request。 |
| `account_balance` | 查询 DeepSeek、WeiLai、BotCF 和 Tavily 的余额或配额。 |
| `chat_model_manage` | 统一执行模型状态、添加、编辑、删除、切换和重置操作。 |
| `chat_model_add` | 验证并新增 OpenAI 兼容模型配置。 |
| `chat_model_edit` | 验证并修改已有模型配置。 |
| `chat_model_delete` | 删除非当前使用的模型配置。 |
| `chat_model_switch` | 验证目标端点后切换全局聊天模型。 |
| `chat_model_reset` | 恢复配置文件指定的默认聊天模型。 |
| `chat_model_status` | 查看当前模型和全部已配置模型。 |

## 消息、成员与检索

| 工具 | 用途 |
| --- | --- |
| `chat_log` | 查询当前会话的近期消息，可按发送者过滤并分页。 |
| `sender_log` | 按关键词查询当前发送者在当前会话或当前平台的历史消息。 |
| `recall_recent_self_messages` | 撤回 FM 在当前 QQ 会话两分钟内发送的消息。 |
| `send_reply` | 在当前会话发送额外文本或网络图片。 |
| `send_file` | 向当前会话上传并发送本地文件。 |
| `mention_user` | 在当前会话发送带平台用户提及的消息。 |
| `sender_info` | 获取当前群聊发送者的平台成员资料。 |
| `member_info` | 获取当前群聊中指定用户的平台成员资料。 |
| `user_avatar` | 获取指定用户头像并发送到当前会话。 |
| `group_members` | 列出当前群聊成员及平台身份。 |
| `message_info` | 返回当前消息的平台、会话、发送者、引用、媒体和正文元数据。 |

## 网络、时间与 Matrix

| 工具 | 用途 |
| --- | --- |
| `search_web` | 使用已配置搜索服务检索网页及可用图片地址。 |
| `fetch_url` | 获取 HTTP 或 HTTPS 页面并提取可读正文。 |
| `now` | 返回 UTC 和机器人本地时区的当前时间。 |
| `matrix_request` | 调用带机器人身份认证的 Matrix Client-Server API。 |

## 文库与发文

| 工具 | 用途 |
| --- | --- |
| `fm_group_status` | 查看 FM 已登记群及其能力状态。 |
| `fm_library_search` | 按标题、主题、题材或正文关键词搜索练习文库，并返回已分类的难度、题材和文体。 |
| `fm_library_pick` | 从练习文库选择一篇符合条件的完整文章，并返回整篇分类元数据。 |
| `fm_library_start` | 创建持续发文会话并发送首段，可指定难度、题材和长度；发文结果包含整篇难度、题材、文体和置信度。 |
| `fm_library_continue` | 沿用当前随机或难度模式抽取并发送下一篇文章。 |
| `fm_library_continue_same` | 继续发送当前文章的下一段。 |
| `fm_library_continue_previous` | 撤回当前新文并恢复成绩提交前的上一篇文章。 |
| `fm_library_recall_recent` | 撤回当前 QQ 会话内近期发送的练习文章。 |
| `fm_library_stop` | 停止当前发文会话并尝试撤回最近一段。 |
| `fm_library_stats` | 查看文库规模、难度索引和活动会话统计。 |

文库文章分类在领域服务内完成，不调用聊天模型：分类器读取标题和正文信号，保存主题材、多标签题材、文体、全文难度分数、置信度、证据和分类器版本。发文头部会显示 `难度分数·题材·文体`；工具结果同时返回全文分类和当前段难度。指定题材或难度时，候选筛选使用已保存的全文分类，自动续文会沿用原题材与难度模式。

首次导入或新增文库后，可在领域服务所在环境预先建立分类索引：

```bash
python3 fm-domain/app.py classify-library --db /data/fm-domain.sqlite3
```

分类规则更新后会自动通过分类器版本重新处理旧文章；未预分类的文章也会在首次搜索或发文时按需分类。
| `fm_recall_query` | 查询已捕获的 QQ 撤回消息原文。 |
| `fm_score_query` | 按 QQ 群或发送者查询已归档跟打成绩。 |
| `fm_group_set_online` | 设置 FM 在指定 QQ 群上线或下线。 |
| `fm_group_set_capability` | 启用或停用指定 QQ 群的一项 FM 能力。 |

## 赛文、成绩与报表

| 工具 | 用途 |
| --- | --- |
| `fm_contest_search` | 按来源、日期、标题、主题或正文搜索历史赛文库。 |
| `fm_contest_send` | 发送一篇完整历史赛文并建立可续发会话。 |
| `fm_live_competition_rank` | 查询并发送当前或指定日期的公开赛事排行榜图片。 |
| `fm_live_competition_text` | 获取并发送支持来源的公开实时赛文正文。 |
| `fm_ai_contest_text` | 查询已保存的每日 555 AI 赛文。 |
| `fm_ai_contest_publish` | 创建或复用每日 555 AI 赛文并发送。 |
| `fm_ai_contest_leaderboard` | 查询指定日期的 555 赛文排行榜数据。 |
| `fm_ai_contest_leaderboard_image` | 生成并发送指定日期的 555 赛文排行榜图片。 |
| `fm_competition_score_query` | 按 QQ、姓名或赛事来源查询个人赛事成绩。 |
| `fm_competition_score_summary` | 汇总个人赛事成绩的次数、最佳值和平均值。 |
| `fm_competition_score_image` | 生成并发送个人赛事历史成绩图。 |
| `fm_chart` | 根据个人赛事成绩生成高分辨率趋势图，支持姓名、QQ号、赛事来源、天数和指标筛选。 |
| `fm_bot_guard_accounts` | 管理参与机器人循环保护的 QQ 机器人账号。 |
| `fm_domain_stats` | 查看文库、赛文、成绩、撤回记录和群数据数量。 |

## 桥接与接管

| 工具 | 用途 |
| --- | --- |
| `fm_bridge_status` | 查看 Matrix 到 QQ 桥接配置及当前消息是否使用桥接链路。 |
| `fm_bridge_manage` | 启用、停用或修改 Matrix 房间与 QQ 群映射。 |
| `fm_bridge_test` | 经真实桥接链路向 Matrix 与 QQ 发送端到端测试消息。 |
| `fm_relay_to_owner` | 在用户明确要求传话时，将单条消息真正发送到 FM 所有者的 QQ 私聊。 |
| `fm_relay_message` | 在用户明确要求传话时，按 QQ 号或当前 QQ 群内唯一昵称，将单条消息发送到其他人的 QQ 私聊。 |
| `fm_takeover_manage` | 启动、查看或停止 QQ/Matrix 双向对话接管。 |

## 媒体与输出

| 工具 | 用途 |
| --- | --- |
| `media_text` | 分段读取媒体缓存中的 UTF-8 文本对象。 |
| `media_to_file` | 将媒体缓存对象解析为现有本地文件路径。 |
| `send_media` | 向当前会话发送媒体缓存中的文件。 |
| `image_view` | 读取图片内容并提供给当前 Agent 分析。 |
| `image_generate` | 使用已配置图像模型生成图片并发送。 |
| `image_edit` | 使用已配置图像模型编辑、重绘或组合图片并发送。 |
| `audio_generate` | 生成并发送平台可播放的语音或音频消息。 |
| `typst_render` | 渲染完整 Typst 文档并发送结果。 |

## 记忆与人设

| 工具 | 用途 |
| --- | --- |
| `sender_memory` | 查看、替换或清除当前发送者的持久记忆。 |
| `chat_memory` | 查看当前会话共享记忆；修改和清除受管理员权限限制。 |
| `fm_private_persona` | 管理 QQ 私聊默认人设及用户级覆盖。 |
| `fm_group_persona` | 管理 QQ 默认群人设及群级覆盖。 |
| `fm_member_style` | 管理 QQ 群成员跨群生效的个人回复风格。 |

## 任务、执行与工作空间

| 工具 | 用途 |
| --- | --- |
| `schedule` | 创建、列出或删除当前用户在当前会话中的定时 Agent 任务。 |
| `load_skill` | 按名称加载一个已安装 Skill 的完整说明。 |
| `sandbox` | 管理隔离容器沙盒，并在其中执行命令或交换文件。 |
| `command` | 查询、等待或取消异步命令句柄。 |
| `run_bash` | 执行 Bash 脚本并返回输出。 |
| `py` | 在受限环境中执行一次性 Python 程序并编排多个工具调用。 |
| `workspace` | 管理用于代码、研究和运维任务的持久工作空间。 |
| `capture_continuation` | 捕获当前 Agent 上下文，建立一次性恢复点。 |
| `resume_continuation` | 携带结构化结果恢复到指定上下文捕获点。 |
| `subagent` | 创建和管理当前会话范围内的后台 Agent。 |
| `emacs_eval` | 在 Cosmobot 专用持久 Emacs 进程中执行 Emacs Lisp。 |

## ACP 专用工具

以下工具不属于普通聊天默认工具，只在连接 ACP 客户端时注册：

| 工具 | 用途 |
| --- | --- |
| `acp_read_client_file` | 读取 ACP 客户端工作区中的 UTF-8 文件。 |
| `acp_write_client_file` | 写入 ACP 客户端工作区中的 UTF-8 文件。 |
| `terminal` | 创建、读取、等待、终止或释放 ACP 客户端命令进程。 |
