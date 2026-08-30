# FM Control Center

FM Control Center 是 FM 的管理后台第一版，使用 Python 提供管理 API 和静态网页；Cosmobot RPC 接入使用固定版本的 `websocket-client`。

## 本地运行

```bash
FM_ADMIN_STATE=/tmp/fm-admin-state.json \
FM_DOMAIN_URL=http://127.0.0.1:8077 \
FM_RPC_ENABLED=false \
FM_ADMIN_PORT=8090 \
python3 admin/app.py
```

打开 <http://127.0.0.1:8090>。

## 生产运行

生产环境必须设置 `FM_ADMIN_TOKEN`，并通过 HTTPS 反向代理访问：

```bash
FM_ADMIN_HOST=127.0.0.1 \
FM_ADMIN_TOKEN='use-a-long-random-token' \
FM_ADMIN_STATE=/data/fm-admin-state.json \
FM_DOMAIN_URL=http://fm-domain:8077 \
FM_RPC_ENABLED=true \
FM_RPC_HOST=fm-cosmobot \
FM_RPC_PORT=38765 \
FM_RPC_TOKEN='cosmobot-rpc-token' \
python3 admin/app.py
```

后台当前提供：

- FM Domain 健康、统计和归档状态总览；
- 文库、赛文、成绩和归档数据的真实只读查询接口；
- 触发规则、人设、模型和群配置的 JSON CRUD；
- 配置修改审计日志；
- Cosmobot RPC 的工具审计、媒体缓存和后台任务只读状态；
- 同源网页控制台。

控制平面配置与 FM Domain 业务数据库分开保存。当前版本不会直接修改 Cosmobot 的生产配置；运行时接入应通过受认证的 RPC 或管理 API 完成，并保留配置版本和回滚能力。

## API

所有 `/api/*` 请求在设置 `FM_ADMIN_TOKEN` 后都需要携带：

```http
X-FM-Admin-Token: <token>
```

资源接口：

```text
GET    /api/collections/{groups|triggers|personas|models}
GET    /api/collections/{resource}/{id}
POST   /api/collections/{resource}
PUT    /api/collections/{resource}/{id}
DELETE /api/collections/{resource}/{id}
```

领域数据只读接口：

```text
GET /api/domain/library
GET /api/domain/contests
GET /api/domain/scores
GET /api/domain/stats
GET /api/domain/archive
```

生产容器模板见 `deploy/fm-admin.compose.yaml`。它与 `fm-domain` 位于同一
Docker 网络，必须设置 `FM_ADMIN_TOKEN`，并通过外部 Nginx 或其他 HTTPS
反向代理对外提供访问。要启用 Cosmobot 运行时状态，还要设置
`FM_RPC_ENABLED=true` 和 `FM_RPC_TOKEN`；RPC 不可用时相关接口返回 `502`，
不会影响 FM Domain 页面。

Cosmobot RPC 只读接口：

```text
GET /api/runtime/audit
GET /api/runtime/media
GET /api/runtime/concurrency
```

这些接口分别调用 `audit.recent`、`media.stats` 和 `concurrency.list`。
后台只做代理和错误归一化，不保存或回显 RPC token，也不提供运行时写操作。

## 后续接入顺序

1. 将触发规则、人设、模型和群配置的 CRUD 接到 FM 实际配置存储。
2. 增加 SSE 实时状态和消息链路事件。
3. 接入 FM Domain 的文库、赛文、成绩和报表管理。
