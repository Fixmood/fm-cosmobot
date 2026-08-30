# FM Control Center

FM Control Center 是 FM 的管理后台第一版，使用 Python 标准库提供管理 API 和静态网页，不新增第三方运行时依赖。

## 本地运行

```bash
FM_ADMIN_STATE=/tmp/fm-admin-state.json \
FM_DOMAIN_URL=http://127.0.0.1:8077 \
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
python3 admin/app.py
```

后台当前提供：

- FM Domain 健康、统计和归档状态总览；
- 触发规则、人设、模型和群配置的 JSON CRUD；
- 配置修改审计日志；
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

## 后续接入顺序

1. 通过 Cosmobot RPC 接入工具审计、会话状态和任务控制。
2. 将触发规则、人设、模型和群配置的 CRUD 接到 FM 实际配置存储。
3. 增加 SSE 实时状态和消息链路事件。
4. 接入 FM Domain 的文库、赛文、成绩和报表管理。
