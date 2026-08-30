# Cosmobot RPC Protocol

Cosmobot can expose a local WebSocket RPC endpoint from `cosmobot serve`.
The RPC service is intended for local chat, audit inspection, and CLI queries
against the running daemon. The wire envelope is JSON-RPC 2.0 over WebSocket,
using the Haskell `jsonrpc` package for protocol types.

## Configuration

RPC is configured under `[rpc]` in `config.toml`:

```toml
[rpc]
enabled = false
host = "127.0.0.1"
port = 38765
token = ""
```

`enabled` defaults to `false`. When `enabled = true`, `token` must be non-empty.
The default host is loopback-only. Cosmobot serves the WebSocket RPC endpoint at
`/rpc`. Uploaded media is stored in the shared media cache; RPC responses return
the public URL produced by the media interpreter.

## Authentication

Clients authenticate with `Authorization: Bearer TOKEN` during the WebSocket
handshake. Query-string tokens are not accepted for WebSocket authentication.
Unauthorized WebSocket connections are rejected with HTTP 401.

## Envelopes

Requests are JSON-RPC 2.0 objects with `jsonrpc: "2.0"`, `id`, `method`, and
optional `params`:

```json
{"jsonrpc":"2.0","id":"1","method":"audit.recent","params":{"limit":20}}
```

Successful responses include `result`:

```json
{"jsonrpc":"2.0","id":"1","result":[]}
```

Failed responses include `error`:

```json
{"jsonrpc":"2.0","id":"1","error":{"code":-32601,"message":"Unknown RPC method: x","data":{"code":"method_not_found"}}}
```

Notifications have no `id`:

```json
{"jsonrpc":"2.0","method":"audit.event","params":{}}
```

Standard JSON-RPC numeric error codes are used. Cosmobot's stable textual error
code is preserved in `error.data.code` where applicable.

## Audit Methods

### `audit.recent`

Returns recent audit records. `limit` is optional and defaults to `20`.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.recent","params":{"limit":50}}
```

### `audit.get`

Returns one audit record by audit id. The preferred parameter is `audit_id`;
`id` is also accepted.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.get","params":{"audit_id":123}}
```

### `audit.thread`

Returns audit records associated with one platform/chat-scoped message id.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.thread","params":{"platform":"telegram","chat_id":42,"message_id":"7"}}
```

### `audit.thread_messages`

Returns audit records associated with multiple message ids in one platform/chat.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.thread_messages","params":{"platform":"telegram","chat_id":42,"message_ids":["m1","m2"]}}
```

### `audit.subscribe`

Acknowledges audit live updates. Current broadcasts are global to connected RPC
clients; this method does not create per-client subscription state yet.

```json
{"jsonrpc":"2.0","id":"1","method":"audit.subscribe","params":{}}
```

Result:

```json
{"subscribed":true}
```

Persisted audit records are broadcast as `audit.event` notifications:

```json
{"jsonrpc":"2.0","method":"audit.event","params":{ /* AgentAuditRecord JSON */ }}
```

Live audit events report newly persisted records. Query methods keep the normal
audit storage behavior, including stale running-tool marking.

## Manager Methods

Possession of the RPC bearer token grants superuser resource access. Resource
methods can therefore inspect or modify resources owned through any chat
platform.

### Concurrency

| Method | Parameters | Result |
|---|---|---|
| `concurrency.list` | none | `entries`: all known tasks |
| `concurrency.lookup` | `id` | `entry`: the task or `null` |
| `concurrency.cancel` | `id` | `id` and `cancelled` |
| `concurrency.await` | `id` | waits for completion, then returns `id` and `awaited` |

Task entries contain `id`, `label`, `status`, `error`, `startedAt`, and
`finishedAt`. `status` is one of `running`, `completed`, `failed`, or
`cancelled`; `error` is populated only for failed tasks.

```json
{"jsonrpc":"2.0","id":"1","method":"concurrency.lookup","params":{"id":42}}
```

### Resources

| Method | Parameters | Result |
|---|---|---|
| `resource.list` | none | `resources`: all available resources |
| `resource.detail` | `id` | `id` and `detail` |
| `resource.destroy` | `id` | `id` and `destroyed` |
| `resource.rename` | `id`, `newId` | the renamed `id` |
| `resource.keep_alive` | `id` | `id` and `refreshed` |
| `resource.make_permanent` | `id` | `id` and `permanent` |
| `resource.destroy_associated` | concurrency `id` | per-resource cleanup `results` |

`resource.rename` also accepts `new_id`. Resource list entries contain `id`,
`type`, `sessionId`, `description`, `probe`, and `remainingLifeMinutes`.
`remainingLifeMinutes` is `null` for permanent resources. `probe` contains an
`ok` boolean and either `result` or `error`.

```json
{"jsonrpc":"2.0","id":"2","method":"resource.keep_alive","params":{"id":"sandbox-1"}}
```

Resource failures use textual error codes such as `not_found`,
`invalid_params`, `already_exists`, `unavailable`, and `resource_error` in
`error.data.code`.

## Chat Methods

RPC chat is exposed to the bot as the virtual `PlatformRPC` platform. Incoming
messages use the normal route, agent, memory, chat-log, and audit path.

RPC chat sessions are private chats. The RPC sender is allowed and treated as a
superuser because possession of the RPC token is the authorization boundary.

### `chat.open_session`

Creates a virtual chat session. `label` is optional.

```json
{"jsonrpc":"2.0","id":"1","method":"chat.open_session","params":{"label":"local"}}
```

Result:

```json
{"sessionId":"local-1"}
```

Blank labels are ignored and use the base name `session`.

### `chat.send`

Injects a user message into the virtual RPC chat session.

```json
{
  "id": "2",
  "jsonrpc": "2.0",
  "method": "chat.send",
  "params": {
    "sessionId": "local-1",
    "text": "!ask hello",
    "imageUrls": [],
    "attachments": [],
    "replyToMessageId": null
  }
}
```

Accepted aliases:

- `sessionId` or `session_id`
- `imageUrls` or `image_urls`
- `replyToMessageId` or `reply_to_message_id`

Uploaded image attachments are also exposed to handlers as
`IncomingMessage.imageUrls`. Audio and file attachments remain in message
history as `attachments` and are summarized in the RPC message context passed to
handlers.

Result:

```json
{"sessionId":"local-1","messageId":"rpc-1"}
```

The sent user message is also broadcast as a `chat.message` notification with
`sender: "user"`.

Sending to a session id that does not exist fails with textual error code
`not_found`; no message is persisted or broadcast.

### `chat.fork`

Creates a new session whose history starts from a message in an existing
session. The fork stores `parentSessionId` and `parentMessageId` and reads parent
history immutably through that message.

### `chat.delete_session`

Deletes a session and its stored messages. If other sessions were forked from
the deleted session, deletion cascades to those descendant sessions and their
messages because forked sessions depend on parent history.

### `chat.upload_attachment`

Stores an RPC attachment before sending it in chat:

```json
{
  "id": "2",
  "jsonrpc": "2.0",
  "method": "chat.upload_attachment",
  "params": {
    "name": "notes.txt",
    "mediaType": "text/plain",
    "kind": "file",
    "size": 5,
    "data": "aGVsbG8="
  }
}
```

`data` is base64 without a data-URL prefix. The decoded byte length must match
`size` when `size` is provided. Uploaded bytes are stored in the shared media
cache and the returned `attachmentId` is a `media:<file_id>` reference.

Result:

```json
{"id":"media:mf_abc","attachmentId":"media:mf_abc","mediaRef":"media:mf_abc","fileId":"mf_abc","name":"notes.txt","mediaType":"text/plain","kind":"file","size":5,"url":"https://media.example.com/cosmobot-media/sha256.png"}
```

### `media.stats`

Returns media cache counts and a bounded list of media files. `limit` defaults
to `50`.

```json
{"jsonrpc":"2.0","id":"3","method":"media.stats","params":{"limit":20}}
```

### `media.resolve_source`

Looks up a media cache entry by source id and returns its `media:<file_id>`
reference when known.

```json
{"jsonrpc":"2.0","id":"4","method":"media.resolve_source","params":{"sourceRef":"telegram:file-123"}}
```

Result:

```json
{"sourceRef":"telegram:file-123","mediaId":"media:mf_abc","fileId":"mf_abc"}
```

### `media.get`

Returns one cached media entry by `mediaId` or `fileId`, including source refs,
platform refs, public URL, and local cache path.

```json
{"jsonrpc":"2.0","id":"5","method":"media.get","params":{"mediaId":"media:mf_abc"}}
```

### `media.delete`

Deletes one media id, its source/platform refs, and its local cached file when
the file is not shared by another media row.

```json
{"jsonrpc":"2.0","id":"6","method":"media.delete","params":{"mediaId":"media:mf_abc"}}
```

### `media.gc`

Runs media cache GC manually. `maxAgeSeconds` defaults to `0`. Media file ids
referenced by RPC chat history are retained even if they are older than the GC
cutoff.

```json
{"jsonrpc":"2.0","id":"4","method":"media.gc","params":{"maxAgeSeconds":604800}}
```

## Chat Notifications

Bot replies are sent as `chat.message` notifications:

```json
{
  "method": "chat.message",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "local-1",
    "messageId": "rpc-2",
    "text": "reply text"
  }
}
```

Editable reply stream updates are sent as `chat.message_update`:

```json
{
  "method": "chat.message_update",
  "jsonrpc": "2.0",
  "params": {
    "sessionId": "local-1",
    "messageId": "rpc-2",
    "text": "updated reply text"
  }
}
```

User-originated chat notifications include `sender`, `imageUrls`, and
`replyToMessageId` fields. Bot-originated reply notifications currently include
`sessionId`, `messageId`, and `text`.

## CLI

The CLI reads `[rpc]` from `config.toml` and connects to the running daemon:

```sh
cosmobot rpc audit recent --limit 20
cosmobot rpc audit show 123
cosmobot rpc audit thread MESSAGE_ID
cosmobot rpc media stats --limit 20
cosmobot rpc media resolve-source SOURCE_REF
cosmobot rpc media get MEDIA_ID_OR_FILE_ID
cosmobot rpc media delete MEDIA_ID_OR_FILE_ID
cosmobot rpc media gc --max-age-seconds 604800
cosmobot rpc call METHOD JSON
```

Use `--host`, `--port`, or `--token` after `rpc` to override the `[rpc]`
settings from the config file:

```sh
cosmobot rpc --host 127.0.0.1 --port 38765 --token "$TOKEN" media stats
```

Responses are printed as pretty JSON.

Use `cosmobot rpc --config FILE ...` to read a config file other than
`config.toml`.

Uploaded media bytes are not served by the RPC HTTP app. Configure
`[media].public_base_url` and optional `[media.s3]` settings when RPC clients
need dereferenceable public URLs.

## Limitations

All connected RPC clients currently receive broadcast notifications. Clients
that only need one chat should filter messages by session id.

The virtual RPC chat driver supports replies, streamed reply edits, audio/file
fallback text, and mentions. It does not support deleting messages, fetching old
message content, member lookups, avatars, group member listing, or member title
changes.
