# FM Runtime Configuration Map

This document defines the boundary for the fourth control-center phase. The
control-center collections are not runtime configuration and must not be shown
as applied until a Cosmobot configuration RPC reports successful application.

## Sources

| Configuration | Runtime source | Owner | Scope |
| --- | --- | --- | --- |
| QQ and Matrix trigger modes | `trigger-config.json` | `Bot.Trigger` | chat/platform key |
| Private personas | `memory/qq/private_persona/*.md` | `Bot.Memory` | QQ sender |
| Default private persona | `memory/qq/private_persona/_default.md` | `Bot.Memory` | all QQ private chats without override |
| Group personas | `memory/qq/group_persona/*.md` | `Bot.Memory` | QQ group |
| Default group persona | `memory/qq/group_persona/_default.md` | `Bot.Memory` | all QQ groups without override |
| Cross-group member styles | `memory/qq/member_style/*.md` | `Bot.Memory` | QQ sender |
| Chat model profiles | `config.toml` under `[llm.chat_provider.*]` | `Bot.LLM.OpenAI` | global |
| Active chat model | `chat-model-selection` | `Bot.LLM.OpenAI` | global |

## Required Control API

The control center must use an authenticated Cosmobot RPC adapter for these
operations:

- read and replace or clear a scoped memory document;
- list and update trigger configuration by platform and chat scope;
- list, add, edit, delete, switch, and reset chat model profiles;
- return `saved`, `applied`, and `active_version` separately.

The adapter must validate scope identity, preserve atomic writes, avoid
returning API keys, and report application failures without claiming success.
The existing `/api/collections/*` endpoints remain control-plane storage until
this adapter is complete.

## Current Gap

Cosmobot currently exposes RPC methods for audit, media, concurrency, and
resources, but no configuration method. The next implementation step is to add
the narrow `config.*` RPC family in Cosmobot and then map the admin CRUD routes
to it. Direct writes from the admin container to `config.toml`, memory files,
or `trigger-config.json` are intentionally out of scope because they bypass
Cosmobot's in-process locks, validation, and model selection state.
