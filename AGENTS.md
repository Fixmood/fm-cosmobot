You are a Haskell engineer working on cosmobot. Favor correctness, explicit data flow, small algebraic modules, and abstractions that make the code clearer in practice.

This repo hosts two Cabal packages, cosmobot and cosmocode.

## Cosmobot

Placed under ./cosmobot.

### Architecture Rules

- Preserve the dependency direction:
  `platform event -> core message -> route -> handler -> effects -> interpreter/concrete capability`.
- Handlers own user-visible policy. They may call effects, but must not perform platform transport, SQLite/Selda work, LLM HTTP work, or local process execution directly.
- Concrete integrations stay behind interpreters or infrastructure modules: chat drivers, storage modules, LLM transport, memory files, and `Bot.System.*`.
- `app/Main.hs` is the composition root. Keep it declarative: read config, create stores, install interpreters, start drivers, register routes, connect streams.

### Module Ownership

- `Bot.Core.*`: platform-neutral vocabulary: messages, routes, reply bodies, pure conversation/history/tree values. No QQ/Telegram/Matrix/Discord, SQLite, Selda, LLM transport, or process details.
- `Bot.Handler.*`: user-facing command and conversation flows.
- `Bot.Effect.*`: narrow capability facades only.
- `Bot.Chat.Driver.*`: platform APIs and normalized `IncomingMessage` construction.
- `Bot.Chat.*`: shared chat-domain helpers such as reply streaming types/logic.
- `Bot.Agent.*`: agent loop, agent tools, and middleware. Tools belong in `Bot.Agent.Tools.*`, not handlers or routing.
- `Bot.AgentAudit.*`: audit event/domain/projection/storage behavior. User-facing audit commands stay in `Bot.Handler.Audit`.
- `Bot.LLM.*`: OpenAI-compatible config, request/response types, transport, retry, streaming protocol, and test LLM interpreters.
- `Bot.Scheduler.*`: scheduler domain state, pure queue logic, and interpreter runtime.
- `Bot.ChatLog.*`: chat-log domain records and normalization. `Bot.Storage.ChatLog` owns durable query mechanics.
- `Bot.Storage.*`: Selda tables, persistence rules, component-owned durable state, and SQLite interpreter wiring. `Bot.Effect.Storage` should remain only the storage capability for running Selda actions.
- `Bot.Memory`: persistent user/chat memory behavior.
- `Bot.System.*`: local executable or operating-system integrations such as Typst.
- `Bot.Config`: top-level assembly only. Concrete parsers belong beside their owner, e.g. `Bot.Chat.Driver.*.Config`, `Bot.Handler.*.Config`, `Bot.LLM.*.Config`, `Bot.Memory.Config`.

### Effect Facade Rules

Keep `Bot.Effect.*` modules boring:

- define the effect GADT and `DispatchOf`;
- expose smart constructors;
- expose small stream adapters that only send effect operations;
- re-export public domain types intentionally for compatibility;
- avoid storing real interpreters, persistence, projection logic, transport protocol code, or large state machines there.

Move larger code to its owner:

- pure domain/state/projection logic -> owning `Bot.*` domain module;
- durable tables and queries -> `Bot.Storage.*`;
- LLM wire behavior -> `Bot.LLM.*`;
- local process execution -> `Bot.System.*`;
- test interpreters -> beside the implementation family, such as `Bot.LLM.Test` or `Bot.System.Typst.Test`.

Avoid import cycles when extracting from effects. Prefer explicit callback records or narrower types over importing the facade from the extracted implementation.

### Agent Operations

When changing the agent loop or middleware:

- Start in `Bot.Agent.Core` only to change the generic calculus vocabulary: `Program`, `Step`, `AgentEvent`, `TurnState`, or `Runtime`. Keep the execution fold in `Bot.Agent` as direct event interpretation; do not add persistence, audit, media, chat logging, platform linking, or handler policy there.
- Keep `Program`, `Step`, `Runtime`, tools, and observers polymorphic in their carrier `m`. Specialize them to `Eff es` at application boundaries; do not recover an effect row from `m` with a type family.
- Start every agent through the higher-order `Bot.Effect.Agent` capability. Main agents and child agents use the same `Agent.withRun`; differences such as origin identity and resource ownership are interpreter-local metadata installed with `Agent.withAgentMetadata`, not fields on `RunAgent`, runner callbacks, or separate child-runner APIs.
- Treat a main agent as an ordinary root run, not as a resource-manager object or a separate runner kind. The default interpreter gives a root run its own origin id; callers may locally inject a `resourceOwner`. A managed child inherits the originating run id and uses its worker as `resourceOwner`; the `Agent` effect itself remains unaware of those policies.
- Add cross-cutting behavior as `Bot.Agent.Middleware.*`, then compose it in `Bot.Agent.defaultRuntime` or the handler-specific runtime assembly. Add new modules to `cosmobot.cabal`.
- Use `TurnState` only for state that must survive across model/tool turns. Middleware-private state belongs in lexical closures; typed dynamic context belongs in the middleware HList.
- Use the `Runtime context` HList for dynamic middleware environment passed from outer middleware to inner middleware. Use this for values like `ObservationContext`, `EventObservation`, and `ToolResultObservation`.
- Do not add general-purpose fields to `Context`. It is for per-message tool capabilities, permissions, input, and system context.
- Do not make tools return platform message ids through `ToolResult`. Tool-emitted chat messages should be captured by `Chat` interposition middleware.

Choose the hook by the behavior you need:

- Change the transcript sent to the LLM: implement `modelInputTranscript`.
- Wrap a whole run: implement `aroundAgentRun`.
- Wrap one model request/decision: implement `aroundModelTurn`.
- Wrap a whole tool phase after a tool request: implement `aroundToolTurn`.
- Wrap a single tool call: implement `aroundToolCall`.

Use the existing middleware contracts this way:

- For large tool results, use `withToolResultCompaction`. It stores the full result in media cache, keeps the immediate model view in `TurnState.nextModelTranscript`, leaves later canonical conversation state omitted, and provides `ToolResultObservation` to inner middleware.
- For lifecycle/audit events, use `withObservation`. It may read typed context, but it should not import media, storage, chat drivers, or concrete audit storage.
- For noisy tool announcements, use `withToolMessage`. It expects `ObservationContext` so audit ids can appear in progress messages.
- For platform messages emitted by tools, use `withLinkingToolEmittedMessagesToConversation` with a handler-owned sink. The handler knows the active conversation; the agent core does not.
- For chat-log recording of tool-emitted self messages, use `withRecordingToolSelfMessages`. Keep chat-log recording separate from conversation linking.

When changing tool-result or conversation persistence semantics:

- Keep full tool results available to the immediate next model turn if the current turn produced them.
- Ensure later turns and durable conversation rows see omitted tool results.
- Keep conversation storage boring: it should persist the conversation it receives, not know about tool-result media storage.
- Keep agent audit storage boring: it should persist the event it receives, not know about tool-result media storage.
- Put durable projection before storage, normally in agent middleware order.

For agent changes, add or update focused tests in `test/AgentSpec.hs` for:

- current-turn versus later-turn model input;
- durable conversation shape;
- audit event result shape;
- tool-emitted chat message linking;
- middleware ordering when context is provided by one middleware and consumed by another.

For changes to `Program`, `Step`, or their instances, add algebraic properties
to `test/ProgramSpec.hs`. Compare programs up to finite `Continues` steps,
inspect every generated `Visible` continuation, and keep these calculus
laws separate from middleware policy scenarios in `AgentSpec.hs`.

Put deterministic model/tool fault injection in `test/FailureSpec.hs`. Keep
fault scripts and synchronization probes test-only, and assert recovery,
cleanup, and tool-call transcript completeness together.

### Coding Rules

- For Haskell code changes, use the local `haskell` skill's fast-feedback workflow: keep `ghcid --outputfile .ghcid-errors` running when practical, read `.ghcid-errors` for concise type diagnostics, and avoid repeated full builds while iterating.
- Work in `Eff es`. Add `IOE :> es` only at real external boundaries.
- Haskell code should not read as imperative choreography. If correctness depends on remembering the order of acquire/use/release, register/use/unregister, insert/update/delete, or write/cleanup steps, extract the lifecycle into a bracket-style helper, domain operation, or small combinator that names and enforces the invariant.
- Prefer declarative data transformations and pure planning functions over interleaving traversal, mutation, and persistence. For multi-step storage changes, make a component-owned operation that expresses the whole state transition.
- Prefer `effectful` capabilities (`Concurrent`, `STM`, `MVar`, `IORef`, `Timeout`, `Process`, `FileSystem`) over raw `base` concurrency/process/file APIs.
- For filesystem work, prefer `Effectful.FileSystem` and `Effectful.FileSystem.IO*`. Do not import `System.Directory` or `System.IO` for ordinary file operations, temporary-file handling, handle closing, or byte-string reads/writes when an `effectful` operation exists.
- Do not import `Control.Exception` or `Control.Concurrent` for new code. Use `Effectful.Exception` via `Bot.Prelude` and effectful concurrency modules.
- Never catch async exceptions for classification/control flow. Use `trySync`, `catchSync`, and structured cleanup for ordinary failure recovery. `catchSync` never catches async exceptions, so do not write `catchSync` handlers that call `isAsyncException` or rethrow async exceptions.
- Use structured APIs: `aeson` for JSON, `Toml.Schema`/local parsers for TOML, and Selda via `Bot.Storage.Prelude` for queryable state.
- Do not add indirection for appearance. Add an abstraction only when it removes real duplication, isolates an external system, or gives a growing responsibility a clear home.
- Keep broad refactors separate from behavior changes unless the refactor is required to implement the behavior safely.
- Prefer composition over nested `$` chains. Write `foo . bar . baz $ xxx` instead of `foo $ bar $ baz $ xxx` when the composition form is clear.

### Concurrency Rules

- Use `Bot.Effect.Concurrency` for application-level background work. Modules should import it qualified and call plain API names such as `Concurrency.fork`, `Concurrency.fire`, `Concurrency.cancel`, `Concurrency.await`, `Concurrency.list`, and `Concurrency.lookup`; do not expose "resource" terminology from the concurrency API.
- Implement the concurrency interpreter with `Effectful.Concurrent.Async`, not raw `Control.Concurrent` APIs.
- Keep concurrency structured. Any thread started by the manager must be registered before it can run user action, and manager exit must cancel and await every live thread so no ghost threads remain.
- Treat acquisition of async handles as a lifecycle operation: mask the create/register/start sequence, and cancel any thread that was created if registration or start signalling fails.
- On normal manager exit, cancel and await live child threads. On exceptional manager exit, use `cancelWith` so the top-level exception is thrown into each live child, then await them.
- Do not swallow async exceptions to decide ordinary control flow. Use `finally`, `onException`, `bracket`, `mask`, and `trySync`/`catchSync` according to the intended lifecycle boundary.
- Add focused tests in `test/ConcurrencySpec.hs` for manager lifecycle changes: normal-exit cleanup, exceptional-exit propagation, cancellation, awaiting, and any new race-sensitive acquire/register/start behavior.

### Resource Rules

- Use `Bot.Resource` for in-memory long-running objects owned by a person in a chat. Keep `Bot.Effect.Resource` a boring facade and concrete integrations in their owning infrastructure modules.
- Scope resources by `(platform, chatId, senderId)` and store the creating agent run as `agentId`; reject operations when chat or sender identity is missing.
- Never let managed objects escape the manager. Use `Resource.withResource` so active concurrency handles are tracked for the callback and cleared with structured cleanup.
- Destruction must first make the object unavailable, cancel and await active users, then run object cleanup. Restore explicit removals after cleanup failure so callers can retry; manager shutdown continues past individual cleanup failures.
- Keep resource registrations out of `Bot.Effect.Concurrency`. Concurrency manages threads; `Bot.Resource` manages long-running objects and their cleanup.
- Add concrete `ResourceObject` instances only for resources that exist now; do not add speculative resource kinds.

### Identity And Persistence

- Do not conflate chat identity with sender identity.
- Person-scoped features normally key by `platform` and `senderId`.
- Conversation/room-scoped features normally key by `platform` and `chatId`.
- Message ids are not globally unique. Scope reply-indexed state by `platform` and chat identity, not by bare message id.
- If required identity is missing, reject clearly instead of guessing.
- Keep persistence keying rules close to the state they persist.

### Config Rules

- Driver settings live under `[driver.qq]`, `[driver.telegram]`, `[driver.matrix]`, and `[driver.discord]`.
- Handler settings live under `[handler.*]`.
- LLM settings live under `[llm]` and are parsed by `Bot.LLM.*.Config`.
- Driver access lists and superusers belong in each driver config.
- Do not reintroduce top-level `[qq]`, `[telegram]`, `[matrix]`, `[discord]`, `[saucenao]`, `[handlers.*]`, or handler-owned platform whitelist sections.
- When adding config, update the owner parser, `Bot.Config`, `config.example.toml`, and every runtime consumer.

### Change Guidelines

- Handler changes: start from route admission in `Bot.Core.Route`; compose predicates/combinators instead of duplicating admission logic. Use the existing `forkEff` pattern for LLM/platform work that should not block incoming stream consumption.
- Platform changes: keep API details in the relevant driver or dispatch glue. Do not leak platform request/response types into handlers or tools.
- Agent tool changes: update `Bot.Agent.Tools.*`, shared schemas/helpers in `Bot.Agent.Tools.Common`, `defaultTools`, and focused tests in `test/AgentSpec.hs`. Parse tool arguments with `AesonTypes.parseEither`.
- Agent middleware changes: use `Bot.Agent.Middleware.*` and typed middleware context through `Bot.Util.HList`. `AgentContext` is for per-message tool capabilities/permissions only.
- Persistence changes: prefer component-owned `Bot.Storage.*` modules over handler-local files or ad hoc SQL. Model queryable state as columns, not opaque JSON blobs.
- New modules: update `cosmobot.cabal` for the executable plus relevant tests/benchmarks. This package has no library stanza, so missing `other-modules` entries matter.

### Review Requirements

- For substantial changes, especially RPC/web/storage/resource-lifecycle work, run a review cycle before finishing: review the code, summarize risks, fix material issues, then review again. Repeat until no unresolved high or medium risk remains, or explicitly document why a remaining risk is out of scope.
- Include an architecture review against module ownership and dependency direction, a resource-lifecycle review for files/blobs/temp paths/database rows/background queues, and a protocol-contract review for any public JSON/RPC/HTTP surface.
- Include a Haskell abstraction-smell review. Flag code that reads as imperative ordering rather than named lifecycle/domain operations, especially manual cleanup, queue overflow handling, persistence cascades, retry loops, and multi-step resource transitions.
- Include a dependency-surface review. Algorithmic and domain modules must not depend on concrete infrastructure such as databases, filesystem, HTTP, local processes, or platform APIs; those dependencies belong in storage, transport, interpreter, or system-integration modules. If a concrete dependency appears in a higher-level module, extract a pure plan, domain operation, or narrow callback so the infrastructure stays at the edge.
- When using subagents for review or implementation, give each one a disjoint scope, require file/line findings, and require verification commands for code changes. Integrate their work only after reconciling overlapping contracts and rerunning the relevant checks in the main worktree.
- Treat frontend/backend contract mismatches as blockers. If UI calls an RPC/HTTP method, the backend must implement and document it, or the UI must hide/remove that path.

### Verification

- Concurrency manager changes: `cabal test -j concurrency-spec --test-options=--hide-successes`.
- Agent calculus changes: `cabal test -j program-spec --test-options=--hide-successes`.
- Agent middleware/tool/conversation changes: `cabal test -j agent-spec --test-options=--hide-successes`.
- Agent retry, failure, and cancellation changes: `cabal test -j failure-spec --test-options=--hide-successes`.
- Scheduler changes: `cabal test -j scheduler-spec --test-options=--hide-successes`.
- Chat-log changes: `cabal test -j chat-log-spec --test-options=--hide-successes`.
- Executable wiring, config, cabal module lists, or handler signatures: `cabal build -j exe:cosmobot`.
- Always run `git diff --check` before finishing.
- Keep unrelated untracked files out of commits unless explicitly requested.
- Always use `-j` for `cabal` build and test, and pass
  `--test-options=--hide-successes` to `cabal test`.

## Deployment And Runtime

Production facts live here rather than in the Compose files. Read this before
touching images, containers, or the rollback path.

### Live Runtime

The production bot is a plain `docker run` container named `fm-cosmobot`; it
carries no Compose labels. The container itself is the authoritative record:

```bash
docker inspect fm-cosmobot --format '{{.Config.Image}}'
docker inspect fm-cosmobot --format '{{json .Config.Entrypoint}} {{json .Config.Cmd}} {{.Config.WorkingDir}}'
docker inspect fm-cosmobot --format '{{range .Mounts}}{{.Source}} -> {{.Destination}} rw={{.RW}}{{"\n"}}{{end}}'
```

Recorded on 2026-09-12 (verify with the commands above, do not trust the prose):

- image `fm-cosmobot:runtime-mentionfix-20260912` (built from commit `f33a6c3`)
- entrypoint `/opt/cosmobot/cosmobot`, cmd `serve --config config.toml`, workdir `/data`
- restart `unless-stopped`, network `fm-runtime`, `cap_add CAP_SYS_ADMIN`
- env `TZ=Asia/Shanghai`, `LANG`/`LC_ALL=C.UTF-8`, `cosmobot_datadir=/opt/cosmobot/share`
- mounts `/opt/fm-cosmobot/runtime -> /data`, `/opt/fm-cosmobot/work -> /work`,
  `/var/run/docker.sock`, `/var/www/html -> /host-sites:ro`, `/etc/nginx -> /host-nginx:ro`

Runtime state (config, sqlite, memory git repo, media cache) lives in
`/opt/fm-cosmobot/runtime`, outside the image and outside git.

### Reply-Text Markers Must Survive Every Reply Path

### The Bare Marker Is Opt-In, And chat_log Is Not The Delivered Text

Two traps found the hard way on 2026-09-12:

- The `[[bare]]` marker removes the "😻 FM：" prefix, so it must only appear when
  the user asked for that in the same message. A prompt that explained the
  default prefix but never forbade the marker made the model volunteer it on a
  plain `fm hi`, so several groups saw replies with no prefix at all. Always
  spell out the negative case ("never write it otherwise") and re-probe the
  behaviour after any prompt change - a config-only edit can regress production.
- `chat_log` stores the *pre-relay* text, so it is the wrong table for judging
  what a user saw; missing/extra markers there are not delivery bugs. The
  delivered text is whatever the send path produces. A real @-mention must not
  carry the prefix at all: `mention_user` sends `fmMentionBody`, which strips a
  prefix or marker and adds nothing, so the mentioned bot receives the command
  verbatim.

- "以 X 开头" is ambiguous in Chinese: it can ask for OUR reply's opening, or
  merely describe how another bot is triggered. `openingBeforeMarker` used to
  read both the same way, so "你记一下以fw开头就可以把他叫出来" dropped the prefix
  and forced our acknowledgement to start with "fw". It now ignores a clause
  that continues into an outcome (就可以/就能/触发/叫出来/召唤/...). When a
  feature parses user text mechanically, always pin the mis-readable case with
  a test - the existing suite had encoded the buggy reading as intended.
- Prompt wording is not an enforcement mechanism. Telling the model "never write
  [[bare]] unless the user asks" reduced the volunteering but did not stop it
  ("fm Hindi 是什么语？" still came back without the prefix), so
  fmReplyRelayBodyForRequest now requires a real bare-delivery request in the
  user's own message before honouring the marker. When a model-controlled token
  changes user-visible formatting, gate it on something deterministic - and pin
  the phrasings that must keep working, because the suite previously encoded the
  over-permissive reading as intended (it asserted a bare reply for "fm 你好").
- An allow-list of request phrasings is always one phrasing short. The owner said
  "叫一次", the list only knew "叫一下", so a correct [[bare]] was vetoed and the
  prefix landed in front of the other bot's trigger word - which is exactly what
  stops a first-word bot from firing. The reliable input is the roster the bot
  already keeps: a body that starts with a registered first-word trigger is a
  command, whatever the request says (fmReplyRelayBodyForRequestWith). Wiring that
  in needs a Memory effect in agentReplyTextSegments, which is the next step.
- A progress notice is a message, and messages are what the owner notices first.
  The fastest chat tools (mention_user, fm_member_style, chat_log, send_reply,
  recall_recent_self_messages) carried the noisy tag, so "fm @一下子寻" cost four
  messages, two of them progress lines for tools that had already finished.
  shouldAnnounceProgress keeps the tag as "this may be slow" and excludes the fast
  set in one place, and it is unit tested.
- Answer latency is a product of two structures, not of tuning: the reply was
  buffered until the whole model turn was final, and `command` let the model wait
  up to the 5 minute resource TTL inline. Measured: median 4.4s, p90 20.7s, worst
  122s, and one 122s turn was eight `command` polls at 300s each (plus a model
  round trip per poll). Fixes: flush the first finished sentence of an answer of
  140+ characters, and cap command waits at 20s while a watcher thread posts the
  output when the command ends. Keep the requested-opening exception: that
  constraint is enforced on the completed reply.
- Two traps cost real time on the latency work, both about *observing* rather
  than coding. First, the early flush looked broken in every RPC probe because
  RPC (like Matrix and Telegram) uses an editable output policy: the second part
  is an edit of the first message, so history shows one message either way. Only
  QQ chunks into separate messages. Log the event instead of inferring it from a
  channel that hides it. Second, the predicate demanded a sentence end in Chinese
  punctuation only, which meant an English answer never flushed at all.
- When a release changes behaviour on purpose, the tests that pinned the old
  behaviour move with it. The quiet-notice release silenced `send_reply` and
  `user_avatar`, and six agent-spec expectations kept demanding their notices -
  and the run's own log kept only its last thirty lines, so a failing suite could
  hide behind another suite's noise. Read the whole log, or the count of failing
  suites is a guess.
- The speaker prefix belongs to the first part of a delivered message. QQ
  delivers reply chunks as one streaming message and appends later chunks into
  that same body, so a part continuing an early flush must go out unprefixed:
  prefixing each part put a second "😻 FM：" in the middle of the answer. Long
  replies already prefix only their first chunk; the flush now matches that, and
  chat-platform-spec pins both directions.
- A tested helper that nobody calls is not a feature. The roster-aware relay
  (fmReplyRelayBodyForRequestWith) existed and was unit tested for weeks, but the
  production path passed an empty trigger list, so the only thing that ever
  suppressed the prefix for a summoned bot was the model volunteering [[bare]] -
  and a model that forgets breaks the summon. The chat memory the words live in was
  already loaded a few lines away. Wire the tested thing in, then verify it.
- Diagnosis decides what you can see: the Matrix bridge logged every sync at info
  (28% of the stream) while the release diagnostics that matter were buried
  underneath, so per-sync chatter is debug now.
- The acceptance script reads the deploy stamp (/opt/fm-cosmobot/last-deploy.env)
  instead of a hand-edited release name. Stale anchors in that file caused four
  failed verifications across two releases; the image, candidate md5, build log and
  parked container now come from the deploy itself.
- A message that has to cross two platforms must not report one platform's success
  as the whole result. mention_user sent the @ to the Matrix room, relayed a copy to
  QQ, folded the QQ failure through `rights`, and told the model "Sent mention
  message id: ... qq: []" - so the model announced a summon no QQ user ever saw
  ("真·@ 事件直奔 krkr 的脑门"), and the OneBot reason was thrown away with the Left.
  Report per destination and let the failure reach the caller.
The `😻 FM：` prefix is decided from a marker the model may write at the head of
its reply (`[[bare]]`, see `Bot.Chat.Bridge.FM`). That only works if the text
reaching the prefixing step still starts with the marker, so every path that
slices or rewrites a reply before prefixing has to keep it whole. The long-reply
streaming split used to take a two-character first chunk, which would have cut
`[[bare]]` in half, hidden it from the prefixing step and printed it to the
user. When you add or move a marker like this, grep every caller of
`fmReplyRelayBodyForRequest` and `fmReplyBody` (`Bot/Agent/Tools/Chat.hs`,
`Bot/Agent/Tools/Web.hs`, `Bot/Chat/Driver.hs`,
`Bot/Handler/Ask/AgentRun.hs`) and check the chunk boundaries, not just the
happy path.

### Never Drive Production From The Compose Pipeline

`/opt/fm-cosmobot/compose.yaml`, `deploy/cosmobot.compose.yaml`, and the
`FM_STABLE_BOT_IMAGE` defaults in `ops/deploy_*.sh` all name
`fm-cosmobot:runtime-fm-tools`, which is many generations behind production.
Running `docker compose ... up -d --force-recreate fm-cosmobot` or
`ops/deploy_production.sh` against the live container would downgrade
production (and fail on the container-name conflict). Those files are historic.
`docs/ROLLBACK.md` holds the verified container-level procedure.

### Building And Deploying A Change

Build and test inside the pinned toolchain image, never on the host, and never
run two builds at once (they share `/opt/fm-cosmobot/build/source/dist-newstyle`):

```bash
docker run --rm --network host \
  -v <tree>:/source-current:ro \
  -v /opt/fm-cosmobot/build/source/dist-newstyle:/build/dist-newstyle \
  -v /opt/fm-cosmobot/tool-output:/out \
  -v /opt/fm-cosmobot/cabal.project.local:/build/cabal.project.local:ro \
  -v /opt/fm-cosmobot/build/source/vendor:/build/vendor:ro \
  -v /opt/fm-cosmobot/build/cabal-home/config:/root/.cabal \
  -v /opt/fm-cosmobot/build/cabal-home/packages:/root/.cabal/packages \
  -v /opt/fm-cosmobot/build/cabal-home/data:/root/.local/share/cabal \
  -e LANG=C.UTF-8 -e LC_ALL=C.UTF-8 \
  -e PATH=/opt/ghc/9.10.3/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin \
  --entrypoint bash fm-cosmobot:build-test-471e7690b694 -c '...'
```

Inside, use `cabal --project-file=cabal.project.production build cosmobot cosmocode -j all`
and `cabal --project-file=cabal.project.production test cosmobot -j4 --test-options=--hide-successes`.
The warm `dist-newstyle` makes an incremental rebuild take about a minute; a
cold full build takes much longer. `touch` files whose only change is line
endings, otherwise cabal skips them.

Then preflight the candidate image before touching production: `docker run --rm`
it and check the deployed binary's md5 and `--help`. Only then stop and rename
the live container to `fm-cosmobot-prev-<stamp>`, start the candidate with the
exact flag set above, wait about 45 seconds, and verify health. Keep the
previous container parked; never delete the newest rollback point.

### Image Reference Map

Do not delete an image before checking this map: several images that look like
dead history are referenced by tracked files.

- `fm-cosmobot:build-4c782b1` — `deploy/Dockerfile.build-test`,
  `ops/deploy_production.sh`, `ops/deploy_prefix_tmp.sh`, `ops/deploy_skip_tests.sh`,
  `docs/DEPLOYMENT.md`, `/opt/fm-cosmobot/rebuild-*.sh`
- `fm-cosmobot:compiled-current` — `deploy/Dockerfile.runtime`, the `ops/deploy_*.sh` scripts
- `fm-cosmobot:runtime-fm-tools` (also tagged `runtime-image-download-fix`) —
  `/opt/fm-cosmobot/compose.yaml`, `deploy/cosmobot.compose.yaml`, `docs/ROLLBACK.md`
  (historic; keep only because those files reference it)
- `fm-cosmobot:runtime-471e7690b694` — `ops/Dockerfile.prefix-final`
- `fm-cosmobot:build-test-471e7690b694` — `ops/build-prefix-final.sh`, and the
  toolchain every verification build depends on
- The live production image and the newest rollback point change on every
  deploy. Read them from the machine instead of trusting this list:
  `docker inspect fm-cosmobot --format '{{.Config.Image}}'`, and the top row of
  the ladder in `docs/ROLLBACK.md`.
- `fm-cosmobot:runtime-seedream-20260912` — production as of 2026-09-12
- `fm-cosmobot:runtime-retryfix-20260912` — newest rollback point as of 2026-09-12

Find tracked references with `git grep -nE 'fm-cosmobot:[a-zA-Z0-9._-]+'`, and
also check `/opt/fm-cosmobot/*.sh`, which are outside git.

### Verifying A Built Binary

- Only **string literals** survive into the executable as greppable markers.
  GHC inlines small functions and constants, so `grep -c isTransportFailure`,
  `grep -c qqMediaTlsSettings` and `grep -c remoteMediaResponseTimeoutMicro`
  all return 0 even when that code is present and correct. A release gate must
  use literals such as `ftn.qq.com`, `volcengine_seedream`, `image_model_manage`,
  `[[bare]]`
  or a prompt string, never a function or binding name. Identifiers that look
  like they should survive (top-level CAFs) do not: this was measured, not
  assumed, by grepping a binary built from the same tree.
- Because identifiers are unusable, the link between reviewed source and the
  deployed binary has to come from the source tree: assert that
  `git rev-parse HEAD` equals the commit CI verified and that
  `git status --porcelain` is empty before staging the image, and leave the
  tree untouched until the deploy finishes.
- Compare the new `docker run` against the live container before trusting it.
  `docker inspect` on the running container is the authority for mounts
  (including `:ro`), `CapAdd`, `SecurityOpt`, `RestartPolicy`, `NetworkMode`,
  entrypoint and command. `WorkingDir` and the container log limits came from
  the image and `/etc/docker/daemon.json`, so they are inherited rather than
  restated in the run command.
- The config lives in a bind mount shared by the old and the new container, so
  a rollback restarts the old binary against the new config. Check that every
  key in the new config maps to a field in the old binary's schema (the old
  commit's `AgentRun.hs`, or `git log -S <field>`) before relying on rollback.
- Never edit a running shell script in place: write a new one and restart it.
  `bash` reads a script incrementally, so editing it mid-run can corrupt
  execution. Stop the old waiter first.
- Anonymous GitHub API calls are capped at 60/hour per IP and fail *quietly*:
  a rate-limited 403 body is valid JSON without a `workflow_runs` key, so a
  naive parser reports "no run yet" instead of "refused". Poll no faster than
  every 3 minutes and treat a missing `workflow_runs` key as a refusal.

### CI And Pushes

- `.github/workflows/ci.yml` triggers on every `push` (no branch filter), so any
  pushed branch runs four jobs: Source safety, Cabal, FM Domain, FM Control Center.
- There is no `gh` on the server. Watch a run through the unauthenticated API:
  `https://api.github.com/repos/Fixmood/fm-cosmobot/actions/runs?branch=<branch>&per_page=10`,
  matching `head_sha`; wait at least 90s between polls (60 requests/hour anonymous).
- Write access uses `/root/.ssh/id_rsa`:
  `git -c core.sshCommand='ssh -i /root/.ssh/id_rsa -o IdentitiesOnly=yes' push ...`.
  `/opt/fm-cosmobot/work/.ssh/fm_repo_ed25519` is a read-only deploy key: use it
  for fetching only, never for pushing.
- Never force-push. Every push must fast-forward. A local git mirror under
  `/opt/fm-cosmobot/backups/*.git` keeps history outside GitHub.

### Repository Hygiene

- `.gitignore` covers backup shapes (`*.bak`, `*.bak.*`, `*.before-*`, `*.pre-*`,
  `*.orig`, `*.rej`). Keep the tracked tree clean: `git status --porcelain` must
  be empty before committing.
- Do not leave backup copies inside the source tree. Archive them under
  `/opt/fm-cosmobot/backups/<date>-<reason>/` instead; agent greps otherwise
  match stale duplicates of files such as `AgentRun.hs` or `HTTP.hs`.
- Run `git diff --check` before finishing.

## Cosmocode

A TUI interface to interact with Cosmobot RPC server, and specifically, designed for coding tasks.
