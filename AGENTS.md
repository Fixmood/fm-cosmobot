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

## Cosmocode

A TUI interface to interact with Cosmobot RPC server, and specifically, designed for coding tasks.
