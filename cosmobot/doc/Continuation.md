# Agent Continuations

## Motivation

Agent exploration is non-monotonic, while an LLM transcript normally grows
monotonically. A failed hypothesis may consume many tool calls and tokens, then
remain in every later model request even when only its conclusion matters.

Continuations give the agent an explicit context-backtracking boundary. The
agent captures its current model context, explores, and may later resume the
capture point with a compact JSON value. Messages produced by the abandoned
branch are removed from future model input.

However, this is only agent-level continuation. It does not undo file
or database writes, messages, resources, audit events, or output
already shown to the user.

## Tool Interface

### `capture_continuation`

Captures the current agent continuation. It must be the only tool call
in its model turn.

```json
{
  "label": "optional description"
}
```

The initial result is:

```json
{
  "state": "captured",
  "continuation_id": "call_123",
  "label": "optional description",
  "scope": "current_agent_run",
  "one_shot": true
}
```

If the continuation is later resumed, the agent instead observes this call as
returning:

```json
{
  "state": "resumed",
  "continuation_id": "call_123",
  "value": {
    "conclusion": "The parser hypothesis was false",
    "evidence": ["Parser.hs:42"]
  }
}
```

### `resume_continuation`

Non-locally resume a captured continuation with an arbitrary JSON
value. It must also be called alone.

```json
{
  "continuation_id": "call_123",
  "value": {
    "conclusion": "The parser hypothesis was false",
    "evidence": ["Parser.hs:42"]
  }
}
```

Continuations are one-shot and scoped to one agent run. They may be
nested.  Resuming a continuation consumes it and discards every
continuation captured inside its abandoned branch; older outer
continuations remain available.

## Expected Uses

- Test a hypothesis, then return only the conclusion and evidence.
- Explore a noisy log, search, or tool result without retaining the
  full path.
- Try nested alternatives and return to the appropriate decision
  point.
- Recover from a locally unproductive strategy and retain the active run.

Continuations are less useful for ordinary linear work, irreversible
actions, or parallel delegation. Subagents and isolated workspaces
remain better fits for those cases.

## Implementation

The two tools are model-visible control forms interpreted by
`Bot.Agent.Middleware.Continuation`, rather than ordinary external
capabilities.

The middleware wraps the coinductive agent `Program` through
`aroundProgram`. A captured continuation is the actual `Program`
resumption closure together with a monotonically increasing nesting
ordinal. The saved map is lexical to that program run; it is not part
of `TurnState` or the typed middleware context.

The continuation ID is the original tool-call ID. On resume, the
middleware:

1. records the `resume_continuation` call through normal tool
   observation;
2. invokes the saved resumption with the current state and JSON value;
3. restores the capture transcript and appends a tool result to the
   original capture call;
4. removes the resumed continuation and all newer nested
   continuations;
5. advances the current tool-turn counter instead of restoring the old
   counter.

The last rule prevents continuations from bypassing the agent
tool-turn limit.  A valid, sole `resume_continuation` may execute at
the limit so the agent can escape a branch and produce a final answer.

Late steering is an `aroundProgram` structural transformation. It captures a
visible `RunModel` continuation and, after a final answer, constructs another
`RunModel` before reusing that continuation. The resulting `Continues` boundary
starts with an empty continuation map. Ordinary model/tool recursion stays
inside the current program scope.

The middleware intercepts a continuation name only when that tool is
present in the run's exposed tools. Mixed continuation and sibling
tool calls are rejected as a group before any sibling executes. The
final conversation transcript contains only the selected logical
history, while the append-only agent audit retains the actual capture,
abandoned branch, and resume calls.

## Alternatives

### `setjmp` and `longjmp`

These names make the two-return behavior familiar, but also suggest restoration
of a machine stack or process state. Cosmobot restores only model context, so
`capture_continuation` and `resume_continuation` describe the boundary more
accurately.

### One `callcc` or `continuation` Tool

A single tool with an `op` field saves one schema but makes action
selection less explicit.  `callcc` also implies stronger semantics
such as ordinary continuation values and unrestricted multi-shot
invocation. The two verb-named tools expose exactly the supported
actions.

Also `callcc` is high-order and does not immediately translate to what
an agent expects.

### Subagents

Subagents provide clean context, concurrency, independent prompts, and
separate tool permissions. They return a result to the parent but do
not restore the parent agent's own earlier decision point. They are
preferable for delegation; continuations are preferable for local
backtracking.

### Session Forks and Transcript Compaction

Session forks preserve multiple durable branches for users to revisit.
Compaction summarizes old context. Neither provides an
agent-controlled, one-shot return from a speculative branch with an
explicit value.

## Related Work

[Context-Folding](https://arxiv.org/abs/2510.11967) is the closest prior work.
It gives an agent `branch` and `return` actions: the agent enters a temporary
sub-trajectory, then returns a summary while the intermediate branch is removed
from the main context. It establishes prior art for agent-controlled context
branching. Cosmobot is, to our knowledge, the first agent harness to expose
model-callable, opaque, one-shot continuation handles that support nested
capture and resume the original tool-call position with an arbitrary JSON
value. Unlike Context-Folding, it does not prescribe a subtask-and-summary
structure.

[LangGraph time travel](https://docs.langchain.com/oss/python/langgraph/use-time-travel)
uses persisted checkpoints to replay or fork graph execution. It preserves the
original history and re-executes nodes after the selected checkpoint. Cosmobot's
continuation is run-local, model-invoked, and returns a value into the original
capture call instead of creating a durable graph branch.

[Claude Code checkpointing](https://code.claude.com/docs/en/checkpointing)
can rewind conversation, code, or both, and can summarize one side of a chosen
checkpoint. It is user-operated and includes selected file restoration;
Cosmobot exposes transcript backtracking directly to the agent and deliberately
does not claim filesystem rollback.

[AutoGen agents](https://microsoft.github.io/autogen/stable/reference/python/autogen_agentchat.agents.html)
expose application-level `save_state` and `load_state` operations. These allow a
host to persist and restore agent state, but are not a model-callable
capture/resume pair with a returned JSON value.

[OpenAI Agents SDK sessions](https://openai.github.io/openai-agents-python/sessions/)
support managed conversation history, removal of recent items, and compaction.
Those mechanisms manage history between runs; they do not implement local
non-local control flow inside an agent run.

The current design provides agent-level non-local return and backtracking. It
does not provide formal non-deterministic computation such as multi-shot choice
enumeration.
