# The Agent Program

Cosmobot represents an agent run as an observable `Program`. A model response
may construct a tool event, and that event's continuation constructs the next
model turn. `runProgram` interprets this structure. The program continuation is
the single route from one turn to the next.

This gives tool limits, steering, continuation capture, observation, and
context compaction a shared object to transform. Agent control flow stays
explicit and local.

## Motivation

An agent combines two concerns:

- a recursive decision process chooses model calls, tool batches, and the final
  result;
- an execution environment supplies models, tools, streaming, observation, and
  lifecycle management.

`Program` represents the decision process. `Runtime` supplies the execution
environment. Their boundary makes the agent easy to transform while preserving
one meaning for the remainder of a run.

An agent run is itself a composable region of `Program`. `agentProgram` is the
current Haskell entry point into that region: it returns a `Program`, whose
result can feed any computation composed after the run. `agentStream` selects
that entry point for today's application flow and folds the resulting program.

The initial transcript enters the program from the caller. Each later model
input arrives through the continuation of the preceding operation:

```text
initial transcript
      |
      v
  RunModel -------- final answer --------> Finished
      |
   tool request
      v
  RunTools
      |
  returned TurnState
      `-----------> next RunModel
```

This structure records the provenance of every turn. The interpreter returns
an operation response; the program decides how that response shapes the rest
of the run.

## Core model

The core has three structural cases:

```haskell
newtype Program m result = Program
  { observe :: Stream (Of Output) m (Step m result)
  }

data Step m result where
  Finished :: result -> Step m result
  Continues :: Program m result -> Step m result
  Visible
    :: AgentEvent response
    -> (response -> Program m result)
    -> Step m result
```

`Finished` returns a value. `Continues` crosses a silent structural boundary.
`Visible` exposes an operation together with the continuation of the agent at
that point.

The current operation signature is small:

```haskell
data AgentEvent response where
  RunModel
    :: TurnState
    -> AgentEvent (TurnState, LLM.ChatAnswer)

  RunTools
    :: ToolRequest
    -> AgentEvent TurnState
```

Each event determines the response accepted by its continuation. The type
checker therefore connects model responses to model continuations and tool
responses to tool continuations.

The continuation constructs the next `Program` after its response arrives.
The tree therefore grows during interpretation: model answers, tool results,
and interpreter transformations can determine the computation that follows.
This generative property lets an agent run participate in a larger program and
lets running programs produce further program fragments.

`Program` has structural `Functor`, `Applicative`, and `Monad` instances. Bind
continues after `Finished`, retains a `Continues` boundary, and composes through
every `Visible` continuation. Its `MonadTrans` instance embeds carrier actions
directly. `AgentEvent` stays focused on agent control points observed by
middleware.

The definitions live in
[`Bot.Agent.Core`](../lib/Bot/Agent/Core.hs). Their algebraic properties live in
[`ProgramSpec`](../test/ProgramSpec.hs).

## Interpretation and streaming

[`Bot.Agent`](../lib/Bot/Agent.hs) constructs the recursive program and folds
it. `runProgram` interprets each visible event, applies the event response to
its continuation, and resumes the fold.

`Program` carries two complementary forms of observation:

- `Output` streams incremental content, tool-call announcements, and reply
  boundaries to the caller;
- `Step` exposes control structure to the interpreter and structural
  middleware.

The final `Result` travels through the stream's return channel. The caller can
consume output incrementally while the recursive program remains an ordinary
composable value.

`Continues` represents transitions that carry structure while remaining
invisible to the model and user. Middleware scopes and internal boundaries can
therefore retain their shape while `AgentEvent` stays focused on meaningful
interactions.

### Naming interpretation boundaries

Within structural agent-program middleware, use `interpretX` and
`XInterpreter` for code that translates an `X` structure into `Program`. For
example, the internal `PythonInterpreter` handles a parsed `py`
control call by producing the replacement agent program that eventually
resumes the original continuation. Elsewhere, an interpreter may translate a
typed protocol operation into its semantic carrier result; the name still
describes translation, not external execution.

Code that performs an external operation is a runner: name it `runX`, `RunX`,
or `XRunner`. Starting the Python worker and exchanging its protocol messages
is execution, not interpretation, even though it is called from the program
interpreter. Keep implementation-shape aliases internal unless another module
must state the contract directly.

## Program and runtime

`Runtime context m` supplies tools, run metadata, event interpretation, and
middleware hooks. The program describes which operations happen and how their
responses determine subsequent work. The runtime describes how each operation
runs.

The hooks correspond to distinct semantic boundaries:

| Boundary | Hook |
|---|---|
| Select the transcript for the next request | `modelInputTranscript` |
| Transform the recursive computation | `aroundProgram` |
| Scope the lifetime of the whole run | `aroundAgentRun` |
| Wrap one model request and response | `aroundModelTurn` |
| Wrap one complete tool batch | `aroundToolTurn` |
| Wrap one tool call | `aroundToolCall` |

These boundaries support local reasoning. Tool failure classification stays at
one tool call, typing notification stays at run lifetime, and structural
control flow stays in `aroundProgram`.

`aroundModelTurn` ends with `(TurnState, LLM.ChatAnswer)`. The following `Step`
remains inside the program. Model middleware observes or adjusts the event
response; structural steering and continuation capture operate at the
`Program` boundary where the continuation is present.

The type-level `context` carries temporary typed observations between
middleware. The request-level `Context` carries request capabilities.
`TurnState` carries data across model/tool turns. The three channels give
temporary observations, request scope, and recursive state distinct homes.

## Continuations

`Visible event continue` contains the remainder of the computation.
Continuation middleware captures and adapts that function. Resumption returns
directly to the captured program position.

Haskell permits repeated function application; runtime convention gives
visible continuations logical one-shot semantics. The model-facing
continuation tools add run scope, nesting, and a JSON value protocol around a
captured continuation. The captured closure remains ephemeral and local to the
Haskell runtime. [`Continuation.md`](Continuation.md) describes the
model-visible behavior.

A structural transformation can recover at an unconsumed visible boundary by
replacing the event, supplying a response, or selecting another program. The
current boundary has three relevant properties:

- `RunTools` represents one complete concurrent batch;
- synchronous tool failures become protocol-complete result messages in the
  returned `TurnState`;
- asynchronous cancellation terminates the scoped computation.

Recovery therefore uses the same boundary that exposes agent control flow.

## Root agents and subagents

The higher-order `Agent` effect opens the scope in which a prepared runtime is
used. Root agents and subagents both follow `Agent.withRun -> agentStream`.
Identity, resource ownership, and background lifetime are interpreter-local
metadata around that common shape.

Program and middleware changes consequently apply uniformly to every run.
Resource management stays in its owning layer. “Child” describes ownership and
lifetime within the same agent program model.

## Interaction-tree correspondence

The representation is an interaction tree adapted to streaming:

| Interaction tree | Cosmobot |
|---|---|
| `Ret result` | `Finished result` |
| `Tau next` | `Continues next` |
| `Vis event continue` | `Visible event continue` |
| Event signature | `AgentEvent` |
| Handler | `runProgram` plus runtime middleware |

`Stream` allows outputs before the next structural observation, and carrier
`m` performs infrastructure effects. The complete remainder of the agent is
represented by the tree, and interpretation follows its continuations.

The same boundary now admits the model-facing `py` control tool, whose
middleware replaces that tool event with a program that executes Python and
dispatches nested ordinary tools. [`AgentProgram.md`](AgentProgram.md)
describes this frontend and shows how model calls, tool orchestration, and
recursive agents encode directly into the same calculus.
