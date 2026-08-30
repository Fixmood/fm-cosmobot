# Agent Programs

Cosmobot treats an agent run as one region of a composable `Program`. The
program may perform ordinary computation, enter an agent loop, react to its
result, and continue with another computation. A running program can also
produce the program fragment that follows.

This is the larger idea behind agent programs. ReAct supplies an adaptive
model/action loop, CodeAct shows that executable code is a useful action
language, and Programmatic Tool Calling moves local tool orchestration out of
the model context. Cosmobot places all three behind one host-owned program
boundary.

> **Status:** The `Program` calculus, the Haskell agent entry point, and the
> `py` programmatic-tool-calling frontend are implemented. `py` currently
> exposes ordinary tool calls. One additional host operation for nested model
> or agent calls completes the recursive and dynamically generated encodings
> below, with Python source as their portable language.

## Motivation

A conventional tool agent alternates between model inference and environment
interaction:

```text
model -> action -> observation -> model -> action -> observation
```

This ReAct shape is valuable when each observation may change the plan. It is
less efficient when the next steps are ordinary control flow: iterate over a
collection, call several tools concurrently, filter their results, or pass one
result into another call. Returning every intermediate value to the model uses
an inference turn and makes the transcript carry data that may have no lasting
value.

Executable actions address the local problem. Code can express sequencing,
branching, loops, aggregation, and recovery precisely. It can retain raw tool
results in an execution environment and return only the conclusion needed by
the next model turn.

An agent program extends this observation beyond one code action. The agent
loop itself becomes a program fragment. It can be composed with other
fragments, returned to after nested work, and generated from runtime values.

```text
ordinary computation
        |
        v
    agent run ------> result-dependent computation
        ^                          |
        |                          v
        `---------- generated Program
```

## Core model

`Program m result` is the unit of composition. `agentProgram` enters the
recursive model/tool loop and returns its final `Result` as an ordinary
program value:

```haskell
agentProgram
  :: Runtime context (Eff es)
  -> TurnState
  -> Program (Eff es) Result
```

A program can therefore place computation on either side of an agent run:

```haskell
program = do
  input <- prepare
  first <- agent input
  next <- chooseNext first
  agent next
```

The example illustrates the continuation carried by every visible program
event: the remainder of the whole surrounding computation.  Once an event
response arrives, that continuation constructs the next `Program`. Model
answers, tool results, interpreters, and nested agent runs can all influence
what follows.

[`Agent.md`](Agent.md) describes the live calculus, its interaction-tree
structure, and runtime interpretation. Agent programs add the question of how
that live structure is constructed: directly in Haskell today, through a
narrow executable frontend today, and through a larger Python host API when a
consumer needs it.

## Direct encoding

The calculus provides a shared semantic substrate for agent strategies. Once
model calls, tool calls, and ordinary carrier actions are program operations,
the common agent patterns become program definitions or transformations:

```haskell
react state =
  runModel state >>= \case
    Final result -> pure result
    ToolCalls calls -> runTools calls >>= react

pal problem =
  runModel (requestCode problem) >>= runPython

ptc source =
  runPythonWithTools source

rlm context =
  runPythonWithAgentCalls context
```

These schematic definitions use the existing core constructors. `react`
recurses through event continuations. PAL interprets model-produced code as an
ordinary program region. Programmatic Tool Calling lets that region issue
nested `RunTools` events. An RLM gives the same region an operation that
enters another `agentProgram`; Python supplies selection, loops, and
recursion.

A continuation can decode or execute a model result, so a running program can
dynamically produce its next program:

```python
source = cosmobot.ask_agent("write the next Python agent program")
exec(source, globals())
```

Here `ask_agent` is an illustrative extension to the `py` host module. It
would pause the worker, enter an agent through the normal host interpreter,
return the result to Python, and leave the surrounding `Program` continuation
in place. The existing worker protocol already uses this pause/callback shape
for ordinary tools.

## Frontends and interpretation

An agent-program frontend produces or selects a `Program`; the runtime grants
external authority during interpretation.

```text
Haskell entry point ----\
Python source ------------+--> Program --> Runtime --> output and result
decoded data -------------/
```

The runtime remains the authority for model calls, tools, streaming,
observation, cancellation, and lifecycle. Different frontends therefore share
the same middleware and continuation semantics after they enter `Program`.

The live and portable forms have different roles:

|Form                                |Role                                                |
|------------------------------------|----------------------------------------------------|
|`Program m result`                  |Live computation with typed events and continuations|
|Python source or another syntax tree|Serializable, model-visible program data            |

Python source already supplies a portable representation for the implemented
frontend. Additional frontends may validate an AST and decode it into the same
live calculus. The host API decides which capabilities every representation
may use.

## The implemented `py` boundary

`py` is the first executable frontend. Its current surface covers programmatic
orchestration of ordinary tools. A sole `py` call is intercepted at a
`RunTools` event and replaced with a program that:

1. runs Python in a fresh sandbox;
2. lets it invoke ordinary tools already exposed to the model;
3. routes those calls through normal tool dispatch and middleware;
4. returns Python completion or failure as the outer tool result; and
5. resumes the saved program continuation once.

Python may sequence calls, run a batch concurrently, branch on results, and
reduce large intermediate values before returning to the model. Nested results
remain observable to host middleware, while the outer `py` result alone enters
the model transcript.

Python controls composition; the host controls authority. The worker API
contains the exposed ordinary tools, while program-control operations,
`AgentEvent` construction, and Haskell continuations remain host-owned. Nested
side effects persist when later Python code fails.

The current interface exposes ordinary tools. Adding a host-owned agent call
beside `run_tool` lets the same Python region encode recursive agents while
preserving runtime identity, permissions, middleware, cancellation, and
resource ownership.

The structural transformation lives in
[`Bot.Agent.Middleware.Python`](../lib/Bot/Agent/Middleware/Python.hs); the
model-visible contract lives in
[`Bot.Agent.Tools.Python`](../lib/Bot/Agent/Tools/Python.hs); and worker
lifecycle belongs to
[`Bot.Resource.Python`](../lib/Bot/Resource/Python.hs).

## Related work and encodings

The closest work falls into four groups: interleaved tool use, executable
reasoning and actions, planned tool orchestration, and programs that compose
model calls themselves. Their control and observation boundaries differ.

|Approach                 |Direct encoding into `Program`                                                    |Work-specific contribution                                        |
|-------------------------|----------------------------------------------------------------------------------|------------------------------------------------------------------|
|ReAct                    |Recursion alternating `RunModel` and `RunTools`                                   |An effective reasoning/action policy                              |
|Toolformer               |A model policy that emits `RunTools` inside generation                            |Self-supervised learning of tool placement and arguments          |
|ART                      |Decode a textual reasoning/tool program whose tool forms become `RunTools`        |Demonstration selection and editable task programs                |
|PAL                      |`RunModel` for source, followed by a Python carrier action                        |Program-aided symbolic reasoning                                  |
|CodeAct                  |Recursion alternating `RunModel` with executable-code regions                     |Code as the unified learned action space                          |
|Voyager                  |CodeAct-shaped programs plus persistent storage and retrieval of successful code  |Lifelong executable skill acquisition                             |
|ReWOO                    |`RunModel` planner, interpretation of its dependency plan, then `RunModel` solver |Planning tool dependencies before worker execution                |
|LLMCompiler              |Compile a model-produced tool DAG into concurrent `RunTools` regions              |Dependency analysis and parallel scheduling                       |
|Programmatic Tool Calling|One Python region containing nested `RunTools`; return only the region result     |Context and inference efficiency for tool orchestration           |
|DSPy                     |A host-authored `Program` of model calls plus an optimizer over program parameters|Compilation and optimization of declarative LM pipelines          |
|Recursive Language Models|A Python region with access to external context and recursive `agentProgram` calls|Long-context decomposition and recursive inference                |
|Cosmobot Agent Program   |The common `Program` representation used by these encodings                       |Typed continuations, streaming, middleware, and host-owned effects|

At the operational-semantics level, these are genuine encodings: the existing
calculus constructors represent their model, tool, environment, and control
transitions. Their learned policies, training procedures, planners,
schedulers, optimizers, persistence schemes, and empirical results occupy
distinct layers as interpreters, program transformations, runtime services, or
policies over the common representation.

### Interleaved tool use

[ReAct](https://arxiv.org/abs/2210.03629) interleaves reasoning, actions, and
observations so that reasoning can update a plan and actions can gather new
information. Cosmobot keeps this adaptive outer loop through structured tool
calls, with reasoning kept private to the model. `py` can collapse a
deterministic subsequence before the next observation reaches the model.

[Toolformer](https://proceedings.neurips.cc/paper/2023/hash/d842425e4bf79ba039352da0f658a906-Abstract-Conference.html)
trains a model to decide which simple API to call, when to call it, which
arguments to supply, and how to incorporate the result into later tokens. Its
learned tool-selection policy emits calls inside language-model generation;
`Program` supplies their host composition and interpretation.

[ART](https://arxiv.org/abs/2303.09014) generates multi-step reasoning and
tool use as an editable program, selecting demonstrations from a task
library. Its generation pauses when a tool is called and resumes with that
result. It is a textual tool program whose external tool pauses record the
control structure in the model-generation trace. Cosmobot makes the live
remainder an explicit host value.

### Executable reasoning and actions

[PAL](https://proceedings.mlr.press/v202/gao23f.html) asks a model to
translate natural-language reasoning into a program and delegates exact
symbolic or arithmetic execution to Python. It establishes the value of
separating decomposition from deterministic solving. PAL's interpreter
computes an answer; Cosmobot extends the same program region with effectful
tools and agent runs.

[CodeAct](https://proceedings.mlr.press/v235/wang24h.html) takes executable
Python as a unified action space. It also supports multi-turn revision after
execution feedback. Cosmobot uses code for composition and recovery alongside
direct tools, while the host `Program` defines the complete agent computation.

[Voyager](https://arxiv.org/abs/2305.16291) generates executable code for an
embodied Minecraft agent, improves it from environment feedback and execution
errors, and stores successful programs in a growing skill library. It shows
how code can become a durable, composable action vocabulary. Cosmobot's
current `py` worker is intentionally ephemeral. Voyager-style reuse would add
a store around successful source programs while preserving the calculus and
worker lifetime.

### Planned and programmatic tool orchestration

[ReWOO](https://arxiv.org/abs/2305.18323) separates a planner from tool
workers and a solver. The planner names dependencies before worker execution,
reducing repeated model prompts. Its plan gives a declarative form of
orchestration; Python additionally branches, loops, and recovers from the
actual values it receives.

[LLMCompiler](https://proceedings.mlr.press/v235/kim24y.html) turns a function
calling plan into a dependency graph and schedules independent calls in
parallel. Its compiler-shaped executor is well suited to visible dataflow.
Cosmobot exposes batch concurrency inside `py`, with the generated Python
program owning the remaining scheduling decisions. LLMCompiler contributes
graph inference and optimization as a program transformation.

[Programmatic Tool
Calling](https://platform.claude.com/docs/en/agents-and-tools/tool-use/programmatic-tool-calling)
lets sandboxed code invoke registered tools and process intermediate results
inside the execution environment. This is the nearest operational precedent
for `py`: both reduce context pollution and inference overhead while leaving
tool execution under host control. Cosmobot additionally routes nested calls
through its own middleware and resumes an explicit `Program` continuation.

### Programs containing model calls

[DSPy](https://arxiv.org/abs/2310.03714) represents LM pipelines as programs
of declarative modules and compiles their prompts or demonstrations against a
metric. It shares the view that model calls belong inside a larger program.
DSPy programs are application-authored and optimized ahead of use; Cosmobot's
`Program` may also be selected or generated while an agent computation is
running. DSPy's optimizer contributes an additional program transformation.

[Recursive Language Models](https://arxiv.org/abs/2512.24601) treat a long
prompt as an external environment that code can inspect, decompose, and pass
to recursive model calls. This is the closest precedent for extending `py` to
enter nested agent fragments from generated code. Its purpose is scalable
long-context inference, whereas Cosmobot's `Program` also covers tools,
streaming, middleware, lifecycle, and a typed final result.

These form complementary levels. ReAct and its relatives decide when the model
should observe and reconsider. PAL, CodeAct, and Voyager establish code as an
execution medium. ReWOO, LLMCompiler, and Programmatic Tool Calling reduce
observation round trips. DSPy and Recursive Language Models place model calls
inside larger computations. The agent program is the host abstraction that
composes all of those regions.

## Extending the Python frontend

One host operation that enters an agent completes `py` as a frontend for full
agent programs. It expresses nested model calls, recursion, delegation, and
model-generated Python. Ordinary Python supplies sequencing, functions, data
structures, exceptions, and dynamic execution.

The host operation must still enter through the higher-order `Agent`
capability so root and nested runs share the same program semantics. Its
callback should return only the nested agent result to Python; streaming,
audit, permissions, resource ownership, cancellation, and tool dispatch remain
interpreted by the host. This is the same authority boundary as current nested
tool calls.

An additional DSL may serve consumers that need static validation, durable
typed syntax, or execution outside Python. The Python interface covers the
encodings above.
