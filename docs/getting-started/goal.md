# Goal Feature

The `/goal` command sets an autonomous goal that the agent works toward across multiple turns, with an independent evaluator checking progress. This feature is **enabled by default** since v0.82.5.

## Basic Usage

```
/goal "make all tests pass"
/goal "implement user authentication"
/goal "fix the failing CI build"
```

The agent enters an autonomous loop:
1. Works on the goal for one turn
2. An evaluator reviews the transcript
3. If the goal is met, the loop stops
4. If not, the agent continues (up to a turn limit)

## With Checks

Add shell commands as verification checks:

```
/goal "make tests pass" --check "raco test tests/"
/goal "fix the bug" --check "racket tests/test-foo.rkt"
```

Checks run before each evaluation. Results are passed to the evaluator as deterministic evidence. Unsafe commands (e.g., `rm -rf`) are rejected.

## Monitoring

- **Status bar**: Shows `◎ goal 3/8 · active` during execution
- **`/goal status`**: Displays active goal text, status, and turn count
- **`/goal`** (no args): Same as `/goal status`

## Controlling

- **`/goal clear`**: Cancels the active goal
- **`/g`**: Short alias for `/goal`

## Goal State & Audit Trail

Every goal mutation is persisted to the session store as structured
`goal.state` entries, and every evaluator decision is persisted as a
`goal.evaluation` entry. Verification evidence carries provenance as
`goal.evidence` entries. You can inspect the goal trail at any time —
even while a goal is running — by grepping the session log:

```sh
# Latest goal snapshot (text, status, turns-used, max-turns, updated-at)
grep '"kind":"goal.state"' <session-log>.jsonl | tail -1

# Full evaluation trail (turn, achieved?, reason, model, token-cost)
grep '"kind":"goal.evaluation"' <session-log>.jsonl

# Verification evidence with provenance (base-sha, tree-hash, current/stale)
grep '"kind":"goal.evidence"' <session-log>.jsonl
```

- **`/goal history`**: Renders the evaluation trail (turn, ok, reason, model, cost)
- **`/goal evidence`**: Lists captured verification evidence with current/stale flags
- **`/goal status`**: Renders the current interface state; use the session log for the durable audit trail

## How It Works

1. **Goal State**: Each session tracks one active goal with status (`active`, `achieved`, `failed`, `cancelled`)
2. **Turn Budget**: Default 8 turns max (configurable)
3. **Evaluator**: A separate LLM call reviews the transcript and decides if the goal is met
4. **Evidence Discipline**: The worker is instructed to produce verifiable evidence
5. **No-Progress Detection**: After 3 consecutive evaluations with the same failure reason, the goal is marked failed
6. **Deterministic Checks**: Shell commands provide objective pass/fail evidence

## Limitations

- Only one active goal per session
- Evaluation is transcript-based (not agent-based yet)
- Goal state is excluded from LLM context to prevent bias
- Maximum 8 turns per goal (configurable)

## Security

- Check commands are validated against shell risk classification
- Commands with critical severity findings are rejected
- Check execution has a 30-second timeout
- All checks run in a sandboxed subprocess
