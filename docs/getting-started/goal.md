# Goal Feature

The `/goal` command sets an autonomous goal that the agent works toward across multiple turns, with an independent evaluator checking progress.

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
