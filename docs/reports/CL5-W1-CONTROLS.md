# CL5 Wave 1 — T2 oracle controls (#9982)

Controls for the T2 mutation oracle, executed on the **actual pinned baseline**
(`003e425d2f1206436eb9c0b84353604ce85bb082`), not on a synthetic tree.

Purpose: prove that the oracle credits a valid added regression test and refuses a
null/irrelevant addition, before any W3 measurement relies on its verdicts.

Reproduction:

```
racket scripts/cl5/t2-controls.rkt \
  --worktree <clean checkout at 003e425d> \
  --revision 003e425d2f1206436eb9c0b84353604ce85bb082
```

The script refuses a dirty worktree, applies each fixed fixture (never an agent's patch),
restores the tree in `dynamic-wind`, and prints one JSON report. It does not involve a model:
this is a control, not a trial.

## Result (2026-11-22)

```json
{
  "all-as-expected?": true,
  "controls": [
    {
      "added-cases": 1,
      "expected": "pass",
      "isolation": {
        "clean-exit": 0,
        "detected?": true,
        "mutated-exit": 1,
        "passed-clean?": true
      },
      "name": "valid-row-pinning",
      "pass?": true,
      "verdict": "pass"
    },
    {
      "added-cases": 1,
      "expected": "fail",
      "isolation": {
        "clean-exit": 0,
        "detected?": false,
        "mutated-exit": 0,
        "passed-clean?": true
      },
      "name": "null-unrelated",
      "pass?": true,
      "verdict": "fail"
    }
  ],
  "revision": "003e425d2f1206436eb9c0b84353604ce85bb082",
  "worktree": "<disposable checkout at the pinned revision>"
}
```

## What each control demonstrates

| Control | Fixture | Expected | Observed |
|---|---|---|---|
| `valid-row-pinning` | a second pinned behavior of the `review-infra` row (explicit reason under a preflight fallback) | PASS | PASS — the added case passes clean and **fails under M-STOP-1** in isolation |
| `null-unrelated` | a true but unrelated assertion (the `blocked` kind) | FAIL | FAIL — the added case passes clean and **still passes under M-STOP-1**, so it is not credited as the detecting regression test |

## Why this replaces the previous "baseline must not detect" rule

The frozen CL0 corpus documents that one **pre-existing** case already detects M-STOP-1. The
earlier harness rule therefore required something the agent cannot influence, and would have
rejected valid submissions. The oracle now isolates the **added** cases (diffed against the arm
revision and run in a scratch module next to the test file) and requires only that *they* pass
clean and fail under the mutation. The review's reproduction guidance is satisfied directly: a
valid added test can PASS on the real baseline, and a null addition cannot.
