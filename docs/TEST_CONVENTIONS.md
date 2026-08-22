# Test Conventions

Governance: the canonical test-strategy governance document is
[`docs/TDD-TEST-STRATEGY-PLAN.md`](TDD-TEST-STRATEGY-PLAN.md) (adopted in v1.00.04;
supersedes PR #9348). This file records the day-to-day conventions
that implement it.

## Suites

| Suite | Speed | Purpose | Command |
|---|---|---|---|
| smoke | ~40s | Quick sanity | `--suite smoke` |
| fast | ~1m | Full minus slow | `--suite fast` |
| runtime | ~2m | Runtime/provider/tool | `--suite runtime` |
| tui | ~2m | TUI rendering/state | `--suite tui` |
| workflows | ~3m | Integration workflows | `--suite workflows` |
| slow | ~5m | Long-running | `--suite slow` |
| all | ~5m | Everything | `--suite all` |

## Local feedback loop

The normal developer workflow (rollout stage 2C) is a three-level feedback
ladder. The L1/L2 command is the selector from the W4 change-impact work
(`--changed-base`/`--changed-head`); run it locally whenever you want the
impact-selected set for your change.

Initial time budgets — **targets pending the Phase 0 baseline measurement**,
not yet measured SLOs:

| Level | Scope | Time budget (target) |
|---|---|---|
| L0 — current behavior | The test file being edited or added | p90 ≤ 5s |
| L1 — direct unit impact | Direct tests of changed modules + required local contract tests | p90 ≤ 30s |
| L2 — transitive impact | L1 + tests of dependent modules + boundary contracts | p90 ≤ 120s |

### The three commands

**L0** — during the red-green-refactor loop, explicit-file mode on the test
being edited:

```bash
racket scripts/run-tests.rkt tests/test-<module>.rkt
```

**L1/L2** — impact selection for the change under development (direct
`@covers` tests of changed modules, their transitive dependents, and changed
boundary contracts; `--prioritize impact` advances likely failures without
ever changing the selected set):

```bash
racket scripts/run-tests.rkt --changed-base origin/main --changed-head HEAD --prioritize impact
```

**Preview the selection locally** — merge-base range, selection only,
nothing executed:

```bash
base="$(git merge-base origin/main HEAD)" && \
  racket scripts/run-tests.rkt --changed-base "$base" --changed-head HEAD \
    --prioritize impact --impact-dry-run --json-out impact-plan.json
```

Swap `--impact-dry-run` for `--explain` to get the human-readable view
(changed file → category → selected test → reason → dependency path →
escalation + fallback). In real (executing) mode, add
`--json-out impact-results.json` to retain the same JSON evidence CI retains.

### Escalation semantics (fail open, never silently narrower)

Selection is conservative by design:

- **Escalations run a declared broad fallback suite** (`fast` plus the area
  suites of the changed files), never a silently smaller set. Triggers:
  `dynamic-require`, macro definitions, `#lang reader`/generated code,
  configuration/workflow/package changes, fixture changes, runner/helper
  script changes, git failure, unparseable modules inside the change's
  dependency cone, and **unmapped sources** (no `@covers` mapping reaches the
  changed module directly or transitively).
- **Empty selection on a non-doc-only change is an error** (exit 3 with the
  selection JSON), never a silent green.
- **Doc-only changes are an explicit zero-test no-op** with JSON evidence,
  never a silent pass.
- Every selected test, escalation, and fallback carries a machine-readable
  reason in the JSON evidence.

### Prioritization (`--prioritize impact`, default off)

- **Ordering only.** It reorders the already-selected set and never adds or
  drops a test; omitting the flag leaves runner behavior unchanged.
- **Priority tiers**, run earliest first: (a) explicitly named current-test
  files, (b) direct `@covers` tests of changed modules, (c) transitive
  dependents, (d) changed-boundary contract tests, (e) recently-failed tests,
  (f) all remaining selected tests. Every emitted test carries a
  `priority-reason`; ties break stably by repository path.
- **Mutation-sensitive (serial) and parallel partitions are prioritized
  independently**; the serial-first serialization semantics are unchanged.
- **`--failure-history PATH`** feeds tier (e) from retained CI JSON artifacts
  (`--json-out` results, recency-bounded, decay-weighted so a stale failure
  cannot permanently dominate). Missing or corrupt history yields a logged,
  deterministic neutral order (plain path sort) — never an error, never a
  guess. No network access, no database.
- **First-failure reporting:** when a prioritized run fails, the summary
  surfaces the first failing selected test with its selection and priority
  reason. The summary is a pointer, not the source of truth — full results
  and failure logs remain available unchanged.

## Local impact selection (opt-in)

Change-impact selection (implemented in `scripts/run-tests/impact.rkt`, driven
through the `scripts/run-tests.rkt` CLI) is an **opt-in local developer
workflow**. It is never invoked by CI (rationale below) and never replaces the
normal suites.

### Always start with the dry run and the explanation

Never execute a blind impact run. First inspect what would be selected — and
why — for your change range:

```bash
racket scripts/run-tests.rkt --impact-dry-run \
  --changed-base <base> --changed-head <head> --explain
```

`<base>` is typically `origin/main` (or `$(git merge-base origin/main HEAD)`);
`<head>` defaults to `HEAD`. Nothing executes: the run prints the reasoned
selection and exits 0.

### Reading the output

- **Deterministic selection reasons.** Every selected test reports its reason
  code (changed test file, direct `@covers` match, transitive dependent with
  its dependency path, changed-boundary contract), the changed file it traces
  back to, and the mapping source. Selection is a pure function of the change
  range: same range, same set, same reasons.
- **Escalations widen the set to the declared fallback (`fast` plus the area
  suites of the changed files) — they never narrow it.** When the selector is
  uncertain it escalates and runs the declared broad fallback. Escalation
  cases: dynamic loading (`dynamic-require`, unresolvable requires), macro
  definitions in the changed cone, generated/configuration/fixture/
  runner-helper changes, dependency-graph parse failures (unparseable
  modules), unmapped sources (no `@covers` mapping reaches the changed module
  directly or transitively), and any git failure. Each escalation prints its
  code, the triggering file, and the fallback suites. An escalated run is a
  broad run by design — that is the fail-open contract, not a defect.

### When to execute the selected set

Execute the selected set only when it is useful for the change under
development — e.g. iterating on a focused change where the transitive-impact
set is meaningful:

```bash
racket scripts/run-tests.rkt --changed-base <base> --changed-head <head> --prioritize impact
```

(`--prioritize impact` only reorders the already-selected set; it never
changes which tests run.) Otherwise, use the normal `fast` profile
(`--suite fast`) — it is what PR CI executes and remains the default local
loop.

### Why this is not a CI gate

Impact selection is **not a PR or scheduled CI gate**. The former `test-impact`
GitHub job was removed because it exceeded the repository's 30-minute job
runtime limit; do not reintroduce it into `.github/workflows/`. The
authoritative CI protection remains the static gates in
`.github/workflows/ci.yml` — the three-shard `fast` suite on every PR/push
plus the platform, security, smoke, and workflow gates. The non-CI boundary is
enforced by grepping `.github/workflows/` for `--changed-base`,
`--changed-head`, `--impact-dry-run`, and `--prioritize impact` (zero matches
expected).

## Known Flaky Tests

The following tests are known to fail in parallel/subprocess mode but pass
in isolation. They are **pre-existing environment/harness bugs**, not
product defects. A gate failure on one of these must be re-run in isolation
before being treated as a real regression.

| Test | Classification | Failure mode | Isolated result |
|---|---|---|---|
| `tests/test-interfaces-tui.rkt` | ENVIRONMENT_BUG/HARNESS_BUG | Exit 1, 0 passed / 0 failed (crash under parallel subprocess mode) | Exit 0 |
| `tests/test-settings.rkt` | ENVIRONMENT_BUG/HARNESS_BUG | 1 assertion failure (parallel temp-file interference) | Exit 0 |
| `tests/test-run-tests-ledger.rkt` | ENVIRONMENT_BUG/HARNESS_BUG | Internal ledger/temp-file contention with concurrent test-run-tests-* files | Exit 0 (3/3) |

## Metadata Tags

Add these to the first 30 lines of test files:

```racket
;; @suite runtime       ; which suite this belongs to
;; @speed fast           ; fast | slow
;; @boundary unit        ; unit | integration
;; @mutates none         ; none | env | cwd | env,cwd
;; @isolation none       ; none | mutating | process (v0.83.10+)
;; @timeout 30           ; per-file timeout in seconds (v0.83.10+)
```

The runner reads these tags for classification. Files without tags use heuristic fallbacks.

### Tag Reference

| Tag | Values | Effect |
|-----|--------|--------|
| `@suite` | `runtime`, `tui`, `cli`, `llm`, `tools`, `extensions` | Suite classification for parallel grouping |
| `@speed` | `fast`, `slow` | Slow tests skipped in `--suite fast` |
| `@boundary` | `unit`, `integration` | Integration tests may need sandbox isolation |
| `@mutates` | `none`, `env`, `cwd`, `env,cwd` | Declares what the test mutates; affects sandbox setup |
| `@isolation` | `none`, `mutating`, `process` | `mutating`/`process` forces sandbox setup; overrides heuristic |
| `@timeout` | integer (seconds) | Per-file timeout override; replaces default timeout |

### Test metadata schema: required vs conditional

Not every field applies to every file. The schema distinguishes fields that are
mandatory on **every** test file from fields that are **conditional** (required
only when the stated condition holds). CI enforces missing mandatory tags via
the blocking `metadata-lint` step in `.github/workflows/ci.yml`; invalid tags
are hard failures inside the lint itself and always were.

| Field | Status | Condition |
|-------|--------|-----------|
| `@speed` | **Mandatory on every test file** | — (complete: missing-required=0 on the enforced canonical inventory, re-verified at v1.00.11 against digest `ab265d57edba32d7499390578bee67c776ce3e1afe1fc9eac3e88096bf29f7e4`) |
| `@boundary` | **Mandatory on every test file** | — (complete: missing-required=0 on the enforced canonical inventory, re-verified at v1.00.11 against digest `ab265d57edba32d7499390578bee67c776ce3e1afe1fc9eac3e88096bf29f7e4`) |
| `@area` | **Mandatory on every test file** | — (ownership map complete) |
| `@suite` | Conditional | Required when the test belongs to a named suite (`runtime`, `tui`, `cli`, `llm`, `tools`, `extensions`) |
| `@mutates` | Conditional | Required when the test mutates persistent state (values other than `none`) |
| `@isolation` | Conditional | Required when the test needs non-default isolation (`mutating` or `process`) |
| `@timeout` | Conditional | Required when the test needs a non-default timeout |

**Enforcement status:** missing mandatory tags (`@speed`, `@boundary`, `@area`)
were report-only (`continue-on-error: true`) until the inventory reached zero
gaps. Enforcement was scheduled for v1.00.12 but flipped to blocking one
release later, in **v1.00.12**: the report-only inventory run at that point
returned exit 0 with `missing-required=0` across all 1,299 test files
(invalid=0, deprecated-alias=0), so the `metadata-lint` CI step dropped
`continue-on-error: true` and became `metadata-lint (blocking)`. CI
configuration and this document now describe the same enforced state; the
v1.00.12 milestone slipping was deliberate, not silent — it waited on the
evidence (a clean inventory) rather than on a date.

**v1.00.11 re-verification:** the enforced canonical inventory was re-run at
milestone close from a clean checkout (`racket scripts/run-tests/classify-metadata.rkt
--lint-metadata` and `--metadata-inventory-json` from the repository root):
exit 0 with `files=1336`, `invalid=0`, `deprecated-alias=0`,
`missing-required=0`, `explicit=1336`, `heuristic-only=0`, inventory schema
version 1, file-list digest
`ab265d57edba32d7499390578bee67c776ce3e1afe1fc9eac3e88096bf29f7e4`. This
digest — recorded in `docs/reports/test-regression-log.md` — is the canonical
backing for every `missing-required=0` claim in this document; the claim is
withdrawn if a same-command local invocation against this digest produces a
nonzero count.

**Single discovery implementation (W2):** local `--lint-metadata` and the CI
`metadata-lint` step invoke the *identical* repository command —
`racket scripts/run-tests/classify-metadata.rkt --lint-metadata` from the
checkout root — so no CI-only discovery branch exists. The former
`scripts/run-tests.rkt --lint-metadata` wrapper dispatch (the source of the
W0 local-vs-CI inventory divergence) was deleted. CI also uploads a
`--metadata-inventory-json` artifact (schema version, invocation root,
SHA-256 digest over the sorted normalized paths, file count, per-area counts,
and full violation details); a clean local run of the same command must match
that digest for the same commit.

## Test Sandbox

Use `with-test-sandbox` for tests that need isolated filesystem:

```racket
(require "helpers/test-sandbox.rkt")

(with-test-sandbox
  (lambda (sandbox)
    ;; sandbox has project-dir, session-dir, home-dir, temp-root
    ...))
```

Cleanup is guaranteed via `dynamic-wind`.

## Scenario Harnesses

### Provider Scenarios
```racket
(require "helpers/provider-scenarios.rkt")
(define-values (prov cap) (make-scenario-provider
                            (list (scenario-text "hello")
                                  (scenario-tool-call "bash"))))
```

### Tool-Turn Scenarios
```racket
(require "helpers/tool-turn-scenarios.rkt")
(define sc (turn-scenario-tool-call "bash" #:result "file.txt"))
(define prov (scenario->provider sc))
(define reg (scenario->tool-registry sc))
```

### Goal Scenarios
```racket
(require "helpers/goal-scenarios.rkt")
(define cap (make-goal-capture))
(goal-run! "test goal" provider "model" run-prompt-fn!
  #:on-event (make-on-event cap)
  #:on-status (make-on-status cap))
```

## Gate Evidence

The runner writes gate evidence to `.gate-evidence/<suite>.json`:

```bash
racket scripts/run-tests.rkt --suite smoke --record-gate
```

## Output Bounds

- Test output is truncated at 64KB with head+tail preservation
- Failure logs use unique names with path hash to avoid collisions
- Logs saved to `/tmp/q-test-fail-<basename>-<hash>.log`


## File Naming

- **Convention:** `test-<module>.rkt` or `test-<module>-<aspect>.rkt`
- **1:1 correspondence preferred:** `tests/test-foo.rkt` ↔ `source/foo.rkt`
- **Support modules** (helpers, fixtures, scenarios) keep descriptive names without `test-` prefix
- **All test files in subdirectories** must use `test-` prefix (e.g., `tests/tui/test-state.rkt`)

## Test-Case Naming

- Prefix test-case names with module context for global uniqueness
- Pattern: `"<module-context>: <descriptive name>"`
- Example: `"goal-checks: parse-goal-checks extracts command"` not just `"parse-goal-checks extracts command"`
- This ensures `grep -rn 'test-case "' tests/ | sort | uniq -c | sort -rn` shows all counts ≤ 1

## Temp File Pattern

- Always use `with-temp-dir` from `tests/helpers/temp-fs.rkt` for temporary directories
- **Never** use bare `make-temporary-file` with `'directory` — it leaks on test failure
- `with-temp-dir` uses `dynamic-wind` for guaranteed cleanup

```racket
(require (only-in "helpers/temp-fs.rkt" with-temp-dir))

(test-case "my test"
  (with-temp-dir (tmpdir)
    ;; tmpdir is auto-deleted when test completes or fails
    ...))
```

## State Isolation

- **No `set!` on module-level variables** — use `make-parameter` with per-test reset
- For counters: `(define counter (make-parameter 0))` + `(counter (add1 (counter)))`
- For logs: `(define log (make-parameter '()))` + `(log (cons entry (log)))`
- Reset at each test-case start: `(reset-my-counter!)`

```racket
(define msg-counter (make-parameter 0))
(define (next-id!)
  (msg-counter (add1 (msg-counter)))
  (format "msg-~a" (msg-counter)))
(define (reset-msg-counter!) (msg-counter 0))

(test-case "my test"
  (reset-msg-counter!)
  ...)
```

## Environment Variable Guards

- Tests that modify env vars must use `dynamic-wind` for guaranteed restore
- Tests requiring external env vars (API keys, DISPLAY) must skip gracefully when absent

```racket
(test-case "env-dependent test"
  (define old (getenv "MY_API_KEY"))
  (dynamic-wind
    (lambda () (putenv "MY_API_KEY" "test-value"))
    (lambda () ...)
    (lambda ()
      (if old (putenv "MY_API_KEY" old) (putenv "MY_API_KEY" "")))))
```

## Adding a New Test

1. Create `tests/test-<feature>.rkt`
2. Add metadata tags in header
3. Use `#lang racket/base` with `rackunit`
4. Use `test-case` for each test
5. Use sandbox/helpers for isolation
6. Run: `raco test tests/test-<feature>.rkt`
