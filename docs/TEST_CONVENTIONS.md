# Test Conventions

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
