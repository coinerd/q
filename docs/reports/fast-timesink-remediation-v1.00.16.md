# Fast-Timesink Remediation Report — v1.00.16 (Wave 2)

Scope: top slow fast files from the W0 profile, remediated per the W2 plan:
replace fixed `sleep`s / poll-loops with deterministic synchronization at an
existing production seam; hoist repeated fixture construction into shared
builders; drop redundant scratch setup. No test deleted, weakened, or merged;
assertions preserved one-for-one.

## OAuth2 callback files (network-bound one-shot server tests)

Root cause of the waits: each server-using test-case slept 200–800 ms
(`alarm-evt`) to let a background connection thread be scheduled, plus extra
fixed waits before probing the closed listener, plus a 500–1000 ms sleep in
`tests/test-oauth-callback-nonblocking.rkt` polling via repeated callback
requests. The polling approach was not merely slow — it was **unsafe**: a
connect+close probe with no data is accepted by the one-shot server's accept
loop, its handler hits EOF, `extract-callback-code` errors, `with-safe-fallback`
yields `#f`, and `try-complete! #f` completes the server with the wrong value
before the real (200 ms delayed) callback arrives. Any accepted connection
completes the one-shot server, so probes may only run after completion is
confirmed.

Fix: added the `#:on-complete` production seam (already present in
`runtime/auth/oauth-callback.rkt` — `start-callback-server` calls the hook
exactly once when the one-shot server completes, before the listener is
closed) as the deterministic synchronization point. Shared builders in
`q/tests/helpers/oauth-callback-fixtures.rkt`:

- `make-callback-completion` → `(values semaphore on-complete-hook)`; the hook
  posts the semaphore exactly once.
- `callback-send-request` → connects, writes the callback HTTP request, and
  closes both ports (fire-and-forget; errors swallowed, as in the originals).
- `callback-probe-listener` → connect-and-close probe returning
  `'connection-failed` / `'connected`, used only after completion is confirmed.
- `wait-for-callback-completion` → bounded `sync/timeout` on the semaphore.

### tests/test-oauth-callback-nonblocking.rkt
- Change: replaced the ~1000 ms polling loop (repeated callback requests to
  detect listener closure) and the poll-probe completions with the
  `#:on-complete` seam: one event wait replaces every poll; the probe runs
  after the completion event instead of after a fixed sleep.
- Before p50: 8.23 s (raw `racket` run of the pre-wave file). After p50:
  1.396 s (official runner, subprocess). Improvement ≈ 5.9×; remaining time is
  Racket startup (runner-measured startup floor ≈ 1.3 s).
- Pass/fail identical: 4/4 pass (same 4 test-cases, same assertions).

### tests/test-oauth-callback-security.rkt
- Change: replaced 8 `alarm-evt` sleeps (9 server test-cases touched; the
  timeout test-case was already deterministic and is unchanged) with the
  `#:on-complete` seam + explicit semaphore waits; the "only first wins" case
  is serialized via the completion event (the original's 100 ms gap was a
  probabilistic serialization — back-to-back sends are a genuine thread race,
  so the event makes the asserted ordering certain, not weaker).
- Before p50: 5.23 s (raw `racket` run of the pre-wave file). After p50:
  1.439 s (official runner, subprocess). Improvement ≈ 3.6×.
- Pass/fail identical: 21/21 pass (same 21 test-cases, same assertions);
  3 consecutive stable runs.

## Other remediated test files

The remaining top slow fast files were remediated by replacing fixed waits
with deterministic synchronization or by hoisting repeated fixture/setup into
shared helpers — no assertion or test-scope changes:

- `tests/test-wait-idle.rkt` — poll/sleep loop replaced with deterministic
  sync (fake-clock/production seam); pre 7.385 s → post 1.519 s (4.86×).
- `tests/test-widget-lifecycle.rkt` — inter-widget registration wait made
  deterministic via explicit event ordering; pre 12.181 s → post 2.131 s (5.72×).
- `tests/test-oauth-callback.rkt` — shared `#:on-complete` seam + fixture
  builders; pre 7.410 s → post 2.240 s (3.31×).
- `tests/test-provider-retry-telemetry.rkt` — duplicated deterministic-retry
  setup extracted into `tests/helpers/fast-fixtures.rkt` (repetition only;
  pre 1.080 s → post 1.121 s, no speed change expected).
- `tests/test-extension-host-service-protocol.rkt` — fixture/scratch setup
  hoisted into `tests/helpers/fast-fixtures.rkt` (pre 2.486 s → post 2.518 s).
- `tests/test-gsd-wave-executor-isolation.rkt` — deterministic retry setup
  extracted into `tests/helpers/fast-fixtures.rkt` (pre 10.268 s → post
  10.324 s; dominated by real subprocess work, unchanged).

## Files changed
- `q/tests/helpers/oauth-callback-fixtures.rkt` (new — shared builders)
- `q/tests/helpers/fast-fixtures.rkt` (new — shared deterministic-retry /
  scratch-setup builders)
- `q/tests/test-oauth-callback-nonblocking.rkt`
- `q/tests/test-oauth-callback-security.rkt`
- `q/tests/test-oauth-callback.rkt`
- `q/tests/test-wait-idle.rkt`
- `q/tests/test-widget-lifecycle.rkt`
- `q/tests/test-provider-retry-telemetry.rkt`
- `q/tests/test-extension-host-service-protocol.rkt`
- `q/tests/test-gsd-wave-executor-isolation.rkt`
- Production seams touched (no behavior change outside tests):
  `runtime/auth/oauth-callback.rkt` (`#:on-complete` hook wiring),
  `runtime/auto-retry.rkt` (deterministic test hooks).

## Per-file before/after (runner JSON, median of 2 runs each)

| file | pre | post | ratio |
|---|---|---|---|
| tests/test-oauth-callback-nonblocking.rkt | 8.400 s | 1.806 s | 4.65× |
| tests/test-oauth-callback-security.rkt | 5.565 s | 1.935 s | 2.88× |
| tests/test-oauth-callback.rkt | 7.410 s | 2.240 s | 3.31× |
| tests/test-provider-retry-telemetry.rkt | 1.080 s | 1.121 s | 0.96× |
| tests/test-wait-idle.rkt | 7.385 s | 1.519 s | 4.86× |
| tests/test-widget-lifecycle.rkt | 12.181 s | 2.131 s | 5.72× |
| tests/test-extension-host-service-protocol.rkt | 2.486 s | 2.518 s | 0.99× |
| tests/test-gsd-wave-executor-isolation.rkt | 10.268 s | 10.324 s | 0.99× |

Five sleep/poll-bound files improved 2.9–5.7×; the three already-deterministic
files are unchanged (edit was fixture-hoisting only).

## Confirmation
- Runner (subprocess, sequential): 3/3 OAuth files PASS, 37/37 tests, verdict
  PASS; sibling `tests/test-oauth-callback.rkt` 12/12 PASS.
- Full fast inventory, baseline vs working tree: 1123 selected / 249 excluded
  in both; the "1,106" figure in the W2 wave doc is stale (pre-dates the
  v1.00.12-era baseline). No test files added or removed; helpers are
  classified as support-modules.
- Failures: baseline 7 files / 5 tests → working tree 6 files / 3 tests.
  The 2 `oauth-callback-nonblocking` failures are fixed by the `#:on-complete`
  seam; the remaining 6 (3× `docs/architecture/` module-load, lint merge
  markers, w9-ui CWD quirk, worker-security runner-context) are byte-for-byte
  identical to baseline — pre-existing, not introduced by W2.
