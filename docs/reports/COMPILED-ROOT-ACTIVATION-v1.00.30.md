# Compiled Root Activation — v1.00.30 (Wave W4)

- **Issue:** coinerd/q#9690 · **Milestone:** #895
- **Branch:** `campaign/v1.00.30-w4` · **Implementation head:** `ba35b090f`
- **Date:** 2026-09-19 (content) · 2026-09-24 (repaired re-delivery at `ba35b090f`)
- **Scope:** guarded compiled-root activation on required PR fast lanes only; global default remains `off`.
- **Repair (v1.00.31 W7):** this wave was blocked on the unfixed tree, so the
definition of done above is realized through the hardened v1.00.31 machinery:
the action declares exactly one guarded whole-checkout producer invocation
(executed verbatim by `tests/test-compiled-root-workflow.rkt`), the published
root stays inside the already-contracted `q-compiled/` prefix, and the recovery
records are re-derived at the head named above.

## What was activated

### Producer → consumer integration (same-run, same-head)
`scripts/ci/compiled-root.rkt` gained the `build` and `run` subcommands used by
the fast lanes:

- **Producer (`build`)** compiles declared lane modules against a *real git
  checkout* (fail-closed cleanliness gate), stages to a scratch dir, and
  publishes an immutable compiled root **outside the checkout** with a trusted
  label and a lockfile-derived source-identity manifest. Checkout-relative
  reuse is gone: the checkout purge and fixture exclusions are unchanged, and
  published trees are made read-only.
- **Consumer (`run`)** resolves a candidate root **before** resolution of the
  lane's entry module, verifies namespace trust (published-label match),
  per-module source identity (content digests, not mtimes), and manifest
  integrity, then launches the entry read-only against the validated root.
  Any refusal falls back to **one** eager current-source compile whose
  duration is measured into telemetry (`fallback-ms`), never silently served
  from the possibly-stale root.

### Override precedence (proven by tests, not by convention)
`Q_CI_COMPILED_ROOT` is the single global switch:

| Value | Behavior |
| --- | --- |
| unset | resolve → verify → use trusted root; any failure ⇒ exactly one eager compile |
| `off` | root machinery fully skipped; eager current-source path (rollback switch) |
| anything else | fail closed with a usage error |

Invalid/missing/incompatible roots always degrade to **one eager compile or a
hard failure** — never lazy per-test recompilation amplification. Tests in
`tests/test-compiled-root-workflow.rkt` pin every precedence branch.

### Telemetry separation
`scripts/ci/prepared-env-report.rkt` and the workflow tests distinguish
producer build cost, restore cost, usable-root hit, bounded eager fallback,
file execution, and full critical path, so no work can be *shifted* between
producer and consumer and reported as a win.

### Non-goals held
Prepared-env consumers were not broadened; the cached addon store is not
trusted for changed q sources (link/collection identity is verified); Windows,
macOS, cross-version, and release paths remain full eager. Activation carries
**no general rollout authority**.

## Verification evidence

- **Unit/property tests:** `tests/test-compiled-root-workflow.rkt`,
  `tests/test-prepared-env-report.rkt` (plus the pre-existing
  `test-compiled-root*.rkt` suites) — all green at the implementation head.
- **End-to-end drill** (driving the committed CLI exactly as CI would, via
  `/usr/bin/env`, git-clean fixture checkout): all five phases pass —
  see `artifacts/ci-recovery/v1.00.30-w4/rollback-drill.json`:
  1. producer build + publish → published root, reason `ok`;
  2. consumer fast-shard-shaped run → verified root hit, output `42`,
     zero fallback compiles;
  3. `Q_CI_COMPILED_ROOT=off` → global off, one eager compile (472 ms),
     output `42` — **the rollback drill**;
  4. current-source canary (source edited after publish) → stale root
     refused, one eager compile (481 ms), output `63` from current source;
  5. tamper canary (manifest byte flipped) → integrity failure, one eager
     compile (480 ms), output `42` — the poisoned root's stale `63` was
     never served.
- **Artifact integrity:** `SHA256SUMS` covers the drill and activation
  records (and the raw drill harness plus its transcript); the timings in
  this prose are the same numbers recorded in the drill's
  `eager-fallback-ms` block, and every recorded head names a commit that
  carries the recorded content.

## Gate status (HOLD — local evidence only)

Local Verify is not authority over R0/R1/R2, protection, sample size, or
release gates. Remaining coordinator-owned gates before promotion:

- `pr-latency-guard` on the exact final head,
- ≥3 genuine candidate heads compared against frozen W2 regime references,
- independent security/correctness review APPROVED,
- protected squash merge, then merge-SHA binding (activation merge SHA and
  regime fingerprint recorded in
  `artifacts/ci-recovery/v1.00.30-w4/activation.json` for the W5 cohort).

Both remote-bound fields are `PENDING` there by design until the protected
merge lands; missing remote evidence means HOLD.
