# Test Suite Baseline: v0.99.30 W0 — Overhead Diagnostics

**Wave:** W0 (#8308)  
**Branch:** `feature/v09930-w0-overhead-diagnostics`  
**Purpose:** Establish measured test-runner startup/harness overhead before deeper runner architecture changes.

---

## 1. Diagnostic Command

v0.99.30 W0 adds:

```bash
racket scripts/run-tests.rkt --diagnose-overhead
```

The command measures representative fixed-cost operations:

- raw `racket -e '(void)'` process startup,
- direct `racket` execution of an empty module,
- `raco test` execution of an empty module,
- `raco test` execution of a tiny rackunit module,
- `raco test` execution of one normal project test (`tests/test-version.rkt`) when available.

---

## 2. Local Baseline

Environment path:

```text
Base dir: /home/user/src/q-agent/q
Racket:   /usr/bin/racket
raco:     /usr/bin/raco
```

Observed output:

```text
racket-noop:              382ms exit=0
racket-empty:             188ms exit=0
raco-empty:               395ms exit=0
raco-rackunit-empty:      474ms exit=0
raco-representative-test: 479ms exit=0
```

## 3. Initial Interpretation

On this local environment, per-file subprocess/raco overhead is sub-second (~0.4–0.5s for empty/rackunit files). This is consistent with the v0.99.28 local broad-gate observation that the suite can complete in minutes and return `VERDICT: FAIL` with 0 timeouts.

This does **not** invalidate the remote VPS observation. Instead, it establishes that broad-suite behavior is environment-dependent and must be measured per profile:

- local/dev may be able to complete the current subprocess-based broad runner,
- VPS may be dominated by per-file subprocess overhead and return `INCOMPLETE`,
- CI may differ again depending on bytecode cache, filesystem, and CPU scheduling.

---

## 4. Required Follow-up Measurements

Run the same command on each execution profile:

```bash
racket scripts/run-tests.rkt --diagnose-overhead
```

Record:

| Profile | racket-noop | racket-empty | raco-empty | raco-rackunit-empty | representative | Notes |
|---------|-------------|--------------|------------|---------------------|----------------|-------|
| local | 382ms | 188ms | 395ms | 474ms | 479ms | W0 local baseline |
| vps | TBD | TBD | TBD | TBD | TBD | Needed to validate remote overhead finding |
| ci | TBD | TBD | TBD | TBD | TBD | Needed before CI gate policy |

---

## 5. W0 Result

W0 provides the measurement tool and first local baseline. Later waves should use this data to justify:

- in-process or grouped execution for pure tests,
- environment profiles,
- truthful `FAIL` vs `INCOMPLETE` broad-suite reports,
- release gate policy.

---

*Generated as part of v0.99.30 W0.*
