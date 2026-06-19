# Audit: v0.99.30 Test Suite Reliability & Harness Modernization

**Milestone:** v0.99.30 — Test Suite Reliability & Harness Modernization  
**Wave:** W8 (#8316)  
**Commit audited:** `7aaa0a35` plus W8 documentation branch changes  
**Audit type:** final reliability audit and release-gate policy codification

## Verdict

**APPROVED AS TEST-HARNESS RELIABILITY MILESTONE, WITH KNOWN BROAD-SUITE DEBT REMAINING.**

v0.99.30 did not make the entire historical suite green. It did resolve the core reliability failure: broad-suite status is now profile-specific, classified, ledger-aware, JSON-emittable, and strict about zero-parsed false greens.

## What changed across W0–W7

| Wave | Result |
|------|--------|
| W0 | Added overhead diagnostic and local baseline report. |
| W1 | Added metadata parsing/discovery cleanup and `@not-test` handling. |
| W2 | Added strict zero-parsed detection and inventory/reporting support. |
| W3 | Added explicit runner modes: `auto`, `subprocess`, `in-process`, `grouped`; added `unit-fast`. |
| W4 | Added normalized categories and JSON output. |
| W5 | Added broad-suite known-failure ledger and known/new/unclassified/resolved reporting. |
| W6 | Added environment profiles and explicit `SKIPPED_BY_PROFILE` semantics. |
| W7 | Fixed a bounded deterministic failure batch and removed the `tests/test-benchmarks.rkt` zero-parsed sentinel. |
| W8 | Codifies release-gate policy and records final local/VPS reliability evidence. |

## Baseline vs final state

### W0 local overhead baseline

From `docs/reports/TEST-SUITE-BASELINE-v0.99.30.md`:

```text
racket-noop:              382ms exit=0
racket-empty:             188ms exit=0
raco-empty:               395ms exit=0
raco-rackunit-empty:      474ms exit=0
raco-representative-test: 479ms exit=0
```

Interpretation: local per-file subprocess overhead was sub-second, so local broad could complete and truthfully return `FAIL` rather than `INCOMPLETE`.

### W8 VPS overhead diagnostic

Command run on synced VPS main at `7aaa0a35`:

```bash
ssh user@vps2402959.fastwebserver.de \
  'cd ~/src/q-agent/q && git fetch origin main && git reset --hard origin/main && racket scripts/run-tests.rkt --diagnose-overhead'
```

Observed:

```text
Base dir:   /home/user/src/q-agent/q
Racket:     /home/user/.racket/bin/racket
raco:       /home/user/.racket/bin/raco
racket-noop: 1330ms exit=0
racket-empty: 204ms exit=0
raco-empty: 14844ms exit=0
raco-rackunit-empty: 15114ms exit=0
raco-representative-test: 15472ms exit=0
```

Interpretation: VPS broad infeasibility is dominated by per-file `raco test`/bytecode/harness overhead, not by a single universal hanging test. This validates the profile-specific reporting requirement.

## Final local gate evidence

Working directory: `/home/user/src/q-agent/q`  
Base commit before W8 docs: `7aaa0a35`  
Artifacts: `tmp/v09930-w8-gates/*.json`, `*.out`, `*.err` (local workspace artifacts; not committed)

### Local smoke

```bash
racket scripts/run-tests.rkt --suite smoke --profile local --json-out tmp/v09930-w8-gates/smoke-local.json
```

Result:

```text
exit=1
suite=smoke mode=subprocess profile=local
Files: 946 total, 887 passed, 59 failed, 0 timeouts
Tests: 11851 total, 11793 passed, 58 failed
Category: PASS=887, ASSERTION_FAILURE=57, MODULE_LOAD_FAILURE=2
VERDICT: FAIL
```

Interpretation: smoke remains red because the current smoke selection is still broad-like. This is not claimed green.

### Local unit-fast in-process

```bash
racket scripts/run-tests.rkt --suite unit-fast --mode in-process --profile local --json-out tmp/v09930-w8-gates/unit-fast-inprocess-local.json
```

Result:

```text
exit=0
suite=unit-fast mode=in-process profile=local
Files: 10 total, 10 passed, 0 failed, 0 timeouts
Tests: 102 total, 102 passed, 0 failed
Category: PASS=10
VERDICT: PASS
```

### Local unit-fast subprocess

```bash
racket scripts/run-tests.rkt --suite unit-fast --mode subprocess --profile local --json-out tmp/v09930-w8-gates/unit-fast-subprocess-local.json
```

Result:

```text
exit=0
suite=unit-fast mode=subprocess profile=local
Files: 10 total, 10 passed, 0 failed, 0 timeouts
Tests: 102 total, 102 passed, 0 failed
Category: PASS=10
VERDICT: PASS
```

### Local broad with ledger

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --profile local --ledger tests/test-suite-ledger.json --json-out tmp/v09930-w8-gates/broad-ledger-local.json
```

Result:

```text
exit=1
suite=broad mode=subprocess profile=local
Files: 1042 total, 964 passed, 78 failed, 0 timeouts
Tests: 12617 total, 12546 passed, 71 failed
Category: PASS=964, ASSERTION_FAILURE=74, MODULE_LOAD_FAILURE=2, ENVIRONMENT_MISSING=1, UNKNOWN_FAILURE=1
VERDICT: FAIL
Known failures: 78
New failures: 0
Unclassified failures: 0
Resolved known failures: 6
Release-blocking known failures: 0
```

Interpretation: local broad is red but actionable: no new failures, no unclassified failures, no release-blocking known failures, and no strict zero-parsed sentinel was reported.

## Final VPS gate evidence

Working directory: remote `~/src/q-agent/q` synced to `origin/main` at `7aaa0a35`.  
Artifacts: remote `tmp/v09930-w8-gates/*.json`, `*.out`, `*.err` where produced.

### VPS unit-fast in-process

```bash
racket scripts/run-tests.rkt --suite unit-fast --mode in-process --profile vps --json-out tmp/v09930-w8-gates/unit-fast-inprocess-vps.json
```

Result:

```text
exit=0
suite=unit-fast files=10 jobs=4 mode=in-process profile=vps
Files: 10 total, 10 passed, 0 failed, 0 timeouts
Tests: 102 total, 102 passed, 0 failed
Category: PASS=10
VERDICT: PASS
```

### VPS unit-fast subprocess

```bash
racket scripts/run-tests.rkt --suite unit-fast --mode subprocess --profile vps --json-out tmp/v09930-w8-gates/unit-fast-subprocess-vps.json
```

Result:

```text
exit=0
suite=unit-fast files=10 jobs=4 mode=subprocess profile=vps
Files: 10 total, 10 passed, 0 failed, 0 timeouts
Tests: 102 total, 102 passed, 0 failed
Category: PASS=10
VERDICT: PASS
```

### VPS broad with ledger

```bash
timeout 900 racket scripts/run-tests.rkt --suite broad --profile vps --ledger tests/test-suite-ledger.json --json-out tmp/v09930-w8-gates/broad-ledger-vps.json
```

Result:

```text
exit=124
suite=broad files=1042 jobs=4 mode=subprocess profile=vps
profile=vps skipped 2 files by @requires metadata
stderr: user break at timeout
stdout ended after serial mutation-sensitive files and partial progress; no final summary was produced.
```

Interpretation: VPS broad remains **INCOMPLETE** under a 900s subprocess broad run. The audit does not infer final pass/fail/known/new counts for VPS broad. This is distinct from local broad, which completed and returned known-debt `FAIL`.

## Acceptance questions

### Did per-file overhead decrease for pure tests?

Partially. The milestone added `unit-fast` and in-process/grouped modes so pure tests can avoid per-file `raco test` subprocess overhead. The measured `unit-fast` suite passes on local and VPS in both in-process and subprocess modes. The historical broad suite still defaults to subprocess isolation because many files are integration/mutation-sensitive.

### Are zero-parsed false greens eliminated?

The runner now has strict zero-parsed detection. W7 fixed the known strict sentinel `tests/test-benchmarks.rkt` by adding an explicit RackUnit text-ui runner. The final local broad ledger run did not report a strict zero-parsed blocker.

### Are broad failures classified?

Yes for completed profiles. Local broad reports normalized categories and ledger split:

```text
PASS=964, ASSERTION_FAILURE=74, MODULE_LOAD_FAILURE=2, ENVIRONMENT_MISSING=1, UNKNOWN_FAILURE=1
Known=78, New=0, Unclassified=0, Release-blocking known=0
```

### Are local/VPS/CI statuses separated?

Yes for local and VPS evidence in W8. CI remains not measured in this wave and must not be inferred. Local broad is completed known-debt `FAIL`; VPS broad is 900s `INCOMPLETE` with high measured `raco test` overhead.

### Is there a ledger for known failures?

Yes: `tests/test-suite-ledger.json`, documented by `docs/reports/TEST-SUITE-LEDGER.md`. Known failures remain visible and are not counted as passes.

### What gates are release-blocking now?

For post-v0.99.30 release audits:

- focused changed/adjacent `raco test` suites must pass,
- fresh-bytecode `raco make main.rkt` must pass for release approval,
- `unit-fast` must pass for the profile(s) used as release evidence,
- broad with ledger must have `New failures: 0`, `Unclassified failures: 0`, `Release-blocking known failures: 0`, and no strict zero-parsed blockers if it is used as a known-debt truth gate,
- profile-specific incomplete broad results must be reported as incomplete and cannot be upgraded to pass.

## Remaining open debt

- Local smoke remains red because its current selection is still broad-like; future work should narrow or redefine smoke if it is intended as a hard green gate.
- Local broad still has 78 known non-release-blocking failures in this W8 run.
- VPS broad remains infeasible within 900s under subprocess mode; more tests need metadata/in-process eligibility or a dedicated VPS/CI execution plan.
- CI profile evidence is not available in this audit.

## Final conclusion

v0.99.30 succeeds at its core objective: it turns ambiguous broad-suite narratives into explicit, profile-aware, classified, reproducible gate evidence. It does not certify that the whole historical suite is green.

