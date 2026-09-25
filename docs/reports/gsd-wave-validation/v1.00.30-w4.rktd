#hasheq(
 (schema-version . 2)
 (status . "current")
 (milestone . 895)
 (wave . "W4")
 (issue . 9690)
 (branch . "campaign/v1.00.30-w4")
 (implementation-sha . "1158f1b7023b08c9608f59c8cd4c0c04915d5de6")
 (content-digest . "cc357910188fae92cc8b1f0b04cbd3485e367c363c12f007ee5634930db5cea3")
 (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.30-w4.rktd")
 (planning-sync . "current")
 (red-first
  . #hasheq(
     (command . "reproduced against the blocked wave head ce13d0388: `grep -c 'compiled-root:' .github/workflows/ci.yml` and `racket scripts/ci/compiled-root.rkt run --checkout $PWD --final-dir ... --map-dir ... --trusted-label q-trusted-producer`")
     (failure . "The blocked wave was not deliverable at all: its producer invocation was unexecutable (`compiled-root: unknown switch: --out`, reproduced in the v1.00.31 W0 w4-reproduction.json), no consumer could ever hit a root because ci.yml never enabled the producer input (0 matching lines at ce13d0388), and setup-racket's declared pre-resolution step failed with `compiled-root: run requires at least one --module`. Both additional captures are retained at artifacts/ci-recovery/v1.00.30-w4/raw/red-first-review-fixes.txt.")))
 (focused-tests
  . #hasheq(
     (command . "raco test tests/test-compiled-root-workflow.rkt tests/test-compiled-root.rkt tests/test-compiled-root-manifest.rkt tests/test-prepared-env-report.rkt && raco test tests/test-ci-runtime-contract.rkt tests/test-w9-ci-workflow-verification.rkt")
     (result . "passed")
     (details . "184 tests in the declared focused group (including the round-3 lane-plumbing tests and the round-4 telemetry-wiring tests: no hard-coded code outcome, both axis vocabularies, producer/fallback cost plumbing, eager gate on the off lane, telemetry file, upload ordering) (including the two new CLI `run` cases: resolution-only and module-bearing), plus 38 ci-runtime-contract and 7 workflow-verification cases that pin the re-stamped W2 checkpoint hash and both action hashes.")))
 (format-compile
  . #hasheq(
     (command . "raco fmt -i && raco make on the changed Racket sources at the reviewed implementation head")
     (result . "passed")))
 (lint
  . #hasheq(
     (command . "racket scripts/check-deps.rkt && racket scripts/metrics.rkt --lint && racket scripts/ci/verify-artifact-provenance.rkt --root .")
     (result . "passed")
     (details . "dependencies OK; all 5 static metrics match README.md after the canonical resync; the artifact provenance lint exits 0 (only historical provenance notes), including the re-derived ci-recovery/v1.00.30-w4 artifacts whose SHA256SUMS binds all six files and whose recorded heads are ancestors of the tip.")))
 (fast
  . #hasheq(
     (command . "racket scripts/run-tests.rkt --suite fast")
     (result . "passed")
     (run-summary . "RUN-SUMMARY runner-version=1.00.29 suite=fast profile=local shard=none execution-mode=subprocess file-count=1208 pass=1208 fail=0 timeout=0 skip=0 wall-clock-seconds=1363.742 metadata-completeness=explicit:1180/heuristic:0/missing:28")))
 (suites
  . #hasheq(
     (arch . "RUN-SUMMARY runner-version=1.00.29 suite=arch profile=local shard=none execution-mode=subprocess file-count=32 pass=32 fail=0 timeout=0 skip=0 wall-clock-seconds=27.109 metadata-completeness=explicit:32/heuristic:0/missing:0")
     (security . "RUN-SUMMARY runner-version=1.00.29 suite=security profile=local shard=none execution-mode=subprocess file-count=64 pass=64 fail=0 timeout=0 skip=0 wall-clock-seconds=63.182 metadata-completeness=explicit:64/heuristic:0/missing:0")
     (workflows . "RUN-SUMMARY runner-version=1.00.29 suite=workflows profile=local shard=none execution-mode=subprocess file-count=33 pass=33 fail=0 timeout=0 skip=0 wall-clock-seconds=79.063 metadata-completeness=explicit:33/heuristic:0/missing:0")))
 (remaining-items
  . (#hasheq(
      (classification . "deferred-noncritical")
      (owner . "coordinator (W4 pilot activation)")
      (rationale . "The wave is a fast-lane pilot: promotion beyond the declared fast lanes, the pr-latency-guard sample on the exact final head, and the regime comparison against the frozen W2 references are coordinator-owned remote gates recorded in artifacts/ci-recovery/v1.00.30-w4/activation.json (status HOLD; activation-merge-sha and regime-fingerprint PENDING until the protected merge lands)."))
     #hasheq(
      (classification . "deferred-noncritical")
      (owner . "W7 release record")
      (rationale . "The v1.00.30 W4 outcome (delivered through the hardened ladder, with its merge SHA) is recorded in the v1.00.31 W7 wave artifacts (artifacts/wave-delivery-integrity/v1.00.31-w7/w4-recovery.json) rather than inside this wave's own records, so the blocked-wave outcome is explicit without editing frozen v1.00.30 wave documents.")))))