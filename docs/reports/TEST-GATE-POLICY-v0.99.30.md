# Test Gate Policy — v0.99.30

**Milestone:** v0.99.30 — Test Suite Reliability & Harness Modernization  
**Wave:** W8 (#8316)  
**Status:** Active policy for post-v0.99.30 release audits

## Purpose

This policy prevents broad-suite ambiguity from becoming release narrative drift. A release audit must report the exact suite, profile, runner mode, JSON/log artifact, and known/new failure split for every gate it cites.

## Gate tiers

| Gate | Command shape | Release meaning |
|------|---------------|-----------------|
| Focused wave tests | `raco test <changed/adjacent tests>` | Required for every implementation wave. Must pass except explicitly documented unrelated pre-existing failures. |
| Fresh build | `find . -name compiled -type d -exec rm -rf {} + && raco make main.rkt` | Required for audit/release approval. Prevents stale-bytecode false confidence. |
| Unit-fast | `racket scripts/run-tests.rkt --suite unit-fast --mode in-process --profile <profile>` and subprocess parity when relevant | Required green for releases that rely on the modernized runner. |
| Smoke / fast | `racket scripts/run-tests.rkt --suite smoke|fast --profile <profile>` | Required to be reported exactly. Green is required only if the milestone declares it as a release-blocking gate; otherwise red must be classified. |
| Broad with ledger | `racket scripts/run-tests.rkt --suite broad --profile <profile> --ledger tests/test-suite-ledger.json --json-out <path>` | Truth gate. Broad may remain red only when all failures are known/non-release-blocking or profile-skipped, with zero new/unclassified/release-blocking known failures. |
| VPS / CI broad | same as broad, `--profile vps|ci` | Must be reported separately from local. Timeout/incomplete is not interchangeable with local fail/pass. |

## Required report fields

Every gate citation must include:

- exact command,
- working directory / commit,
- profile (`local`, `vps`, `ci`, `headless`, or `full`),
- suite,
- mode (`auto`, `subprocess`, `in-process`, or `grouped`),
- exit code,
- JSON/log artifact path when produced,
- file summary,
- test summary,
- normalized category summary,
- ledger summary when `--ledger` is used,
- skipped-by-profile count,
- timeout/incomplete/user-break status.

## Broad-suite approval rules

A broad-suite result can support release approval only when all of the following are true for the cited profile:

1. `New failures: 0`.
2. `Unclassified failures: 0`.
3. `Release-blocking known failures: 0`.
4. Strict zero-parsed detection reports no blocking files.
5. Skips are explicit `SKIPPED_BY_PROFILE`, not silent passes.
6. The audit does not claim PASS unless the runner verdict and process exit indicate PASS.

If the broad command times out or is interrupted, the audit must state `INCOMPLETE` for that profile and must not infer the final known/new split unless JSON/log evidence exists.

## Known-failure ledger rules

- Known failures are not passes.
- Ledger entries must include file, category, owner, issue, note, and `release_blocking` status.
- Category changes are treated as new/unclassified until triaged.
- Resolved known failures should be reported and removed or updated in a later cleanup wave.
- The ledger must not be used to hide strict zero-parsed files; zero-parsed files must be fixed, marked `@not-test`, or explicitly classified by runner policy.

## Environment profile rules

Local, VPS, CI, headless, and full profile results are independent facts. A release audit may say, for example, "local broad completed with known debt" and "VPS broad timed out/incomplete due high per-file `raco test` overhead". It must not collapse those into a universal broad-suite status.

## v0.99.30 policy baseline

As of W8:

- `unit-fast` is green on local and VPS in both in-process/subprocess modes where measured.
- local broad with ledger completes with no new/unclassified failures but still has known non-release-blocking debt.
- VPS broad remains infeasible under the current per-file subprocess broad runner within 900s; this is reported as profile-specific incomplete evidence, not a universal suite result.
- strict zero-parsed false green for `tests/test-benchmarks.rkt` was fixed in W7.

