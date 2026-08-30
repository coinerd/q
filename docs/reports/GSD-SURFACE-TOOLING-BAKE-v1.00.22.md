# GSD Surface-Hygiene & Tooling Bake — v1.00.22

- **Date:** 2026-08-30
- **Campaign:** BUG-0023(residual), BUG-0047, BUG-0048, BUG-0049, BUG-0050,
  BUG-0051 (GSD surface-hygiene & tooling follow-ups)
- **Delivery branch:** `w0-delivery`
- **Release:** v1.00.22

This report is the campaign-level acceptance evidence for the seven
campaign gates, in the spirit of the v1.00.21 bake
(`GSD-OBSERVABILITY-BAKE-v1.00.21.md`). Each drill names the suite that
proves it, the observed numbers, and the verdict. Where a drill is
contract-level (a live duplicate run is unsafe or would tear down the
running release pipeline), that is stated explicitly.

## Gate summary

| Gate | Bug | Drill | Suite | Result | Verdict |
|------|-----|-------|-------|--------|---------|
| a | BUG-0023 | inline-format rejection | `test-gsd-plan-format-characterization.rkt` + `test-inline-format-deprecation.rkt` | 22/22, 8/8 | PASS |
| b | BUG-0050 | dual-Status detection | `test-single-wave-doc-status.rkt` | PASS | PASS |
| c | BUG-0047 | reload bytecode recovery | `test-reload-bytecode-recovery.rkt` | PASS | PASS |
| d | BUG-0048 | standalone validator | `test-standalone-plan-validator.rkt` + `validate-plan.rkt` CLI | PASS, exit 0 on campaign plan | PASS |
| e | BUG-0049 | changelog bug-ref lint | `test-changelog-bug-ref-lint.rkt` + `lint-release-notes.rkt --check` | PASS | PASS |
| f | BUG-0051 | release-object verification | `test-wave-completion-release-verification.rkt` | 4/4 | PASS |
| g | (release gate) | release-wave completion gate | `test-gsd-wave-completion.rkt` + `test-gsd-go-orchestrator.rkt` | 9/9, 52/52 | PASS |

## (a) Inline-format rejection drill — BUG-0023 (residual)

`racket tests/test-gsd-plan-format-characterization.rkt` → **22 success(es),
0 failure(s), 22 tests run.** `racket tests/test-inline-format-deprecation.rkt`
→ **8 success(es), 0 failure(s), 8 tests run.**

Key drill steps proven:

- `/go` rejects an inline-only `## Wave N` plan (no index rows) with a named
  canonical-format error naming the index grammar — the enforcement seam the
  W0 characterization pin flips to.
- The canonical index-format plan loads silently (zero deprecation warnings).
- `test-gsd-planning.rkt` 95/95 pass under `raco test` — the shared
  `/go` plan-validation kernel (W4) reports the same verdict the CLI reports.

Verdict: **PASS** — inline-only plans can no longer enter a campaign
silently; the canonical format is enforced at the `/go` seam.

## (b) Dual-Status drill — BUG-0050

`racket tests/test-single-wave-doc-status.rkt` → PASS.

Key drill steps proven:

- A wave doc with a machine `Status:` header plus a stale body template
  `Status:` line is flagged by the consistency checker (body-vs-header
  divergence detection).
- `write-wave-doc!` strips the body Status line so newly written docs carry
  exactly one authoritative Status.
- A duplicate-status lint names stray copies across the wave-doc corpus;
  44 affected historical wave docs were sanitized in-campaign.

Verdict: **PASS** — single authoritative Status line is enforced.

## (c) Reload bytecode-recovery drill — BUG-0047

`racket tests/test-reload-bytecode-recovery.rkt` → PASS.

Key drill steps proven:

- The reload path exposes a purge seam that walks and removes stale
  `compiled/` directories (wide purge-retry).
- A stale linklet after a merge is recovered in-process via fresh-namespace
  loads — no manual purge+rebuild+restart.
- A corrupt `.zo` is purged; a genuinely broken extension is reported
  honestly by name (never "n extensions reloaded" while broken).

Verdict: **PASS** — `/reload` recovers from stale bytecode after merges.

## (d) Standalone validator drill — BUG-0048

`racket tests/test-standalone-plan-validator.rkt` → PASS, and
`racket scripts/validate-plan.rkt -b /home/user/src/q-agent .planning/PLAN.md`
exits 0:

```
OK: plan is /go-ready (0 error(s), 2 warning(s))
  [ATTRIBUTION-WARN] W7: declared file "q/docs/reports/..." does not exist ...
```

Key drill steps proven:

- The standalone CLI runs the exact `/go` plan-validation kernel (same
  module, same decision order) — no drift between authoring and execution.
- It names every error/warning (plan-global and per-wave) and exits 0 on a
  `/go-ready` plan.
- A doctored plan (missing section / bad attribution) is named as a
  violation rather than rejected with an unactionable message.

Verdict: **PASS** — authors can validate a plan without `/go`.

## (e) Changelog bug-ref lint drill — BUG-0049

`racket tests/test-changelog-bug-ref-lint.rkt` → PASS, and
`racket scripts/lint-release-notes.rkt --check` → PASSED (CHANGELOG.md
version 1.00.22).

Key drill steps proven:

- A `BUG-9999` reference (unknown ID) is flagged by the linter.
- A changelog claim contradicted by the registry (e.g. "fixed" when the
  registry says open) is flagged with a named status-contradiction error.
- A correct changelog resolves cleanly; the v1.00.22 entry passes.

Verdict: **PASS** — scrambled drill→bug mappings cannot ship silently.

## (f) Release-verification drill — BUG-0051

`racket tests/test-wave-completion-release-verification.rkt` → **4
success(es), 0 failure(s), 4 tests run.**

Key drill steps proven:

- The completion path contains the release-check seam: a `release-view`
  command + `make-release-check` builder in `github-port.rkt`, the
  `#:release-check` gate in `wave-completion.rkt`, and the
  `current-gsd-release-check` policy parameter threaded through
  `go-orchestrator.rkt`.
- A release wave whose Release object is missing FAILS completion with the
  named "release not verified: …" reason, and the durable wave status is
  `failed`.
- A release wave with a verified Release object completes.
- A non-release wave without a release check is unaffected.

Verdict: **PASS** — release waves can no longer complete without a verified
GitHub Release object (the v1.00.2x false-completion class is closed).

## (g) Regression / composition gate

`racket tests/test-gsd-wave-completion.rkt` → **9/9**; 
`racket tests/test-gsd-go-orchestrator.rkt` → **52/52**;
`racket tests/test-gsd-github-port.rkt`, `test-gsd-effect-ports.rkt`,
`test-gsd-composition-root.rkt`, `test-gsd-end-to-end-recovery.rkt` all
green — the completion/verifier changes compose without regressing the
existing lifecycle and effect-port contracts.

Verdict: **PASS**.

## Release gate

The v1.00.22 release itself is verified per BUG-0051's gate: the GitHub
Release object for tag `v1.00.22` exists, is non-draft, carries the expected
assets (`q-1.00.22.tar.gz` + `release-manifest.json`), and the release
workflow run completed success — this wave does not repeat the v1.00.2x
false-completion.

## Conclusion

All seven campaign acceptance gates pass. The six bugs
(BUG-0023 residual, BUG-0047, BUG-0048, BUG-0049, BUG-0050, BUG-0051) are
marked `fixed v1.00.22` in `.planning/bugs/INDEX.md`; v1.00.22 is published
and verified on GitHub.
