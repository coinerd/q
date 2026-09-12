;; GSD Wave Evidence — v1.00.29 W6: Prepared-environment expansion
;; Bound to branch head: campaign/v1.00.29-w6 (implementation commit 9cc9b370;
;; docs/trio commit is the second checkpoint on this branch, squash-merge binds
;; the trio per the wave contract)
;; Date: 2026-09-14

(evidence
 (wave v1.00.29-w6)
 (implementation-sha 9cc9b370)
 (branch campaign/v1.00.29-w6)
 (base 520805da)
 (ticket "campaign v1.00.29 W6 (milestone #894)")
 (spec-ref "PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md §6 W4 (authoritative for this wave; the spec's W4 section is the campaign's prepared-environment-expansion wave)")
 (commits
  ((sha 9cc9b370)
   (scope "prepared-env identity manifest (emit/compare/fallback-record modes), setup-racket identity steps (compare-at-restore + save-side manifest + loud counted cold fallback), ci.yml verified-restore expansion to six provably-identical consumers with per-consumer rollback variables, consumers matrix + savings ledger artifacts with checksums, checksum re-stamps (dag-checkpoint setup_action_sha256 + SHA256SUMS + both literal pins), extended report + purge-contract test suites"))
  ((sha "this-commit")
   (scope "PREPARED-ENV-EXPANSION-v1.00.29.md report, wave evidence trio, README static metrics resync")))
 (deliverables
  ((file .github/actions/setup-racket/action.yml)
   (detail "W6 identity steps: (1) compare-at-restore — after a verified restore the action derives the expected identity (job tuple + checkout: os, os-image, arch x64, racket-version, racket executable sha256, lock digest via manifest.rkt digest, resolved-set/artifact digest over the materialized store, recipe revision setup-racket-prepared-restore-r1, policy fingerprint addon-store-v2|pltaddon-explicit|no-workspace-bytecode) and the observed identity and compares every dimension via scripts/ci/prepared-env-report.rkt --identity-compare; exit 1 = mismatch -> identity-result=mismatch output (loud counted cold fallback), other nonzero = hard gate failure; (2) save-side manifest — every full-path build stamps prepared-env-identity-saved.json + save stamp + counted ::notice + step summary; (3) the four full-path steps' if-conditions gained the identity-mismatch routing clause so a mismatch reroutes to the legacy cold path; the W4 BUG-0065 purge step and the guarded restore/fallback-marker/restored-summary steps are byte-identical"))
  ((file scripts/ci/prepared-env-report.rkt)
   (detail "new W6 modes: --identity-emit (ordered prepared-env-identity@1 manifest; well-formed 64-hex digests enforced, byte-deterministic output, artifact identity = base + 16-hex profile hash over ALL nine dimensions so any dimension change yields a distinct identity); --identity-compare (expected vs observed, every dimension, fail-closed on missing fields, verdict prepared-env-identity-verdict@1 with named mismatches + ::warning:: line + loud/counted fallback block, exit 1 on mismatch); --identity-fallback-record (counted record, named reason required); --consumers-check (fail-closed consumers matrix validation: activated requires provably-identical env-profile + producer artifact identity, distinct profile with producer identity rejected, one-command rollback required, exact counts); --savings-check (measured rows need retained evidence + numeric saving, projected rows need formula, rows must match the activated set exactly); --rollback-drill (executable §11.6 decision evaluation)"))
  ((file .github/workflows/ci.yml)
   (detail "verified-restore expansion: smoke, workflows (2 shards), release-dry-run, gsd-governance, abstraction-audit, prepared-env-report now gate PREPARED_ENV per-consumer (global RACKET_PREPARED_ARTIFACT + per-consumer RACKET_PREPARED_<NAME> rollback variable + workflow_dispatch guard + needs.fast-env.result), add needs fast-env with !cancelled() && needs.lint.result == 'success' fallback semantics, and pass prepared-environment/prepared-artifact-name/prepared-installer-sha256 to setup-racket; lint-quality comment documents its pinned W2 no-edge deferral; file header documents the W6 expansion + per-consumer rollback"))
  ((file artifacts/proof-graph/v1.00.29-w6/consumers.json)
   (detail "prepared-env-consumers@1: producer tuple + 31 consumer records (7 activated covering 10 job instances, 24 deferred with named reasons) + 9 non-Racket exclusions — all 41 W0 graph jobs accounted for; identity-dimensions = the nine §6 W4 dimensions; artifact-identity tokens separate racket-8.11, macos-arm64 and strict-queue classes (reserved, no artifact produced); checksummed by SHA256SUMS"))
  ((file artifacts/proof-graph/v1.00.29-w6/setup-savings.json)
   (detail "prepared-env-setup-savings@1: W0 baseline derived from retained run 34450964386 per-step setup-racket wall-clocks (full-path class mean 259.3 s over 11 lanes; restored-path test shards measured 23/20/40 s); ci:test row measured (11.6 saved runner-minutes per main-CI run, evidence cited); six W6 rows projected-from-w0-baseline each with its formula (total ~26.2 minutes per run, declared upper bound); restore/fallback observations + honesty rules (no fabricated measurements, unknown stays unknown); checksummed by SHA256SUMS"))
  ((file tests/test-prepared-env-report.rkt)
   (detail "extended 14 test cases (30 total): identity emit determinism + nine-dimension mutation -> distinct artifact identities (no cross-reuse); verified compare; wrong racket digest / wrong os / wrong os-image / wrong lock digest -> loud counted cold fallback with named fields and ::warning; multi-dimension mismatch; incomplete manifest fails closed; counted fallback record semantics; committed consumers.json + setup-savings.json pass their gates; cross-environment activation, stolen artifact identity, missing rollback, formula-free projection, evidence-free measurement and wrong-consumer savings rows all rejected; §11.6 rollback drill flips exactly its consumer and honors the global switch/dispatch/skipped-producer"))
  ((file tests/test-workflow-purge-contract.rkt)
   (detail "extended W6 consumer-matrix scan: every activated consumer's live job body must route through the shared setup-racket action (which owns the always-on BUG-0065 purge) and wire its §11.6 rollback variable into its PREPARED_ENV expression; negative fixture proves a bypass lane (raw upstream installer) turns the scan red; existing W4 pins untouched"))
  ((file artifacts/ci-topology/v1.00.26-w2/dag-checkpoint.json + SHA256SUMS + tests/test-ci-runtime-contract.rkt + tests/test-w9-ci-workflow-verification.rkt)
   (detail "checksum re-stamp: setup_action_sha256 4d93721474c14f9e396ce1ea4fd7e4c08732c8de9870d7cf6ac4662f2d6c396c -> a8532d7cfa683fe3e9ed3426dfcf11dd43531acec166ee14f7e802e016062784; checkpoint file hash 3cd242bf89934566a49327f26947b201c04da4baabbfc73ca3373505b8da9d22 -> 2ee4c26a41beb38d53ffe78aa8988c0286efe50dc1877b77fc119a09c17249ff (SHA256SUMS re-bound, w9 test verifies 'OK'); both literal pins moved with surrounding message strings intact; checkpoint gained a w6_restamp provenance note; prepare_action_sha256 (frozen producer) unchanged"))
  ((file docs/reports/PREPARED-ENV-EXPANSION-v1.00.29.md)
   (detail "consumer matrix summary, activation decisions, savings, fallback observations, per-consumer §11.6 rollback with the exercised ci:smoke drill (exact commands + outputs), checksum re-stamp table, honesty notes")))
 (honesty-declarations
  ((no-post-activation-ci-runs "this sandbox has no GitHub runner: no measured post-activation restore/savings numbers exist; all six W6 lanes are marked projected-from-w0-baseline with formulas, and the v1.00.26 committed observation window (129 pre-activation records) honestly carries restore-ms 'unknown'")
   (producer-half-frozen "the W3 producer action is outside this wave's file list, so os-image/racket-executable-digest/resolved-set/recipe dimensions are recorded consumer-side and at save time; the compare tooling fail-closes on them the moment a producer manifest records them — declared in the action comment, the report §7, and consumers.json identity_note")
   (pre-existing-tui-failure "tests/test-interfaces-tui.rkt fails in this sandbox (selection-text P1, TTY-sensitive) — same signature W5 recorded as pre-existing; this wave touches no TUI file")))
 (issues-referenced ("campaign v1.00.29 W6")))
