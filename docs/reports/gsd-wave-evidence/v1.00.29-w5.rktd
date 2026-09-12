;; GSD Wave Evidence — v1.00.29 W5: Provenance-safe proof-bundle prototype
;; Bound to branch head: campaign/v1.00.29-w5 (implementation commit 77f84a10;
;; docs/trio commit is the second checkpoint on this branch, squash-merge binds
;; the trio per the wave contract)
;; Date: 2026-09-13

(evidence
 (wave v1.00.29-w5)
 (implementation-sha 77f84a10)
 (branch campaign/v1.00.29-w5)
 (base a80d4abf)
 (ticket "campaign v1.00.29 W5 (milestone #894)")
 (commits
  ((sha 77f84a10)
   (scope "q.proof-bundle/1 writer + fail-closed validator + deterministic fixture generator, valid-base + twenty §9 fixtures with SHA256SUMS, writer/validator test suites"))
  ((sha "this-commit")
   (scope "PROOF-BUNDLE-PROTOTYPE-v1.00.29.md shadow report, wave evidence trio, README static metrics resync, tier-ownership matrix resync for two new test families")))
 (deliverables
  ((file scripts/proof-bundle/bundle-writer.rkt)
   (detail "canonical q.proof-bundle/1 writer: canonical JSON (sorted keys, deterministic), content-addressed bundle_id = sha256 over the canonical form with bundle_id empty (§5.2), producer completeness gate over all §5.2–§5.16 required fields, in-module FIPS 180-4 SHA-256 so producers/consumers share one digest implementation; deps racket/base + json/contract/file/string only"))
  ((file scripts/proof-bundle/bundle-validator.rkt)
   (detail "§5.17 15-step consumer validation, read-only, fail-closed; emits 'reusable / (not-reusable <reason>) / (invalid <reason>); every step is a short-circuit or-chain so no violation is discarded; identity + immutable workflow revision verified against consumer expectations, never checksum-only (§5.13); missing fields are invalid, never compatible; deterministic q.reuse-decision/1 record (step 15, no wall-clock fields)"))
  ((file scripts/proof-bundle/gen-test-fixtures.rkt)
   (detail "deterministic generator (guarded one-off, W3 pattern): valid-base written by the canonical writer; each §9 case is a minimal named mutation with bundle_id RECOMPUTED (consistent-forgery discipline) so only semantic steps can catch it; byte-identical regeneration pinned by test"))
  ((file tests/fixtures/proof-bundle/)
   (detail "valid-base + all twenty §9 threat-model fixtures, each with SHA256SUMS consistent with its own bundle.json — so no validator rejection can be explained by checksums (no checksum-only acceptance path)"))
  ((file tests/test-proof-bundle-writer.rkt)
   (detail "23 checks: FIPS known-answer vectors (empty/abc/56-byte boundary/multi-block), canonicalization stability + key-order invariance, content-address derivation, required-field completeness vs the 17 schema names (hardcoded), schema conformance incl. per-section required sub-fields, full fixture-corpus byte-identical regeneration"))
  ((file tests/test-proof-bundle-validator.rkt)
   (detail "16 checks: every fixture directory validated with the expected named decision, category semantics (missing-field is invalid, never not-reusable), no checksum-only path (consistent producer-identity forgery with matching SHA256SUMS still rejected), moving-ref-only provenance, retention shortfall, dirty subject, non-canonical encoding, unparseable bundle, bundle-id mismatch, incomplete consumer request, artifact expectation mismatches, reuse-decision record"))
  ((file docs/reports/PROOF-BUNDLE-PROTOTYPE-v1.00.29.md)
   (detail "shadow-decision report for the one low-risk same-SHA main→later-workflow overlap case (decision 'reusable' + q.reuse-decision/1 record + full 20-fixture negative-control table), compatibility-policy notes, honest prototype limitations"))
  ((file artifacts/tier-ownership/v1.00.28-w0/ownership-matrix.json)
   (detail "resynced for the two new test families (test family drift gate is red otherwise); --check PASS with zero drift; recorded as a required deviation from the wave's 'do not touch tier-ownership' note — same precedent as W3 (\"matrix regenerated for the two new flake test families\")")))
 (shadow-decision
  ((case "same-SHA main → later-workflow overlap: ci.yml @ 3f7a1c2b produced claim:unit:fast-gate on 9c8b7a65 (tree 1a2b3c4d, attempt 3); a later workflow needs the same proof for the same commit")
   (decision reusable)
   (why "exact commit+tree match, immutable workflow revision verified, identical environment class + package lock, strict profile intact, pass/no-cancel/no-skip, artifact digests match consumer-side recomputation, retention immutable + horizon-covered, consumer authorized, non-release context")
   (negative-controls "all twenty §9 fixtures rejected with named reasons (table in PROOF-BUNDLE-PROTOTYPE-v1.00.29.md); consumer execution unchanged — the reusable decision was recorded, not acted on")))
 (residual
  "tests/test-interfaces-tui.rkt fails in this sandbox (selection-text P1 check; TTY-sensitive) — pre-existing on the untouched base tree, fails identically standalone and in-shard before and after this wave's files were added; same test family is already on record in W3 as an environment/parallel-run-sensitive failure. Recorded, not coerced; this wave's diff cannot affect it.")
 (issues-referenced ("campaign v1.00.29 W5")))
