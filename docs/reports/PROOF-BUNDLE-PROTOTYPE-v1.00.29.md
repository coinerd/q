# Proof-Bundle Prototype (v1.00.29 W5) — Shadow-Decision Report

Wave: v1.00.29 W5 — Provenance-safe proof-bundle prototype
Spec: `.planning/v1.00.29-spec/PLAN-v1.00.29-PROOF-GRAPH-REDUCTION.md` §5 + §9
Schema: `.planning/v1.00.29-spec/proof-bundle-schema-v1.00.29.json` (`q.proof-bundle/1`)
Status: PLANNED → implemented as a **shadow** on one low-risk case. No consumer execution changed; no required proof removed.

## 1. What shipped

| Deliverable | File |
|---|---|
| Canonical `q.proof-bundle/1` writer | `scripts/proof-bundle/bundle-writer.rkt` |
| §5.17 read-only, fail-closed consumer validator | `scripts/proof-bundle/bundle-validator.rkt` |
| Deterministic §9 fixture generator | `scripts/proof-bundle/gen-test-fixtures.rkt` |
| §9 threat-model fixtures (valid-base + 20 cases, each with SHA256SUMS) | `tests/fixtures/proof-bundle/` |
| Writer tests (23 checks) | `tests/test-proof-bundle-writer.rkt` |
| Validator tests (16 checks) | `tests/test-proof-bundle-validator.rkt` |

Writer properties: canonicalized JSON (sorted keys, deterministic formatting, single-line + newline), content-addressed `bundle_id = "sha256:" + sha256(canonical JSON of the bundle with `bundle_id` empty)` (§5.2), producer completeness gate over every required §5.2–§5.16 field, dependency-free FIPS 180-4 SHA-256 (pinned in-module so producers and consumers share one digest implementation; dependencies: `racket/base`, `json`, `racket/contract`, `racket/file`, `racket/string`).

Validator properties: implements §5.17 steps 1–15 in order; every step is a short-circuit or-chain, so the first violated check aborts with a named reason; decisions are `reusable`, `(not-reusable <reason>)` (authentic but incompatible), `(invalid <reason>)` (corrupt/inauthentic/incomplete); a missing field is **invalid, never compatible**; read-only; no network, no threads, no sleeps.

## 2. Shadow case: same-SHA main → later-workflow overlap

**The case (the one low-risk reuse candidate this wave is allowed to shadow).**
The main CI workflow (`.github/workflows/ci.yml` @ immutable revision `3f7a1c2b…`) ran the `unit:fast-gate` proof on commit `9c8b7a6…` (tree `1a2b3c4…`), run attempt 3, tier-1, strict security profile, Linux/x86_64/Racket 8.10, package lock `sha256:1414…`. The proof bundle records that run (fixture `tests/fixtures/proof-bundle/valid-base/`).
A **later workflow** (a follow-up lane starting after the producer run) needs the same `claim:unit:fast-gate` proof for the **same commit SHA**. Instead of silently re-running, the consumer gate runs the §5.17 validation algorithm over the producer bundle, stating its expectations independently (consumer request in `tests/test-proof-bundle-validator.rkt`).

**Shadow decision (recorded, not acted on):**

```racket
(validate-proof-bundle-file "tests/fixtures/proof-bundle/valid-base/bundle.json"
                            (base-request))
;; => 'reusable

(reuse-decision-record <bundle> <request> 'reusable)
;; => (hash 'schema "q.reuse-decision/1" 'decision 'reusable 'reason #f
;;          'bundle_id "sha256:6c792c9095f58d71a6181d92685b7d22a0977808eea9638afcfe0efc7e2f2079"
;;          'consumer_id "gate:main-required" 'consumer_mode "regular"
;;          'claim_ids '("claim:unit:fast-gate")
;;          'subject_commit_sha "9c8b7a654321fedcba9876543210fedcba987654"
;;          'validated_steps 15)
```

Why `reusable` is sound for this case: identical commit **and** tree SHA (§5.4, exact-commit binding); immutable workflow revision matches producer and provenance (§5.3/§5.13); same Racket version, OS, architecture, runner image, package lock and resolved package set (§5.8); strict security profile is not degraded (§5.9); result is `pass`, not cancelled, zero timeouts, no unexplained skips (§5.11); artifact digests match the consumer's independently computed values (§5.12); retention is immutable, unexpired and covers the consumer's horizon (§5.14); the consumer is explicitly authorized and this is not a release-context reuse (§5.15).

**Negative controls:** the same consumer request run against the twenty §9 threat-model fixtures rejects every one with a named reason (all 20 verified in `tests/test-proof-bundle-validator.rkt`):

| §9 | Fixture | Decision |
|---|---|---|
| — | `valid-base` (shadow case) | `reusable` |
| 1 | `wrong-racket` | `not-reusable:environment-mismatch:racket_version` |
| 2 | `wrong-os` | `not-reusable:environment-mismatch:os` |
| 3 | `weaker-security` | `not-reusable:security-profile-insufficient` |
| 4 | `changed-package-lock` | `not-reusable:environment-mismatch:package_lock_digest` |
| 5 | `changed-workflow-revision` | `not-reusable:workflow-revision-mismatch` |
| 6 | `changed-manifest` | `not-reusable:manifest-mismatch` |
| 7 | `tree-vs-commit` | `not-reusable:tree-only-substitution` |
| 8 | `expired` | `not-reusable:retention-expired` |
| 9 | `mutable-store` | `not-reusable:store-not-immutable` |
| 10 | `forged-identity` | `invalid:producer-identity-mismatch` |
| 11 | `checksum-mismatch` | `invalid:artifact-digest-mismatch:unit-tests-log` |
| 12 | `result-mismatch` | `not-reusable:result-summary-mismatch` |
| 13 | `timeout-cancel` | `not-reusable:producer-cancelled` |
| 14 | `unexpected-skip` | `not-reusable:unexpected-skip` |
| 15 | `unauthorized-consumer` | `not-reusable:consumer-not-authorized` |
| 16 | `release-reusable-false` | `not-reusable:release-reuse-not-permitted` |
| 17 | `prepared-env-digest` | `not-reusable:prepared-env-mismatch` |
| 18 | `missing-field` | `invalid:missing-field:selection` |
| 19 | `observational-to-required` | `not-reusable:observational-proof-for-required-gate` |
| 20 | `stale-attempt` | `not-reusable:stale-attempt` |

Additional adversarial checks: moving-ref-only provenance (`refs/heads/main` as `workflow_revision_sha`) → `invalid:moving-ref-provenance`; retention horizon shorter than the consumer's minimum → `not-reusable:retention-shortfall`; dirty subject → `invalid:dirty-subject`; non-canonical encoding → `invalid:non-canonical-encoding`; unparseable bundle → `invalid:unparseable-bundle`; tampered-but-re-addressed bundle → `invalid:bundle-id-mismatch`; incomplete consumer request → `invalid:incomplete-consumer-request`; stale/unknown artifact expectations → named `invalid:stale-artifact-expectation:*` / `invalid:unknown-artifact:*`; missing claim field → `invalid:missing-field:claims.0`.

**No checksum-only path (§5.13).** Every fixture directory's `SHA256SUMS` is *consistent with its own (possibly corrupted) `bundle.json`*, and the forgery test additionally re-addresses a tampered bundle and regenerates a matching SHA256SUMS before validating. Every rejection above therefore comes from a semantic step — identity, environment, retention, authorization — not from a checksum. Internal consistency never grants acceptance.

## 3. Compatibility-policy notes

1. **Default: incompatible.** Cross-version, cross-platform, cross-security-profile and cross-release reuse is prohibited unless the consuming claim explicitly declares compatibility (§5.8/§5.9). The shadow case reuses only inside one exact environment class (`linux-racket-8.10-strict`).
2. **No implicit "close enough".** Every comparison is exact (versions, digests, SHAs, profiles). A weaker security profile is never satisfied by a stronger-looking neighbor; ranks are `none < ordinary < strict` and any equality break is a named mismatch.
3. **Release reuse is opt-in.** `release_reusable` defaults to `false`; a release-context consumer with `release_reusable=false` is rejected even when explicitly allowed as a regular consumer (`release-reusable-false` fixture).
4. **Fail-closed on unknown.** Unknown top-level or sub-object fields, unknown enum values (subject mode, security profile, gate class, prepared-env mode), and unknown artifact expectations are rejected, not ignored. Missing fields are `invalid` — never "compatible".
5. **Observational ≠ required.** A claim whose `gate_class` is `observational` (the W0 claim-metadata extension, carried in the bundle as `claims[].gate_class`) is never reusable by a required gate (§9 case 19).
6. **Every rejection is a retained decision.** Step 15 emits a `q.reuse-decision/1` record (consumer, bundle_id, claim ids, subject, decision, reason) so §11.3's no-silent-fallback rule has a concrete carrier. Fallback remains the consumer's normal proof path; a silent skip is prohibited.
7. **W9 gate.** This wave removes nothing. The validator is proven fail-closed first; only then can W9 consider replacing a recomputation, and §11.6's rollback discipline applies to any such activation.

## 4. Prototype limitations (honest scope)

- The fixture corpus uses synthetic-but-realistic digests and SHAs; no live evidence store was integrated in W5 (the store contract is W9 scope).
- Timestamps accept the strict UTC (`Z`) RFC3339 subset; offsets are rejected as malformed (fail-closed).
- `bundle_id` covers the whole bundle; artifact digests are verified against the *consumer's* independently computed expectations, which in CI will come from the evidence-store manifest.
- The writer validates completeness (structure), while compatibility/policy semantics live in the validator; producers additionally self-check with the writer test corpus.

## 5. Verification

- `racket tests/test-proof-bundle-writer.rkt` — 23/23 checks green.
- `racket tests/test-proof-bundle-validator.rkt` — 16/16 checks green (20/20 fixtures rejected with named reasons; `valid-base` → `reusable`).
- `racket scripts/run-tests.rkt tests/test-proof-bundle-writer.rkt tests/test-proof-bundle-validator.rkt` — 2 files / 39 tests, 0 failures (RUN-SUMMARY retained in the wave validation file).
- `racket scripts/run-tests.rkt --suite fast` — unsharded (coordinator-owned lane, /var/tmp/q-w5-chain.log): **1187/1187 files, 17,575/17,575 assertions PASS + metrics 5/5** on head `eb6393b5`. (The sharded 8× executor observation of a `tests/test-interfaces-tui.rkt` failure was NOT reproduced: the file passes standalone (exit 0) and in the unsharded chain — recorded as an unreproduced executor observation, consistent with the W3 precedent in FLAKE-FORENSICS-v1.00.29.md.)
- `racket scripts/metrics.rkt --sync-all README.md && racket scripts/metrics.rkt --lint` — PASS (README metrics resynced).
