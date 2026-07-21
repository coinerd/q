# AUDIT — v0.99.52 Post-Release In Depth

**Date:** 2026-07-18  
**Audited release:** `v0.99.52`  
**Release/main/tag target:** `7b169a7f354fd8655e2726297c704f36e6b9ddbb`  
**Source milestone:** GitHub milestone #837  
**Source plan:** `.planning/PLAN-v0.99.52-v0.99.50-AUDIT-REMEDIATION.md`  
**Audit disposition:** **REJECTED — release authentic; substantive milestone acceptance failed**

## 1. Executive Summary

v0.99.52 is an authentic published release. Main, the annotated tag, downloaded tarball, and manifest agree sufficiently to identify the released source and artifact. Current deterministic lint, compile, fast, smoke, architecture, workflow, and TUI gates pass after one superseded stale-compiled failure. All nine PRs were merged from heads with successful protected checks, and milestone #837 is mechanically closed with 25/25 issues closed.

Those facts do **not** establish that all F-01 through F-15 were remediated. The audit found acceptance-critical defects and evidence gaps:

1. dangerous model-invokable subagent batches remain permissively approved by default in headless execution;
2. approval decisions are correlated only to request IDs, not to the immutable snapshot commitment shown to the user;
3. no retained nine-scenario real-provider W6 acceptance campaign exists, and several scenario drivers/verifiers remain incompatible with production event shapes;
4. compact lifecycle reducer handling lacks session correlation;
5. GUI resume still constructs a sibling session;
6. session model/tool-call metadata remains untruthful;
7. contemporaneous external STATE and VALIDATION were left contradictory at milestone closure, directly recurring the GSD drift finding;
8. release publication occurs before exact-artifact smoke, and release repair retains the same bypass;
9. gate-evidence and uploaded-manifest validators retain fail-open/under-specified paths.

Accordingly, the v0.99.52 tag and release should remain historical facts, but milestone #837 must not be treated as substantively acceptance-complete. Remediation belongs in a follow-up milestone; do not rewrite the release.

## 2. Method

The audit followed GSD review discipline:

- pulled GitHub `main`, tag, and external planning state;
- compared plan, STATE, VALIDATION, q-side wave evidence/review/validation records, PR heads/check runs, workflow runs, and milestone state;
- independently reviewed security, runtime/session/compact, explorer/verifier, governance, and release boundaries;
- ran focused and broad deterministic tests at exact release SHA;
- downloaded and independently hashed/extracted release assets;
- separated release authenticity from substantive acceptance;
- retained evidence under `.planning/v0.99.52-post-release-audit/`.

No credential-bearing real-provider run was performed. Existing retained evidence was audited instead; no provider cost was incurred.

## 3. Release and Repository Truth

### 3.1 Authenticity — PASS

- `main`, `origin/main`, and `v0.99.52^{}` resolve to `7b169a7f354fd8655e2726297c704f36e6b9ddbb`.
- Annotated tag object: `dcb887e74c3a3b52b8478a760a06002d29855b3c`.
- Main CI run `29640934881`: success.
- Release workflow run `29640943562`: success.
- Public assets:
  - `q-0.99.52.tar.gz`: 3,380,925 bytes, SHA-256 `aa0cca5566c86d3e4ae5011e31b061802fdb5f306226d56818c8d028d8909074`;
  - `release-manifest.json`: 506 bytes, SHA-256 `f9d340e03f479afe772061cecf54fe942b50af22cd57ec3d675b7a150a7ad67f`.
- Manifest asset name, size, and digest match the downloaded tarball.
- Manifest traceability contains the full tag commit and annotated tag object matching Git.
- Clean extraction reports `q version 0.99.52`.
- Milestone #837 is closed with 0 open / 25 closed.

Raw independent download/hash/tag/extraction output is retained in `v0.99.52-post-release-audit/release-asset-verification.log`; GitHub governance state is retained in `github-governance.json`.

### 3.2 PR gate truth — mostly PASS

PRs #8754–#8762 have final heads and squash merge SHAs recorded in `v0.99.52-post-release-audit/pr-merge-inventory.tsv`. Protected checks succeeded on each final head before merge. Earlier cancelled/failed runs exist but were superseded; they must not be conflated with the final head conclusion.

This is stronger than the retained external ledger for W7/W8, which omits final exact-head evidence. GitHub establishes green-before-merge mechanics, but does not cure missing substantive acceptance evidence.

## 4. Current Deterministic Verification

At exact release SHA:

| Gate | Result |
|---|---|
| `raco make main.rkt` | PASS |
| `racket scripts/lint-all.rkt` | PASS — 23 checks, 1 warning |
| smoke | PASS — 19/19 files, 289/289 tests |
| architecture | PASS — 14/14 files, 182/182 tests |
| workflows | PASS — 28/28 files, 146/146 tests |
| TUI | PASS — 83/83 files, 1310/1310 tests |
| fast, first run | FAIL — 2 stale compiled/linklet load failures |
| direct failed-file reruns | PASS — 4 and 80 tests |
| fast, superseding run | PASS — 1038/1038 files, 15317/15317 tests |
| focused audit defect set | PASS — 90 tests |
| focused accepted-findings set | PASS — 89 tests |
| release-note lint 0.99.52 | PASS |

Raw timestamped output for the current compile/lint/suite/focused run is retained in `v0.99.52-post-release-audit/current-deterministic-gates.log`; accepted-finding focused output is in `accepted-findings-focused.log`. The first fast failure logs are retained separately and classified as stale compiled-artifact interference, not silently replaced. The complete superseding run establishes current deterministic suite health. Green tests do not negate boundary defects absent from those tests.

## 5. Finding-by-Finding Disposition

| Finding | Release claim | Audit disposition | Reason |
|---|---|---|---|
| F-01 | Closed | **FAIL — Critical** | Dangerous headless batches default to approved; snapshot commitment not bound to approval registry/decision. |
| F-02 | Closed | **PARTIAL — High** | Lexical/link validation works, but check-to-open symlink replacement remains raceable. |
| F-03 | Closed | **FAIL — Critical** | No retained all-nine real-provider campaign at PR head, merged main, or release SHA. |
| F-04 | Closed | **PASS** | Canonical provider-neutral transport is implemented and adapter round trips are tested. |
| F-05 | Closed | **PASS** | Denied/no-child and five child terminal outcomes are typed and exactly-once. |
| F-06 | Closed | **PARTIAL — High** | Runtime compact chain is correlated; TUI reducer omits session matching. |
| F-07 | Closed | **FAIL — High** | Provider identity is uncorrelated asserted trace metadata; production producer can emit empty identity. |
| F-08 | Closed | **FAIL — High** | Several real drivers cannot produce verifier-required production events; manifest/injection checks are weak. |
| F-09 | Closed | **PASS with residual risk** | Actual OAuth access/refresh/client secret keys are covered; generic OIDC/cloud/private-key fields remain outside schema. |
| F-10 | Closed | **PARTIAL — High** | Protected PR checks worked, but external `wave_finish` still merges immediately, ignores merge response, then closes issues. |
| F-11 | Closed | **PARTIAL — High** | CLI/print/interactive paths improved; GUI ignores requested session and creates a new one. |
| F-12 | Closed | **PASS with residual risk** | Correlated approval lifecycle is materially fixed; exported legacy compatibility API retains a lower-confidence stale-decision race. |
| F-13 | Closed | **FAIL — Medium** | At closure, STATE said complete while VALIDATION said no wave accepted/W5–W8 not started/not release-eligible. |
| F-14 | Closed | **FAIL — Medium** | Tool calls are counted using unrepresentative entry kind; model selects first rather than latest change. |
| F-15 | Closed | **FAIL — Medium** | Public release precedes smoke; repair bypass remains; gate/manifest validators fail open or under-verify. |

## 6. Detailed Findings

### A-01 — Critical — Dangerous headless batch execution still bypasses HITL (F-01)

Evidence:

- `tools/builtins/spawn-approval.rkt:20-23` defaults `current-spawn-approval-result` to `#t`.
- `tui/approval-channel.rkt:55-59` starts lifecycle mode as `'headless`.
- `tools/builtins/spawn-approval.rkt:74-91,143-150` uses that permissive decision whenever no interactive channel exists.
- `tools/builtins/spawn-subagent-helpers.rkt:110-120` gives omitted child declarations `read-only`, `file-write`, and `shell-exec` defaults.
- `tests/test-spawn-subagents-approval.rkt:371-388` intentionally proves an approved headless dangerous batch executes.

Impact: channel-less model execution can run shell-capable subagents without correlated human authorization. This violates the locked requirement that every model-invokable dangerous capability path use correlated approval. The audit did not separately prove reachability through every frontend, so the claim is deliberately scoped to the shared channel-less boundary.

Required remediation: default headless dangerous approval to deny. Any automation authorization must be explicit, authenticated, capability-bounded, request/snapshot-correlated, and production-wired rather than a permissive parameter default.

### A-02 — High — Approval is not bound to the immutable snapshot commitment (F-01)

The batch snapshot and digest are generated and published, but `pending-approval` stores only request ID/generation/state/boolean (`tui/approval-channel.rkt:124-153`). `approval-put-for-id!` delivers only request ID and Boolean. The TUI normalizes and retains request ID/capabilities/preview while discarding authoritative snapshot/digest (`tui/state-events/core-handlers.rkt:569-586`).

Impact: a subscriber on the public synchronous event bus can race a substituted presentation under the observed live request ID; the user-visible prompt is not cryptographically or structurally bound to the execution commitment.

Required remediation: register the immutable digest with the pending request; require the same digest on presentation and decision delivery; reject any mismatch or duplicate presentation.

### A-03 — High — Session containment remains check-to-use raceable (F-02)

`runtime/session/session-path.rkt:80-105` validates links and returns a pathname. Consumers then separately open/replace/mutate it, including session store and trace paths. Existing tests cover pre-existing symlinks and some post-open replacement, not an attacker swapping the session/artifact between validation and open.

Required remediation: use descriptor-relative/no-follow opens and mutations or an equivalent atomic filesystem boundary. Add synchronized check-to-open swap tests for session JSONL, pending markers, index, trace, and recursive deletion.

### A-04 — Critical — Required all-nine real-provider acceptance evidence is absent (F-03/F-08)

`docs/reports/gsd-wave-validation/v0.99.52-w6.rktd` explicitly leaves this remaining item:

> Real-provider acceptance campaign: all nine scenarios must PASS at exact PR-head SHA before merge.

No v0.99.52 evidence manifest, real summary, captures, or trace campaign exists under `.planning/` or `docs/reports/`. No evidence binds all nine scenarios to W6 final head `06df4536…`, merged main `233b9ac9…`, or release `7b169a7f…`.

Required remediation: do not infer acceptance from deterministic verifier tests. Run a protected, credential-redacted, default-executor campaign for all nine exact tags at a remediation PR head and merged main; retain immutable manifest, per-scenario traces/captures, provider/model/session/turn correlation, digests, cleanup proof, and exact SHA.

### A-05 — High — Explorer drivers and verifiers remain production-incompatible (F-08)

Examples:

- MAS and durable-memory are registered with tools disabled in `scripts/tmux-tui-explore.rkt:62-67,81-86`.
- The executor sends only one prompt for most scenarios and does not perform store→restart→retrieve or exact resume sequences.
- `verify-mas` requires request `child-id`/`tool-call-id` fields that singleton production approval does not emit.
- `verify-durable-memory` requires retrieval `memory-id`/`result-present?` fields absent from the production retrieval event.
- `verify-resume` requires `session.started(reason=resume, previous-session-id)`, while production resume emits a different shape.
- Tests use hand-built fixtures containing the expected fields rather than serialized production emitter output.

Required remediation: implement multi-step production drivers and align verifiers with real emitted schemas. Positive fixtures must be created through production constructors/serialization, and decoy/mismatch cases must fail.

### A-06 — High — Provider provenance remains weak and uncorrelated (F-07)

`positive-provider-identity?` in `scripts/tmux-explore/verifiers.rkt:97-106` accepts any non-mock `turn.started` provider/model anywhere. It does not correlate that event to the selected completion/session/turn. Production inner-turn emission can populate empty provider/model. The values are local assertions, not tied to an adapter response or provider request ID.

Required remediation: require exact session/turn/request correlation to a production adapter response and configured provider/model identity. Unrelated or injected `turn.started` events must fail.

### A-07 — High — Evidence manifests and gating can accept filtered/injected evidence (F-08/F-15)

- Explorer execution exposes an injected executor and can label a filtered one-scenario result as all-scenarios.
- Gating does not require all nine tags exactly once.
- checkout SHA can be environment-overridden.
- `verify-evidence-manifest` checks presence/positive counts but does not recompute digests or bind exact expected SHA/version/tag inventory.

Required remediation: acceptance mode must make executor injection and tag filtering impossible, derive SHA from Git only, require the exact nine-tag inventory once each, and recompute every artifact digest.

### A-08 — High — Compact TUI handling omits session correlation (F-06)

`tui/state-events/core-handlers.rkt:490-506` correlates compact lifecycle events only by pending request ID. When no request is pending, every compact lifecycle event is accepted as a legacy/automatic event. It never compares `event-session-id` with `ui-state-session-id`, although interrupt handling has that pattern.

Impact: a foreign session on a shared bus can alter status/transcript or clear compact state.

Required remediation: require target session equality for all session-scoped compact events and request equality whenever pending. Add foreign-session reducer tests with and without pending requests.

### A-09 — High — GUI resume creates a sibling (F-11)

`gui/main.rkt:244-256` unconditionally calls `make-agent-session rt-config)` instead of the canonical open/resume resolver. CLI parsing permits GUI mode with a requested session.

Required remediation: use the canonical resolver and add exact-ID/no-sibling GUI construction tests.

### A-10 — Medium — Session metadata remains untruthful (F-14)

- `interfaces/sessions.rkt:164-176` returns the first model-change, not the latest/current model.
- `interfaces/sessions.rkt:279-282` counts only entries whose kind is `tool-call`.
- Production tool calls are normally content parts inside assistant message/text entries, so real tool-call counts are zero or low.
- Tests fabricate an unrepresentative top-level `kind="tool-call"` entry and only one model change.

Required remediation: scan chronologically and retain the last valid model change; count canonical tool-call parts inside production-persisted assistant messages with legacy compatibility tests.

### A-11 — Medium — Contemporaneous GSD state was contradictory and stale at closure (F-13)

At milestone closure, external STATE declared COMPLETE, all findings closed, and milestone closed, but:

- omits W7 from the wave table;
- retains a post-W4 blocker and W5 as next action;
- external VALIDATION header says no wave accepted;
- VALIDATION marks W5–W8 Not started;
- VALIDATION final result says not eligible for completion, tag, or release.

The q-side W6 validation simultaneously marked a required real campaign outstanding. This was a contemporaneous governance failure and direct recurrence of the finding the milestone claimed to close. The post-release audit has now corrected the top-level STATE/VALIDATION disposition to authentic-release/substantive-rejection; the historical incomplete wave sections are intentionally retained as evidence of the closure-time drift.

Required remediation: maintain one authoritative synchronized ledger. Release mechanics and substantive acceptance must have separate statuses. Never mark a wave Done while required evidence is listed as outstanding.

### A-12 — Medium — Release is published before exact-artifact smoke (F-15)

The actual release run proves this order:

1. test completed at 10:41:14Z;
2. release/publication ran 10:41:17Z–10:44:32Z;
3. public release was published at 10:44:29Z;
4. smoke ran 10:44:34Z–10:48:30Z.

`.github/workflows/release.yml` defines `release needs: test` and `smoke needs: release`. `tests/test-release-workflow-contract.rkt:55-61` enforces the unsafe order.

Impact: smoke failure leaves a public release, exactly the failure mode F-15 intended to prevent.

Required remediation: build/upload an internal workflow artifact, verify/install/smoke it, then publish the already-verified bytes. Alternatively create a draft and promote only after smoke; draft must not be treated as public completion.

### A-13 — Medium — Repair publication bypasses equivalent safeguards (F-15)

`.github/workflows/release-repair.yml` builds and publishes/replaces assets before any exact-artifact install/smoke. It lacks equivalent mandatory release-note lint, exact-SHA gate-evidence enforcement, and full downloaded asset/manifest verification.

Required remediation: share one release pipeline contract between normal and repair publication; no mutating mode may bypass pre-publication smoke and traceability.

### A-14 — Medium — Gate and manifest hardening remains fail-open (F-15)

- `validate-gate-evidence-entry` accepts `git_sha="unknown"` even when an exact expected SHA exists.
- legacy `.passed` evidence fabricates unknown SHA, one test, zero failures/timeouts.
- missing/future timestamp, suite identity, inventory/count consistency, and explicit result are under-validated.
- `verify-uploaded-manifest` compares only version/tag/tarball name, not expected full commit, tag object, asset size, or downloaded asset digest.
- standard manifest validation explicitly tolerates unknown traceability.
- milestone gate can infer manifest availability from asset existence rather than parsing and verifying downloaded content.

Required remediation: uncertainty must fail closed. Remove legacy evidence from strict readiness. Require full SHA, suite, inventory, result, counts, bounded timestamp, recomputed hashes, tag commit/object, and downloaded uploaded-manifest verification.

### A-15 — High — External merge helper can report/close after failed merge (F-10)

`/home/user/src/q-agent/scripts/gh_helpers.py:737-784` creates a PR, immediately calls merge, ignores the response, prints “merged,” then closes issue tree and marks Done. It does not wait for current-head checks/review/validation or confirm the merge succeeded.

GitHub branch protection prevented this from causing red final-head merges in W0–W8, but the helper remains operationally unsafe and can corrupt issue/board truth after a rejected merge.

Required remediation: replace it with exact-head two-phase finalize; verify all required checks and review/validation evidence; merge conditionally; refetch merged state/SHA; only then close issues/board.

### Residual R-01 — Medium — Legacy approval compatibility API may accept a stale decision after teardown

`approval-await-result` snapshots a channel and waits on decision/cancellation events; `approval-put!` separately snapshots the current channel and posts after releasing the registry lock (`tui/approval-channel.rkt:97-120`). Teardown can race between observation and post, leaving both cancellation and an old `#t` decision ready. Production batch approval uses the correlated APIs, so production reachability is not established and this does not by itself fail F-12. Retire the legacy API or make generation validation atomic, and add a synchronized put-vs-clear race test.

## 7. Accepted Work and PASS Evidence

### F-04 — PASS

- Production owner: `util/message/provider-transport.rkt`; parent/child bridges delegate and provider adapters translate at native boundaries.
- Focused tests: `tests/test-provider-tool-transport.rkt` and `tests/test-provider-transport-architecture.rkt`.
- Current focused evidence: `v0.99.52-post-release-audit/accepted-findings-focused.log` (part of 89 tests PASS).

### F-05 — PASS

- Production boundaries: denied/no-child result and terminalization in `tools/builtins/spawn-subagent.rkt`; helper terminal mapping in `spawn-subagent-helpers.rkt`.
- Focused tests: `tests/test-spawn-subagent-terminal-outcomes.rkt` and `tests/test-spawn-subagent-helpers-terminal.rkt`.
- Current focused evidence: `accepted-findings-focused.log`.

### F-09 — PASS with residual defense-in-depth risk

- Production boundary: `util/credential-redaction.rkt` covers normalized access-token, refresh-token, OAuth token, client-secret, API-key, capability token/secret, authorization code, and code verifier keys.
- Focused tests: `tests/test-oauth.rkt` and `tests/test-w2-credential-redaction.rkt`.
- Current focused evidence: `accepted-findings-focused.log`.
- Residual: generic OIDC `id_token`, service-account private-key, and cloud-session credential names are not covered. Add them if supported provider/config schemas can carry those fields.

### Other accepted mechanics

The audit confirms meaningful delivered work:

- F-04 canonical provider transport is materially improved and tested across adapters.
- F-05 typed terminal outcomes and exactly-once child terminalization are materially improved.
- OAuth access/refresh/client-secret redaction is centralized and tested.
- lexical session-ID validation blocks straightforward traversal and pre-existing symlinks.
- approval lifecycle generation, pending-state locking, teardown, and immutable local execution snapshots are materially improved.
- runtime compact requested→started→terminal correlation is materially improved.
- print/interactive/SDK resume and session information improved.
- branch protection and protected check matrices were active for all nine final PR heads.
- release notes/version surfaces and deterministic suites are current.
- release assets are authentic and independently traceable after publication.

These partial successes should be preserved during remediation.

## 8. Required Follow-Up Order

### P0 — immediate

1. Deny dangerous headless subagents by default; bind approval registry/decision to immutable snapshot digest.
2. Repair production explorer drivers/verifiers and retain all-nine real acceptance on exact remediation PR/main SHAs.
3. Correct STATE/VALIDATION truth and reopen/track unresolved finding acceptance in a follow-up milestone.
4. Change release DAG so exact artifact smoke precedes public publication; harden repair path equivalently.

### P1

5. Add compact reducer session correlation.
6. Fix GUI exact-session resume.
7. Correct current-model and real tool-call metadata.
8. Make strict gate/manifest evidence fully fail closed and independently downloaded/verified.
9. Replace unsafe external wave finalizer.
10. Close session filesystem check-to-use races with atomic no-follow operations.

### P2

11. Retire or repair legacy approval compatibility race.
12. Expand redaction defense for OIDC identity tokens, private keys, and cloud session credentials where supported schemas can carry them.

## 9. Final Verdict

| Dimension | Verdict |
|---|---|
| Git/tag/release authenticity | PASS |
| Public asset hash/version traceability | PASS after independent download |
| Current deterministic suite health | PASS after superseding stale-compiled rerun |
| Green protected checks on final PR heads | PASS |
| GSD ledger truth | FAIL |
| Security acceptance | FAIL |
| Real-provider explorer acceptance | FAIL |
| Resume/session/compact completeness | FAIL/PARTIAL |
| Fail-closed release publication | FAIL |
| All F-01–F-15 substantively closed | **NO** |
| Milestone #837 substantive acceptance | **REJECTED** |

v0.99.52 remains a valid historical release, but the statement “all 15 audit findings are now closed” is not supported by implementation and evidence. A follow-up remediation milestone is required.
