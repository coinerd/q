# Wave-Delivery Integrity Release — v1.00.31

Wave: W7 (issue #9730, milestone 896) of the v1.00.31 GSD Wave Delivery Integrity
campaign, plan `fb67e0429ed155a4b4e3f31afe3ef3ca7748ee891c06247b11e5e2a86cfebd75`.
Branch: `campaign/v1.00.31-w7`.

This report records the two things W7 was asked to record: the v1.00.30 W4 repair
outcome, and the release bake of v1.00.31. It states what was verified, on which
tree, and what remains outstanding. Nothing here is projected, estimated or
carried over from a previous release.

## 1. v1.00.30 W4 outcome: DELIVERED

The v1.00.30 W4 wave (issue #9690, milestone 895, plan
`96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8`) was blocked
before its re-delivery because the action declared a producer invocation of
`scripts/ci/compiled-root.rkt` that could not execute as written, while the
text-only contract test matched the declared string and the rollback drill drove
the CLI with the correct flags — so the wave was green on a broken call. The block
report records that primary blocker, 7 secondary findings and 6 numbered required
repairs.

It was re-delivered through the hardened ladder:

| step | result |
|---|---|
| independent review | 12 rounds, 11 REQUEST_CHANGES, final **APPROVED**, 0 non-blocking observations |
| implementation | `1158f1b7023b08c9608f59c8cd4c0c04915d5de6`, content digest `cc357910…`, record commit pure |
| implementation PR #9752 | squash-merged as `cc25b3663362d032b9a23a9d22f9dd5edbe637d8`, 22/22 required checks green |
| binding PR #9753 | squash-merged as `5f4a80fcda0c80396ca033184ae085139f02002c`; diff is exactly the three record files; publication digest is EMPTY_SHA; record-commit pure; independent binding review APPROVED |
| governance | main run 36087264560 concluded success → `governed` → `sync` → `synchronized` → `status` → **delivered** |
| issue #9690 | closed as completed, board Done — after `status` returned delivered |

Full detail, including every collateral defect the repair exposed in
already-published state: `artifacts/wave-delivery-integrity/v1.00.31-w7/w4-recovery.json`.

### One thing this report will not smooth over

The governance gate requires the binding commit's own main run to have concluded
success. Its first attempt failed on a teardown race inside the delivery
controller's own disposable test-world fixtures: a detached `git gc --auto`
repacked `.git/objects` while `unittest`'s `TemporaryDirectory` cleanup walked
the tree, and unittest reported that finalizer error as a case `ERROR` even
though all 129 assertions had passed. The race was fixed at its source on main
(#9754, `50bdf7334`) by disabling auto-gc in those throwaway repositories, and
the failed job was then re-run, after which the run concluded success. No check
was skipped, weakened or re-implemented to obtain that result, and the fix is a
separate commit on main rather than a change to the W4 record.

### Still outstanding

The W4 frozen doc asks for the activation merge SHA **and** the PR latency regime
fingerprint for the compiled-root pilot. The merge SHA is now known and recorded.
The fingerprint needs a hosted latency sample on the exact activation head, which
is a coordinator-owned measurement; it is recorded as outstanding rather than
estimated or back-filled.

### A correction to the W4 local capture, and how it was found

The W4 repair retained a local `verify-chain.txt` capture, and the W4 review
narrative cites its numbers. When the W7 bake ran its own verification chain, the
`RUN-SUMMARY` line reported `runner-version=1.00.29` on a tree at 1.00.31 — and the
cause was that `resolve-base-dir` in `scripts/run-tests/classify-metadata.rkt` tried
the `q`-shaped candidates *before* the directory it was handed. Every worktree here
sits next to a `q/` clone, so the runner silently collected and executed **the clone's**
tests and printed a summary for a checkout nobody asked about.

Two things follow, and both are recorded rather than smoothed over:

- **The W4 capture's four runner-suite sections** (arch, security, workflows, fast)
  described that neighbouring clone, not the repaired W4 tree. The suite passed
  there, so the capture was not wrong in its verdict — it was simply not a
  measurement of the wave it was filed under. The focused `raco test`,
  `check-deps`, metrics and provenance sections did run against the W4 worktree
  (their outputs carry its absolute paths). W4's authoritative gates are untouched:
  PR #9752's 22/22 required checks ran on the branch in CI, main run 36087264560
  concluded success, and governance/`status delivered` followed from those.
- **The W4 content is green when measured properly.** Re-measured on 2026-09-25
  against implementation head `1158f1b70…` in a worktree with no `q/` sibling:
  focused 185 tests, arch 32/32, security 64/64, workflows 33/33, fast 1208/1208
  (17987 tests), deps/metrics/provenance all exit 0, and all six bound W4 artifact
  checksums verify. Retained at
  `artifacts/wave-delivery-integrity/v1.00.31-w7/raw/w4-remeasure-2026-09-25.txt`.

The root cause is fixed in this bake — the launch tree now wins over any `q`-shaped
sibling, with four regression tests — and the first chain that actually measured its
own tree is what surfaced it, along with **12 test files that every previous
misdirected run had been hiding**. That is the whole argument for measuring the tree
you think you are measuring.

## 2. Release bake of v1.00.31

v1.00.30 never shipped a tag: its W4 wave was blocked, so v1.00.31 is the next
actual release. The bake is minimal and generator-driven:

- `util/version.rkt` is the single source of truth (1.00.29 → 1.00.31).
- `info.rkt`, `README.md`, `docs/` and `wiki-src/` are synced **only** by the
  canonical generators: `sync-version.rkt --write --all`,
  `lint-doc-freshness.rkt --fix`, `sync-readme-status.rkt --sync`,
  `metrics.rkt --sync-all`.
- The CHANGELOG entry names the exact W6 verdict and the exact W4 outcome, and
  `lint-release-notes.rkt --version 1.00.31 --check` passes.

### BUG-0009 literal sweep

Bumping the canonical version made the BUG-0009 lint flag 56 occurrences of
"1.00.31" in `tests/`, because the campaign's own artifact paths, fixture branch
names and comments carry the version. The same pattern the v1.00.29 bake used was
applied: current-campaign artifact paths, fixture branch names and the wave-gate
fixture now derive from `q-version` (so the next bump fails loudly instead of
silently pinning a stale literal), frozen prior-release artifacts stay literal
with the reason recorded in a comment, and comment-only mentions are reworded.
The `define-runtime-path` forms keep their literal runtime-path anchor and compute
only the version segment. The lint now reports
`1511 test files scanned, 0 hard-coded "1.00.31" literals`, and a 19-file sweep
run green at the final head through the project's **own** runner: **376 tests,
376 passed, 0 failed, 0 timeouts**, every file exit 0, and 0 files reporting no
test count (`raw/version-derivation-tests.log`). The instrument is the point.
Bare `raco test` — which the first two attempts used — is wrong twice over: it
exits 0 even when a case fails, and for a file whose entry point is `module+ main`
rather than `module+ test` it executes *nothing* and still exits 0. Two of the 19
have that shape, so their greens were vacuous: a pass that ran zero tests. The
capture also miscounted its own footer once, by matching rackunit's green
`0 failure(s)` line as a failure. Three detectors in a row were wrong before the
subject was finally measured with the instrument the runner actually gates on
(`failed-result?` is purely *exit code neither 0 nor 2*) — which is precisely the
trap that hid the `worker-security` W2 regression in the first place, and the
reason the capture now carries the runner's own `Tests:` lines so a vacuous green
is detectable rather than assumed absent.

### Release preflight

Six of the seven CI-strict tag-publish gates run on the clean bake commit
`b0f19d6bd` and pass: release-notes,
fmt-canonicality (93 changed `.rkt` files since the last tag), metrics lint,
README status sync, the plain-tar symlink audit and the bundle dry-run
(`artifacts/wave-delivery-integrity/v1.00.31-w7/release-preflight.json`,
`raw/preflight-locally-runnable.txt`).

The seventh gate — `lint-release-readiness.rkt --strict` — is **deferred by
design**, not skipped for convenience. It refuses to run on a campaign branch
(`check-main-branch` accepts only `main` or a detached HEAD in tag-publish
context), and the `.gate-evidence/` records it consumes must name the release
commit SHA and the release version. Running it before the merge would require
faking either the branch or the evidence.

### Artifact checksums

All three manifests verify in full from the repository root — 6/6 W6, 11/11 W7,
1/1 tier-ownership, 18/18 entries — and the **unfiltered per-manifest output is
retained in the chain capture** (`raw/verify-chain.txt`, section "artifact
checksum verification, full unfiltered output"). It lives there rather than in a
separate log because the artifact-provenance linter requires every file under the
wave artifact directory to be bound by `SHA256SUMS`, and an unbound sidecar is a
drift finding in itself.

That section exists because the first chain excerpt showed one representative
line per manifest while its summary claimed all of them verified, so the evidence
did not substantiate the claim. On the self-reference, the section is written
first, `SHA256SUMS` is bound over the finalized log, and the verification is
re-run — so the `verify-chain.txt: OK` line in the retained output is true of the
committed bytes rather than of a superseded state. Review round 4 challenged the
earlier wording of that caveat, which claimed the log could not contain the
verification of its own entry while the output contains exactly that line; the
challenge was upheld and the wording corrected. The authoritative check is still
one `sha256sum -c` from the repository root, which returns 18/18 OK.

It is therefore run, in this order, after this wave is merged and main CI is
green:

1. check out clean main at the release SHA;
2. run the `fast`, `tui`, `arch` and `workflows` suites with
   `--record-gate-evidence` (refused for sharded runs by design, so they run
   un-sharded);
3. run `lint-release-readiness.rkt --strict --context pre-tag`;
4. create the annotated `v1.00.31` tag locally and run the full CI-strict
   preflight in `--context tag-publish`;
5. push the tag **only** if all seven gates pass, then verify the public assets,
   checksums and provenance and the `verify-public` workflow.

The outcome of steps 1-5 is recorded in `.planning/STATE.md` and
`.planning/HANDOFF.json`, which are the project's session record; this in-repo
report stays valid because the deferred gate is described by design, not by
result.

## 3. Verdict boundaries

- **PERMANENT** is the W6 verdict and is bounded to its rehearsal: 13 registered
  rows, 13 injected defects refused, 0 failing, clean synthetic control accepted,
  rehearsal head `50bdf7334…`, inputs digest `fb224a6f…`. It is evidence that
  these 13 guards refuse these 13 defects — not a claim that no future defect can
  pass. (Both identities moved when this bake regenerated the matrix after its own
  derivation sweep changed two of the nine digest-bound rehearsal inputs; the
  head is re-stamped to a published commit so it still resolves, and
  `docs/reports/WAVE-INTEGRITY-REHEARSAL-v1.00.31.md` carries the full account.)
- **DELIVERED** for v1.00.30 W4 is a delivery statement with a real merge-SHA
  binding and a successful governance run behind it, not a statement about the
  compiled-root pilot's latency.
- No latency target, cohort result or regime fingerprint is claimed anywhere in
  this release. Where a number does not exist yet, this report says so.
