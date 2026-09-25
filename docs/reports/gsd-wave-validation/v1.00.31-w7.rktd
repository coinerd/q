#hasheq(
 (schema-version . 2)
 (status . "current")
 (milestone . 896)
 (wave . "W7")
 (issue . 9730)
 (branch . "campaign/v1.00.31-w7")
 (implementation-sha . "86002515f9de49d4abd83d83fd279656ab7c15e7")
 (chain-measured-sha . "e9af72fee9c652b16a017d37219d15e7eb5f9f0b")
 (content-digest . "63d84476839ef65f17c6e919e4892b0031d0ecc1b43914d9e56bad6d66452e35")
 (review-artifact . "docs/reports/gsd-wave-reviews/v1.00.31-w7.rktd")
 (planning-sync . "current")
 (red-first
  . #hasheq(
     (command . "The BUG-0009 sweep: a scratch worktree at the version-bumped commit with the 15 swept test files reverted to base, then racket scripts/check-version-expectations.rkt run with that scratch tree as cwd. The lint resolves util/version.rkt relative to the cwd, so running the script by absolute path from another tree reports the wrong tree and yields a false PASS.")
     (failure . "With util/version.rkt at 1.00.31 and the pre-derivation test content, the version-expectation lint exits 1: 56 hard-coded 1.00.31 literals across 15 test files, each of which would go green forever and silently pin a stale expectation after the next bump. Retained: artifacts/wave-delivery-integrity/v1.00.31-w7/raw/red-first-version-expectations.txt.")))
 (red-first-gate-integrity
  . #hasheq(
     (command . "tests/test-worker-security.rkt with the two W2 artifact paths pointed at artifacts/tier-ownership/v9.99.99-w2/ (a directory that does not exist), run through racket scripts/run-tests.rkt")
     (failure . "After the fix the file exits 1 and the runner reports FAILED (38 tests, 35 passed, 3 failed). The identical broken state BEFORE the fix reported PASSED (33 tests, 33 passed, exit=0) while printing FAILURE blocks and a summary line reading 0 failure(s). That is the whole defect: the W2 checks could not fail the gate, so a missing artifact was invisible. Retained: artifacts/wave-delivery-integrity/v1.00.31-w7/raw/red-first-worker-security-w2.txt.")))
 (gate-integrity-mechanism
  . "The suite runner decides a file failed purely on its exit code (failed-result? in scripts/run-tests/reporting.rkt is 'exit code neither 0 nor 2'; the FAILURE-text patterns in parse.rkt only classify the category of a failure that already happened). This was verified directly with two throwaway probes: a failing top-level check-true and an escaping exception each make the runner report the file FAILED, while the same file structured as module-body test-cases after a run-tests call exits 0. W7 corrected a comment in tests/test-runner-base-dir-resolution.rkt that had asserted the opposite.")
 (focused-tests
  . #hasheq(
     (command . "raco test on the release-surface tests, and racket scripts/run-tests.rkt on the four files the W7 repairs touch")
     (result . "passed")
     (details . "test-worker-security.rkt, test-runner-base-dir-resolution.rkt, test-wave-integrity-adversarial.rkt and test-release-entry-current.rkt through the runner: 4 files, 55 tests, 0 failed. The release-surface set (test-version, test-lint-release-readiness, test-lint-release-notes, test-release-workflow-contract) passed earlier in the bake with 109 tests.")))
 (full-regression
  . #hasheq(
     (command . "racket scripts/run-tests.rkt (suite=all, 1409 files, 12 jobs, profile=local) at e9af72fee")
     (result . "passed-with-one-known-environmental-failure")
     (details . "1409 files: 1400 passed, 1 failed, 8 skipped by profile, 29m36s; 20553 of 20554 tests passed. The single failure is tests/test-browser-playwright-sidecar.rkt (Sidecar EOF) because the playwright npm module does not resolve for the node on this host, so the sidecar exits at launch. It is not a W7 regression and is recorded as such rather than waived: the file is untouched by this branch, it fails identically on main at 50bdf7334, and the ci profile declares @requires browser so CI skips it. Raw capture: raw/verify-chain.txt.")))
 (raw-captures
  . #hasheq((suite-chain . "raw/verify-chain.txt — head e9af72fee, every number in this record measured here")
             (affected-sweep . "raw/version-derivation-tests.log — 19 files run through the project runner (racket scripts/run-tests.rkt per file): 376 tests, 376 passed, 0 failed, 0 timeouts, every file exit 0, and 0 files reporting no test count. Supersedes three earlier captures of the same name, each wrong in a different way: the 05:11Z mid-repair capture recording test-wave-integrity-adversarial.rkt red; the 21:53Z capture whose footer counted rackunit's green 0 failure(s) line as a failure; and the 22:38Z capture, still using bare raco test, whose greens for test-deterministic-clock.rkt and test-wave-delivery-integrity-register.rkt were vacuous because raco test executes nothing in a file whose entry point is module+ main")
             (checksums . "raw/verify-chain.txt, section 'artifact checksum verification, full unfiltered output' — 6/6 W6, 9/9 W7, 1/1 tier-ownership, 16/16 entries from the repository root")))
(suites
  . #hasheq((fast . "1209 files, 1209 passed, 0 failed, 20m53s (SUITE-fast-EXIT=0)")
            (arch . "32 files, 32 passed, 0 failed (SUITE-arch-EXIT=0)")
            (workflows . "33 files, 33 passed, 0 failed (SUITE-workflows-EXIT=0)")
            (security . "64 files, 64 passed, 0 failed (SUITE-security-EXIT=0)")
            (tui . "not run locally: the tui lane requires a prepared environment and the readiness gate's .gate-evidence records for it are produced post-merge on clean main, which is where that gate runs")
            (all-four-report . "each RUN-SUMMARY line reports runner-version=1.00.31 and the resolver's base-dir is this worktree, which is the property the base-dir fix restores and the reason these numbers describe this tree")))
 (lint
  . #hasheq(
     (command . "racket scripts/lint-all.rkt, check-version-expectations.rkt, check-deps.rkt, metrics.rkt --lint, sync-readme-status.rkt --check, lint-release-notes.rkt --version 1.00.31 --check, verify-artifact-provenance.rkt --current-wave v1.00.31-w6 and --current-wave v1.00.31-w7")
     (result . "passed")
     (details . "lint-all 23 passed, 0 failed, 2 non-blocking warnings (arch; release-readiness is the branch-gated gate recorded as deferred in release-preflight.json). Version-expectation lint PASSED over 1511 test files with 0 hard-coded 1.00.31 literals. Dependencies OK; all 5 static metrics match README.md; README Status block in sync with the CHANGELOG; release notes valid for 1.00.31. Artifact provenance ok for the w6 directory (951 notes) and the w7 directory (950 notes), each JSON artifact canonical and each SHA256SUMS regenerating byte-identically.")))
 (checksums
  . #hasheq(
     (command . "sha256sum -c on the three manifest files, run FROM THE REPO ROOT (the manifests record repo-root-relative paths, so a run from inside the artifact directory resolves nothing and reports every file missing)")
     (result . "passed")
     (details . "artifacts/wave-delivery-integrity/v1.00.31-w6/SHA256SUMS, artifacts/wave-delivery-integrity/v1.00.31-w7/SHA256SUMS (8 captures) and artifacts/tier-ownership/v1.00.31-w0/SHA256SUMS all verify. An earlier W7 capture recorded a failed checksum run caused by that working-directory mistake; it has been replaced.")))
 (release-preflight
  . #hasheq(
     (command . "release_preflight (7 CI-strict tag-publish gates) at b0f19d6bd, tree clean")
     (result . "six-pass-one-deferred")
     (details . "release-notes, fmt-canonical (93 changed .rkt files since v1.00.29, all canonical), metrics, readme-status, tarball-symlinks and bundle-dry-run pass. lint-release-readiness --strict is recorded as deferred-by-design, not passed: check-main-branch accepts only main or a detached tag-publish HEAD, and the .gate-evidence records it consumes must name the release commit SHA and version, so it cannot pass on a campaign branch without faking the branch or the evidence. It runs on clean main after the merge and before the tag push, together with the --record-gate-evidence runs of the fast, tui, arch and workflows suites; the tag is pushed only if all seven gates pass. Recorded in artifacts/wave-delivery-integrity/v1.00.31-w7/release-preflight.json and raw/preflight-locally-runnable.txt.")))
 (w4-outcome
  . #hasheq(
     (command . "python3 scripts/gsd-delivery.py governance|sync|status --plan 96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8 --wave 4, and GitHub issue/board inspection")
     (result . "delivered")
     (details . "governance -> governed (main run 36087264560 concluded success), sync -> synchronized, status -> delivered at merge cc25b3663362d032b9a23a9d22f9dd5edbe637d8 / publication 5f4a80fcda0c80396ca033184ae085139f02002c; issue 9690 closed as completed and board Done afterwards. The W4-retained local capture is corrected in w4-recovery.json: its four runner-suite sections described the neighbouring q/ clone rather than the W4 worktree, while the focused raco test, check-deps, metrics and provenance sections did run in that worktree. W4's authoritative gates are unaffected because CI checks the branch out itself. A true re-measurement of the W4 head 1158f1b70 in a worktree with no q/ sibling is retained at raw/w4-remeasure-2026-09-25.txt: focused 185 tests, arch 32/32, security 64/64, workflows 33/33, fast 1208/1208, deps/metrics/provenance exit 0, all six bound W4 checksums OK.")))
 (remaining-items
  . (#hasheq(
      (classification . "deferred-by-design")
      (owner . "post-merge release step (coordinator)")
      (rationale . "lint-release-readiness --strict is branch-gated: it requires the release commit to be on main and requires .gate-evidence for the fast, tui, arch and workflows suites that names the release SHA and version. It therefore runs on clean main after this wave merges and before the annotated v1.00.31 tag is pushed, together with the full CI-strict tag-publish preflight. This is recorded as deferred by design rather than reported as passing, and the changelog says six of seven gates pass rather than all of them."))
     #hasheq(
      (classification . "outstanding-coordinator-owned")
      (owner . "coordinator")
      (rationale . "The v1.00.30 W4 frozen doc asks for the activation merge SHA and the PR latency regime fingerprint for the guarded compiled-root pilot. The merge SHA is recorded (cc25b3663362d032b9a23a9d22f9dd5edbe637d8); the fingerprint needs a hosted latency sample on that exact head and is recorded as outstanding rather than estimated or back-filled. It is not a wave-completion gate for W7 and is not claimed anywhere in the release notes."))
     #hasheq(
      (classification . "verdict-bounded")
      (owner . "release report")
      (rationale . "PERMANENT is the W6 verdict bounded to its rehearsal (13 registered rows, 13 refused, 0 failing, clean control accepted, rehearsal head 50bdf7334, inputs digest fb224a6f, 9 rehearsal inputs). It is not a claim that no future defect can pass, and the release report states that boundary explicitly. The head and digest are the values in the committed matrix after this bake regenerated it; the superseded pair is named in the report rather than quietly dropped."))
     #hasheq(
      (classification . "environmental-not-a-regression")
      (owner . "local environment")
      (rationale . "tests/test-browser-playwright-sidecar.rkt fails on this host because the playwright npm module does not resolve for the available node. It fails identically on main, the file is untouched here, and the ci profile skips it. Recorded as a known local gap rather than a pass. Installing the module, or declaring the browser requirement for the local profile, is left to a follow-up rather than changed during a release bake."))
     #hasheq(
      (classification . "follow-up-not-blocking")
      (owner . "campaign backlog")
      (rationale . "The runner treats a file as failed purely on its exit code. W7 fixed the one file that defeated that contract, but the general property (a test file that swallows its own failures while exiting 0 reads as green) is a property of rackunit structure rather than of this wave, and no other test file installs a custom current-check-handler. Worth a dedicated audit; not introduced or widened here."))))
 (head-coherence
  . "The content head is b9a0ac1ad7771306cda061a3ca4e9a4d3ee29278. The suite chain was run at e9af72fee, and `git diff --name-only e9af72fee..b9a0ac1ad` lists twelve paths, none of them under tests/, scripts/, util/, extensions/, ci/ or .github/: the W7 artifact directory's own SHA256SUMS and raw captures, release-preflight.json, w4-recovery.json, the release report, the three evidence records, .gitignore, and one chain log untracked in the same commit. No source or test file changed after the chain ran, so its numbers describe the merge candidate's code. The preflight was re-run at b0f19d6bd and the commits after it change only the artifacts and prose that commit's own capture is about. Each capture names the tree it was actually run against, because a committed artifact cannot contain the SHA of the commit that carries it; the three evidence records are then committed on top of the content head as an evidence-only change.")
(raw-captures
  . #hasheq((suite-chain . "raw/verify-chain.txt — head e9af72fee, every number in this record measured here")
             (affected-sweep . "raw/version-derivation-tests.log — final-head re-run of 19 files, 0 failures, 0 errors; supersedes the mid-repair capture of the same name that recorded test-wave-integrity-adversarial.rkt red")
             (checksums . "raw/verify-chain.txt, section 'artifact checksum verification, full unfiltered output' — 6/6 W6, 9/9 W7, 1/1 tier-ownership, 16/16 entries from the repository root")))
(suites
  . #hasheq((fast . "1209 files, 1209 passed, 0 failed, 20m53s (SUITE-fast-EXIT=0)")
            (arch . "32 files, 32 passed, 0 failed (SUITE-arch-EXIT=0)")
            (workflows . "33 files, 33 passed, 0 failed (SUITE-workflows-EXIT=0)")
            (security . "64 files, 64 passed, 0 failed (SUITE-security-EXIT=0)")
            (tui . "not run locally: the tui lane requires a prepared environment and the readiness gate's .gate-evidence records for it are produced post-merge on clean main, which is where that gate runs")
            (all-four-report . "each RUN-SUMMARY line reports runner-version=1.00.31 and the resolver's base-dir is this worktree, which is the property the base-dir fix restores and the reason these numbers describe this tree")))
 (lint
  . #hasheq(
     (command . "racket scripts/lint-all.rkt, check-version-expectations.rkt, check-deps.rkt, metrics.rkt --lint, sync-readme-status.rkt --check, lint-release-notes.rkt --version 1.00.31 --check, verify-artifact-provenance.rkt --current-wave v1.00.31-w6 and --current-wave v1.00.31-w7")
     (result . "passed")
     (details . "lint-all 23 passed, 0 failed, 2 non-blocking warnings (arch; release-readiness is the branch-gated gate recorded as deferred in release-preflight.json). Version-expectation lint PASSED over 1511 test files with 0 hard-coded 1.00.31 literals. Dependencies OK; all 5 static metrics match README.md; README Status block in sync with the CHANGELOG; release notes valid for 1.00.31. Artifact provenance ok for the w6 directory (951 notes) and the w7 directory (950 notes), each JSON artifact canonical and each SHA256SUMS regenerating byte-identically.")))
 (checksums
  . #hasheq(
     (command . "sha256sum -c on the three manifest files, run FROM THE REPO ROOT (the manifests record repo-root-relative paths, so a run from inside the artifact directory resolves nothing and reports every file missing)")
     (result . "passed")
     (details . "artifacts/wave-delivery-integrity/v1.00.31-w6/SHA256SUMS, artifacts/wave-delivery-integrity/v1.00.31-w7/SHA256SUMS (8 captures) and artifacts/tier-ownership/v1.00.31-w0/SHA256SUMS all verify. An earlier W7 capture recorded a failed checksum run caused by that working-directory mistake; it has been replaced.")))
 (release-preflight
  . #hasheq(
     (command . "release_preflight (7 CI-strict tag-publish gates) at b0f19d6bd, tree clean")
     (result . "six-pass-one-deferred")
     (details . "release-notes, fmt-canonical (93 changed .rkt files since v1.00.29, all canonical), metrics, readme-status, tarball-symlinks and bundle-dry-run pass. lint-release-readiness --strict is recorded as deferred-by-design, not passed: check-main-branch accepts only main or a detached tag-publish HEAD, and the .gate-evidence records it consumes must name the release commit SHA and version, so it cannot pass on a campaign branch without faking the branch or the evidence. It runs on clean main after the merge and before the tag push, together with the --record-gate-evidence runs of the fast, tui, arch and workflows suites; the tag is pushed only if all seven gates pass. Recorded in artifacts/wave-delivery-integrity/v1.00.31-w7/release-preflight.json and raw/preflight-locally-runnable.txt.")))
 (w4-outcome
  . #hasheq(
     (command . "python3 scripts/gsd-delivery.py governance|sync|status --plan 96974d2cd97b152f99d296d6956f13b4f3b2bc4f79428894f74a1f3df49753d8 --wave 4, and GitHub issue/board inspection")
     (result . "delivered")
     (details . "governance -> governed (main run 36087264560 concluded success), sync -> synchronized, status -> delivered at merge cc25b3663362d032b9a23a9d22f9dd5edbe637d8 / publication 5f4a80fcda0c80396ca033184ae085139f02002c; issue 9690 closed as completed and board Done afterwards. The W4-retained local capture is corrected in w4-recovery.json: its four runner-suite sections described the neighbouring q/ clone rather than the W4 worktree, while the focused raco test, check-deps, metrics and provenance sections did run in that worktree. W4's authoritative gates are unaffected because CI checks the branch out itself. A true re-measurement of the W4 head 1158f1b70 in a worktree with no q/ sibling is retained at raw/w4-remeasure-2026-09-25.txt: focused 185 tests, arch 32/32, security 64/64, workflows 33/33, fast 1208/1208, deps/metrics/provenance exit 0, all six bound W4 checksums OK.")))
 (remaining-items
  . (#hasheq(
      (classification . "deferred-by-design")
      (owner . "post-merge release step (coordinator)")
      (rationale . "lint-release-readiness --strict is branch-gated: it requires the release commit to be on main and requires .gate-evidence for the fast, tui, arch and workflows suites that names the release SHA and version. It therefore runs on clean main after this wave merges and before the annotated v1.00.31 tag is pushed, together with the full CI-strict tag-publish preflight. This is recorded as deferred by design rather than reported as passing, and the changelog says six of seven gates pass rather than all of them."))
     #hasheq(
      (classification . "outstanding-coordinator-owned")
      (owner . "coordinator")
      (rationale . "The v1.00.30 W4 frozen doc asks for the activation merge SHA and the PR latency regime fingerprint for the guarded compiled-root pilot. The merge SHA is recorded (cc25b3663362d032b9a23a9d22f9dd5edbe637d8); the fingerprint needs a hosted latency sample on that exact head and is recorded as outstanding rather than estimated or back-filled. It is not a wave-completion gate for W7 and is not claimed anywhere in the release notes."))
     #hasheq(
      (classification . "verdict-bounded")
      (owner . "release report")
      (rationale . "PERMANENT is the W6 verdict bounded to its rehearsal (13 registered rows, 13 refused, 0 failing, clean control accepted, rehearsal head 50bdf7334, inputs digest fb224a6f, 9 rehearsal inputs). It is not a claim that no future defect can pass, and the release report states that boundary explicitly. The head and digest are the values in the committed matrix after this bake regenerated it; the superseded pair is named in the report rather than quietly dropped."))
     #hasheq(
      (classification . "environmental-not-a-regression")
      (owner . "local environment")
      (rationale . "tests/test-browser-playwright-sidecar.rkt fails on this host because the playwright npm module does not resolve for the available node. It fails identically on main, the file is untouched here, and the ci profile skips it. Recorded as a known local gap rather than a pass. Installing the module, or declaring the browser requirement for the local profile, is left to a follow-up rather than changed during a release bake."))
     #hasheq(
      (classification . "follow-up-not-blocking")
      (owner . "campaign backlog")
      (rationale . "The runner treats a file as failed purely on its exit code. W7 fixed the one file that defeated that contract, but the general property (a test file that swallows its own failures while exiting 0 reads as green) is a property of rackunit structure rather than of this wave, and no other test file installs a custom current-check-handler. Worth a dedicated audit; not introduced or widened here."))))
 (head-coherence
  . "The content head is 86002515f9de49d4abd83d83fd279656ab7c15e7. The suite chain was run at e9af72fee, and `git diff --name-only e9af72fee..86002515f` shows the only later changes are artifacts/wave-delivery-integrity/v1.00.31-w7/{SHA256SUMS,raw/preflight-locally-runnable.txt,raw/verify-chain.txt,release-preflight.json,w4-recovery.json} and docs/reports/WAVE-DELIVERY-INTEGRITY-RELEASE-v1.00.31.md — no source or test file, so the chain's numbers describe the merge candidate's code. The preflight was re-run at b0f19d6bd, after which the only change is the capture of that run. Each capture names the tree it was actually run against, because a committed artifact cannot contain the SHA of the commit that carries it; the three evidence records are then committed on top of the content head as an evidence-only change.")
 (notes
  . "This record is written at the W7 content head b9a0ac1ad7771306cda061a3ca4e9a4d3ee29278. Every number above was measured at the head named in head-coherence and retained as a raw capture; none is invented. Four independent review rounds have completed and all four returned CHANGES-REQUESTED, with all nineteen findings reproduced against the tree before being fixed. Round 4 found no new defect in the W7 content itself: its six findings were three more instances of the claim-versus-evidence class in the artifacts the earlier rounds had produced, one schema-convention warning accepted with its reason, and one prose error. A fifth round is outstanding, so the wave is not merge-authorized: the readiness gate, the seven-gate tag-publish preflight, the merge itself, and the v1.00.31 tag all remain ahead, in that order."))
