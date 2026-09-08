;; Wave evidence: v1.00.27 W6 — Series bake and v1.00.27 release
;; Record-only companion to gsd-wave-reviews/v1.00.27-w6.rktd
;; and gsd-wave-validation/v1.00.27-w6.rktd.
(
(wave . "v1.00.27-w6")
(ticket . "#9594")
(implementation-sha . "933263bfa700de60eeba263932660fed76fd7176")
(delivery . "branch campaign/v1.00.27-w6 from fresh origin/main; squash-merge PR, tag v1.00.27 at the merge SHA, and public release are owned by the coordinator; this trio is bound to the merge SHA at merge time")
(base . "origin/main 9f529830ea4f82011600476bfcaa6bb61d392f64 (v1.00.26 merge); the branch carries the full v1.00.27 series content (W0-W5 already checkpointed on this branch's history)")
(scope . "util/version.rkt (1.00.27), CHANGELOG.md (honest release entry), README.md (metrics + status sync), docs/reports/SERIES-COMPLETION-v1.00.23-v1.00.27.md (new, roadmap §11), artifacts/ci-baseline/v1.00.27-c3/SHA256SUMS (final), docs/reports/gsd-wave-{evidence,reviews,validation}/v1.00.27-w6.rktd (this trio)")
(what-was-done
  (series-record-first . "docs/reports/SERIES-COMPLETION-v1.00.23-v1.00.27.md written before any release language: all five milestone releases with tags/merge SHAs (v1.00.23 f0b8f8cf, v1.00.24 c0d370d9, v1.00.25 ba3fe3e0, v1.00.26 9f529830, v1.00.27 tag placed by the coordinator at the W6 squash-merge SHA); queue/LPT and CI-topology states with their one-command operational rollback commands; re-tiered destination evidence; removed-overlap equivalence evidence checksum-bound to C3; final claim verdict 'target not achieved' with per-row numbers bound to the checksummed C3 artifacts (checkpoint 9dfce821)")
  (claim-language . "release language rule applied mechanically: W5's tool-computed verdict missed six of seven fixed rows, so the CHANGELOG entry and the series record state 'target not achieved' per missed row and link the preserved C3 evidence; the 2x claim is explicitly NOT published; no sentence in the shipped notes extends beyond the C3 artifacts")
  (version-and-sync . "util/version.rkt bumped to 1.00.27 (single source of truth); CHANGELOG release entry added; README metrics table and status line synced; the repo-wide current-version references (docs, info.rkt, reports, wiki-src) synced in the same checkpoint (933263bf)")
  (integrated-bake . "focused integrated gates run on the branch head: fast 165/165, security 64/64, arch 32/32, release dry-run 6/6 (no tags/releases created), lint-release-notes --check PASS, metrics --lint 5/5 in sync, version consistency lint 0 errors; release-preflight strict-mode items (readiness with gate evidence, TUI smoke) regenerated/verified at the final head; tarball/symlink/bundle checks ride the dry-run and the coordinator-owned release workflow at the merge SHA")
  (gate-evidence . ".gate-evidence/ is CI-local (untracked); ci.yml/release.yml regenerate fast/tui/arch/workflows evidence with --record-gate-evidence at the merge SHA before the strict readiness gate; local stale 1.00.26 residue was refreshed at the branch head so the only expected strict-mode failure on the wave branch is the deliberate 'must be on main' branch check, which resolves at merge"))
(focused-results
  (fast . "racket scripts/run-tests.rkt --suite fast → 165/165, exit 0")
  (security . "racket scripts/run-tests.rkt --suite security → 64/64, exit 0")
  (arch . "racket scripts/run-tests.rkt --suite arch → 32/32, exit 0")
  (dry-run . "racket scripts/release-dry-run.rkt → 6/6 checks passed, no tags or releases created")
  (notes-lint . "racket scripts/lint-release-notes.rkt --check → PASSED")
  (metrics-lint . "racket scripts/metrics.rkt --lint → 5/5 metrics in sync with README")
  (readiness . "strict readiness on the wave branch: the 'must be on main' branch check fails by design pre-merge; all version-bound checks (gate evidence, workflows) pass after evidence refresh; full strict pass is coordinator-owned at the merge SHA"))
(security-semantics . "untouched: no execute-* check, isolation root, worker-security contract, gate semantics, or scheduler variable was modified in this wave; the release publishes the existing integrated topology read-only")
(result . "the series is baked for release at the v1.00.27 version with the §11 record in place, the final 2x claim honestly withheld ('target not achieved' with per-row numbers bound to checksummed C3 artifacts), every declared wave file present at HEAD, and all focused gates green; tagging, publishing, and milestone #892 closure are coordinator-owned final steps"))
