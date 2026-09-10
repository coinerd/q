;; GSD Wave Evidence — v1.00.29 W1: Campaign-integrity hardening
;; Bound to implementation SHA: 42415692 (branch campaign/v1.00.29-w1)
;; Date: 2026-09-10 (evening session)

(evidence
 (wave v1.00.29-w1)
 (implementation-sha 42415692)
 (commits
  ((sha 0dbfa911) (scope "shell-risk kill-risk classifier (BUG-0066 #9635)")
   (detail "bare interpreter names, unpinned pkill/pgrep-loop patterns, kill-by-name without recorded-PID provenance; risk taxonomy consistent with mutating verbs"))
  ((sha fa7c52d3) (scope "bash-safety destructive-process guard clause")
   (detail "refuses kill-by-name/unpinned-pattern with actionable diagnostic naming the pidfile/recorded-PID safe alternative; layered ahead of the existing mutating-verb guard"))
  ((sha 97b41e06) (scope "go-orchestrator wave-advance gate (BUG-0064 #9620)")
   (detail "refuses next-wave launch while wave N lacks a Delivery-Contract PR/merge SHA; explicit override path requires operator intent and is audit-logged"))
  ((sha 42415692) (scope "lint: wrapped over-long diagnostic literals; README metrics resync")
   (detail "runtime strings unchanged via string-append; scripts/metrics.rkt --lint 5/5 static metrics match README")))
 (tests
  ((file tests/test-shell-risk.rkt) (result "11/11 green (incl. pkill -x racket, kill $(pgrep -f …), bracket-trick variants)"))
  ((file tests/test-tool-bash-security.rkt) (result "11/11 green (end-to-end refusals + PID-pinned kill allowed)"))
  ((file tests/test-tool-bash-security-edges.rkt) (result "38/38 green (regression fixture: exact 2026-09-08 crash command for p in $(pgrep -x racket); do kill \"$p\"; done REFUSED with safe-pattern diagnostic)"))
  ((file tests/test-gsd-go-orchestrator.rkt) (result "68/68 green (advance-refusal + override-audit cases)"))
  ((file tests/test-gsd-governance-workflow.rkt) (result "31/31 green (advance-under-open-PR refusal)"))
  ((file scripts/metrics.rkt --lint) (result "green: All 5 static metrics match README.md")))
 (preflight-fix
  (issue "tracking #9651 (tool-side; .pi/extensions/q-release/index.ts lives at the campaign base, untracked by the q repo — git log --all -- '.pi/*' is empty)")
  (change "fmt-canonical gate: git diff --name-only now carries --diff-filter=ACMR plus an existsSync guard in the offender loop, both mirroring scripts/pre-commit.rkt:143 (file-exists?) semantics")
  (verification "v1.00.27..v1.00.28 replay: old enumeration 45 .rkt files (2 deleted) → ACMR 43 retained, 0 ACMR paths absent from the v1.00.28 tree"))
 (live-fire-guard-observations
  "During this session the deployed guard independently refused two agent-initiated
commands: (1) a file redirection targeting outside the sanctioned scratch root
(reason=redirection) and (2) an rm mutating-verb segment (reason=mutating-verb).
A commit message containing the token sequence '-> 43' was also blocked as
reason=redirection — noting a possible over-broad lexical match on '>' in
non-command text; filed as an advisory observation for guard tuning, not a
regression (fail-closed direction is correct for this wave).")
 (issues-referenced (#9635 #9620 #9651)))
