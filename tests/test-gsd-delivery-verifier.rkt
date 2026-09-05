#lang racket/base
;; @covers extensions/gsd/delivery-verifier.rkt
;; @speed fast  ;; @suite extensions
;; @boundary integration
;; tests/test-gsd-delivery-verifier.rkt — structured delivery verification for /go
;;
;; TDD tests for the delivery verifier that replaces the hardcoded fail-closed
;; `#f` verifier in the /go coordinator. A wave is DONE only when real
;; delivery evidence exists:
;;   1. git repository reachable from base-dir;
;;   2. current branch matches the wave's expected feature/issue-<N>-wave;
;;   3. at least one wave target file changed vs HEAD (or untracked-new);
;;   4. the wave's DECLARED verify command exits 0 — executed through the
;;      process-wide owned-singleton verification registry. An explicit
;;      override parameter wins for tests; the derived compile gate runs
;;      only as a separately described fallback for genuinely EMPTY verify
;;      declarations. Only a reaped 'completed job with exit 0 approves
;;      (timed-out/exit 124, cancelled, orphan-recovered, failed are
;;      failures).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         (only-in "helpers/private-fixture-templates.rkt"
                  call-with-private-git-environment
                  make-private-git-fixture!
                  private-fixture-root
                  private-git-fixture-repo)
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  make-delivery-verifier
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message
                  current-gsd-delivery-verify-command
                  current-gsd-delivery-verify-timeout-sec)
         (only-in "../extensions/gsd/composition-root.rkt" current-gsd-verification-registry)
         (only-in "../extensions/gsd/verification-job.rkt"
                  make-verification-registry
                  registry-active-count)
         (only-in "../extensions/gsd/plan-types.rkt" gsd-plan make-gsd-wave plan-wave-ref)
         (only-in "../extensions/gsd/campaign-state.rkt" migrate-campaign!)
         (only-in "../extensions/gsd/go-orchestrator.rkt" run-campaign-wave campaign-result-status)
         (only-in "../util/loop-result.rkt" make-loop-result))

;; ============================================================
;; Fixture: temp git repo with a plan + wave doc + STATE.md
;; ============================================================

;; Create a temp dir that is itself a git repo, with a q/ subdir to mimic
;; the two-tier checkout layout (base-dir = repo root, git root = base/q).
(define GIT (find-executable-path "git"))

(define (make-tmp-git-repo)
  (define tmp (make-temporary-file "dv-base-~a" 'directory))
  (define fx (make-private-git-fixture! #:parent-root tmp #:tag "dv-base"))
  (define base (private-fixture-root fx))
  (rename-file-or-directory (private-git-fixture-repo fx) (build-path base "q"))
  (make-directory* (build-path base ".planning" "waves"))
  (make-directory* (build-path base "q" "ui-core"))
  (make-directory* (build-path base "q" "tui"))
  (define q-dir (build-path base "q"))
  (define (sh . args)
    (define exit
      (parameterize ([current-directory q-dir])
        (apply system*/exit-code GIT args)))
    (unless (zero? exit)
      (error 'make-tmp-git-repo "command failed: ~a" (cons 'sh args))))
  ;; The private-fixture contract already verifies that clones start on
  ;; `main`; avoid a redundant checkout subprocess in every test fixture.
  ;; wave target file, committed as baseline (family-specific)
  (call-with-output-file (build-path base "q" "ui-core" "preferences.rkt")
                         (lambda (out)
                           (display "#lang racket/base\n(provide foo)\n(define foo 1)\n" out))
                         #:exists 'truncate)
  (sh "add" "-A")
  (sh "commit" "-q" "-m" "baseline")
  ;; Re-pin the offline origin/main stand-in to the rebuilt baseline: the
  ;; verifier measures delivery as `origin/main...HEAD`, so the base ref must
  ;; equal pre-test HEAD (the clone template tip is one commit behind).
  (sh "update-ref" "refs/heads/origin/main" "HEAD")
  base)

(define (write-plan! base-dir idx title slug)
  (call-with-output-file
   (build-path base-dir ".planning" "PLAN.md")
   (lambda (out)
     (display (string-append "# Plan: Delivery Verifier Test\n\n## Waves\n\n- [Inbox] W"
                             (number->string idx)
                             ": "
                             title
                             " → waves/W"
                             (number->string idx)
                             "-"
                             slug
                             ".md\n")
              out))
   #:exists 'truncate))

(define (write-wave-doc! base-dir idx slug files verify)
  (call-with-output-file
   (build-path base-dir ".planning" "waves" (string-append "W" (number->string idx) "-" slug ".md"))
   (lambda (out)
     (display (string-append "# Wave "
                             (number->string idx)
                             "\n"
                             "Status: Inbox\n\n"
                             "## Files\n"
                             (apply string-append
                                    (for/list ([f files])
                                      (string-append "- File: " f "\n")))
                             "\n## Verify\n"
                             verify
                             "\n")
              out))
   #:exists 'truncate))

(define (write-state! base-dir idx issue)
  ;; Rows mirror the real tracker format:
  ;;   | W<n> | #<issue> | PENDING | [waves/W<n>-<slug>.md](waves/W<n>-<slug>.md) |
  ;; The linked wave doc must match the current plan's slug (fixture = "zero").
  (call-with-output-file (build-path base-dir ".planning" "STATE.md")
                         (lambda (out)
                           (display (string-append "| W"
                                                   (number->string idx)
                                                   " | #"
                                                   issue
                                                   " | PENDING | [waves/W"
                                                   (number->string idx)
                                                   "-zero.md](waves/W"
                                                   (number->string idx)
                                                   "-zero.md) |\n")
                                    out))
                         #:exists 'truncate))

(define (load-plan* base-dir)
  ;; minimal plan: one wave with the given files
  (load-plan** base-dir (list "q/ui-core/preferences.rkt")))

(define (load-plan** base-dir files [verify "verify"])
  ;; verify defaults to the historical placeholder string; delivery
  ;; verification reads the DECLARED command from the wave doc, so the
  ;; placeholder never executes in these fixtures. Tests that exercise the
  ;; compile-gate FALLBACK pass "" (a genuinely empty declaration).
  (define w0 (make-gsd-wave 0 "Wave Zero" "" files '() verify (list "done")))
  (gsd-plan (list w0) "" '() '()))

(define (make-git-file-change! base-dir)
  ;; modify the wave file after baseline
  (call-with-output-file
   (build-path base-dir "q" "ui-core" "preferences.rkt")
   (lambda (out)
     (display "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n" out))
   #:exists 'truncate))

(define (make-git-branch! base-dir branch)
  ;; git root is base-dir/q in the two-tier fixture layout
  (parameterize ([current-directory (build-path base-dir "q")])
    (system*/exit-code GIT "checkout" "-q" "-b" branch)))

;; Fully scaffolded campaign: git repo on feature/issue-42-wave, modified wave
;; file, matching PLAN.md / wave doc / STATE.md (issue #42).
(define (setup-standard-campaign!)
  (define base (make-tmp-git-repo))
  (make-git-branch! base "feature/issue-42-wave")
  (make-git-file-change! base)
  (write-plan! base 0 "Wave Zero" "zero")
  (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "raco make q/ui-core/preferences.rkt")
  (write-state! base 0 "42")
  base)

(define (cleanup-tmp dir)
  (delete-directory/files dir #:must-exist? #f))

;; ============================================================
;; Tests
;; ============================================================

(define (delivery-suite)
  (test-suite "delivery-verifier"

    (test-case "approves when branch + files changed + verify passes"
      (define base (setup-standard-campaign!))
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (check-true (pair? (delivery-verification-evidence result)))
      (cleanup-tmp base))

    (test-case "rejects when branch does not match expected issue branch"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-99-wave") ; wrong issue
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42") ; wave expects #42
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? result)
                   "wrong branch must fail delivery verification")
      (cleanup-tmp base))

    (test-case "approves committed delivery (wave committed + pushed + PR)"
      ;; A wave doc may instruct the agent to commit + push + open a PR. In
      ;; that flow the working tree is clean at verification time, so the
      ;; evidence is the commits on the current branch relative to its base
      ;; (origin/main or main), not an uncommitted working-tree diff.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      ;; commit the change on the feature branch (no uncommitted diff)
      (make-git-file-change! base)
      ;; git commands must run inside the repo (base/q), not the base dir
      (parameterize ([current-directory (build-path base "q")])
        (system*/exit-code GIT "add" "-A")
        (system*/exit-code GIT "commit" "-q" "-m" "wave delivery"))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result)
                  "committed delivery on a feature branch must be approved")
      (cleanup-tmp base))

    (test-case "rejects when wave files unchanged"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      ;; no file change
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? result) "no delivery artifact must fail")
      (cleanup-tmp base))

    (test-case "approves directory target when a file under it changed"
      ;; A wave may scope a target to a DIRECTORY (e.g. "q/tests/memory/") for
      ;; "existing tests + new focused tests under this dir". git diff lists
      ;; files, never directories, so a directory target must be satisfied by
      ;; prefix: ANY changed file under the directory counts as delivery.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      ;; create the target directory + a changed file inside it
      (make-directory* (build-path base "q" "tests" "memory"))
      (call-with-output-file (build-path base "q" "tests" "memory" "policy-boundary-test.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide x)\n(define x 1)\n" out))
                             #:exists 'truncate)
      (parameterize ([current-directory (build-path base "q")])
        (system*/exit-code GIT "add" "-A")
        (system*/exit-code GIT "commit" "-q" "-m" "wave delivery under dir"))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/tests/memory/") "true")
      (write-state! base 0 "42")
      (define plan (load-plan** base '("q/tests/memory/")))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result)
                  "directory target satisfied by a changed file under it")
      (cleanup-tmp base))

    (test-case "rejects directory target when nothing under it changed"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-directory* (build-path base "q" "tests" "memory"))
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/tests/memory/") "true")
      (write-state! base 0 "42")
      (define plan (load-plan** base '("q/tests/memory/")))
      (define result (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? result)
                   "directory target with no changed file under it must fail")
      (cleanup-tmp base))

    (test-case "rejects when verify command fails"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "false")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-delivery-verify-command "false"])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result)
                   "failing verify command must fail delivery verification")
      (cleanup-tmp base))

    (test-case "rejects when git not available"
      (define base (make-temporary-file "dv-nogit-~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? result) "no git repository must fail closed")
      (cleanup-tmp base))

    (test-case "make-delivery-verifier returns a working verifier callback"
      (define base (setup-standard-campaign!))
      (define plan (load-plan* base))
      (define verifier (make-delivery-verifier base plan))
      (define result (verifier 0))
      (check-true (delivery-verification? result))
      (check-true (delivery-verification-approved? result))
      (cleanup-tmp base))

    (test-case "coordinator marks wave done when structured verifier approves"
      (define base (setup-standard-campaign!))
      (define plan (load-plan* base))
      (define rec (migrate-campaign! base))
      (define result
        (run-campaign-wave base
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (make-delivery-verifier base plan)))
      (check-eq? (campaign-result-status result) 'wave-done)
      (cleanup-tmp base))

    (test-case "coordinator marks wave failed when structured verifier rejects"
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-99-wave") ; wrong branch
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define rec (migrate-campaign! base))
      (define result
        (run-campaign-wave base
                           rec
                           0
                           #:runner (lambda (_) 'ok)
                           #:verifier (make-delivery-verifier base plan)))
      (check-eq? (campaign-result-status result) 'wave-failed)
      (cleanup-tmp base))

    (test-case "approves issue-less campaign on main (no per-wave issues)"
      ;; A campaign that does not use per-wave GitHub issues runs on the
      ;; current branch (main) and has no STATE.md issue row. The branch
      ;; check must not invent an expected feature branch.
      (define base (make-tmp-git-repo))
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      ;; no write-state!: STATE.md absent → issue-less
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result)
                  "issue-less campaign on main must be approved")
      (check-false (or (regexp-match? #rx"expected=" (delivery-verification-message result)) #f)
                   "no spurious expected branch")
      (cleanup-tmp base))

    (test-case "ignores stale STATE.md issue row from a previous campaign"
      ;; STATE.md still lists the OLD campaign's issue mapping (different
      ;; wave-doc slug), so it must be treated as issue-less, not as a
      ;; feature/issue-N-wave expectation.
      (define base (make-tmp-git-repo))
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       "raco make q/ui-core/preferences.rkt")
      ;; stale row linking a different wave doc (old slug "legacy")
      (call-with-output-file
       (build-path base ".planning" "STATE.md")
       (lambda (out)
         (display "| W0 | #42 | PENDING | [waves/W0-legacy.md](waves/W0-legacy.md) |\n" out))
       #:exists 'truncate)
      (define plan (load-plan* base))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result)
                  "stale issue row referencing a different wave doc must be ignored")
      (cleanup-tmp base))

    (test-case "approves git-root-relative wave files (no q/ prefix)"
      ;; regression (W-campaign): wave docs may declare CI/workflow paths
      ;; git-root-relative (".github/workflows/ci.yml", "scripts/run-tests/...")
      ;; instead of repo-root-relative ("q/.github/..."). This mirrors the real
      ;; two-tier checkout: base-dir (/home/user/src/q-agent) is NOT a git repo;
      ;; the git root is base/q. The verifier must accept BOTH conventions — the
      ;; repo-root mapping of a git-root-relative declaration escapes the git
      ;; root ("../...") and must fall back to the declared path verbatim.
      (define base (make-temporary-file "dv-gitrel-~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (make-directory* (build-path base "q" ".github" "workflows"))
      (make-directory* (build-path base "q" "scripts" "run-tests"))
      (define (sh . args)
        (define exit
          (parameterize ([current-directory (build-path base "q")])
            (apply system*/exit-code GIT args)))
        (unless (zero? exit)
          (error 'gitrel "command failed: ~a" (cons 'sh args))))
      (sh "init" "-q" ".")
      (sh "config" "user.email" "test@example.com")
      (sh "config" "user.name" "Test")
      (sh "checkout" "-q" "-b" "main")
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display
          "#lang racket/base\n(provide write-json-results!)\n(define (write-json-results! p) p)\n"
          out))
       #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "add wave targets")
      (sh "checkout" "-q" "-b" "feature/issue-42-wave")
      ;; modify both targets (git-root-relative paths under q/)
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression-v2\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display (string-append
                   "#lang racket/base\n"
                   "(require racket/path racket/file)\n"
                   "(provide write-json-results!)\n"
                   "(define (write-json-results! p) (when p (make-directory* (path-only p))))\n")
                  out))
       #:exists 'truncate)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       (list ".github/workflows/full-regression.yml"
                             "scripts/run-tests/reporting.rkt")
                       "raco make q/scripts/run-tests/reporting.rkt")
      (write-state! base 0 "42")
      (define plan
        (load-plan** base
                     (list ".github/workflows/full-regression.yml"
                           "scripts/run-tests/reporting.rkt")))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (cleanup-tmp base))

    (test-case "approves merged-to-main delivery via campaign created-at"
      ;; Regression: the wave agent committed + merged its work to
      ;; main, so HEAD == origin/main and the base-relative diff is empty. The
      ;; verifier must recognize delivery when the wave target files changed in
      ;; commits since the campaign's creation time (the campaign base). Without
      ;; created-at the wave is rejected; with it the wave is approved.
      (define base (make-temporary-file "dv-merged-~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (make-directory* (build-path base "q" ".github" "workflows"))
      (make-directory* (build-path base "q" "scripts" "run-tests"))
      (define (sh . args)
        (define exit
          (parameterize ([current-directory (build-path base "q")])
            (apply system*/exit-code GIT args)))
        (unless (zero? exit)
          (error 'merged "command failed: ~a" (cons 'sh args))))
      (sh "init" "-q" ".")
      (sh "config" "user.email" "test@example.com")
      (sh "config" "user.name" "Test")
      (sh "checkout" "-q" "-b" "main")
      ;; baseline commit BEFORE the campaign
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display
          "#lang racket/base\n(provide write-json-results!)\n(define (write-json-results! p) p)\n"
          out))
       #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "baseline")
      ;; campaign created-at = now (before the wave work)
      (define created-at (current-seconds))
      (sleep 2)
      ;; wave work committed + merged to main (HEAD == origin/main after merge)
      (call-with-output-file (build-path base "q" ".github" "workflows" "full-regression.yml")
                             (lambda (out) (display "name: full-regression-v2\n" out))
                             #:exists 'truncate)
      (call-with-output-file
       (build-path base "q" "scripts" "run-tests" "reporting.rkt")
       (lambda (out)
         (display (string-append
                   "#lang racket/base\n"
                   "(require racket/path racket/file)\n"
                   "(provide write-json-results!)\n"
                   "(define (write-json-results! p) (when p (make-directory* (path-only p))))\n")
                  out))
       #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "W0: repair full-regression evidence path")
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       (list ".github/workflows/full-regression.yml"
                             "scripts/run-tests/reporting.rkt")
                       "raco make q/scripts/run-tests/reporting.rkt")
      ;; no write-state!: issue-less campaign, work on main
      (define plan
        (load-plan** base
                     (list ".github/workflows/full-regression.yml"
                           "scripts/run-tests/reporting.rkt")))
      (define without (run-delivery-verification base plan 0))
      (check-false (delivery-verification-approved? without)
                   "without created-at, merged-to-main work is not visible (HEAD == base)")
      (define with-created (run-delivery-verification base plan 0 created-at))
      (check-true (delivery-verification-approved? with-created)
                  (delivery-verification-message with-created))
      (cleanup-tmp base))

    (test-case "compile gate skips non-Racket files (ci.yml, docs)"
      ;; Waves that touch .yml/.md alongside .rkt must compile only the
      ;; Racket targets; raco make fails on non-module files.
      (define base (make-tmp-git-repo))
      (make-directory* (build-path base "q" ".github" "workflows"))
      (make-directory* (build-path base "q" "docs" "reports"))
      (call-with-output-file (build-path base "q" ".github" "workflows" "ci.yml")
                             (lambda (out) (display "name: ci\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path base "q" "docs" "reports" "x.md")
                             (lambda (out) (display "# x\n" out))
                             #:exists 'truncate)
      (parameterize ([current-directory (build-path base "q")])
        (system*/exit-code GIT "add" "-A")
        (system*/exit-code GIT "commit" "-q" "-m" "add non-rkt files"))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (call-with-output-file (build-path base "q" ".github" "workflows" "ci.yml")
                             (lambda (out) (display "name: ci2\n" out))
                             #:exists 'truncate)
      (call-with-output-file (build-path base "q" "docs" "reports" "x.md")
                             (lambda (out) (display "# x2\n" out))
                             #:exists 'truncate)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc!
       base
       0
       "zero"
       (list "q/ui-core/preferences.rkt" "q/.github/workflows/ci.yml" "q/docs/reports/x.md")
       "raco make q/ui-core/preferences.rkt")
      (write-state! base 0 "42")
      (define plan
        (load-plan**
         base
         (list "q/ui-core/preferences.rkt" "q/.github/workflows/ci.yml" "q/docs/reports/x.md")))
      (define result (run-delivery-verification base plan 0))
      (check-true (delivery-verification-approved? result)
                  "verify gate must ignore non-Racket changed files")
      (cleanup-tmp base))

    (test-case "declared wave verify command executes when no override is bound"
      ;; Truthful verification: without an explicit override the verifier runs
      ;; the wave's DECLARED verify command (doc `## Verify` — the source
      ;; gsd-wave-verify is built from), not a silently derived gate. The
      ;; marker file proves the declared command actually executed.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (define marker
        (build-path (find-system-path 'temp-dir)
                    (format "dv-declared-~a.marker" (current-inexact-milliseconds))))
      (with-handlers ([exn:fail? void])
        (delete-file marker))
      (write-wave-doc! base
                       0
                       "zero"
                       '("q/ui-core/preferences.rkt")
                       (format "echo declared-verify-ran > ~a" marker))
      (write-state! base 0 "42")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (check-true (file-exists? marker) "the DECLARED command executed")
      (delete-file marker)
      (cleanup-tmp base))

    (test-case "declared verify command runs from the base-dir project root"
      ;; Declared commands are authored against the PLAN.md/.planning layout
      ;; ("q/…"-prefixed targets), so the two-tier checkout must resolve them
      ;; from base-dir even though the git root is <base>/q.
      ;; Include spaces so placeholder expansion must shell-quote the path.
      (define base (make-temporary-file "dv cwd ~a" 'directory))
      (make-directory* (build-path base ".planning" "waves"))
      (make-directory* (build-path base "q" "scripts" "run-tests"))
      (define (sh . args)
        (define exit
          (parameterize ([current-directory (build-path base "q")])
            (apply system*/exit-code GIT args)))
        (unless (zero? exit)
          (error 'cwd-pin "command failed: ~a" (cons 'sh args))))
      (sh "init" "-q" ".")
      (sh "config" "user.email" "test@example.com")
      (sh "config" "user.name" "Test")
      (sh "checkout" "-q" "-b" "main")
      (call-with-output-file (build-path base "q" "scripts" "run-tests" "reporting.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide w)\n(define w 1)\n" out))
                             #:exists 'truncate)
      (sh "add" "-A")
      (sh "commit" "-q" "-m" "baseline")
      ;; modify the target so the files gate passes
      (call-with-output-file (build-path base "q" "scripts" "run-tests" "reporting.rkt")
                             (lambda (out)
                               (display "#lang racket/base\n(provide w)\n(define w 2)\n" out))
                             #:exists 'truncate)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base
                       0
                       "zero"
                       '("scripts/run-tests/reporting.rkt")
                       "cd <project-base>/q && test -f scripts/run-tests/reporting.rkt")
      (define plan (load-plan** base '("scripts/run-tests/reporting.rkt")))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result)
                  (format "base-dir cwd must resolve the <project-base>/q declaration: ~a"
                          (delivery-verification-message result)))
      (cleanup-tmp base))

    (test-case "verify executes through the bound registry: duplicates attach, never launch twice"
      ;; The owned-singleton lane: while one declared verify is running, a
      ;; duplicate verifier call for the same wave+command+checkout attaches
      ;; to the SAME job instead of launching a second gate.
      (define base (setup-standard-campaign!))
      (define reg (make-verification-registry))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "sleep 2; exit 0")
      (define plan (load-plan* base))
      (define first-result (box #f))
      (parameterize ([current-gsd-verification-registry reg])
        (define t
          (thread (lambda () (set-box! first-result (run-delivery-verification base plan 0)))))
        ;; Git evidence checks precede launch and can exceed a fixed 300ms on
        ;; loaded CI hosts. Poll for the owned start under a hard 5s bound.
        (let wait-for-owned-start ([remaining 250])
          (when (and (zero? (registry-active-count reg)) (not (thread-dead? t)) (> remaining 0))
            (sleep 0.02)
            (wait-for-owned-start (sub1 remaining))))
        (check-equal? (registry-active-count reg)
                      1
                      "declared verify runs as ONE owned job in the bound registry")
        ;; duplicate verifier call while the first verify is still running:
        ;; attaches to the running singleton — no second process launch
        (define second (run-delivery-verification base plan 0))
        (sync t)
        (check-equal? (registry-active-count reg)
                      0
                      "job is terminal after both callers' waits returned")
        (check-true (delivery-verification-approved? second) (delivery-verification-message second))
        (check-true (delivery-verification-approved? (unbox first-result))
                    "both attached callers observe the same approved job"))
      (cleanup-tmp base))

    (test-case "timed-out verify is a failure with truthful state and exit 124"
      ;; BUG-0057 class fix: a deadline-killed gate can never approve; the
      ;; verdict carries the attributable terminal state and exit 124.
      (define base (setup-standard-campaign!))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "sleep 30; exit 0")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)]
                       [current-gsd-delivery-verify-timeout-sec 1])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result) "a timed-out gate must never approve")
      (define msg (delivery-verification-message result))
      (check-true (string-contains? msg "exit=124") msg)
      (check-true (string-contains? msg "state=timed-out") msg)
      (check-true (string-contains? msg "log=") msg)
      (cleanup-tmp base))

    (test-case "declared verify failing nonzero is a failure with attributable state"
      (define base (setup-standard-campaign!))
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "echo boom >&2; exit 3")
      (define plan (load-plan* base))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-false (delivery-verification-approved? result)
                   "a nonzero declared verify must fail delivery")
      (define msg (delivery-verification-message result))
      (check-true (string-contains? msg "exit=3") msg)
      (check-true (string-contains? msg "state=failed") msg)
      (check-true (string-contains? msg "log=") msg)
      (cleanup-tmp base))

    (test-case "default delivery verify deadline is 14400s (bounded, multi-hour)"
      ;; Declared gates legitimately run for hours; the default deadline must
      ;; accommodate them while staying BOUNDED.
      (check-equal? (current-gsd-delivery-verify-timeout-sec) 14400))

    (test-case "empty verify declaration: compile gate runs as a separately described fallback"
      ;; A genuinely EMPTY declaration (doc `## Verify` empty and plan verify
      ;; empty) falls back to the derived compile gate — and the fallback is
      ;; DESCRIBED in the evidence, never a silent substitute.
      (define base (make-tmp-git-repo))
      (make-git-branch! base "feature/issue-42-wave")
      (make-git-file-change! base)
      (write-plan! base 0 "Wave Zero" "zero")
      (write-wave-doc! base 0 "zero" '("q/ui-core/preferences.rkt") "")
      (write-state! base 0 "42")
      (define plan (load-plan** base '("q/ui-core/preferences.rkt") ""))
      (define result
        (parameterize ([current-gsd-verification-registry (make-verification-registry)])
          (run-delivery-verification base plan 0)))
      (check-true (delivery-verification-approved? result) (delivery-verification-message result))
      (define verify-detail (cdr (cdr (assoc "verify" (delivery-verification-evidence result)))))
      (check-true (string-contains? verify-detail "compile-gate fallback")
                  (format "fallback must be separately described: ~a" verify-detail))
      (check-true (string-contains? verify-detail "raco make")
                  "the derived gate command is visible in the evidence")
      (cleanup-tmp base))))

(module+ main
  (exit (call-with-private-git-environment (lambda () (run-tests (delivery-suite))))))
