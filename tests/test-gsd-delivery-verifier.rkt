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
;;   4. a bounded verify command exits 0 (compile gate by default).

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         racket/system
         (only-in "../extensions/gsd/delivery-verifier.rkt"
                  run-delivery-verification
                  make-delivery-verifier
                  delivery-verification?
                  delivery-verification-approved?
                  delivery-verification-evidence
                  delivery-verification-message
                  current-gsd-delivery-verify-command)
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
  (define base (make-temporary-file "dv-base-~a" 'directory))
  (make-directory* (build-path base ".planning" "waves"))
  (make-directory* (build-path base "q" "ui-core"))
  (make-directory* (build-path base "q" "tui"))
  (define (sh . args)
    (define exit
      (parameterize ([current-directory base])
        (apply system*/exit-code GIT args)))
    (unless (zero? exit)
      (error 'make-tmp-git-repo "command failed: ~a" (cons 'sh args))))
  (sh "init" "-q" ".")
  (sh "config" "user.email" "test@example.com")
  (sh "config" "user.name" "Test")
  (sh "checkout" "-q" "-b" "main")
  ;; wave target file, committed as baseline
  (call-with-output-file (build-path base "q" "ui-core" "preferences.rkt")
                         (lambda (out)
                           (display "#lang racket/base\n(provide foo)\n(define foo 1)\n" out))
                         #:exists 'truncate)
  (sh "add" "-A")
  (sh "commit" "-q" "-m" "baseline")
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

(define (load-plan** base-dir files)
  (define w0 (make-gsd-wave 0 "Wave Zero" "" files '() "verify" (list "done")))
  (gsd-plan (list w0) "" '() '()))

(define (make-git-file-change! base-dir)
  ;; modify the wave file after baseline
  (call-with-output-file
   (build-path base-dir "q" "ui-core" "preferences.rkt")
   (lambda (out)
     (display "#lang racket/base\n(provide foo bar)\n(define foo 1)\n(define bar 2)\n" out))
   #:exists 'truncate))

(define (make-git-branch! base-dir branch)
  (parameterize ([current-directory base-dir])
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
      (parameterize ([current-directory base])
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
      (parameterize ([current-directory base])
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
      (parameterize ([current-directory base])
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
      (cleanup-tmp base))))

(module+ main
  (exit (run-tests (delivery-suite))))
