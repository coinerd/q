#lang racket/base
;; @speed fast  ;; @suite extensions
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
  (call-with-output-file
   (build-path base-dir ".planning" "STATE.md")
   (lambda (out)
     (display (string-append "| W" (number->string idx) " | #" issue " | PENDING |\n") out))
   #:exists 'truncate))

(define (load-plan* base-dir)
  ;; minimal plan: one wave with one file
  (define w0
    (make-gsd-wave 0 "Wave Zero" "" (list "q/ui-core/preferences.rkt") '() "verify" (list "done")))
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
      (cleanup-tmp base))))

(module+ main
  (exit (run-tests (delivery-suite))))
