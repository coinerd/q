#lang racket/base

;; tests/helpers/delivery-fixtures.rkt — shared real-Git fixtures for the
;; delivery-verifier owners (v1.00.29 W2 boundary extraction).
;;
;; W2 split the former monolithic tests/test-gsd-delivery-verifier.rkt into
;; three runner-visible owners (decision / git-contract / e2e). The real-Git
;; fixture builders below are SHARED by the contract and e2e owners; the
;; decision owner deliberately does NOT use them (it injects synthetic Git
;; facts via current-gsd-git-runner instead — see
;; tests/test-gsd-delivery-verifier-decision.rkt).
;;
;; STABILITY: internal test-support module (tests/helpers/* is excluded from
;; test discovery, like private-fixture-templates.rkt).

(require racket/file
         racket/path
         racket/system
         (only-in "private-fixture-templates.rkt"
                  make-private-git-fixture!
                  private-fixture-root
                  private-git-fixture-repo)
         (only-in "../../extensions/gsd/plan-types.rkt" gsd-plan make-gsd-wave))

(provide GIT
         make-tmp-git-repo
         write-plan!
         write-wave-doc!
         write-state!
         load-plan*
         load-plan**
         make-git-file-change!
         make-git-branch!
         setup-standard-campaign!
         cleanup-tmp)

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
