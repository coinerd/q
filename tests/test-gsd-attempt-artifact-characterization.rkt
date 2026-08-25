#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-attempt-artifact-characterization.rkt
;;
;; WAVE W0 CHARACTERIZATION PIN — BUG-0029
;; (campaign: executor-infrastructure defects; flipping wave: W4)
;;
;; BUG: A failed wave attempt that already created its delivery branch and
;; per-wave worktree leaves NO artifact trace: the campaign record has no
;; artifact entries, and the successor (retry) attempt's prompt contains no
;; inherited-artifacts block. The retry attempt rebuilds its environment
;; from scratch even though perfectly reusable branch/worktree state from
;; the failed attempt exists.
;;
;; Live evidence: see the BUG-0029 report Evidence index (tmux q-go
;; 2026-08-25: attempt terminated on provider/network failure after
;; creating campaign/<id>/w<n> branch + worktree; retry attempt's prompt
;; carried no inherited-artifacts section; campaign record showed no
;; artifact entries for the dead attempt).
;;
;; THIS FILE PINS TODAY'S (BROKEN) BEHAVIOR. It PASSES against the defect:
;;   1. The campaign-wave record schema has NO artifact field (9 slots).
;;   2. A failed attempt that created branch+worktree leaves attempt-context
;;      empty — the successor attempt inherits nothing.
;;   3. Neither the orchestrator nor the wave executor mentions inherited
;;      artifacts — no code path can produce such a prompt block.
;; Wave W4 flips these pins when it records attempt artifacts and injects
;; them into successor prompts. Pure-level pin: temp git repo + exported
;; record accessors only — NO live worker subprocess, NO live campaign.

(require racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         racket/system
         rackunit
         rackunit/text-ui
         "../extensions/gsd/campaign-state.rkt")

;; ------------------------------------------------------------
;; Helpers
;; ------------------------------------------------------------

;; Paths relative to THIS test file (not the invocation cwd).
(define this-file
  (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))

(define here (simplify-path (build-path this-file 'up 'up)))

(define (repo-file . parts)
  (apply build-path (cons here parts)))

(define ORCHESTRATOR-SRC (repo-file "extensions" "gsd" "go-orchestrator.rkt"))
(define EXECUTOR-SRC (repo-file "extensions" "gsd" "wave-executor.rkt"))
(define WORKER-TOOLS-SRC (repo-file "extensions" "gsd" "worker-tools.rkt"))

(define GIT (find-executable-path "git"))

;; Run git in `dir` capturing stdout; return exit code.
(define (git* dir . args)
  (parameterize ([current-directory dir])
    (with-output-to-string (lambda ()
                             (parameterize ([current-error-port (open-output-nowhere)])
                               (apply system*/exit-code (cons GIT args)))))))

(define (git-lines dir . args)
  (define out (apply git* dir args))
  (filter non-empty-string? (string-split out "\n")))

;; ------------------------------------------------------------
;; Suite 1: pure schema/surface pins
;; ------------------------------------------------------------

(define pure-suite
  (test-suite "BUG-0029 characterization (pure): no artifact record or prompt path"

    (test-case "campaign-wave struct has NO artifact field"

      ;; Today's exact field set (transparent struct):
      ;; index title status attempt-count current-attempt delivery-branch
      ;; delivery-head-sha attempt-context + name => 9 slots.
      ;; W4 adds artifact entries => length changes => this pin flips.
      (define w (make-campaign-wave* 0 "W0" 'pending 0 #f))
      (check-equal? (vector-length (struct->vector w)) 9))

    (test-case "no GSD module can produce an inherited-artifacts prompt block"

      (for ([src (in-list (list ORCHESTRATOR-SRC EXECUTOR-SRC WORKER-TOOLS-SRC))]
            #:when (file-exists? src))
        (define text (file->string src))
        (check-false (and (regexp-match? #rx"inherited[- ]artifacts?" text) #t)
                     (format "~a already builds inherited-artifacts — W4 landed; flip this pin"
                             src))))))

;; ------------------------------------------------------------
;; Suite 2: failed-attempt simulation — nothing inherited
;; ------------------------------------------------------------

(define git-suite
  (test-suite "BUG-0029 characterization (git): failed attempt artifacts not inherited"

    (test-case "attempt 1 creates branch+worktree then fails: successor inherits nothing"

      (unless GIT
        (fail "git executable not found"))

      (define repo (make-temporary-file "bug29-~a" 'directory))

      (define (cleanup)
        (delete-directory/files repo #:must-exist? #f))

      (define (attempt-body)
        ;; Base repo + the delivery branch + worktree that the FAILED
        ;; attempt really created (the reusable artifacts).
        (git* repo "init" "-q")
        (git* repo "config" "user.email" "pin@localhost")
        (git* repo "config" "user.name" "pin")
        (call-with-output-file* (build-path repo "base.rkt")
                                (lambda (o) (displayln "#lang racket/base" o))
                                #:exists 'replace)
        (git* repo "add" "-A")
        (git* repo "commit" "-q" "-m" "base")
        (define attempt-branch "campaign/abc123/w0")
        (git* repo "branch" attempt-branch)
        (git* repo
              "worktree"
              "add"
              "-q"
              (build-path repo "wt-w0")
              "-b"
              (string-append attempt-branch "-attempt1"))
        (define artifacts-exist
          (and (directory-exists? (build-path repo "wt-w0"))
               (member attempt-branch (git-lines repo "branch" "--format=%(refname:short)"))
               #t))
        (check-true artifacts-exist "fixture: failed attempt did create branch+worktree")

        ;; TODAY'S BEHAVIOR: the campaign record after that failed attempt.
        ;; make-campaign-wave* is the only validated constructor; whatever
        ;; it defaults for delivery-branch/attempt-context is what the
        ;; successor attempt sees. Today: NO artifact trace and empty
        ;; attempt-context — nothing reusable is communicated.
        (define record-after-failed-attempt (make-campaign-wave* 0 "W0" 'pending 1 #f))
        (check-equal? (campaign-wave-attempt-context record-after-failed-attempt) "")
        (check-false (non-empty-string? (campaign-wave-delivery-branch record-after-failed-attempt))
                     "record carries no delivery-branch/artifact entry for the dead attempt")

        ;; And nothing anywhere references the reusable artifacts: today
        ;; no prompt text, record field, or module surface mentions them.
        (define reusable (string-append attempt-branch "-attempt1"))
        (for ([src (in-list (list ORCHESTRATOR-SRC EXECUTOR-SRC))])
          (define text (file->string src))
          (check-false (and (regexp-match? (regexp-quote reusable) text) #t)
                       (format "~a references attempt artifacts — W4 landed; flip this pin" src))))

      (dynamic-wind void attempt-body cleanup))))

(module+ main
  (define failures (+ (run-tests pure-suite) (run-tests git-suite)))
  (exit failures))
