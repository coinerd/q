#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-checkpoint-characterization.rkt
;;
;; WAVE W0 CHARACTERIZATION PIN — BUG-0030
;; (campaign: executor-infrastructure defects; flipping wave: W5)
;;
;; BUG: A wave attempt that dies infra-stopped mid-edit (provider drop,
;; health-gate kill) leaves ZERO checkpoint evidence on the delivery
;; branch: no mid-wave commit of work-so-far, no progress marker in the
;; campaign record — only a dirty working tree. Every later retry starts
;; from the wave's base state and silently loses the uncommitted work.
;;
;; Live evidence: see the BUG-0030 report Evidence index (tmux q-go
;; 2026-08-25: attempt terminated on provider/network failure after
;; substantial in-flight edits; delivery branch carried no interim commit;
;; git status showed the edits stranded in the worktree).
;;
;; THIS FILE PINS TODAY'S (BROKEN) BEHAVIOR. It PASSES against the defect:
;;   1. campaign-wave carries NO checkpoint/progress field (field count).
;;   2. The executor/orchestrator modules contain NO checkpoint concept.
;;   3. A simulated infra-stopped-mid-edit attempt leaves the delivery
;;      branch at its base commit — zero checkpoint commits — with the
;;      work stranded as a dirty working tree, exactly as live.
;; Wave W5 flips these pins when it adds mid-wave checkpoint commits and
;; recorded progress. Pure-level pin: temp git repo simulation via git CLI
;; only — NO live worker subprocess, NO live campaign.

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

(define EXECUTOR-SRC (repo-file "extensions" "gsd" "wave-executor.rkt"))
(define ORCHESTRATOR-SRC (repo-file "extensions" "gsd" "go-orchestrator.rkt"))

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
  (test-suite "BUG-0030 characterization (pure): no checkpoint in schema or surface"

    (test-case "campaign-wave struct has NO checkpoint/progress field"

      ;; Today's exact field set (transparent struct):
      ;; #(struct:campaign-wave index title status attempt-count
      ;;   current-attempt delivery-branch delivery-head-sha
      ;;   attempt-context) => 9 slots. W5 adds checkpoint/progress state
      ;; => length changes => this pin flips.
      (define w (make-campaign-wave* 0 "W0" 'pending 0 #f))
      (check-equal? (vector-length (struct->vector w)) 9)

      ;; ...and the closest existing thing to "progress" (attempt-context)
      ;; starts empty: no attempt ever records recovered work state.
      (check-equal? (campaign-wave-attempt-context w) ""))

    (test-case "executor and orchestrator modules contain no checkpoint concept"

      (for ([src (in-list (list EXECUTOR-SRC ORCHESTRATOR-SRC))])
        (define text (file->string src))
        (check-false (and (regexp-match? #rx"checkpoint" text) #t)
                     (format "~a already mentions checkpoints — W5 landed; flip this pin" src))))))

;; ------------------------------------------------------------
;; Suite 2: git-level simulation of the infra-stopped attempt
;; ------------------------------------------------------------

(define git-suite
  (test-suite "BUG-0030 characterization (git): infra-stop mid-edit leaves no commit"

    (test-case "simulated infra-stopped mid-edit attempt: zero checkpoint commits, dirty tree only"

      (unless GIT
        (fail "git executable not found"))

      (define repo (make-temporary-file "bug30-~a" 'directory))

      (define (cleanup)
        (delete-directory/files repo #:must-exist? #f))

      (define (attempt-body)
        ;; Stand up a delivery worktree-equivalent repo (same shape the
        ;; wave executor creates: base commit + delivery branch).
        (git* repo "init" "-q")
        (git* repo "config" "user.email" "pin@localhost")
        (git* repo "config" "user.name" "pin")
        (call-with-output-file* (build-path repo "base.rkt")
                                (lambda (o) (displayln "#lang racket/base\n;; wave base state" o))
                                #:exists 'replace)
        (git* repo "add" "-A")
        (git* repo "commit" "-q" "-m" "wave-base")
        (define base-count (string->number (first (git-lines repo "rev-list" "--count" "HEAD"))))
        (git* repo "checkout" "-q" "-b" "delivery/w0")

        ;; Attempt runs: substantial mid-wave edits land in the tree —
        ;; exactly what the live failed attempt had in flight.
        (call-with-output-file*
         (build-path repo "work.rkt")
         (lambda (o) (displayln "#lang racket/base\n;; 40 minutes of in-flight wave work" o))
         #:exists 'replace)

        ;; Infra stop: the attempt process dies HERE. Today nothing in the
        ;; executor stack performs any mid-wave commit, so the branch must
        ;; still sit on the base commit with a dirty tree.
        (define tip-subject (first (git-lines repo "log" "-1" "--format=%s")))
        (define tip-count
          (string->number (first (git-lines repo "rev-list" "--count" "delivery/w0"))))
        (define dirty (git-lines repo "status" "--porcelain"))

        (check-equal? tip-subject
                      "wave-base"
                      "delivery tip is still the base commit — no checkpoint exists")

        (check-equal? tip-count
                      base-count
                      "ZERO checkpoint commits on the delivery branch — today's bug")

        (check-true (pair? dirty) "all in-flight work is stranded as a dirty working tree")

        ;; The ONLY trace of the lost work is the untracked file — nothing
        ;; recoverable is on the branch itself.
        (check-equal? (length dirty) 1)
        (check-true (and (regexp-match? #rx"^\\?\\?" (first dirty)) #t)
                    "in-flight file exists only as untracked"))

      (dynamic-wind void attempt-body cleanup))))

(module+ main
  (define failures (+ (run-tests pure-suite) (run-tests git-suite)))
  (exit failures))
