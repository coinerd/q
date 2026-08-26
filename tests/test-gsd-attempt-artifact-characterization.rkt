#lang racket/base
;; @covers extensions/gsd/campaign-state.rkt

;; @speed fast  ;; @suite gsd
;; @boundary integration

;; tests/test-gsd-attempt-artifact-characterization.rkt — BUG-0029 W5.
;;
;; FLIPPED PIN (v1.00.21 W5): this file originally characterized the BUG as
;; the deliberate absence of attempt-artifact ownership — campaign-wave had
;; no ledger field, neither the orchestrator nor the executor could build an
;; inherited-artifacts prompt block, and nothing reconciled the durable
;; leftovers (delivery branches + worktrees) of failed/killed attempts.
;; W5 delivered the attempt-artifact ledger + PRIOR ARTIFACTS prompt block +
;; end-of-campaign leftovers report, so every pin below now asserts the
;; FIXED behavior and goes red if a refactor regresses it:
;;   1. campaign-wave HAS the artifact-ledger field (10 slots) and
;;      wave-artifact-ledger normalizes the legacy sentinel "" to '()
;;      (pre-W5 records load with an empty ledger, never fail).
;;   2. The wave executor builds the bounded inherited-artifacts prompt
;;      block and the orchestrator injects it into the successor prompt
;;      (both modules carry the BUG-0029 wiring).
;;   3. A failed attempt's branch + worktree are discoverable through a
;;      ledger entry, render into the successor prompt block (≤ 1 KiB),
;;      and are NEVER deleted by rendering or reporting — reclaim is
;;      operator-approved only (explicit reclaim offer, no auto-delete).

(require racket/file
         racket/format
         racket/list
         racket/port
         racket/string
         racket/struct
         racket/system
         rackunit
         rackunit/text-ui
         "../extensions/gsd/campaign-state.rkt"
         (only-in "../extensions/gsd/wave-executor.rkt"
                  inherited-artifacts-block
                  current-gsd-wave-inherited-artifacts
                  PRIOR-ARTIFACTS-BLOCK-BUDGET))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-dir [prefix "bug29-"])
  (define base (find-system-path 'temp-dir))
  (define d
    (build-path base (format "~a~a-~a" prefix (current-inexact-milliseconds) (random 100000))))
  (make-directory d)
  d)

(define (git-out dir . args)
  (parameterize ([current-directory dir])
    (with-output-to-string
     (λ ()
       (unless (apply system* (find-executable-path "git") args)
         (fail (format "git ~a failed in ~a" (string-join (map ~a args) " ") dir)))))))

(define (quiet-delete-dir d)
  (with-handlers ([exn:fail? (λ (_) (void))])
    (delete-directory/files d #:must-exist? #f)))

;; Build a repo with one commit on main, an attempt delivery branch, and a
;; worktree checked out on that branch — the exact durable artifacts a
;; failed attempt leaves behind (BUG-0029 root cause).
(define (make-attempt-fixture)
  (define repo (make-temp-dir "bug29repo-"))
  (define wt-dir (make-temp-dir "bug29wt-"))
  (define campaign-hash "d079a35e")
  (define base-branch (format "campaign/~a/w0" campaign-hash))
  (define attempt-branch (format "~a-attempt1" base-branch))
  (git-out repo "init" "-q" "-b" "main")
  (git-out repo "config" "user.email" "test@local")
  (git-out repo "config" "user.name" "Test")
  (with-output-to-file (build-path repo "README.md") (λ () (display "w0\n")))
  (git-out repo "add" ".")
  (git-out repo "commit" "-q" "-m" "base")
  (git-out repo "branch" base-branch)
  (git-out repo "worktree" "add" "-q" (path->string wt-dir) "-b" attempt-branch base-branch)
  (with-output-to-file (build-path wt-dir "attempt.txt") (λ () (display "dead attempt work\n")))
  (git-out wt-dir "add" ".")
  (git-out wt-dir "commit" "-q" "-m" "failed attempt work")
  (define base-sha (string-trim (git-out repo "rev-parse" "HEAD")))
  (values repo wt-dir attempt-branch base-sha))

;; A campaign-wave whose first attempt failed and whose ledger carries the
;; dead attempt's artifacts (what record-attempt-artifact! + terminal
;; marking produce in the orchestrator).
(define (wave-with-failed-attempt attempt-branch wt-path base-sha)
  (define w (make-campaign-wave* 0 "Wave 0" 'failed 1 #f))
  (define e
    (make-campaign-artifact-entry "01KTSFCEBBV9NFT6Z7WY167P4H" attempt-branch wt-path base-sha))
  (set-campaign-artifact-entry-terminal-status! e 'failed)
  (set-campaign-artifact-entry-merge-status! e 'unmerged)
  (set-campaign-wave-artifact-ledger! w (list e))
  w)

(define (repo-file . parts)
  ;; Repo root (q/) relative to THIS test file — never the invocation cwd
  ;; (verify runs from the project base, not from q/).
  (define this-file
    (resolved-module-path-name (variable-reference->resolved-module-path (#%variable-reference))))
  (apply build-path (cons (simplify-path (build-path this-file 'up 'up)) parts)))

;; ============================================================
;; 1. Pure pins: ledger field + inherited-artifacts block exist (W5)
;; ============================================================

(define w5-pure-suite
  (test-suite "BUG-0029 W5 fixed pin: ledger + inherited-artifacts block exist"
    (test-case "campaign-wave struct HAS the artifact-ledger field (10 slots)"
      (define w (make-campaign-wave* 0 "W0" 'pending 0 #f))
      (check-equal? (vector-length (struct->vector w)) 10)
      (check-pred list?
                  (wave-artifact-ledger w)
                  "artifact-ledger is readable through the normalizing accessor"))

    (test-case "legacy waves without the ledger field load with an empty ledger"
      ;; Pre-v1.00.19 records deserialize through the raw 5-field form; the
      ;; #:auto sentinel must never leak and never fail the load.
      (define legacy (make-campaign-wave 0 "W0" 'pending 0 #f))
      (check-equal? (wave-artifact-ledger legacy) '())
      (set-campaign-wave-artifact-ledger! legacy '())
      (check-equal? (wave-artifact-ledger legacy) '()))

    (test-case "artifact-entry constructor enforces invariants + lifecycle defaults"
      (define e (make-campaign-artifact-entry "att-1" "campaign/x/w0" "/tmp/wt-x" "abc123"))
      (check-equal? (campaign-artifact-entry-attempt-id e) "att-1")
      (check-equal? (campaign-artifact-entry-branch e) "campaign/x/w0")
      (check-equal? (campaign-artifact-entry-worktree-path e) "/tmp/wt-x")
      (check-equal? (campaign-artifact-entry-base-sha e) "abc123")
      (check-eq? (campaign-artifact-entry-terminal-status e) 'running)
      (check-eq? (campaign-artifact-entry-merge-status e) 'undetermined)
      (check-eq? (campaign-artifact-entry-teardown-status e) 'pending)
      (check-exn exn:fail:contract?
                 (λ () (make-campaign-artifact-entry "" "b" "/w" "s"))
                 "empty attempt-id rejected")
      (check-exn exn:fail:contract? (λ () (make-campaign-artifact-entry "a" 'not-a-string "/w" "s")))
      ;; Lifecycle setters mutate the entry (terminal/merge/teardown).
      (set-campaign-artifact-entry-terminal-status! e 'failed)
      (set-campaign-artifact-entry-merge-status! e 'merged-into-base)
      (set-campaign-artifact-entry-teardown-status! e 'worktree-remove-failed)
      (check-eq? (campaign-artifact-entry-terminal-status e) 'failed)
      (check-eq? (campaign-artifact-entry-merge-status e) 'merged-into-base)
      (check-eq? (campaign-artifact-entry-teardown-status e) 'worktree-remove-failed))

    (test-case "GSD modules build the inherited-artifacts prompt block"
      (define exec-src (file->string (repo-file "extensions" "gsd" "wave-executor.rkt")))
      (define orch-src (file->string (repo-file "extensions" "gsd" "go-orchestrator.rkt")))
      (check-true (regexp-match? #rx"inherited-artifacts-block" exec-src)
                  "wave executor renders the PRIOR ARTIFACTS block")
      (check-true (regexp-match? #rx"inherited-artifacts-block" orch-src)
                  "orchestrator injects the block into the successor prompt")
      (check-false (inherited-artifacts-block '()) "no prior artifacts → no block (#f)"))

    (test-case "block is bounded to the PRIOR ARTIFACTS byte budget"
      ;; 50 fake entries with long ids/paths must still render ≤ 1 KiB.
      (define entries
        (for/list ([i (in-range 50)])
          (define e
            (make-campaign-artifact-entry
             (format "attempt-~a-0123456789abcdef" i)
             (format "campaign/d079a35e-w0-attempt~a" i)
             (format "/tmp/wt-campaign-d079a35e-w0-attempt~a-some-long-suffix" i)
             "7912fb7b"))
          (set-campaign-artifact-entry-terminal-status! e 'failed)
          e))
      (define block (inherited-artifacts-block entries))
      (check-true (string? block))
      (check-true (regexp-match? #rx"PRIOR ARTIFACTS" block) "newest entries survive the elision")
      (check-true (regexp-match? #rx"attempt-49" block)
                  "newest attempt listed first-priority under budget")
      (check-true (<= (bytes-length (string->bytes/utf-8 block)) PRIOR-ARTIFACTS-BLOCK-BUDGET)
                  (format "block ~a bytes ≤ budget ~a"
                          (bytes-length (string->bytes/utf-8 block))
                          PRIOR-ARTIFACTS-BLOCK-BUDGET)))))

;; ============================================================
;; 2. Git pins: failed-attempt artifacts discoverable, inherited,
;;    never auto-deleted
;; ============================================================

(define w5-git-suite
  (test-suite "BUG-0029 W5 fixed pin: failed attempt leaves a discoverable, inherited, non-deleted record"
    (test-case "failed attempt's branch + worktree land in a discoverable ledger entry"
      (define-values (repo wt-dir attempt-branch base-sha) (make-attempt-fixture))
      (dynamic-wind void
                    (λ ()
                      (define w
                        (wave-with-failed-attempt attempt-branch (path->string wt-dir) base-sha))
                      (define ledger (wave-artifact-ledger w))
                      (check-equal? (length ledger) 1 "one entry per attempt that created artifacts")
                      (define e (car ledger))
                      (check-equal? (campaign-artifact-entry-branch e) attempt-branch)
                      (check-equal? (campaign-artifact-entry-worktree-path e) (path->string wt-dir))
                      (check-eq? (campaign-artifact-entry-terminal-status e) 'failed)
                      ;; discoverable from git itself: the branch exists
                      (check-true (regexp-match? (regexp attempt-branch)
                                                 (git-out repo "branch" "--list" attempt-branch))
                                  "dead attempt branch still exists in git (ledger mirrors reality)"))
                    (λ ()
                      (quiet-delete-dir wt-dir)
                      (quiet-delete-dir repo))))

    (test-case "successor prompt contains the inherited-artifacts block"
      (define-values (repo wt-dir attempt-branch base-sha) (make-attempt-fixture))
      (dynamic-wind void
                    (λ ()
                      (define w
                        (wave-with-failed-attempt attempt-branch (path->string wt-dir) base-sha))
                      (define block (inherited-artifacts-block (wave-artifact-ledger w)))
                      (check-true (and (string? block) (positive? (string-length block))))
                      (check-true (regexp-match? #rx"=== PRIOR ARTIFACTS" block))
                      (check-true (regexp-match? (regexp attempt-branch) block)
                                  "prior attempt's branch is named — no git archaeology needed")
                      (check-true (regexp-match? #rx"terminal:failed" block))
                      (check-true (regexp-match? #rx"merge:unmerged" block))
                      (check-true (regexp-match? #rx"\\(on disk\\)" block)
                                  "worktree presence is probed, not guessed")
                      ;; The executor prompt carries the block through the parameter —
                      ;; same plumbing shape as the BUG-0024/#9515 failure context.
                      (parameterize ([current-gsd-wave-inherited-artifacts block])
                        (check-equal? (current-gsd-wave-inherited-artifacts) block)))
                    (λ ()
                      (quiet-delete-dir wt-dir)
                      (quiet-delete-dir repo))))

    (test-case "rendering/reporting NEVER deletes artifacts (operator-approved reclaim only)"
      (define-values (repo wt-dir attempt-branch base-sha) (make-attempt-fixture))
      (dynamic-wind
       void
       (λ ()
         (define w (wave-with-failed-attempt attempt-branch (path->string wt-dir) base-sha))
         ;; Render the block twice (idempotent read-only path).
         (inherited-artifacts-block (wave-artifact-ledger w))
         (inherited-artifacts-block (wave-artifact-ledger w))
         ;; NOTHING deleted: worktree dir still on disk, branch still in git.
         (check-true (directory-exists? wt-dir) "worktree directory survives prompt rendering")
         (check-true (file-exists? (build-path wt-dir "attempt.txt")))
         (check-true (regexp-match? (regexp attempt-branch)
                                    (git-out repo "branch" "--list" attempt-branch))
                     "attempt branch survives prompt rendering")
         ;; End-of-campaign leftovers report: operator-visible summary with
         ;; an explicit reclaim offer — the orchestrator source must keep
         ;; both the report and the no-auto-delete guarantee.
         (define orch-src (file->string (repo-file "extensions" "gsd" "go-orchestrator.rkt")))
         (check-true (regexp-match? #rx"report-campaign-artifact-leftovers!" orch-src)
                     "campaign end (success OR terminal failure) reports leftovers")
         (check-true (regexp-match? #rx"NOTHING has been deleted" orch-src)
                     "reclaim offer states explicitly that nothing was auto-deleted")
         (check-false (regexp-match? #rx"git -C[^\"]*-D[^\"]*-f" orch-src)
                      "no force-branch-delete hidden in the report path"))
       (λ ()
         (quiet-delete-dir wt-dir)
         (quiet-delete-dir repo))))))

;; ============================================================
;; Entry points
;; ============================================================

(module+ test
  (void (run-tests (test-suite "test-gsd-attempt-artifact-characterization"
                     w5-pure-suite
                     w5-git-suite))))

(module+ main
  (define failures
    (run-tests (test-suite "test-gsd-attempt-artifact-characterization"
                 w5-pure-suite
                 w5-git-suite)))
  (exit (if (zero? failures) 0 1)))
