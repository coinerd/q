#lang racket/base

;; @speed fast
;; @suite gsd
;; @boundary pure

;; q/tests/test-gsd-worker-roots-characterization.rkt
;;
;; WAVE W0 CHARACTERIZATION PIN — BUG-0028
;; (campaign: executor-infrastructure defects; flipping wave: W1)
;;
;; BUG: With worktree isolation ON, per-attempt worktrees invalidate the
;; worker's captured allowed-roots. `current-allowed-roots` is captured once
;; at worker start (sandbox/worker-tools.rkt: default `(list (current-directory))`
;; at module load, refreshed by nothing in extensions/gsd/) and is NEVER
;; refreshed across worktree lifecycle events (make-wave-worktree! /
;; cleanup-wave-worktree! do not touch it). After a worktree is recreated at
;; a new path, the worker can still only edit the OLD (stale) worktree path
;; and CANNOT edit any path in the new worktree — executors fall back to raw
;; shell mutation.
;;
;; Live evidence: see the BUG-0028 report Evidence index (v1.00.17 hotfix —
;; `current-gsd-worktree-isolation` default rolled back to #f with the
;; comment "worker allowed-roots track worktree lifecycle" as the reopen
;; condition; extensions/gsd/wave-executor.rkt lines ~542-549).
;;
;; THIS FILE PINS TODAY'S (BROKEN) BEHAVIOR. It PASSES against the defect.
;; Wave W1 flips these pins into fix-regression tests when worker
;; allowed-roots track the worktree lifecycle. No live worker subprocess is
;; spawned here (W0 pins are pure-level: parameter + exported accessors +
;; temp dirs); boundary behavior is W1's job.

(require racket/file
         racket/format
         racket/path
         racket/string
         rackunit
         rackunit/text-ui
         "../sandbox/worker-tools.rkt"
         "../extensions/gsd/wave-executor.rkt")

(define GIT (find-executable-path "git"))

;; Simulate worker start: the ONLY thing that ever sets allowed-roots today.
;; worker-main captures roots at spawn time from its cwd; there is no
;; refresh entry point, which is exactly the defect.
(define (worker-start! cwd)
  (current-allowed-roots (list cwd)))

;; ----------------------------------------------------------------
;; Layer 1 — pure parameter/flag pins (no git binary, no subprocess)
;; ----------------------------------------------------------------

(define pure-suite
  (test-suite "BUG-0028 characterization (pure): worker allowed-roots never track worktree recreation"

    (test-case "worktree isolation flag defaults OFF — the BUG-0028 hotfix rollback pinned"
      ;; TODAY: default is #f (v1.00.17 hotfix). This is itself part of the
      ;; broken-state pin: isolation is disabled BECAUSE allowed-roots do not
      ;; track worktrees. W1 restores default ON and flips this assertion.
      (parameterize ([current-gsd-worktree-isolation #f])
        (check-false (current-gsd-worktree-isolation)
                     "BUG-0028 pin: isolation default must be #f today (hotfix v1.00.17)"))
      (check-false (parameterize ([current-gsd-worktree-isolation #f])
                     (worktree-isolation-enabled?))
                   "BUG-0028 pin: worktree-isolation-enabled? with param #f -> #f today")
      (check-true (worktree-isolation-enabled? #:isolate? #t)
                  "explicit #:isolate? #t override is honored (the repro switch)"))

    (test-case "BUG-0028 pin: allowed-roots captured at worker start are STALE after worktree recreation"
      (define tmp (make-temporary-file "bug0028-~a" 'directory))
      (define wt1 (build-path tmp "wt-campaign-aaaaaaaa-w0"))
      (define wt2 (build-path tmp "wt-campaign-bbbbbbbb-w0"))
      (make-directory* wt1)
      (make-directory* wt2)
      (define f1 (build-path wt1 "edit-target.rkt"))
      (define f2 (build-path wt2 "edit-target.rkt"))
      (call-with-output-file* f1 (lambda (p) (display ";; attempt 1\n" p)))
      (call-with-output-file* f2 (lambda (p) (display ";; attempt 2\n" p)))

      ;; Attempt 1: worker starts inside worktree 1 — roots = [wt1].
      (parameterize ([current-allowed-roots '()])
        (worker-start! wt1)
        (check-true (path-allowed? (path->string f1))
                    "sanity: worker CAN edit inside the worktree it started in")
        ;; Attempt 2: worktree recreated at a DIFFERENT hash path. Today NO
        ;; lifecycle event refreshes the worker's roots.
        (check-equal?
         (map (lambda (p)
                (if (path? p)
                    (path->string p)
                    p))
              (current-allowed-roots))
         (list (path->string wt1))
         "BUG-0028 pin: TODAY the captured roots still reference the FIRST worktree path")
        (check-false (path-allowed? (path->string f2))
                     "BUG-0028 pin: TODAY the worker CANNOT edit inside the recreated worktree")
        (check-true
         (path-allowed? (path->string f1))
         "BUG-0028 pin: TODAY the STALE first worktree path remains the only allowed root"))
      (delete-directory/files tmp #:must-exist? #f))

    (test-case "BUG-0028 pin: naming seam derives distinct per-hash worktree paths from one repo root"
      ;; The recreation scenario the pin above simulates: two campaign hash8s
      ;; on the same repo root produce two different sibling worktrees, while
      ;; the worker parameter is uninvolved.
      (define repo (string->path "/x/proj/q"))
      (check-not-equal? (wave-worktree-dir repo "aaaaaaaa00000000" 0)
                        (wave-worktree-dir repo "bbbbbbbb00000000" 0)
                        "different campaign hash8 -> different worktree path (recreation premise)")
      (check-equal? (wave-worktree-dirname "aaaaaaaa00000000" 0) "wt-campaign-aaaaaaaa-w0")
      (check-equal? (wave-worktree-dirname "bbbbbbbb00000000" 0) "wt-campaign-bbbbbbbb-w0"))))

;; ----------------------------------------------------------------
;; Layer 2 — real git sandbox: recreation actually leaves stale roots
;; ----------------------------------------------------------------

(define (git! repo . args)
  (define r (default-run-git repo args))
  (unless (zero? (git-result-code r))
    (eprintf "git ~a failed: ~a\n"
             (string-join (map ~a args) " ")
             (string-trim (git-result-stderr r))))
  (check-equal? (git-result-code r) 0 (format "git ~a failed" (string-join (map ~a args) " ")))
  r)

(define (make-sandbox)
  (define tmp (make-temporary-file "bug0028-git-~a" 'directory))
  (define proj (build-path tmp "proj"))
  (make-directory* (build-path proj ".planning"))
  (define repo (build-path proj "q"))
  (make-directory* repo)
  (git! repo "init" "-q" "-b" "main")
  (git! repo "config" "user.email" "bug0028@test.local")
  (git! repo "config" "user.name" "BUG-0028 Test")
  (call-with-output-file* (build-path repo "README.md") (lambda (p) (display "base\n" p)))
  (git! repo "add" "-A")
  (git! repo "commit" "-q" "-m" "base")
  (git! repo "update-ref" "refs/heads/origin/main" "HEAD")
  (values proj repo))

(define git-suite
  (test-suite "BUG-0028 characterization (git): make-wave-worktree! recreation does not refresh worker roots"

    (test-case "attempt 2 worktree exists on disk; captured worker roots still point at attempt 1"
      (if (not GIT)
          (log-warning "test-gsd-worker-roots-characterization: git unavailable; skipping git layer")
          (let ()
            (define-values (proj repo) (make-sandbox))
            (define cid1 "1111111100000000000000000000000000000000000000000000000000000000")
            (define cid2 "2222222200000000000000000000000000000000000000000000000000000000")
            (define wt1 (make-wave-worktree! proj #:campaign-id cid1 #:wave-index 0))
            (parameterize ([current-allowed-roots '()])
              (worker-start! (wave-worktree-path wt1))
              ;; attempt 1 ends; attempt 2 creates a NEW worktree (different hash)
              (define wt2 (make-wave-worktree! proj #:campaign-id cid2 #:wave-index 0))
              (define f2 (build-path (wave-worktree-path wt2) "README.md"))
              (check-true (directory-exists? (wave-worktree-path wt2)))
              (check-not-equal? (path->string (wave-worktree-path wt1))
                                (path->string (wave-worktree-path wt2)))
              (check-false (path-allowed? (path->string f2))
                           "BUG-0028 pin: TODAY the live worker cannot edit the new worktree")
              (check-true (path-allowed? (path->string (build-path (wave-worktree-path wt1)
                                                                   "README.md")))
                          "BUG-0028 pin: TODAY only the stale first worktree is editable")
              ;; cleanup also refreshes nothing
              (cleanup-wave-worktree! wt2)
              (cleanup-wave-worktree! wt1)
              (check-false (path-allowed? (path->string f2))
                           "BUG-0028 pin: cleanup/release is also not a refresh point today"))
            (delete-directory/files proj #:must-exist? #f))))))

(module+ main
  (define failures (+ (run-tests pure-suite) (run-tests git-suite)))
  (exit failures))
