#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-verifier-complexity-heuristic.rkt
;; v0.99.22 §6.1: Tests for complexity-based verifier skip heuristic.
;;
;; Tests should-skip-verification? as a pure function of plan-context,
;; and verifies that execute-verification-gate auto-approves trivial waves.

(require rackunit
         rackunit/text-ui
         "../agent/verification/verifier-gate.rkt"
         "../agent/verification/verifier-core.rkt"
         (only-in "../extensions/gsd/session-state.rkt" make-gsd-context)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition!))

;; ── Helpers ──

;; Create a plan-context with explicit capabilities and files.
(define (make-plan-context #:files [files '("a.rkt")] #:capabilities [caps '(read-only)])
  (hasheq 'files-changed files 'capabilities-used caps 'plan-summary "test" 'wave-name "W1"))

;; Create a fresh GSD context in 'verifying state.
(define (make-verifying-ctx)
  (define ctx (make-gsd-context))
  (gsm-ctx-transition! ctx 'exploring)
  (gsm-ctx-transition! ctx 'plan-written)
  (gsm-ctx-transition! ctx 'executing)
  (gsm-ctx-transition! ctx 'verifying)
  ctx)

;; ── Test Suite ──

(define suite
  (test-suite "Verifier Complexity Heuristic (v0.99.22 §6.1)"

    ;; ── should-skip-verification? — Pure function tests ──

    (test-case "skip: 1 file, read-only only → #t"
      (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only)))
      (check-true (should-skip-verification? pc)))

    (test-case "skip: 2 files, read-only only → #t (boundary)"
      (define pc (make-plan-context #:files '("a.rkt" "b.rkt") #:capabilities '(read-only)))
      (check-true (should-skip-verification? pc)))

    (test-case "no-skip: 3 files, read-only → #f"
      (define pc (make-plan-context #:files '("a.rkt" "b.rkt" "c.rkt") #:capabilities '(read-only)))
      (check-false (should-skip-verification? pc)))

    (test-case "no-skip: shell-exec capability → #f"
      (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only shell-exec)))
      (check-false (should-skip-verification? pc)))

    (test-case "no-skip: git-write capability → #f"
      (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only git-write)))
      (check-false (should-skip-verification? pc)))

    (test-case "no-skip: file-write capability → #f"
      (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only file-write)))
      (check-false (should-skip-verification? pc)))

    (test-case "no-skip: empty plan-context → #f (conservative default)"
      (check-false (should-skip-verification? (hasheq))))

    (test-case "no-skip: capabilities key absent → #f (backward compat)"
      ;; Even with ≤2 files, without explicit capabilities we verify
      (define pc (hasheq 'files-changed '("a.rkt")))
      (check-false (should-skip-verification? pc)))

    (test-case "skip: capabilities empty list → #t (no capabilities used = safe)"
      (define pc (hasheq 'files-changed '("a.rkt") 'capabilities-used '()))
      (check-true (should-skip-verification? pc)))

    (test-case "skip: multiple read-only capabilities → #t"
      (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only network)))
      ;; network is not dangerous in our taxonomy, so should skip
      (check-true (should-skip-verification? pc)))

    ;; ── should-run-verification-gate? integration ──

    (test-case "should-run returns #f for trivial wave when plan-context given"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(read-only)))
        (check-false (should-run-verification-gate? ctx pc))))

    (test-case "should-run returns #t for non-trivial wave"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(file-write)))
        (check-true (should-run-verification-gate? ctx pc))))

    (test-case "should-run backward compat: no plan-context → #t when verifying"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (check-true (should-run-verification-gate? ctx))))

    ;; ── execute-verification-gate: trivial wave auto-approval ──

    (test-case "gate auto-approves trivial wave (read-only, 1 file)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define pc (make-plan-context #:files '("read-only.rkt") #:capabilities '(read-only)))
        (define result (execute-verification-gate ctx pc))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    (test-case "gate auto-approves trivial wave at boundary (2 files)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        (define pc (make-plan-context #:files '("a.rkt" "b.rkt") #:capabilities '(read-only)))
        (define result (execute-verification-gate ctx pc))
        (check-equal? result 'approved)
        (check-equal? (gsm-ctx-current ctx) 'idle)))

    (test-case "gate does NOT skip for file-write wave (goes through verifier)"
      (parameterize ([current-verifier-enabled #t])
        (define ctx (make-verifying-ctx))
        ;; No provider → will hit 'no provider' path which also auto-approves
        ;; but this is NOT the skip path. The key is that should-skip-verification?
        ;; returns #f for file-write.
        (define pc (make-plan-context #:files '("a.rkt") #:capabilities '(file-write)))
        (check-false (should-skip-verification? pc))))))

(run-tests suite)
