#lang racket/base

;; tests/test-iteration-wiring.rkt — Verify decide-next-action is wired into production loop
;;
;; v0.29.8 W0: Integration verification that the pure decision function
;; is called from the iteration loop's main dispatch.

(require rackunit
         racket/port
         racket/runtime-path
         (only-in "../runtime/iteration.rkt" decide-next-action))

(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))

;; ============================================================
;; 1. decide-next-action is imported and used in iteration.rkt
;; ============================================================

(test-case "decide-next-action is exported from iteration.rkt"
  (check-not-false (procedure? decide-next-action) "decide-next-action should be a procedure"))

(test-case "iteration.rkt source contains decide-next-action call in loop body"
  (define content (call-with-input-file (build-path q-root "runtime" "iteration.rkt") port->string))
  ;; The function is defined once and called at least once in the loop
  (define def-count (length (regexp-match* #rx"decide-next-action [(]iteration-ctx" content)))
  (check-true (>= def-count 1)
              (format "expected >= 1 decide-next-action call with iteration-ctx, found ~a"
                      def-count)))

(test-case "iteration-ctx struct is used to construct decision context"
  (define content (call-with-input-file (build-path q-root "runtime" "iteration.rkt") port->string))
  (define ctx-usage (length (regexp-match* #rx"[(]iteration-ctx " content)))
  ;; At least 1 usage in the loop body (beyond the struct definition)
  (check-true (>= ctx-usage 1)
              (format "expected >= 1 iteration-ctx call site (beyond definition), found ~a"
                      ctx-usage)))

(test-case "cond on termination is replaced by match on action"
  (define content (call-with-input-file (build-path q-root "runtime" "iteration.rkt") port->string))
  ;; The loop should use match on action, not direct cond on termination
  (check-not-false (regexp-match? #rx"[(]match action" content)
                   "loop should use (match action ...) for dispatch"))
