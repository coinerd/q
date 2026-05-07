#lang racket/base

;; tests/test-di-explicit.rkt — Tests for explicit DI (no parameter fallbacks)
;;
;; v0.29.5 W2: Verify DI parameters removed, resolvers use direct imports.
;; v0.33.0 W3: Resolve functions removed (RA-06), keep pattern checks.

(require rackunit
         racket/port
         racket/runtime-path
         (only-in "../runtime/compactor.rkt" compact-history)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens))

;; Resolve paths relative to q/ root (works from both q/ and q/tests/ CWD)
(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))
(define (q-file . parts)
  (apply build-path q-root parts))

;; ============================================================
;; 1. No make-parameter in loop-state.rkt
;; ============================================================

(test-case "loop-state.rkt has no make-parameter"
  (define source-file (q-file "runtime" "iteration" "loop-state.rkt"))
  (define content (call-with-input-file source-file port->string))
  (check-false (regexp-match? #rx"make-parameter" content)
               "loop-state.rkt should not contain make-parameter"))

(test-case "loop-state.rkt has no lazy-require"
  (define source-file (q-file "runtime" "iteration" "loop-state.rkt"))
  (define content (call-with-input-file source-file port->string))
  (check-false (regexp-match? #rx"[(]lazy-require" content)
               "loop-state.rkt should not contain lazy-require"))

;; ============================================================
;; 2. No current-*-proc parameters exported from iteration.rkt
;; ============================================================

(test-case "loop-state.rkt does not export current-compact-proc"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"current-compact-proc" content)
               "current-compact-proc should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export current-estimate-tokens"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"current-estimate-tokens" content)
               "current-estimate-tokens should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export current-inject-topic"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"current-inject-topic" content)
               "current-inject-topic should not be in loop-state.rkt"))

;; ============================================================
;; 3. resolve-* functions removed (RA-06, v0.33.0 W3)
;; ============================================================

(test-case "loop-state.rkt does not export resolve-compact-proc"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"resolve-compact-proc" content)
               "resolve-compact-proc should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export resolve-estimate-tokens"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"resolve-estimate-tokens" content)
               "resolve-estimate-tokens should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export resolve-inject-topic"
  (define content (call-with-input-file (q-file "runtime" "iteration" "loop-state.rkt") port->string))
  (check-false (regexp-match? #rx"resolve-inject-topic" content)
               "resolve-inject-topic should not be in loop-state.rkt"))
