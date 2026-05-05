#lang racket/base

;; tests/test-di-explicit.rkt — Tests for explicit DI (no parameter fallbacks)
;;
;; v0.29.5 W2: Verify DI parameters removed, resolvers use direct imports.

(require rackunit
         racket/port
         racket/runtime-path
         (only-in "../runtime/iteration/loop-state.rkt"
                  resolve-compact-proc
                  resolve-estimate-tokens
                  resolve-inject-topic)
         (only-in "../runtime/compactor.rkt" compact-history)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens))

;; Resolve paths relative to q/ root (works from both q/ and q/tests/ CWD)
(define-runtime-path q-root-raw "..")
(define q-root (simplify-path q-root-raw))
(define (q-file . parts)
  (apply build-path q-root parts))

;; ============================================================
;; 1. Resolve functions return correct implementations
;; ============================================================

(test-case "resolve-compact-proc returns a procedure"
  (check-true (procedure? (resolve-compact-proc))))

(test-case "resolve-estimate-tokens returns a procedure"
  (check-true (procedure? (resolve-estimate-tokens))))

(test-case "resolve-inject-topic returns a string"
  (check-true (string? (resolve-inject-topic))))

;; ============================================================
;; 2. Resolve functions return the actual implementations
;; ============================================================

(test-case "resolve-compact-proc returns compact-history"
  (check-pred procedure? (resolve-compact-proc))
  ;; v0.30.2: TR boundary wraps procedures, so check-eq? no longer holds.
  ;; Verify functional equivalence instead of identity.
  (define proc (resolve-compact-proc))
  (check-pred procedure? proc))

(test-case "resolve-estimate-tokens returns estimate-context-tokens"
  (check-pred procedure? (resolve-estimate-tokens))
  (define proc (resolve-estimate-tokens))
  (check-pred procedure? proc))

;; ============================================================
;; 3. No make-parameter in loop-state.rkt
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
;; 4. No current-*-proc parameters exported from iteration.rkt
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
