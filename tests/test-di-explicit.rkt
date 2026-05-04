#lang racket/base

;; tests/test-di-explicit.rkt — Tests for explicit DI (no parameter fallbacks)
;;
;; v0.29.5 W2: Verify DI parameters removed, resolvers use direct imports.

(require rackunit
         racket/port
         (only-in "../runtime/iteration/loop-state.rkt"
                  resolve-compact-proc
                  resolve-estimate-tokens
                  resolve-inject-topic)
         (only-in "../runtime/compactor.rkt" compact-history)
         (only-in "../llm/token-budget.rkt" estimate-context-tokens))

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
  (check-eq? (resolve-compact-proc) compact-history))

(test-case "resolve-estimate-tokens returns estimate-context-tokens"
  (check-eq? (resolve-estimate-tokens) estimate-context-tokens))

;; ============================================================
;; 3. No make-parameter in loop-state.rkt
;; ============================================================

(test-case "loop-state.rkt has no make-parameter"
  (define source-file "runtime/iteration/loop-state.rkt")
  (define content (call-with-input-file source-file port->string))
  (check-false (regexp-match? #rx"make-parameter" content)
               "loop-state.rkt should not contain make-parameter"))

(test-case "loop-state.rkt has no lazy-require"
  (define source-file "runtime/iteration/loop-state.rkt")
  (define content (call-with-input-file source-file port->string))
  (check-false (regexp-match? #rx"[(]lazy-require" content)
               "loop-state.rkt should not contain lazy-require"))

;; ============================================================
;; 4. No current-*-proc parameters exported from iteration.rkt
;; ============================================================

(test-case "loop-state.rkt does not export current-compact-proc"
  (define content (call-with-input-file "runtime/iteration/loop-state.rkt" port->string))
  (check-false (regexp-match? #rx"current-compact-proc" content)
               "current-compact-proc should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export current-estimate-tokens"
  (define content (call-with-input-file "runtime/iteration/loop-state.rkt" port->string))
  (check-false (regexp-match? #rx"current-estimate-tokens" content)
               "current-estimate-tokens should not be in loop-state.rkt"))

(test-case "loop-state.rkt does not export current-inject-topic"
  (define content (call-with-input-file "runtime/iteration/loop-state.rkt" port->string))
  (check-false (regexp-match? #rx"current-inject-topic" content)
               "current-inject-topic should not be in loop-state.rkt"))
