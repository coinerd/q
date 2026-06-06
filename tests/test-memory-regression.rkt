#lang racket/base
;; tests/test-memory-regression.rkt — Regression invariants for memory system (F8)
;;
;; Verifies that existing tools (session_recall) and config are unaffected
;; by the memory system. No behavior change when memory enabled/disabled.

(require rackunit
         "../runtime/session/session-config.rkt"
         "../tools/builtins/memory-tools.rkt"
         "../tools/builtins/session-recall.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../tools/tool.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-config #:memory-enabled? [enabled? #f])
  (hash->session-config (hasheq 'memory-backend (if enabled? 'memory-hash #f))))

(define test-ctx
  (make-exec-context #:working-directory "/tmp/q-memory-regression"
                     #:session-metadata (hasheq 'session-id "sess-regression")))

;; ---------------------------------------------------------------------------
;; Regression: session_recall unaffected by memory (F8)
;; ---------------------------------------------------------------------------

(test-case "session_recall returns tool-result with memory disabled"
  (parameterize ([current-memory-backend #f])
    (define result (tool-session-recall (hash) test-ctx))
    (check-true (tool-result? result))))

(test-case "session_recall returns tool-result with memory enabled"
  (define b (make-memory-hash-backend))
  (parameterize ([current-memory-backend b])
    (define result (tool-session-recall (hash) test-ctx))
    (check-true (tool-result? result))))

;; ---------------------------------------------------------------------------
;; Regression: session config unaffected by memory global state (F8)
;; ---------------------------------------------------------------------------

(test-case "session-config defaults have memory disabled regardless of global backend"
  (define b (make-memory-hash-backend))
  (parameterize ([current-memory-backend b])
    (define cfg (hash->session-config (hasheq)))
    (check-false (config-memory-enabled? cfg))
    (check-false (config-memory-backend cfg))))

;; ---------------------------------------------------------------------------
;; Invariant: memory store does not leak into unrelated tool results
;; ---------------------------------------------------------------------------

(test-case "memory items do not affect session_recall results"
  (define b (make-memory-hash-backend))
  (parameterize ([current-memory-backend b])
    ;; Store a memory item
    (tool-store-memory (hash 'content "memory content" 'scope 'session) test-ctx)
    ;; session_recall should not see memory items in its details
    (define result (tool-session-recall (hash) test-ctx))
    (define details (tool-result-details result))
    (check-false (and (hash? details) (hash-has-key? details 'memory-items)))
    (check-false (and (hash? details) (hash-has-key? details 'memory-id)))))
