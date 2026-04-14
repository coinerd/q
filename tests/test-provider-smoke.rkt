#lang racket

;; tests/test-provider-smoke.rkt — Compilation smoke tests for all LLM provider adapters
;;
;; Ensures no provider ships with syntax errors. These tests simply
;; dynamic-require each provider module to verify it compiles and loads.
;; Refs: #420, #425

(require rackunit)

;; ============================================================
;; Provider module smoke tests
;; ============================================================

(define (q-modpath str)
  ;; Build a (file ...) module path relative to q/ root
  (define-values (base _ __)
    (split-path (resolved-module-path-name
                 (variable-reference->resolved-module-path
                  (#%variable-reference)))))
  (define full-path (simplify-path (build-path base ".." str)))
  `(file ,(path->string full-path)))

(test-case "openai-compatible-provider-compiles"
  ;; The critical syntax error from #420 must never recur
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/openai-compatible.rkt") #f))))

(test-case "anthropic-provider-compiles"
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/anthropic.rkt") #f))))

(test-case "gemini-provider-compiles"
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/gemini.rkt") #f))))

(test-case "provider-base-compiles"
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/provider.rkt") #f))))

(test-case "stream-module-compiles"
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/stream.rkt") #f))))

(test-case "model-module-compiles"
  (check-not-exn
   (lambda ()
     (dynamic-require (q-modpath "llm/model.rkt") #f))))
