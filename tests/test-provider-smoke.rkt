#lang racket

;; BOUNDARY: integration

;; tests/test-provider-smoke.rkt — Compilation smoke tests for all LLM provider adapters
;;
;; Ensures no provider ships with syntax errors. These tests simply
;; dynamic-require each provider module to verify it compiles and loads.
;; Enhanced: also verify key exports exist (make-*-provider procedures).
;; Refs: #420, #425, T3-02

(require rackunit)

;; ============================================================
;; Helpers
;; ============================================================

(define (q-modpath str)
  ;; Build a (file ...) module path relative to q/ root
  (define-values (base _ __)
    (split-path (resolved-module-path-name (variable-reference->resolved-module-path
                                            (#%variable-reference)))))
  (define full-path (simplify-path (build-path base ".." str)))
  `(file ,(path->string full-path)))

(define (require-and-check modpath export-name predicate)
  ;; Require a module and verify a specific export satisfies a predicate
  (check-not-exn (lambda () (dynamic-require modpath #f)))
  (define val (dynamic-require modpath export-name))
  (check-pred predicate val
              (format "~a should export ~a as ~a" modpath export-name predicate)))

;; ============================================================
;; Provider module smoke tests — compilation + key export checks
;; ============================================================

(test-case "openai-compatible-provider-compiles-and-exports"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/openai-compatible.rkt") #f)))
  (define make-fn (dynamic-require (q-modpath "llm/openai-compatible.rkt")
                                    'make-openai-compatible-provider))
  (check-pred procedure? make-fn))

(test-case "anthropic-provider-compiles-and-exports"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/anthropic.rkt") #f)))
  (define make-fn (dynamic-require (q-modpath "llm/anthropic.rkt")
                                    'make-anthropic-provider))
  (check-pred procedure? make-fn))

(test-case "gemini-provider-compiles-and-exports"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/gemini.rkt") #f)))
  (define make-fn (dynamic-require (q-modpath "llm/gemini.rkt")
                                    'make-gemini-provider))
  (check-pred procedure? make-fn))

(test-case "azure-openai-provider-compiles-and-exports"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/azure-openai.rkt") #f)))
  (define make-fn (dynamic-require (q-modpath "llm/azure-openai.rkt")
                                    'make-azure-openai-provider))
  (check-pred procedure? make-fn))

(test-case "provider-base-compiles-and-exports"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/provider.rkt") #f)))
  ;; Provider base should export the provider? predicate
  (define pred (dynamic-require (q-modpath "llm/provider.rkt") 'provider?))
  (check-pred procedure? pred))

(test-case "stream-module-compiles"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/stream.rkt") #f))))

(test-case "model-module-compiles"
  (check-not-exn (lambda () (dynamic-require (q-modpath "llm/model.rkt") #f)))
  ;; Model should export key struct predicates
  (define mr? (dynamic-require (q-modpath "llm/model.rkt") 'model-request?))
  (check-pred procedure? mr?)
  (define mresp? (dynamic-require (q-modpath "llm/model.rkt") 'model-response?))
  (check-pred procedure? mresp?))
