#lang racket/base

;; runtime/config/memory-config.rkt — Memory subsystem configuration struct
;; STABILITY: internal
;;
;; v0.96.7 (F6): Grouping memory-related parameters into a config struct.
;; Existing parameters remain as backward-compatible wrappers.

(require racket/contract)

(provide (struct-out memory-config)
         default-memory-config)

;; Memory subsystem configuration
(struct memory-config
        (backend ; (or/c #f memory-backend?) — active backend instance
         policy ; memory-policy? — retention/relevance policy
         auto-extraction ; boolean? — enable auto-extraction from tool results
         auto-reflection ; boolean? — enable periodic reflection
         reflection-min-items ; integer? — minimum items before reflection triggers
         injection-budget ; (or/c #f integer?) — max tokens for memory injection
         max-entry-chars ; integer? — max chars per memory entry
         retrieval-timeout-ms ; integer? — timeout for backend retrieval
         )
  #:transparent)

(define (default-memory-config)
  (memory-config #f ; backend — set at runtime
                 'default ; policy
                 #f ; auto-extraction
                 #f ; auto-reflection
                 5 ; reflection-min-items
                 #f ; injection-budget
                 200 ; max-entry-chars
                 2000)) ; retrieval-timeout-ms
