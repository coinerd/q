#lang racket/base
;; runtime/memory/policy.rkt — Memory write/read policy gating
;;
;; Controls what memory operations are allowed:
;;   - Memory must be enabled in config (backend present)
;;   - Sensitivity levels gate who can store what
;;   - Secret content is blocked from storage entirely
;;   - Scope isolation enforced (session-scoped memory not visible cross-session)
;;   - Token/count budgets limit retrieval volume

(require "types.rkt")

(provide memory-policy?
         default-memory-policy
         make-memory-policy
         policy-allows-store?
         policy-allows-retrieve?
         policy-allows-delete?
         policy-check-content-safety
         policy-within-budget?)

;; ---------------------------------------------------------------------------
;; Policy struct
;; ---------------------------------------------------------------------------

(struct memory-policy
        (max-items-per-session ; nat — max stored items per session scope
         max-retrieve-count ; nat — max items returned per query
         max-content-length ; nat — max chars per content field
         allowed-sensitivities ; (listof sensitivity?) — which levels can be stored
         blocked-content-patterns ; (listof regexp) — patterns that prevent storage
         allow-delete? ; bool — whether delete is allowed
         )
  #:transparent)

(define default-memory-policy
  (memory-policy 100 ; max 100 items per session
                 20 ; max 20 items per query
                 10000 ; max 10k chars per content
                 '(public internal
                          sensitive) ; secret blocked from storage
                 '() ; no blocked patterns by default
                 #t)) ; delete allowed

(define (make-memory-policy #:max-items-per-session [max-items 100]
                            #:max-retrieve-count [max-retrieve 20]
                            #:max-content-length [max-content 10000]
                            #:allowed-sensitivities [sensitivities
                                                     '(public internal
                                                              sensitive)]
                            #:blocked-content-patterns [patterns '()]
                            #:allow-delete? [allow-delete #t])
  (memory-policy max-items max-retrieve max-content sensitivities patterns allow-delete))

;; ---------------------------------------------------------------------------
;; Policy checks
;; ---------------------------------------------------------------------------

;; Can this item be stored under this policy?
(define (policy-allows-store? policy item)
  (and (memory-policy? policy)
       (valid-memory-item? item)
       ;; Sensitivity check
       (memq (hash-ref (memory-item-validity item) 'sensitivity 'public)
             (memory-policy-allowed-sensitivities policy))
       ;; Content length check
       (<= (string-length (memory-item-content item)) (memory-policy-max-content-length policy))
       ;; Content safety check
       (policy-check-content-safety policy (memory-item-content item))))

;; Is content safe to store? (no blocked patterns)
(define (policy-check-content-safety policy content)
  (for/and ([pattern (in-list (memory-policy-blocked-content-patterns policy))])
    (not (regexp-match? pattern content))))

;; Is retrieval allowed under budget?
(define (policy-allows-retrieve? policy requested-count)
  (and (memory-policy? policy) (<= (or requested-count 0) (memory-policy-max-retrieve-count policy))))

;; Is delete allowed?
(define (policy-allows-delete? policy)
  (and (memory-policy? policy) (memory-policy-allow-delete? policy)))

;; Check if result set is within budget
(define (policy-within-budget? policy items)
  (and (memory-policy? policy) (<= (length items) (memory-policy-max-retrieve-count policy))))
