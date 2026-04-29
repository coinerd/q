#lang racket/base
;; STABILITY: stable

;; util/event-contracts.rkt — Reusable contracts for structured event payloads
;;
;; These catch payload shape regressions at emission time.
;; Priority: events consumed by extensions and TUI subscribers.
;;
;; Policy: Adding new keys to payloads is OK (forward-compatible).
;; Removing or renaming keys will cause contract failures.
;; Only the top events consumed by extensions need contracts —
;; internal-only events remain uncontracted for performance.

(require racket/contract)

(provide reason-payload/c
         session-id-payload/c
         iteration-payload/c
         budget-payload/c
         compact-result-payload/c
         error-detail-payload/c
         injection-count-payload/c
         turn-cancelled-payload/c
         iteration-decision-payload/c)

;; Simple reason payloads: (hasheq 'reason String ...)
(define reason-payload/c
  (and/c hash? (hash/c symbol? any/c #:immutable #t) (lambda (h) (hash-has-key? h 'reason))))

;; Session ID payloads: (hasheq 'session-id String ...)
(define session-id-payload/c
  (and/c hash? (hash/c symbol? any/c #:immutable #t) (lambda (h) (hash-has-key? h 'session-id))))

;; Iteration tracking: (hasheq 'iteration Natural ...)
(define iteration-payload/c
  (and/c hash? (hash/c symbol? any/c #:immutable #t) (lambda (h) (hash-has-key? h 'iteration))))

;; Budget check: (hasheq 'estimated-tokens Natural 'budget Natural 'max-tokens Natural)
(define budget-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'estimated-tokens)
                (hash-has-key? h 'budget)
                (hash-has-key? h 'max-tokens)))))

;; Compact result: (hasheq 'original-size Natural 'new-size Natural ...)
(define compact-result-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h)
           (and (hash-has-key? h 'original-size)
                (or (hash-has-key? h 'new-size) (hash-has-key? h 'kept-count))))))

;; Error detail: (hasheq 'error String ...)
(define error-detail-payload/c
  (and/c hash? (hash/c symbol? any/c #:immutable #t) (lambda (h) (hash-has-key? h 'error))))

;; Injection count: (hasheq 'count Natural ...)
(define injection-count-payload/c
  (and/c hash? (hash/c symbol? any/c #:immutable #t) (lambda (h) (hash-has-key? h 'count))))

;; Turn cancelled: (hasheq 'reason String 'iteration Natural ...)
(define turn-cancelled-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'reason) (hash-has-key? h 'iteration)))))

;; Iteration decision: (hasheq 'iteration Natural 'termination Any ...)
(define iteration-decision-payload/c
  (and/c hash?
         (hash/c symbol? any/c #:immutable #t)
         (lambda (h) (and (hash-has-key? h 'iteration) (hash-has-key? h 'termination)))))
