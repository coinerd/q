#lang racket/base
;; Public P audit checks only. The caller must obtain AUTHORITY from the
;; authenticated owner/request path, never from the exported bundle itself.
(require "supplemental-evidence-format.rkt"
         "evidence-repair-format.rkt")
(provide audit-promotion-sha256)
;; JSON readers return mutable strings. Copy, without resealing or replacing
;; the addressed digest, before applying the immutable record validator.
(define (freeze-wire value)
  (cond
    [(hash? value)
     (for/hasheq ([(key item) (in-hash value)])
       (values key (freeze-wire item)))]
    [(list? value) (map freeze-wire value)]
    [(string? value) (string->immutable-string value)]
    [else value]))
(define (audit-promotion-sha256 authority)
  (define ref (hash-ref authority 'reference (hasheq)))
  (define contract (hash-ref ref 'supplemental-contract #f))
  (define sha (hash-ref authority 'supplemental-promotion-sha256 #f))
  (define p (hash-ref authority 'supplemental-promotion #f))
  (define (refuse field)
    (error 'owned-audit "missing-or-mismatched-supplemental-promotion: ~a" field))
  (define (same field actual expected)
    (unless (equal? actual expected)
      (refuse field)))
  (cond
    [(not (or contract sha p)) #f]
    [else
     (define context (hash-ref authority 'evidence-context #f))
     (define registration (hash-ref authority 'evidence-registration #f))
     (define red (hash-ref authority 'red (hasheq)))
     (unless (and (hash? contract)
                  (hash? p)
                  (hash? context)
                  (hash? registration)
                  (valid-supplemental-record? (freeze-wire p))
                  (equal? (hash-ref p 'kind #f) "supplemental-evidence-promotion/v1")
                  (string? sha)
                  (regexp-match? #px"^[0-9a-f]{64}$" sha))
       (refuse 'descriptor))
     (same 'sha256 sha (hash-ref p 'sha256 #f))
     (same 'context context (hash-ref p 'context #f))
     (same 'registration-context context (hash-ref registration 'context #f))
     (same 'staged-context-digest
           (hash-ref p 'staged-context-digest #f)
           (evidence-context-digest context))
     (same 'receipt-sha256 (hash-ref p 'receipt-sha256 #f) (hash-ref context 'receipt-sha256 #f))
     (same 'receipt
           (hash-ref p 'receipt-sha256 #f)
           (canonical-evidence-digest (hash-ref authority 'receipt)))
     (same 'generation (hash-ref p 'generation #f) (hash-ref authority 'generation #f))
     (same 'context-generation (hash-ref p 'generation #f) (hash-ref context 'receipt-generation #f))
     (same 'operation-id (hash-ref p 'operation-id #f) (hash-ref registration 'operation-id #f))
     (same 'green-proof-ref (hash-ref p 'green-proof-ref #f) ref)
     (same 'red-proof-ref (hash-ref p 'red-proof-ref #f) (hash-ref red 'reference #f))
     (for ([key '(task-proposal task-approval candidate-proposal candidate-approval)])
       (unless (hash-has-key? contract key)
         (refuse key))
       (same key (hash-ref p key #f) (hash-ref contract key)))
     (unless (and (hash? (hash-ref p 'supersession-proposal #f))
                  (hash? (hash-ref p 'supersession-approval #f)))
       (refuse 'supersession))
     sha]))
