#lang racket/base
;; runtime/memory/protocol.rkt — Backend protocol and generic operations
;;
;; Defines the memory backend struct and generic operations.
;; No IO, no concrete backend imports.

(require racket/contract
         "types.rkt")

(provide (struct-out memory-backend)
         memory-backend/c
         gen:store-memory!
         gen:retrieve-memory
         gen:update-memory!
         gen:delete-memory!
         gen:list-memory
         gen:memory-available?
         gen:manage-memory!
         valid-backend?)

;; ---------------------------------------------------------------------------
;; Backend struct — wraps implementation functions
;; ---------------------------------------------------------------------------

(struct memory-backend
        (name ; string — backend identifier
         store! ; memory-item -> memory-result
         retrieve ; memory-query -> memory-result (list of items)
         update! ; id patch-hash -> memory-result
         delete! ; id scope -> memory-result
         list ; memory-query -> memory-result (list of items)
         available? ; -> boolean
         manage!) ; policy-hash -> memory-result (initially no-op)
  #:transparent)

;; ---------------------------------------------------------------------------
;; Generic operations — delegate to backend functions
;; ---------------------------------------------------------------------------

(define (gen:store-memory! backend item)
  ((memory-backend-store! backend) item))

(define (gen:retrieve-memory backend query)
  ((memory-backend-retrieve backend) query))

(define (gen:update-memory! backend id patch)
  ((memory-backend-update! backend) id patch))

(define (gen:delete-memory! backend id scope)
  ((memory-backend-delete! backend) id scope))

(define (gen:list-memory backend query)
  ((memory-backend-list backend) query))

(define (gen:memory-available? backend)
  ((memory-backend-available? backend)))

(define (gen:manage-memory! backend policy)
  ((memory-backend-manage! backend) policy))

;; ---------------------------------------------------------------------------
;; Validation
;; ---------------------------------------------------------------------------

(define (valid-backend? v)
  (and (memory-backend? v)
       (string? (memory-backend-name v))
       (procedure? (memory-backend-store! v))
       (procedure? (memory-backend-retrieve v))
       (procedure? (memory-backend-update! v))
       (procedure? (memory-backend-delete! v))
       (procedure? (memory-backend-list v))
       (procedure? (memory-backend-available? v))
       (procedure? (memory-backend-manage! v))))

;; ---------------------------------------------------------------------------
;; Contract helpers
;; ---------------------------------------------------------------------------

(define memory-backend/c
  (struct/c memory-backend
            string?
            (-> memory-item? memory-result?)
            (-> memory-query? memory-result?)
            (-> string? hash? memory-result?)
            (-> string? (or/c memory-scope? #f) memory-result?)
            (-> memory-query? memory-result?)
            (-> boolean?)
            (-> hash? memory-result?)))
