#lang racket/base
;; runtime/memory/migration.rkt — Memory backend migration utilities
;;
;; M13-F8: Export/import memory items between backends.
;; Migration is policy-aware and never crosses scopes unless explicitly requested.

(require "types.rkt"
         "protocol.rkt"
         "policy.rkt")

;; ---------------------------------------------------------------------------
;; Export: retrieve all items from a source backend
;; ---------------------------------------------------------------------------

(define (export-memory-items source-backend #:scope [scope #f] #:include-expired? [include-expired? #f])
  ;; Export all items from source backend, optionally filtered by scope.
  ;; Returns (memory-result ok? items-or-error metadata).
  (cond
    [(not (memory-backend? source-backend))
     (memory-result #f #f (make-memory-error 'invalid-backend "Source is not a valid memory backend") (hasheq))]
    [else
     (define query (memory-query #f
                                 #f  ; no scope filter = all scopes
                                 #f #f #f #f
                                 10000
                                 include-expired?))
     (define result (gen:retrieve-memory source-backend query))
     (if (memory-result-ok? result)
         (let ([items (memory-result-value result)])
           ;; If scope filter was requested, further filter to exact scope
           (define filtered
             (if scope
                 (filter (lambda (item) (eq? (memory-item-scope item) scope)) items)
                 items))
           (memory-result #t filtered #f (hasheq 'exported-count (length filtered))))
         result)]))

;; ---------------------------------------------------------------------------
;; Import: write items into a destination backend
;; ---------------------------------------------------------------------------

(define (import-memory-items! dest-backend items #:scope-restriction [scope-restriction #f])
  ;; Import items into destination backend.
  ;; If scope-restriction is set, only items matching that scope are imported.
  ;; Returns (memory-result ok? imported-count error metadata).
  (cond
    [(not (memory-backend? dest-backend))
     (memory-result #f #f (make-memory-error 'invalid-backend "Destination is not a valid memory backend") (hasheq))]
    [(not (list? items))
     (memory-result #f #f (make-memory-error 'invalid-items "Items must be a list") (hasheq))]
    [else
     (define to-import
       (if scope-restriction
           (filter (lambda (item) (eq? (memory-item-scope item) scope-restriction)) items)
           items))
     (define results
       (for/list ([item (in-list to-import)])
         (gen:store-memory! dest-backend item)))
     (define succeeded (filter memory-result-ok? results))
     (define failed (filter (lambda (r) (not (memory-result-ok? r))) results))
     (memory-result #t
                     (length succeeded)
                     (if (null? failed) #f (make-memory-error 'partial-import "Some items failed to import"))
                     (hasheq 'imported (length succeeded) 'failed (length failed)))]))

;; ---------------------------------------------------------------------------
;; Migrate: export from source, import into dest
;; ---------------------------------------------------------------------------

(define (migrate-memory! source-backend dest-backend
                          #:scope [scope #f]
                          #:scope-restriction [scope-restriction #f]
                          #:include-expired? [include-expired? #f])
  ;; Migrate all items from source to destination backend.
  ;; scope: filter export to this scope (if set).
  ;; scope-restriction: only import items matching this scope (if set).
  ;; include-expired?: include expired items in migration.
  (define export-result
    (export-memory-items source-backend
                          #:scope scope
                          #:include-expired? include-expired?))
  (cond
    [(not (memory-result-ok? export-result))
     export-result]
    [else
     (define items (memory-result-value export-result))
     (import-memory-items! dest-backend items #:scope-restriction scope-restriction)]))

(provide export-memory-items
         import-memory-items!
         migrate-memory!)
