#lang racket/base
;; runtime/memory/backends/chained.rkt — L1/L2 chained memory backend
;;
;; v0.95.11: Layered backend that tries L1 (fast/local) first,
;; then falls back to L2 (persistent/external) on miss or failure.
;;
;; Store: writes to both L1 and L2 (write-through)
;; Retrieve: checks L1 first, falls back to L2, merges and dedups
;; Delete: removes from both layers
;; Update: updates both layers
;; List: merges from both, deduped by item id

(require racket/list
         "memory-hash.rkt"
         "../types.rkt"
         "../protocol.rkt")

;; ---------------------------------------------------------------------------
;; Dedup helper
;; ---------------------------------------------------------------------------

(define (dedup-items items)
  ;; Keep first occurrence of each id
  (define seen (make-hasheq))
  (for/list ([item (in-list items)]
             #:when (not (hash-has-key? seen (memory-item-id item))))
    (hash-set! seen (memory-item-id item) #t)
    item))

;; ---------------------------------------------------------------------------
;; Chained backend constructor
;; ---------------------------------------------------------------------------

(define (make-chained-backend l1 l2 #:write-through? [write-through? #t] #:l2-read? [l2-read? #t])
  (memory-backend
   (format "chained(~a+~a)" (memory-backend-name l1) (memory-backend-name l2))
   ;; store!: write to L1, optionally write through to L2
   (lambda (item)
     (define r1 (gen:store-memory! l1 item))
     (cond
       [(and write-through? (gen:memory-available? l2))
        (define r2 (gen:store-memory! l2 item))
        ;; Return L1 result; mark error if L2 failed
        (if (memory-result-ok? r2)
            r1
            (memory-result #f
                           (memory-result-value r1)
                           (hash 'l2-error (memory-result-error r2))
                           (memory-result-metadata r1)))]
       [else r1]))
   ;; retrieve: L1 first, fall back to L2, merge/dedup
   (lambda (query)
     (define r1 (gen:retrieve-memory l1 query))
     (cond
       [(and l2-read? (gen:memory-available? l2))
        (define r2 (gen:retrieve-memory l2 query))
        (define merged (append (memory-result-value r1) (memory-result-value r2)))
        (memory-result #t (dedup-items merged) #f (memory-result-metadata r1))]
       [else r1]))
   ;; update!: update both layers
   (lambda (id patch)
     (define r1 (gen:update-memory! l1 id patch))
     (when (and write-through? (gen:memory-available? l2))
       (gen:update-memory! l2 id patch))
     r1)
   ;; delete!: remove from both layers
   (lambda (id scope)
     (define r1 (gen:delete-memory! l1 id scope))
     (when (gen:memory-available? l2)
       (gen:delete-memory! l2 id scope))
     r1)
   ;; list: merge from both, dedup, preserving protocol shape
   (lambda (query)
     (define r1 (gen:list-memory l1 query))
     (define items-l1
       (if (memory-result-ok? r1)
           (memory-result-value r1)
           '()))
     (cond
       [(and l2-read? (gen:memory-available? l2))
        (define r2 (gen:list-memory l2 query))
        (define items-l2
          (if (memory-result-ok? r2)
              (memory-result-value r2)
              '()))
        (memory-result
         #t
         (dedup-items (append items-l1 items-l2))
         #f
         (hasheq 'backend 'chained 'l1-count (length items-l1) 'l2-count (length items-l2)))]
       [else (memory-result #t items-l1 #f (hasheq 'backend 'chained 'l1-count (length items-l1)))]))
   ;; available?: both layers available
   (lambda () (and (gen:memory-available? l1) (gen:memory-available? l2)))
   ;; manage!
   (lambda (policy)
     (define r1 (gen:manage-memory! l1 policy))
     (when (gen:memory-available? l2)
       (gen:manage-memory! l2 policy))
     r1)))

;; ---------------------------------------------------------------------------
;; Provide
;; ---------------------------------------------------------------------------

(provide make-chained-backend)
