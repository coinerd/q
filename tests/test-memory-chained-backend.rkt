#lang racket/base
;; tests/test-memory-chained-backend.rkt — Chained L1/L2 backend tests
;;
;; v0.95.11: Tests for chained memory backend:
;; - Write-through stores to both L1 and L2
;; - Retrieve merges and dedups from both layers
;; - L1 miss falls back to L2
;; - Delete removes from both
;; - L2 unavailable falls back to L1 only

(require rackunit
         racket/string
         racket/list
         "../runtime/memory/backends/chained.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-item #:id [id "test-1"]
                        #:content [content "test content"]
                        #:scope [scope 'session])
  (memory-item id
               'semantic
               scope
               content
               (hasheq 'source 'test 'project-root "/tmp" 'session-id "s1" 'tags '())
               (hasheq 'sensitivity 'public 'confidence 0.9)
               "2025-01-01T00:00:00Z"
               "2025-01-01T00:00:00Z"))

(define (item-id=? item id)
  (equal? (memory-item-id item) id))

;; ---------------------------------------------------------------------------
;; Write-through
;; ---------------------------------------------------------------------------

(test-case "chained: store writes to both L1 and L2"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (define chained (make-chained-backend l1 l2))
  (define item (make-test-item))
  (define result (gen:store-memory! chained item))
  (check-true (memory-result-ok? result))
  ;; Verify in L1
  (define r1 (gen:retrieve-memory l1 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r1)) 1)
  ;; Verify in L2
  (define r2 (gen:retrieve-memory l2 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r2)) 1))

(test-case "chained: no write-through when disabled"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (define chained (make-chained-backend l1 l2 #:write-through? #f))
  (define item (make-test-item))
  (gen:store-memory! chained item)
  ;; L1 has it
  (define r1 (gen:retrieve-memory l1 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r1)) 1)
  ;; L2 does NOT have it
  (define r2 (gen:retrieve-memory l2 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r2)) 0))

;; ---------------------------------------------------------------------------
;; Retrieve merge/dedup
;; ---------------------------------------------------------------------------

(test-case "chained: retrieve merges L1 and L2 results"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  ;; Store different items in each
  (gen:store-memory! l1 (make-test-item #:id "l1-only" #:content "from L1"))
  (gen:store-memory! l2 (make-test-item #:id "l2-only" #:content "from L2"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? result))
  (define items (memory-result-value result))
  (check-equal? (length items) 2)
  (define ids (map memory-item-id items))
  (check-true (and (member "l1-only" ids) #t))
  (check-true (and (member "l2-only" ids) #t)))

(test-case "chained: retrieve deduplicates by id"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  ;; Store same item in both
  (define shared (make-test-item #:id "shared"))
  (gen:store-memory! l1 shared)
  (gen:store-memory! l2 shared)
  (define chained (make-chained-backend l1 l2))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (define items (memory-result-value result))
  ;; Only one copy despite being in both layers
  (check-equal? (length items) 1)
  (check-equal? (memory-item-id (car items)) "shared"))

(test-case "chained: L1 miss falls back to L2"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  ;; Only L2 has data
  (gen:store-memory! l2 (make-test-item #:id "l2-data"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value result)) 1)
  (check-equal? (memory-item-id (car (memory-result-value result))) "l2-data"))

;; ---------------------------------------------------------------------------
;; Delete
;; ---------------------------------------------------------------------------

(test-case "chained: delete removes from both layers"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (define item (make-test-item))
  (define chained (make-chained-backend l1 l2))
  (gen:store-memory! chained item)
  ;; Delete
  (gen:delete-memory! chained "test-1" 'session)
  ;; Both should be empty
  (define r1 (gen:retrieve-memory l1 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r1)) 0)
  (define r2 (gen:retrieve-memory l2 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r2)) 0))

;; ---------------------------------------------------------------------------
;; L2 unavailable
;; ---------------------------------------------------------------------------

(test-case "chained: L2 read disabled uses L1 only"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  ;; L2 has data but L2-read is disabled
  (gen:store-memory! l2 (make-test-item #:id "l2-hidden"))
  (gen:store-memory! l1 (make-test-item #:id "l1-visible"))
  (define chained (make-chained-backend l1 l2 #:l2-read? #f))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (define ids (map memory-item-id (memory-result-value result)))
  (check-equal? ids '("l1-visible")))

(test-case "chained: available only when both layers available"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (define chained (make-chained-backend l1 l2))
  (check-true (gen:memory-available? chained)))

(test-case "chained: name includes both layer names"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (define chained (make-chained-backend l1 l2))
  (check-true (string-contains? (memory-backend-name chained) "memory-hash")))

(test-case "chained: list returns memory-result"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (gen:store-memory! l1 (make-test-item #:id "l1-list"))
  (gen:store-memory! l2 (make-test-item #:id "l2-list"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:list-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result? result))
  (check-true (memory-result-ok? result))
  (check-equal? (length (memory-result-value result)) 2))

;; ---------------------------------------------------------------------------
;; L1 error fallback (P1-2)
;; ---------------------------------------------------------------------------

(test-case "chained: retrieve falls back when L1 errors"
  (define l1
    (memory-backend
     "failing-l1"
     (lambda (item) (error "L1 store fail"))
     (lambda (q) (memory-result #f #f (hash 'code 'error 'message "L1 broken") (hasheq)))
     (lambda (id patch) (memory-result #t #f #f (hasheq)))
     (lambda (id scope) (memory-result #t #f #f (hasheq)))
     (lambda (q) (memory-result #f #f (hash 'code 'error 'message "L1 list broken") (hasheq)))
     (lambda () #t)
     (lambda (p) (memory-result #t #f #f (hasheq)))))
  (define l2 (make-memory-hash-backend))
  (gen:store-memory! l2 (make-test-item #:id "l2-data" #:content "from L2 only"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? result))
  (define items (memory-result-value result))
  (check-equal? (length items) 1)
  (check-equal? (memory-item-id (car items)) "l2-data"))

(test-case "chained: list falls back when L1 errors"
  (define l1
    (memory-backend "failing-l1"
                    (lambda (item) (error "L1 fail"))
                    (lambda (q) (memory-result #f #f (hash 'code 'error 'message "broken") (hasheq)))
                    (lambda (id patch) (memory-result #t #f #f (hasheq)))
                    (lambda (id scope) (memory-result #t #f #f (hasheq)))
                    (lambda (q) (memory-result #f #f (hash 'code 'error 'message "broken") (hasheq)))
                    (lambda () #t)
                    (lambda (p) (memory-result #t #f #f (hasheq)))))
  (define l2 (make-memory-hash-backend))
  (gen:store-memory! l2 (make-test-item #:id "l2-list-item"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:list-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? result))
  (check-equal? (length (memory-result-value result)) 1)
  (check-equal? (memory-item-id (car (memory-result-value result))) "l2-list-item"))

;; ---------------------------------------------------------------------------
;; Global sort after merge (P3-2)
;; ---------------------------------------------------------------------------

(define (make-timestamped-item #:id [id "ts-item"] #:updated-at [ts "2025-01-01T00:00:00Z"])
  (memory-item id
               'semantic
               'session
               "content"
               (hasheq 'source 'test 'project-root "/tmp" 'session-id "s1" 'tags '())
               (hasheq 'sensitivity 'public 'confidence 0.9)
               "2025-01-01T00:00:00Z"
               ts))

(test-case "chained: merged results are globally sorted by updated-at desc"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  ;; L1 item with old timestamp
  (gen:store-memory! l1 (make-timestamped-item #:id "old-l1" #:updated-at "2025-01-01T00:00:00Z"))
  ;; L2 item with new timestamp
  (gen:store-memory! l2 (make-timestamped-item #:id "new-l2" #:updated-at "2025-12-31T00:00:00Z"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:list-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? result))
  (define items (memory-result-value result))
  (check-equal? (length items) 2)
  ;; Should be sorted: new first, old second
  (check-equal? (memory-item-id (car items)) "new-l2")
  (check-equal? (memory-item-id (cadr items)) "old-l1"))

(test-case "chained: retrieve merged results are globally sorted"
  (define l1 (make-memory-hash-backend))
  (define l2 (make-memory-hash-backend))
  (gen:store-memory! l1 (make-timestamped-item #:id "mid-l1" #:updated-at "2025-06-01T00:00:00Z"))
  (gen:store-memory! l2 (make-timestamped-item #:id "new-l2" #:updated-at "2025-12-01T00:00:00Z"))
  (gen:store-memory! l2 (make-timestamped-item #:id "old-l2" #:updated-at "2025-01-01T00:00:00Z"))
  (define chained (make-chained-backend l1 l2))
  (define result (gen:retrieve-memory chained (memory-query #f #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? result))
  (define items (memory-result-value result))
  (check-equal? (length items) 3)
  ;; Should be sorted by updated-at desc: new, mid, old
  (define ids (map memory-item-id items))
  (check-equal? ids '("new-l2" "mid-l1" "old-l2")))

;; ---------------------------------------------------------------------------
;; Store L2 fallback when L1 fails (P2-5)
;; ---------------------------------------------------------------------------

(test-case "chained: store falls back to L2 when L1 fails"
  (define l1
    (memory-backend
     "failing-l1"
     (lambda (item)
       (memory-result #f #f (hash 'code 'error 'message "L1 store fail" 'retryable? #f) (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda (id patch) (memory-result #t #f #f (hasheq)))
     (lambda (id scope) (memory-result #t #f #f (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda () #t)
     (lambda (p) (memory-result #t #f #f (hasheq)))))
  (define l2 (make-memory-hash-backend))
  (define chained (make-chained-backend l1 l2))
  (define item (make-test-item #:id "fallback-item"))
  (define r (gen:store-memory! chained item))
  ;; L2 should have succeeded
  (check-true (memory-result-ok? r))
  (check-equal? (memory-result-value r) "fallback-item")
  ;; Verify in L2
  (define r2 (gen:retrieve-memory l2 (memory-query #f 'session #f #f #f #f 100 #f)))
  (check-equal? (length (memory-result-value r2)) 1))

(test-case "chained: store returns error when both L1 and L2 fail"
  (define l1
    (memory-backend
     "failing-l1"
     (lambda (item)
       (memory-result #f #f (hash 'code 'error 'message "L1 fail" 'retryable? #f) (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda (id patch) (memory-result #t #f #f (hasheq)))
     (lambda (id scope) (memory-result #t #f #f (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda () #t)
     (lambda (p) (memory-result #t #f #f (hasheq)))))
  (define l2
    (memory-backend
     "failing-l2"
     (lambda (item)
       (memory-result #f #f (hash 'code 'error 'message "L2 fail" 'retryable? #f) (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda (id patch) (memory-result #t #f #f (hasheq)))
     (lambda (id scope) (memory-result #t #f #f (hasheq)))
     (lambda (q) (memory-result #t '() #f (hasheq)))
     (lambda () #t)
     (lambda (p) (memory-result #t #f #f (hasheq)))))
  (define chained (make-chained-backend l1 l2))
  (define item (make-test-item #:id "doom-item"))
  (define r (gen:store-memory! chained item))
  (check-false (memory-result-ok? r)))
