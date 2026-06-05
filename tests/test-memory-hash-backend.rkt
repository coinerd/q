#lang racket/base
;; tests/test-memory-hash-backend.rkt — In-memory hash backend tests

(require rackunit
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-item #:id [id "mem-1"]
                   #:type [type 'semantic]
                   #:scope [scope 'project]
                   #:content [content "Test content"]
                   #:project [project "/tmp/test"]
                   #:session [session "sess-1"]
                   #:tags [tags '("test")]
                   #:sensitivity [sensitivity 'public]
                   #:expires [expires #f]
                   #:created [created "2026-06-05T12:00:00Z"]
                   #:updated [updated "2026-06-05T12:00:00Z"])
  (memory-item id type scope content
               (hasheq 'project-root project
                       'session-id session
                       'tags tags)
               (hasheq 'sensitivity sensitivity
                       'confidence 0.9
                       'expires-at expires)
               created updated))

;; ---------------------------------------------------------------------------
;; Backend construction
;; ---------------------------------------------------------------------------

(test-case "make-memory-hash-backend creates valid backend"
  (define b (make-memory-hash-backend))
  (check-true (valid-backend? b))
  (check-equal? (memory-backend-name b) "memory-hash"))

(test-case "backend is available"
  (define b (make-memory-hash-backend))
  (check-true (gen:memory-available? b)))

;; ---------------------------------------------------------------------------
;; Store
;; ---------------------------------------------------------------------------

(test-case "store item successfully"
  (define b (make-memory-hash-backend))
  (define r (gen:store-memory! b (make-item)))
  (check-true (memory-result-ok? r))
  (check-equal? (memory-result-value r) "mem-1"))

(test-case "store rejects duplicate id"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item))
  (define r (gen:store-memory! b (make-item)))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'duplicate))

(test-case "store rejects invalid item"
  (define b (make-memory-hash-backend))
  (define bad-item (memory-item "" 'semantic 'project ""
                                (hasheq) (hasheq) "" ""))
  (define r (gen:store-memory! b bad-item))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'invalid-item))

;; ---------------------------------------------------------------------------
;; Retrieve
;; ---------------------------------------------------------------------------

(test-case "retrieve by scope"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "a" #:scope 'project))
  (gen:store-memory! b (make-item #:id "b" #:scope 'session))
  (define r (gen:retrieve-memory b (memory-query "" 'project #f #f #f #f 10 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 1)
  (check-equal? (memory-item-id (car (memory-result-value r))) "a"))

(test-case "retrieve by type filter"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "a" #:type 'semantic))
  (gen:store-memory! b (make-item #:id "b" #:type 'episodic))
  (define r (gen:retrieve-memory b (memory-query "" #f #f #f '(semantic) #f 10 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 1)
  (check-equal? (memory-item-id (car (memory-result-value r))) "a"))

(test-case "retrieve by project"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "a" #:project "/proj1"))
  (gen:store-memory! b (make-item #:id "b" #:project "/proj2"))
  (define r (gen:retrieve-memory b (memory-query "" #f "/proj1" #f #f #f 10 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 1)
  (check-equal? (memory-item-id (car (memory-result-value r))) "a"))

(test-case "retrieve by tags"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "a" #:tags '("important" "bug")))
  (gen:store-memory! b (make-item #:id "b" #:tags '("feature")))
  (define r (gen:retrieve-memory b (memory-query "" #f #f #f #f '("important") 10 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 1)
  (check-equal? (memory-item-id (car (memory-result-value r))) "a"))

(test-case "retrieve respects limit"
  (define b (make-memory-hash-backend))
  (for ([i (in-range 5)])
    (gen:store-memory! b (make-item #:id (format "mem-~a" i)
                                    #:updated (format "2026-06-05T12:0~a:00Z" i))))
  (define r (gen:retrieve-memory b (memory-query "" #f #f #f #f #f 3 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 3)
  (check-equal? (hash-ref (memory-result-metadata r) 'total) 5))

(test-case "retrieve excludes expired by default"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "active" #:expires #f))
  (gen:store-memory! b (make-item #:id "expired" #:expires "2020-01-01T00:00:00Z"))
  (define r (gen:retrieve-memory b (memory-query "" #f #f #f #f #f 10 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 1)
  (check-equal? (memory-item-id (car (memory-result-value r))) "active"))

(test-case "retrieve includes expired when requested"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "active" #:expires #f))
  (gen:store-memory! b (make-item #:id "expired" #:expires "2020-01-01T00:00:00Z"))
  (define r (gen:retrieve-memory b (memory-query "" #f #f #f #f #f 10 #t)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 2))

;; ---------------------------------------------------------------------------
;; Update
;; ---------------------------------------------------------------------------

(test-case "update item content"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item))
  (define r (gen:update-memory! b "mem-1" (hash 'content "Updated content")))
  (check-true (memory-result-ok? r))
  (define items (memory-hash-backend-items b))
  (check-equal? (memory-item-content (car items)) "Updated content"))

(test-case "update item tags"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item))
  (define r (gen:update-memory! b "mem-1" (hash 'tags '("updated"))))
  (check-true (memory-result-ok? r))
  (define items (memory-hash-backend-items b))
  (check-equal? (hash-ref (memory-item-metadata (car items)) 'tags) '("updated")))

(test-case "update non-existent item fails"
  (define b (make-memory-hash-backend))
  (define r (gen:update-memory! b "no-such" (hash 'content "x")))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'not-found))

;; ---------------------------------------------------------------------------
;; Delete
;; ---------------------------------------------------------------------------

(test-case "delete item"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item))
  (define r (gen:delete-memory! b "mem-1" 'project))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-hash-backend-items b)) 0))

(test-case "delete with scope mismatch fails"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:scope 'project))
  (define r (gen:delete-memory! b "mem-1" 'session))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'scope-mismatch))

(test-case "delete non-existent item fails"
  (define b (make-memory-hash-backend))
  (define r (gen:delete-memory! b "nope" #f))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'not-found))

(test-case "delete with #f scope bypasses scope check"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:scope 'project))
  (define r (gen:delete-memory! b "mem-1" #f))
  (check-true (memory-result-ok? r)))

;; ---------------------------------------------------------------------------
;; List
;; ---------------------------------------------------------------------------

(test-case "list returns all items"
  (define b (make-memory-hash-backend))
  (for ([i (in-range 3)])
    (gen:store-memory! b (make-item #:id (format "m~a" i))))
  (define r (gen:list-memory b (memory-query "" #f #f #f #f #f 100 #t)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 3))

;; ---------------------------------------------------------------------------
;; Deterministic ordering
;; ---------------------------------------------------------------------------

(test-case "items sorted by updated-at descending"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "old" #:updated "2026-01-01T00:00:00Z"))
  (gen:store-memory! b (make-item #:id "new" #:updated "2026-06-01T00:00:00Z"))
  (define r (gen:list-memory b (memory-query "" #f #f #f #f #f 100 #t)))
  (define items (memory-result-value r))
  (check-equal? (memory-item-id (car items)) "new")
  (check-equal? (memory-item-id (cadr items)) "old"))

;; ---------------------------------------------------------------------------
;; Manage (no-op)
;; ---------------------------------------------------------------------------

(test-case "manage returns ok"
  (define b (make-memory-hash-backend))
  (define r (gen:manage-memory! b (hash)))
  (check-true (memory-result-ok? r)))

;; ---------------------------------------------------------------------------
;; Isolation — separate backends don't share state
;; ---------------------------------------------------------------------------

(test-case "separate backends are isolated"
  (define b1 (make-memory-hash-backend))
  (define b2 (make-memory-hash-backend))
  (gen:store-memory! b1 (make-item))
  (check-equal? (length (memory-hash-backend-items b1)) 1)
  (check-equal? (length (memory-hash-backend-items b2)) 0))
