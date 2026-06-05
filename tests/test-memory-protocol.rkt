#lang racket/base
;; tests/test-memory-protocol.rkt — Memory backend protocol tests

(require rackunit
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt")

;; ---------------------------------------------------------------------------
;; Test backend helpers
;; ---------------------------------------------------------------------------

(define (ok-result val)
  (memory-result #t val #f (hasheq)))

(define (err-result code msg)
  (memory-result #f #f (make-memory-error code msg #f) (hasheq)))

(define test-store (make-hash))

(define (reset-store!)
  (hash-clear! test-store))

(define (make-test-backend)
  (memory-backend
   "test-backend"
   ;; store!
   (lambda (item)
     (hash-set! test-store (memory-item-id item) item)
     (ok-result (memory-item-id item)))
   ;; retrieve
   (lambda (query)
     (define items (hash-values test-store))
     (ok-result (filter (lambda (i)
                          (and (equal? (memory-query-scope query)
                                       (memory-item-scope i))
                               (member (memory-item-type i)
                                       (or (memory-query-types query) '(episodic semantic procedural)))))
                        items)))
   ;; update!
   (lambda (id patch)
     (define existing (hash-ref test-store id #f))
     (if existing
         (let ([updated (struct-copy memory-item existing
                                     [content (hash-ref patch 'content
                                                        (memory-item-content existing))]
                                     [updated-at (hash-ref patch 'updated-at
                                                           "2026-06-05T13:00:00Z")])])
           (hash-set! test-store id updated)
           (ok-result id))
         (err-result 'not-found "Item not found")))
   ;; delete!
   (lambda (id scope)
     (define existing (hash-ref test-store id #f))
     (cond
       [(not existing) (err-result 'not-found "Item not found")]
       [(not (eq? scope (memory-item-scope existing)))
        (err-result 'scope-mismatch "Scope mismatch")]
       [else
        (hash-remove! test-store id)
        (ok-result id)]))
   ;; list
   (lambda (query)
     (ok-result (hash-values test-store)))
   ;; available?
   (lambda () #t)
   ;; manage! (no-op)
   (lambda (policy) (ok-result #f))))

;; ---------------------------------------------------------------------------
;; Backend construction
;; ---------------------------------------------------------------------------

(test-case "valid-backend? accepts well-formed backend"
  (reset-store!)
  (define b (make-test-backend))
  (check-true (valid-backend? b)))

(test-case "valid-backend? rejects non-backend"
  (check-false (valid-backend? "not a backend"))
  (check-false (valid-backend? #f)))

(test-case "backend has required fields"
  (define b (make-test-backend))
  (check-equal? (memory-backend-name b) "test-backend")
  (check-true (procedure? (memory-backend-store! b)))
  (check-true (procedure? (memory-backend-retrieve b)))
  (check-true (procedure? (memory-backend-available? b))))

;; ---------------------------------------------------------------------------
;; Generic operations
;; ---------------------------------------------------------------------------

(test-case "gen:store-memory! stores item"
  (reset-store!)
  (define b (make-test-backend))
  (define item (memory-item "mem-1" 'semantic 'project "Test content"
                            (hasheq 'tags '("test"))
                            (hasheq 'sensitivity 'public)
                            "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z"))
  (define r (gen:store-memory! b item))
  (check-true (memory-result-ok? r))
  (check-equal? (memory-result-value r) "mem-1"))

(test-case "gen:retrieve-memory returns items"
  (reset-store!)
  (define b (make-test-backend))
  (define item (memory-item "mem-2" 'semantic 'project "Retrieve me"
                            (hasheq) (hasheq) "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z"))
  (gen:store-memory! b item)
  (define q (memory-query "test" 'project #f #f #f #f 5 #f))
  (define r (gen:retrieve-memory b q))
  (check-true (memory-result-ok? r))
  (check-true (>= (length (memory-result-value r)) 1)))

(test-case "gen:update-memory! updates content"
  (reset-store!)
  (define b (make-test-backend))
  (define item (memory-item "mem-3" 'semantic 'project "Original"
                            (hasheq) (hasheq) "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z"))
  (gen:store-memory! b item)
  (define r (gen:update-memory! b "mem-3" (hash 'content "Updated")))
  (check-true (memory-result-ok? r))
  ;; Verify updated
  (define q (memory-query "" 'project #f #f #f #f 5 #f))
  (define retrieved (gen:retrieve-memory b q))
  (define found (findf (lambda (i) (equal? (memory-item-id i) "mem-3"))
                       (memory-result-value retrieved)))
  (check-equal? (memory-item-content found) "Updated"))

(test-case "gen:delete-memory! removes item"
  (reset-store!)
  (define b (make-test-backend))
  (define item (memory-item "mem-4" 'semantic 'project "Delete me"
                            (hasheq) (hasheq) "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z"))
  (gen:store-memory! b item)
  (define r (gen:delete-memory! b "mem-4" 'project))
  (check-true (memory-result-ok? r))
  ;; Verify deleted
  (define q (memory-query "" 'project #f #f #f #f 5 #f))
  (define retrieved (gen:retrieve-memory b q))
  (check-equal? (length (memory-result-value retrieved)) 0))

(test-case "gen:delete-memory! scope mismatch fails"
  (reset-store!)
  (define b (make-test-backend))
  (define item (memory-item "mem-5" 'semantic 'project "Scoped"
                            (hasheq) (hasheq) "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z"))
  (gen:store-memory! b item)
  (define r (gen:delete-memory! b "mem-5" 'session))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'scope-mismatch))

(test-case "gen:memory-available? returns true"
  (reset-store!)
  (define b (make-test-backend))
  (check-true (gen:memory-available? b)))

(test-case "gen:manage-memory! returns no-op result"
  (reset-store!)
  (define b (make-test-backend))
  (define r (gen:manage-memory! b (hash)))
  (check-true (memory-result-ok? r)))

(test-case "gen:list-memory returns all items"
  (reset-store!)
  (define b (make-test-backend))
  (for ([i (in-range 3)])
    (gen:store-memory! b (memory-item (format "mem-~a" i) 'semantic 'project (format "Item ~a" i)
                                      (hasheq) (hasheq) "2026-06-05T12:00:00Z" "2026-06-05T12:00:00Z")))
  (define r (gen:list-memory b (memory-query "" #f #f #f #f #f 100 #f)))
  (check-true (memory-result-ok? r))
  (check-equal? (length (memory-result-value r)) 3))
