#lang racket/base
;; tests/test-memory-types.rkt — Memory domain model tests

(require racket/list
         rackunit
         "../runtime/memory/types.rkt")

;; ---------------------------------------------------------------------------
;; Struct construction
;; ---------------------------------------------------------------------------

(define (make-test-item
         #:id [id "mem-001"]
         #:type [type 'semantic]
         #:scope [scope 'project]
         #:content [content "Test memory item"]
         #:metadata [metadata (hasheq 'project-root "/tmp/test" 'session-id "sess-1" 'tags '("test"))]
         #:validity [validity (hasheq 'sensitivity 'public 'confidence 0.9)]
         #:created-at [created-at "2026-06-05T12:00:00Z"]
         #:updated-at [updated-at "2026-06-05T12:00:00Z"])
  (memory-item id type scope content metadata validity created-at updated-at))

(test-case "memory-item construction"
  (define item (make-test-item))
  (check-equal? (memory-item-id item) "mem-001")
  (check-equal? (memory-item-type item) 'semantic)
  (check-equal? (memory-item-scope item) 'project)
  (check-equal? (memory-item-content item) "Test memory item"))

(test-case "valid-memory-item? rejects empty id/content"
  (define empty-id
    (memory-item ""
                 'semantic
                 'project
                 "has content"
                 (hasheq)
                 (hasheq)
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? empty-id))
  (define empty-content
    (memory-item "id"
                 'semantic
                 'project
                 ""
                 (hasheq)
                 (hasheq)
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? empty-content)))

(test-case "valid-memory-item? accepts well-formed items"
  (define item (make-test-item))
  (check-true (valid-memory-item? item)))

(test-case "valid-memory-item? rejects empty content"
  (define item
    (memory-item "id"
                 'semantic
                 'project
                 ""
                 (hasheq)
                 (hasheq)
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? item)))

(test-case "valid-memory-item? rejects invalid type"
  (define item
    (memory-item "id"
                 'invalid
                 'project
                 "content"
                 (hasheq)
                 (hasheq)
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? item)))

(test-case "valid-memory-item? rejects invalid scope"
  (define item
    (memory-item "id"
                 'semantic
                 'invalid
                 "content"
                 (hasheq)
                 (hasheq)
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? item)))

;; ---------------------------------------------------------------------------
;; Predicates
;; ---------------------------------------------------------------------------

(test-case "memory-type? accepts valid types"
  (check-true (memory-type? 'episodic))
  (check-true (memory-type? 'semantic))
  (check-true (memory-type? 'procedural))
  (check-false (memory-type? 'invalid))
  (check-false (memory-type? "semantic")))

(test-case "memory-scope? accepts valid scopes"
  (check-true (memory-scope? 'session))
  (check-true (memory-scope? 'project))
  (check-true (memory-scope? 'user))
  (check-false (memory-scope? 'global)))

(test-case "sensitivity? accepts valid values"
  (check-true (sensitivity? 'public))
  (check-true (sensitivity? 'internal))
  (check-true (sensitivity? 'sensitive))
  (check-true (sensitivity? 'secret))
  (check-false (sensitivity? 'unknown)))

;; ---------------------------------------------------------------------------
;; Hash round-trip
;; ---------------------------------------------------------------------------

(test-case "memory-item->hash / hash->memory-item round-trip: all 8 fields"
  (define item (make-test-item))
  (define h (memory-item->hash item))
  (define restored (hash->memory-item h))
  ;; Verify ALL 8 fields survive round-trip
  (check-equal? (memory-item-id restored) (memory-item-id item))
  (check-equal? (memory-item-type restored) (memory-item-type item))
  (check-equal? (memory-item-scope restored) (memory-item-scope item))
  (check-equal? (memory-item-content restored) (memory-item-content item))
  (check-equal? (memory-item-metadata restored) (memory-item-metadata item))
  (check-equal? (memory-item-validity restored) (memory-item-validity item))
  (check-equal? (memory-item-created-at restored) (memory-item-created-at item))
  (check-equal? (memory-item-updated-at restored) (memory-item-updated-at item)))

(test-case "hash->memory-item tolerates missing keys"
  (define h (hasheq 'id "test-id" 'content "hello"))
  (define item (hash->memory-item h))
  (check-equal? (memory-item-id item) "test-id")
  (check-equal? (memory-item-content item) "hello")
  (check-equal? (memory-item-type item) 'semantic) ; default
  (check-equal? (memory-item-scope item) 'session)) ; default

(test-case "memory-item->hash is jsexpr-safe"
  (define item (make-test-item))
  (define h (memory-item->hash item))
  ;; All keys should be symbols or strings
  (check-true (hash? h)))

;; ---------------------------------------------------------------------------
;; memory-query
;; ---------------------------------------------------------------------------

(test-case "memory-query construction"
  (define q (memory-query "test query" 'project "/tmp" "sess-1" #f #f 10 #f))
  (check-equal? (memory-query-text q) "test query")
  (check-equal? (memory-query-limit q) 10))

(test-case "memory-query round-trip"
  (define q (memory-query "find stuff" 'session #f "sess-2" '(semantic) '("tag1") 3 #t))
  (define h (memory-query->hash q))
  (define q2 (hash->memory-query h))
  (check-equal? (memory-query-text q2) "find stuff")
  (check-equal? (memory-query-limit q2) 3)
  (check-equal? (memory-query-include-expired? q2) #t))

(test-case "hash->memory-query defaults"
  (define h (hasheq 'text "search"))
  (define q (hash->memory-query h))
  (check-equal? (memory-query-text q) "search")
  (check-equal? (memory-query-limit q) default-memory-limit)
  (check-false (memory-query-scope q)))

;; ---------------------------------------------------------------------------
;; memory-result
;; ---------------------------------------------------------------------------

(test-case "memory-result success"
  (define r (memory-result #t "mem-001" #f (hasheq 'backend 'test)))
  (check-true (memory-result-ok? r))
  (check-equal? (memory-result-value r) "mem-001")
  (check-false (memory-result-error r)))

(test-case "memory-result error"
  (define err (make-memory-error 'blocked "Secret content detected" #f))
  (define r (memory-result #f #f err (hasheq)))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'blocked))

(test-case "memory-result->hash serializes item values"
  (define item (make-test-item))
  (define r (memory-result #t item #f (hasheq 'count 1)))
  (define h (memory-result->hash r))
  (check-true (hash? (hash-ref h 'value))))

(test-case "memory-result->hash serializes list values"
  (define items (list (make-test-item #:id "a") (make-test-item #:id "b")))
  (define r (memory-result #t items #f (hasheq 'count 2)))
  (define h (memory-result->hash r))
  (check-equal? (length (hash-ref h 'value)) 2))

(test-case "make-memory-error"
  (define err (make-memory-error 'timeout "Backend timeout" #t))
  (check-equal? (hash-ref err 'code) 'timeout)
  (check-equal? (hash-ref err 'message) "Backend timeout")
  (check-true (hash-ref err 'retryable?)))

;; ---------------------------------------------------------------------------
;; ISO-8601 timestamp validation (P3-1)
;; ---------------------------------------------------------------------------

(test-case "iso-8601-timestamp?: accepts valid timestamp"
  (check-true (iso-8601-timestamp? "2026-06-05T12:00:00Z"))
  (check-true (iso-8601-timestamp? "2025-01-01T00:00:00Z")))

(test-case "iso-8601-timestamp?: rejects non-ISO string"
  (check-false (iso-8601-timestamp? "yesterday"))
  (check-false (iso-8601-timestamp? ""))
  (check-false (iso-8601-timestamp? "not-a-date")))

(test-case "iso-8601-timestamp?: rejects non-string"
  (check-false (iso-8601-timestamp? 12345))
  (check-false (iso-8601-timestamp? #f)))

(test-case "valid-memory-item?: rejects non-ISO created-at"
  (define item
    (memory-item "id"
                 'semantic
                 'project
                 "content"
                 (hasheq)
                 (hasheq)
                 "yesterday"
                 "2026-01-01T00:00:00Z"))
  (check-false (valid-memory-item? item)))

(test-case "valid-memory-item?: rejects non-ISO updated-at"
  (define item
    (memory-item "id" 'semantic 'project "content" (hasheq) (hasheq) "2026-01-01T00:00:00Z" ""))
  (check-false (valid-memory-item? item)))
