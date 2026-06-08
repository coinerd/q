#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-backend-factory-w4.rkt — W4 tests for complex backend config factory
(require rackunit
         "../runtime/memory/service.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/backends/memory-hash.rkt")

(test-case "W4: 'hash spec still works"
  (define cfg (hash 'memory-backend 'hash))
  (define be (initialize-memory-backend! cfg))
  (check-true (memory-backend? be))
  (check-equal? (memory-backend-name be) "memory-hash"))

(test-case "W4: #f disables backend"
  (define cfg (hash))
  (define be (initialize-memory-backend! cfg))
  (check-false be)
  (check-false (current-memory-backend)))

(test-case "W4: pre-constructed backend works"
  (define pre (make-memory-hash-backend))
  (define cfg (hash 'memory-backend pre))
  (define be (initialize-memory-backend! cfg))
  (check-eq? be pre))

(test-case "W4: chained backend spec constructs chained backend"
  (define cfg (hash 'memory-backend (hash 'type 'chained 'l1 'hash 'l2 'hash 'write-through? #t)))
  (define be (initialize-memory-backend! cfg))
  (check-true (memory-backend? be))
  (check-true (string-contains? (memory-backend-name be) "chained"))
  ;; Verify it works: store and retrieve
  (define item
    (memory-item
     "test-id"
     'semantic
     'session
     "Test content."
     (hasheq 'source 'test 'session-id "s" 'project-root "." 'tags '() 'origin-tool-call-id "test")
     (hasheq 'sensitivity 'public 'confidence 0.8 'expires-at #f 'supersedes '())
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (define result (gen:store-memory! be item))
  (check-true (memory-result-ok? result)))

(test-case "W4: chained spec missing l2 fails closed"
  (define cfg (hash 'memory-backend (hash 'type 'chained 'l1 'hash)))
  (parameterize ([current-memory-backend #f])
    (define be (initialize-memory-backend! cfg))
    (check-false be)))

(test-case "W4: chained spec missing l1 fails closed"
  (define cfg (hash 'memory-backend (hash 'type 'chained 'l2 'hash)))
  (parameterize ([current-memory-backend #f])
    (define be (initialize-memory-backend! cfg))
    (check-false be)))

(test-case "W4: unknown spec type fails closed"
  (define cfg (hash 'memory-backend (hash 'type 'unknown)))
  (parameterize ([current-memory-backend #f])
    (define be (initialize-memory-backend! cfg))
    (check-false be)))

(test-case "W4: invalid spec fails closed"
  (define cfg (hash 'memory-backend 42))
  (parameterize ([current-memory-backend #f])
    (define be (initialize-memory-backend! cfg))
    (check-false be)))

(test-case "W4: nested spec does not mutate input config"
  (define spec (hash 'type 'chained 'l1 'hash 'l2 'hash 'write-through? #t))
  (define cfg (hash 'memory-backend spec))
  (define be (initialize-memory-backend! cfg))
  (check-true (memory-backend? be))
  ;; Original spec should be unchanged
  (check-equal? (hash-ref spec 'type) 'chained)
  (check-equal? (hash-ref spec 'write-through?) #t))

(test-case "W4: build-backend-from-spec is available"
  (define be (build-backend-from-spec 'hash #f))
  (check-true (memory-backend? be)))

(require racket/string)
