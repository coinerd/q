#lang racket/base
;; tests/test-memory-tools.rkt — Memory tools (store/search/delete) tests
;;
;; Tests the three memory tools with policy gating, error cases,
;; and backend integration using the in-memory hash backend.

(require rackunit
         "../tools/builtins/memory-tools.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt"
         "../tools/tool.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (with-backend backend thunk)
  (parameterize ([current-memory-backend backend])
    (thunk)))

(define (call-tool tool args)
  ;; Tools created by define-tool are handler functions
  (tool args #f))

;; ---------------------------------------------------------------------------
;; store-memory: error cases
;; ---------------------------------------------------------------------------

(test-case "store-memory: returns error when backend is #f"
  (parameterize ([current-memory-backend #f])
    (define r (call-tool tool-store-memory (hash 'content "test")))
    (check-true (tool-result-is-error? r))))

(test-case "store-memory: requires content"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash)))
      (check-true (tool-result-is-error? r)))))

(test-case "store-memory: rejects empty content"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash 'content "")))
      (check-true (tool-result-is-error? r)))))

(test-case "store-memory: rejects invalid type"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash 'content "test" 'type "invalid")))
      (check-true (tool-result-is-error? r)))))

(test-case "store-memory: rejects invalid scope"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash 'content "test" 'scope "global")))
      (check-true (tool-result-is-error? r)))))

(test-case "store-memory: rejects invalid sensitivity"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash 'content "test" 'sensitivity "top-secret")))
      (check-true (tool-result-is-error? r)))))

;; ---------------------------------------------------------------------------
;; store-memory: success cases
;; ---------------------------------------------------------------------------

(test-case "store-memory: stores with defaults"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory (hash 'content "Hello memory")))
      (check-false (tool-result-is-error? r))
      ;; Verify item was stored
      (define items (memory-hash-backend-items b))
      (check-equal? (length items) 1)
      (check-equal? (memory-item-content (car items)) "Hello memory")
      (check-equal? (memory-item-type (car items)) 'semantic)
      (check-equal? (memory-item-scope (car items)) 'session))))

(test-case "store-memory: stores with explicit type/scope/tags"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-store-memory
                           (hash 'content "Important fact"
                                 'type "episodic"
                                 'scope "project"
                                 'tags '("important" "architecture")
                                 'sensitivity "internal")))
      (check-false (tool-result-is-error? r))
      (define items (memory-hash-backend-items b))
      (check-equal? (memory-item-type (car items)) 'episodic)
      (check-equal? (memory-item-scope (car items)) 'project)
      (check-equal? (hash-ref (memory-item-metadata (car items)) 'tags)
                    '("important" "architecture")))))

(test-case "store-memory: secret sensitivity blocked by policy"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      ;; Default policy allows public, internal, sensitive but NOT secret
      (define r (call-tool tool-store-memory
                           (hash 'content "secret stuff"
                                 'sensitivity "secret")))
      (check-true (tool-result-is-error? r)))))

;; ---------------------------------------------------------------------------
;; search-memory: error cases
;; ---------------------------------------------------------------------------

(test-case "search-memory: returns error when backend is #f"
  (parameterize ([current-memory-backend #f])
    (define r (call-tool tool-search-memory (hash 'query "test")))
    (check-true (tool-result-is-error? r))))

(test-case "search-memory: rejects excessive limit"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-search-memory (hash 'limit 100)))
      (check-true (tool-result-is-error? r)))))

;; ---------------------------------------------------------------------------
;; search-memory: success cases
;; ---------------------------------------------------------------------------

(test-case "search-memory: finds stored items"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (call-tool tool-store-memory (hash 'content "fact one" 'scope "project"))
      (call-tool tool-store-memory (hash 'content "fact two" 'scope "project"))
      (define r (call-tool tool-search-memory
                           (hash 'scope "project" 'limit 10)))
      (check-false (tool-result-is-error? r)))))

(test-case "search-memory: empty results when no match"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (call-tool tool-store-memory (hash 'content "stored item" 'scope "session"))
      (define r (call-tool tool-search-memory
                           (hash 'scope "project" 'limit 10)))
      (check-false (tool-result-is-error? r)))))

;; ---------------------------------------------------------------------------
;; delete-memory: error cases
;; ---------------------------------------------------------------------------

(test-case "delete-memory: returns error when backend is #f"
  (parameterize ([current-memory-backend #f])
    (define r (call-tool tool-delete-memory (hash 'id "mem-1")))
    (check-true (tool-result-is-error? r))))

(test-case "delete-memory: requires id"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-delete-memory (hash)))
      (check-true (tool-result-is-error? r)))))

(test-case "delete-memory: fails for non-existent id"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      (define r (call-tool tool-delete-memory (hash 'id "no-such-id")))
      (check-true (tool-result-is-error? r)))))

;; ---------------------------------------------------------------------------
;; delete-memory: success cases
;; ---------------------------------------------------------------------------

(test-case "delete-memory: deletes stored item"
  (define b (make-memory-hash-backend))
  (with-backend b
    (lambda ()
      ;; Store then delete
      (call-tool tool-store-memory (hash 'content "temporary" 'scope "session"))
      (define items (memory-hash-backend-items b))
      (check-equal? (length items) 1)
      (define id (memory-item-id (car items)))
      ;; Delete
      (define r (call-tool tool-delete-memory (hash 'id id 'scope "session")))
      (check-false (tool-result-is-error? r))
      ;; Verify empty
      (check-equal? (length (memory-hash-backend-items b)) 0))))

;; ---------------------------------------------------------------------------
;; Policy gating
;; ---------------------------------------------------------------------------

(test-case "policy: default blocks secret sensitivity"
  (define policy default-memory-policy)
  (define item
    (memory-item "id" 'semantic 'session "secret"
                 (hasheq 'tags '() 'source 'tool)
                 (hasheq 'sensitivity 'secret 'confidence 1.0)
                 "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
  (check-false (policy-allows-store? policy item)))

(test-case "policy: allows public sensitivity"
  (define policy default-memory-policy)
  (define item
    (memory-item "id" 'semantic 'session "public info"
                 (hasheq 'tags '() 'source 'tool)
                 (hasheq 'sensitivity 'public 'confidence 1.0)
                 "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
  (check-true (policy-allows-store? policy item)))

(test-case "policy: blocks overly long content"
  (define policy default-memory-policy)
  (define long-content (make-string 10001 #\x))
  (define item
    (memory-item "id" 'semantic 'session long-content
                 (hasheq 'tags '() 'source 'tool)
                 (hasheq 'sensitivity 'public 'confidence 1.0)
                 "2026-01-01T00:00:00Z" "2026-01-01T00:00:00Z"))
  (check-false (policy-allows-store? policy item)))

(test-case "policy: respects delete flag"
  (check-true (policy-allows-delete? default-memory-policy))
  (define no-delete (make-memory-policy #:allow-delete? #f))
  (check-false (policy-allows-delete? no-delete)))

(test-case "policy: within budget"
  (check-true (policy-allows-retrieve? default-memory-policy 10))
  (check-true (policy-allows-retrieve? default-memory-policy 20))
  (check-false (policy-allows-retrieve? default-memory-policy 21)))
