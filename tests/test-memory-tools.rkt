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

(define default-test-exec-context
  (make-exec-context #:working-directory "/tmp/q-memory-default-test"
                     #:session-metadata (hasheq 'session-id "default-session")))

(define (call-tool tool args)
  ;; Tools created by define-tool are handler functions.
  ;; Use a default exec context so SPEC default scope can resolve to project.
  (tool args default-test-exec-context))

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
                  (define r
                    (call-tool tool-store-memory (hash 'content "test" 'sensitivity "top-secret")))
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
                  (check-equal? (memory-item-scope (car items)) 'project))))

(test-case "store-memory: stores with explicit type/scope/tags"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r
                    (call-tool tool-store-memory
                               (hash 'content
                                     "Important fact"
                                     'type
                                     "episodic"
                                     'scope
                                     "project"
                                     'tags
                                     '("important" "architecture")
                                     'sensitivity
                                     "internal")))
                  (check-false (tool-result-is-error? r))
                  (define items (memory-hash-backend-items b))
                  (check-equal? (memory-item-type (car items)) 'episodic)
                  (check-equal? (memory-item-scope (car items)) 'project)
                  (check-equal? (hash-ref (memory-item-metadata (car items)) 'tags)
                                '("important" "architecture")))))

(test-case "store-memory: secret sensitivity blocked by policy"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     ;; Default policy allows public, internal, sensitive but NOT secret
     (define r (call-tool tool-store-memory (hash 'content "secret stuff" 'sensitivity "secret")))
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
                  (define r (call-tool tool-search-memory (hash 'scope "project" 'limit 10)))
                  (check-false (tool-result-is-error? r)))))

(test-case "search-memory: empty results when no match"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (call-tool tool-store-memory (hash 'content "stored item" 'scope "session"))
                  (define r (call-tool tool-search-memory (hash 'scope "project" 'limit 10)))
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
    (memory-item "id"
                 'semantic
                 'session
                 "secret"
                 (hasheq 'tags '() 'source 'tool 'origin-message-id "test")
                 (hasheq 'sensitivity 'secret 'confidence 1.0 'supersedes '())
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (policy-allows-store? policy item)))

(test-case "policy: allows public sensitivity"
  (define policy default-memory-policy)
  (define item
    (memory-item
     "id"
     'semantic
     'session
     "public info"
     (hasheq 'tags '() 'source 'tool 'project-root "/tmp" 'session-id "s1" 'origin-message-id "test")
     (hasheq 'sensitivity 'public 'confidence 1.0 'supersedes '())
     "2026-01-01T00:00:00Z"
     "2026-01-01T00:00:00Z"))
  (check-true (policy-allows-store? policy item)))

(test-case "policy: blocks overly long content"
  (define policy default-memory-policy)
  (define long-content (make-string 10001 #\x))
  (define item
    (memory-item "id"
                 'semantic
                 'session
                 long-content
                 (hasheq 'tags '() 'source 'tool 'origin-message-id "test")
                 (hasheq 'sensitivity 'public 'confidence 1.0 'supersedes '())
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (policy-allows-store? policy item)))

(test-case "policy: respects delete flag"
  (check-true (policy-allows-delete? default-memory-policy))
  (define no-delete (make-memory-policy #:allow-delete? #f))
  (check-false (policy-allows-delete? no-delete)))

(test-case "policy: within budget"
  (check-true (policy-allows-retrieve? default-memory-policy 10))
  (check-true (policy-allows-retrieve? default-memory-policy 20))
  (check-false (policy-allows-retrieve? default-memory-policy 21)))

(test-case "policy: default blocks secret-looking public content"
  (define policy default-memory-policy)
  (define item
    (memory-item "id"
                 'semantic
                 'session
                 "API_KEY=sk-test-secret"
                 (hasheq 'tags '() 'source 'tool 'origin-message-id "test")
                 (hasheq 'sensitivity 'public 'confidence 1.0 'supersedes '())
                 "2026-01-01T00:00:00Z"
                 "2026-01-01T00:00:00Z"))
  (check-false (policy-allows-store? policy item)))

(test-case "store-memory: records project-root and session-id metadata from exec context"
  (define b (make-memory-hash-backend))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-project-a"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (with-backend
   b
   (lambda ()
     (define r (tool-store-memory (hash 'content "project scoped fact" 'scope "project") ctx))
     (check-false (tool-result-is-error? r))
     (define item (car (memory-hash-backend-items b)))
     (check-equal? (hash-ref (memory-item-metadata item) 'project-root) "/tmp/q-memory-project-a")
     (check-equal? (hash-ref (memory-item-metadata item) 'session-id) "sess-a"))))

(test-case "search-memory: filters project-scoped memories by exec-context project root"
  (define b (make-memory-hash-backend))
  (define ctx-a
    (make-exec-context #:working-directory "/tmp/q-memory-project-a"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (define ctx-b
    (make-exec-context #:working-directory "/tmp/q-memory-project-b"
                       #:session-metadata (hasheq 'session-id "sess-b")))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "fact for project a" 'scope "project") ctx-a)
                  (tool-store-memory (hash 'content "fact for project b" 'scope "project") ctx-b)
                  (define r (tool-search-memory (hash 'scope "project" 'limit 10) ctx-a))
                  (check-false (tool-result-is-error? r))
                  (define items (hash-ref (tool-result-details r) 'items))
                  (check-equal? (length items) 1)
                  (check-equal? (hash-ref (car items) 'content) "fact for project a"))))

(test-case "delete-memory: refuses to delete matching scope from a different project"
  (define b (make-memory-hash-backend))
  (define ctx-a
    (make-exec-context #:working-directory "/tmp/q-memory-project-a"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (define ctx-b
    (make-exec-context #:working-directory "/tmp/q-memory-project-b"
                       #:session-metadata (hasheq 'session-id "sess-b")))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "fact for project a" 'scope "project") ctx-a)
                  (define item (car (memory-hash-backend-items b)))
                  (define r
                    (tool-delete-memory (hash 'id (memory-item-id item) 'scope "project") ctx-b))
                  (check-true (tool-result-is-error? r))
                  (check-equal? (length (memory-hash-backend-items b)) 1))))

(test-case "store-memory: default scope is project when project root is available"
  (define b (make-memory-hash-backend))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-default-project"
                       #:session-metadata (hasheq 'session-id "sess-default")))
  (with-backend b
                (lambda ()
                  (define r (tool-store-memory (hash 'content "default scoped fact") ctx))
                  (check-false (tool-result-is-error? r))
                  (define item (car (memory-hash-backend-items b)))
                  (check-equal? (memory-item-scope item) 'project)
                  (check-equal? (hash-ref (memory-item-metadata item) 'project-root)
                                "/tmp/q-memory-default-project")
                  (check-equal? (hash-ref (memory-item-metadata item) 'session-id) "sess-default"))))

(test-case "store-memory: default scope falls back to session without project root"
  (define b (make-memory-hash-backend))
  (define ctx
    (make-exec-context #:working-directory #f #:session-metadata (hasheq 'session-id "sess-only")))
  (with-backend b
                (lambda ()
                  (define r (tool-store-memory (hash 'content "session default fact") ctx))
                  (check-false (tool-result-is-error? r))
                  (define item (car (memory-hash-backend-items b)))
                  (check-equal? (memory-item-scope item) 'session)
                  (check-equal? (hash-ref (memory-item-metadata item) 'session-id) "sess-only"))))

(test-case "search-memory: omitted scope does not leak same-project other-session session items"
  (define b (make-memory-hash-backend))
  (define ctx-a
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (define ctx-b
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-b")))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "sess-a private" 'scope "session") ctx-a)
                  (tool-store-memory (hash 'content "shared project" 'scope "project") ctx-a)
                  (define r (tool-search-memory (hash 'limit 10) ctx-b))
                  (check-false (tool-result-is-error? r))
                  (define contents
                    (map (lambda (h) (hash-ref h 'content))
                         (hash-ref (tool-result-details r) 'items)))
                  (check-false (member "sess-a private" contents))
                  (check-not-false (member "shared project" contents)))))

(test-case "list-memory: omitted scope does not leak same-project other-session session items"
  (define b (make-memory-hash-backend))
  (define ctx-a
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (define ctx-b
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-b")))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "sess-a list private" 'scope "session") ctx-a)
                  (tool-store-memory (hash 'content "project list shared" 'scope "project") ctx-a)
                  (define r (tool-list-memory (hash 'limit 10) ctx-b))
                  (check-false (tool-result-is-error? r))
                  (define snippets
                    (map (lambda (h) (hash-ref h 'snippet))
                         (hash-ref (tool-result-details r) 'items)))
                  (check-false (member "sess-a list private" snippets))
                  (check-not-false (member "project list shared" snippets)))))

(test-case "delete-memory: omitted scope cannot delete same-project other-session session item"
  (define b (make-memory-hash-backend))
  (define ctx-a
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-a")))
  (define ctx-b
    (make-exec-context #:working-directory "/tmp/q-memory-same-project"
                       #:session-metadata (hasheq 'session-id "sess-b")))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "sess-a delete private" 'scope "session") ctx-a)
                  (define item (car (memory-hash-backend-items b)))
                  (define r (tool-delete-memory (hash 'id (memory-item-id item)) ctx-b))
                  (check-true (tool-result-is-error? r))
                  (check-equal? (length (memory-hash-backend-items b)) 1))))

(test-case "memory tools: user scope is rejected without explicit opt-in"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (check-true (tool-result-is-error?
                               (tool-store-memory (hash 'content "global" 'scope "user") #f)))
                  (check-true (tool-result-is-error? (tool-search-memory (hash 'scope "user") #f)))
                  (check-true (tool-result-is-error? (tool-list-memory (hash 'scope "user") #f))))))

;; ---------------------------------------------------------------------------
;; F26: Safe-mode integration test
;; ---------------------------------------------------------------------------

(test-case "store-memory blocked by restrictive policy (F26)"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-safe"
                       #:session-metadata (hasheq 'session-id "sess-safe")
                       #:event-publisher (lambda (evt) (set-box! events (cons evt (unbox events))))))
  (with-backend b
                (lambda ()
                  ;; Use a restrictive policy with empty allowed sensitivities to block all stores
                  (parameterize ([current-memory-policy (make-memory-policy #:allowed-sensitivities
                                                                            '())])
                    (define r (tool-store-memory (hash 'content "safe mode test") ctx))
                    (check-true (tool-result-is-error? r))
                    ;; Verify event stream contains policy.blocked
                    (define evts (reverse (unbox events)))
                    (define blocked-types
                      (filter (lambda (t) (equal? t "memory.policy.blocked"))
                              (map (lambda (evt) (hash-ref evt 'type #f)) evts)))
                    (check-equal? (length blocked-types) 1)))))
