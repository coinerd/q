#lang racket/base

;; @speed fast
;; @suite default
;; tests/test-audit-v09945-w1-memory.rkt — Memory subsystem real-world audit
;;
;; Audit of the memory subsystem covering:
;;   1. Store/retrieve lifecycle with real-world content shapes
;;   2. file-jsonl persistence across "restarts" (new backend from same dir)
;;   3. Scope isolation (session vs project vs user)
;;   4. Sensitivity gating (credential-like content blocked)
;;   5. Backend switching (hash → file-jsonl)
;;   6. Management operations (list, delete)
;;   7. Content deduplication and supersession
;;   8. Policy enforcement (content length, budget limits)
;;   9. Chained backend (L1 hash + L2 file-jsonl)
;;
;; All tests use mock/synthetic data — no real API keys needed.

(require rackunit
         racket/file
         racket/list
         racket/string
         json
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/backends/file-jsonl.rkt"
         "../runtime/memory/backends/chained.rkt")

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(define (make-item #:id [id "mem-1"]
                   #:type [type 'semantic]
                   #:scope [scope 'project]
                   #:content [content "The project uses 2-space indentation"]
                   #:project [project "project/myapp"]
                   #:session [session "sess-001"]
                   #:tags [tags '("convention" "style")]
                   #:sensitivity [sensitivity 'public]
                   #:confidence [confidence 0.9]
                   #:expires [expires #f]
                   #:supersedes [supersedes '()]
                   #:origin [origin "user-message"]
                   #:created [created "2026-07-04T10:00:00Z"]
                   #:updated [updated "2026-07-04T10:00:00Z"])
  (memory-item
   id
   type
   scope
   content
   (hasheq 'project-root
           project
           'session-id
           session
           'tags
           tags
           'source
           origin
           'origin-message-id
           "msg-001")
   (hasheq 'sensitivity sensitivity 'confidence confidence 'expires-at expires 'supersedes supersedes)
   created
   updated))

(define (make-query #:text [text "indent"]
                    #:scope [scope #f]
                    #:project [project #f]
                    #:session [session #f]
                    #:types [types #f]
                    #:tags [tags #f]
                    #:limit [limit 10]
                    #:include-expired? [include-expired? #f])
  (memory-query text scope project session types tags limit include-expired?))

(define (result-length r)
  (define v (memory-result-value r))
  (if (list? v)
      (length v)
      0))

;; ---------------------------------------------------------------------------
;; 1. Store/Retrieve Lifecycle
;; ---------------------------------------------------------------------------

(test-case "audit-store-retrieve-lifecycle"
  (define b (make-memory-hash-backend))
  (define arch-item
    (make-item #:id "arch-001"
               #:type 'semantic
               #:content "The system uses a hexagonal architecture with ports and adapters"))
  (define r-store (gen:store-memory! b arch-item))
  (check-true (memory-result-ok? r-store))
  (check-equal? (memory-result-value r-store) "arch-001")
  (define r-retrieval (gen:retrieve-memory b (make-query #:text "architecture")))
  (check-true (memory-result-ok? r-retrieval))
  (check-true (>= (result-length r-retrieval) 1)))

(test-case "audit-store-episodic-memory"
  (define b (make-memory-hash-backend))
  (define ep-item
    (make-item #:id "ep-001"
               #:type 'episodic
               #:content "Refactored the auth module to use credential backends"))
  (define r (gen:store-memory! b ep-item))
  (check-true (memory-result-ok? r))
  (define r2 (gen:retrieve-memory b (make-query #:text "auth")))
  (check-true (memory-result-ok? r2))
  (check-true (>= (result-length r2) 1)))

(test-case "audit-store-procedural-memory"
  (define b (make-memory-hash-backend))
  (define proc-item
    (make-item #:id "proc-001"
               #:type 'procedural
               #:content "To deploy: run raco make main.rkt then racket main.rkt"))
  (define r (gen:store-memory! b proc-item))
  (check-true (memory-result-ok? r))
  (define r2 (gen:retrieve-memory b (make-query #:text "deploy")))
  (check-true (memory-result-ok? r2))
  (check-true (>= (result-length r2) 1)))

;; ---------------------------------------------------------------------------
;; 2. file-jsonl Persistence Across "Restarts"
;; ---------------------------------------------------------------------------

(test-case "audit-file-jsonl-persistence"
  (define tmp-dir (make-temporary-file "audit-mem-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define b1 (make-file-jsonl-backend tmp-dir))
                  (define item
                    (make-item #:id "persist-001"
                               #:content "Database uses PostgreSQL 16 with pgvector"
                               #:project (path->string tmp-dir)))
                  (define r1 (gen:store-memory! b1 item))
                  (check-true (memory-result-ok? r1))
                  (define jsonl-path (build-path tmp-dir "memory.jsonl"))
                  (check-true (file-exists? jsonl-path))
                  ;; Session 2: new backend from same dir — "restart"
                  (define b2 (make-file-jsonl-backend tmp-dir))
                  (define r2 (gen:retrieve-memory b2 (make-query #:text "database")))
                  (check-true (memory-result-ok? r2))
                  (check-true (>= (result-length r2) 1)))
                (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

(test-case "audit-file-jsonl-multiple-items"
  (define tmp-dir (make-temporary-file "audit-mem-multi-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define b (make-file-jsonl-backend tmp-dir))
                  (for ([i (in-range 3)])
                    (gen:store-memory! b
                                       (make-item #:id (format "multi-~a" i)
                                                  #:content (format "Fact number ~a about the system"
                                                                    i)
                                                  #:project (path->string tmp-dir))))
                  (define b2 (make-file-jsonl-backend tmp-dir))
                  (define r (gen:retrieve-memory b2 (make-query #:text "Fact" #:limit 10)))
                  (check-true (memory-result-ok? r))
                  (check-equal? (result-length r) 3))
                (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

;; ---------------------------------------------------------------------------
;; 3. Scope Isolation
;; ---------------------------------------------------------------------------

(test-case "audit-scope-isolation-session-vs-project"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b
                     (make-item #:id "sess-only"
                                #:scope 'session
                                #:session "sess-A"
                                #:content "Temporary note for session A only"))
  (gen:store-memory! b
                     (make-item #:id "proj-shared"
                                #:scope 'project
                                #:content "Shared project convention: use tabs not spaces"
                                #:project "proj/main"))
  ;; Query with session-B should only see project-scoped items
  (define r-sess-B
    (gen:retrieve-memory
     b
     (make-query #:text "" #:scope 'session #:session "sess-B" #:project "proj/main")))
  (check-true (memory-result-ok? r-sess-B))
  (define sess-B-items (memory-result-value r-sess-B))
  (for ([item (in-list sess-B-items)])
    (check-false (string-contains? (memory-item-content item) "session A"))))

;; ---------------------------------------------------------------------------
;; 4. Sensitivity Gating — Credential Content Blocked
;; ---------------------------------------------------------------------------

(test-case "audit-sensitivity-gate-blocks-api-keys"
  (define policy default-memory-policy)
  (define secret-item
    (make-item #:id "secret-001" #:content "The API key is sk-ant-api03-1234567890abcdef"))
  (check-false (policy-allows-store? policy secret-item)))

(test-case "audit-sensitivity-gate-blocks-passwords"
  (define policy default-memory-policy)
  (define pw-item (make-item #:id "secret-002" #:content "The database password=mySuperSecret123"))
  (check-false (policy-allows-store? policy pw-item)))

(test-case "audit-sensitivity-gate-blocks-bearer-tokens"
  (define policy default-memory-policy)
  (define bearer-item
    (make-item #:id "secret-003"
               #:content "Authorization: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"))
  (check-false (policy-allows-store? policy bearer-item)))

(test-case "audit-sensitivity-gate-blocks-aws-keys"
  (define policy default-memory-policy)
  (define aws-item (make-item #:id "secret-004" #:content "AWS credentials: AKIAIOSFODNN7EXAMPLE"))
  (check-false (policy-allows-store? policy aws-item)))

(test-case "audit-sensitivity-gate-allows-normal-content"
  (define policy default-memory-policy)
  (define normal-item (make-item #:id "normal-001" #:content "The build system uses make and racket"))
  (check-true (policy-allows-store? policy normal-item)))

(test-case "audit-sensitivity-gate-blocks-secret-scope"
  (define policy default-memory-policy)
  (define secret-scope-item
    (make-item #:id "secret-005" #:content "Normal content but marked secret" #:sensitivity 'secret))
  (check-false (policy-allows-store? policy secret-scope-item)))

;; ---------------------------------------------------------------------------
;; 5. Backend Switching (hash → file-jsonl)
;; ---------------------------------------------------------------------------

(test-case "audit-backend-switching"
  (define tmp-dir (make-temporary-file "audit-switch-~a" 'directory))
  (dynamic-wind
   void
   (lambda ()
     (define hash-b (make-memory-hash-backend))
     (gen:store-memory! hash-b (make-item #:id "ephemeral-001" #:content "This is ephemeral"))
     (define file-b (make-file-jsonl-backend tmp-dir))
     (gen:store-memory!
      file-b
      (make-item #:id "durable-001" #:content "This is durable" #:project (path->string tmp-dir)))
     (define file-b2 (make-file-jsonl-backend tmp-dir))
     (define r-durable (gen:retrieve-memory file-b2 (make-query #:text "durable")))
     (check-true (>= (result-length r-durable) 1))
     (define r-ephemeral (gen:retrieve-memory file-b2 (make-query #:text "ephemeral")))
     (check-equal? (result-length r-ephemeral) 0))
   (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

;; ---------------------------------------------------------------------------
;; 6. Management Operations
;; ---------------------------------------------------------------------------

(test-case "audit-management-list"
  (define b (make-memory-hash-backend))
  (for ([i (in-range 5)])
    (gen:store-memory! b
                       (make-item #:id (format "mgmt-~a" i)
                                  #:content (format "Management test item ~a" i))))
  (define r (gen:list-memory b (make-query #:text "" #:limit 100)))
  (check-true (memory-result-ok? r))
  (check-equal? (result-length r) 5))

(test-case "audit-management-delete"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "del-001" #:content "Delete me"))
  (gen:store-memory! b (make-item #:id "keep-001" #:content "Keep me"))
  (define r-del (gen:delete-memory! b "del-001" #f))
  (check-true (memory-result-ok? r-del))
  (define r-list (gen:list-memory b (make-query #:text "" #:limit 100)))
  (check-equal? (result-length r-list) 1)
  (check-equal? (memory-item-id (car (memory-result-value r-list))) "keep-001"))

(test-case "audit-management-delete-nonexistent"
  (define b (make-memory-hash-backend))
  (define r (gen:delete-memory! b "nonexistent" #f))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'not-found))

;; ---------------------------------------------------------------------------
;; 7. Content Deduplication and Supersession
;; ---------------------------------------------------------------------------

(test-case "audit-content-deduplication"
  (define b (make-memory-hash-backend))
  (define r1 (gen:store-memory! b (make-item #:id "dup-001" #:content "The same identical fact")))
  (check-true (memory-result-ok? r1))
  (define r2 (gen:store-memory! b (make-item #:id "dup-002" #:content "The same identical fact")))
  (check-true (memory-result-ok? r2))
  (check-true (hash-has-key? (memory-result-metadata r2) 'idempotent)))

(test-case "audit-idempotent-re-store"
  (define b (make-memory-hash-backend))
  (define item (make-item #:id "idem-001" #:content "An idempotent item"))
  (define r1 (gen:store-memory! b item))
  (check-true (memory-result-ok? r1))
  (define r2 (gen:store-memory! b item))
  (check-true (memory-result-ok? r2))
  (check-true (hash-has-key? (memory-result-metadata r2) 'idempotent)))

;; ---------------------------------------------------------------------------
;; 8. Policy Enforcement
;; ---------------------------------------------------------------------------

(test-case "audit-policy-content-length-limit"
  (define policy (make-memory-policy #:max-content-length 50))
  (define long-item (make-item #:id "long-001" #:content (make-string 100 #\x)))
  (check-false (policy-allows-store? policy long-item)))

(test-case "audit-policy-retrieve-budget"
  (define policy (make-memory-policy #:max-retrieve-count 3))
  (check-true (policy-allows-retrieve? policy 0))
  (check-true (policy-allows-retrieve? policy 3))
  (check-false (policy-allows-retrieve? policy 4)))

(test-case "audit-policy-user-scope-disabled-by-default"
  (define policy default-memory-policy)
  (check-false (policy-user-scope-enabled? policy))
  (check-false (policy-allows-scope? policy 'user))
  (check-true (policy-allows-scope? policy 'session))
  (check-true (policy-allows-scope? policy 'project)))

;; ---------------------------------------------------------------------------
;; 9. Chained Backend (L1 + L2)
;; ---------------------------------------------------------------------------

(test-case "audit-chained-backend-store-and-retrieve"
  (define tmp-dir (make-temporary-file "audit-chained-~a" 'directory))
  (dynamic-wind void
                (lambda ()
                  (define l1 (make-memory-hash-backend))
                  (define l2 (make-file-jsonl-backend tmp-dir))
                  (define chained (make-chained-backend l1 l2))
                  (define item
                    (make-item #:id "chained-001"
                               #:content "Chained backend test item"
                               #:project (path->string tmp-dir)))
                  (define r (gen:store-memory! chained item))
                  (check-true (memory-result-ok? r))
                  (define r2 (gen:retrieve-memory chained (make-query #:text "chained")))
                  (check-true (memory-result-ok? r2))
                  (check-true (>= (result-length r2) 1))
                  ;; Verify write-through: L2 should have the item
                  (define l2-only (make-file-jsonl-backend tmp-dir))
                  (define r3 (gen:retrieve-memory l2-only (make-query #:text "chained")))
                  (check-true (>= (result-length r3) 1)))
                (lambda () (delete-directory/files tmp-dir #:must-exist? #f))))

;; ---------------------------------------------------------------------------
;; 10. Update Operations
;; ---------------------------------------------------------------------------

(test-case "audit-update-content"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "upd-001" #:content "Original content"))
  (define r (gen:update-memory! b "upd-001" (hasheq 'content "Updated content")))
  (check-true (memory-result-ok? r))
  (define r2 (gen:retrieve-memory b (make-query #:text "Updated")))
  (check-true (>= (result-length r2) 1)))

(test-case "audit-update-nonexistent"
  (define b (make-memory-hash-backend))
  (define r (gen:update-memory! b "nonexistent" (hasheq 'content "new")))
  (check-false (memory-result-ok? r))
  (check-equal? (hash-ref (memory-result-error r) 'code) 'not-found))

;; ---------------------------------------------------------------------------
;; 11. Redaction
;; ---------------------------------------------------------------------------

(test-case "audit-redaction-replaces-api-keys"
  (define content "The key is sk-ant-api03-1234567890abcdef in the config")
  (define redacted (redact-memory-content content))
  (check-false (string-contains? redacted "sk-ant-api03"))
  (check-true (string-contains? redacted "[REDACTED]")))

(test-case "audit-redacted-snippet-truncates"
  (define long-content (string-append (make-string 200 #\x) " sk-ant-api03-1234567890"))
  (define snippet (redacted-memory-snippet long-content 50))
  (check-true (<= (string-length snippet) 50)))

;; ---------------------------------------------------------------------------
;; 12. Retrieval Ranking and Limit
;; ---------------------------------------------------------------------------

(test-case "audit-retrieval-respects-limit"
  (define b (make-memory-hash-backend))
  (for ([i (in-range 10)])
    (gen:store-memory! b
                       (make-item #:id (format "lim-~a" i)
                                  #:content (format "Limit test item ~a about testing" i)
                                  #:created (format "2026-07-04T~a:00:00Z" (+ 10 i))
                                  #:updated (format "2026-07-04T~a:00:00Z" (+ 10 i)))))
  (define r (gen:retrieve-memory b (make-query #:text "testing" #:limit 3)))
  (check-true (memory-result-ok? r))
  (check-true (<= (result-length r) 3)))

(test-case "audit-retrieval-empty-query"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-item #:id "empty-001" #:content "Some content here"))
  (define r (gen:retrieve-memory b (make-query #:text "")))
  (check-true (memory-result-ok? r))
  (check-true (>= (result-length r) 1)))

;; ---------------------------------------------------------------------------
;; 13. Type Filtering
;; ---------------------------------------------------------------------------

(test-case "audit-type-filtering"
  (define b (make-memory-hash-backend))
  (gen:store-memory!
   b
   (make-item #:id "type-ep" #:type 'episodic #:content "An episodic event happened"))
  (gen:store-memory!
   b
   (make-item #:id "type-sem" #:type 'semantic #:content "A semantic fact about events"))
  (define r (gen:retrieve-memory b (make-query #:text "event" #:types '(episodic))))
  (check-true (memory-result-ok? r))
  (define items (memory-result-value r))
  (for ([item (in-list items)])
    (check-equal? (memory-item-type item) 'episodic)))
