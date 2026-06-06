#lang racket/base
;; tests/test-memory-file-jsonl-backend.rkt — Project-local JSONL backend tests
;;
;; v0.95.8: Tests for file-jsonl memory backend:
;; - Store/reload round-trip
;; - Delete persists tombstone
;; - Corrupt JSONL line skipped (not fatal)
;; - Ordering stable after reload
;; - Path normalization prevents escaping project memory root

(require rackunit
         racket/file
         racket/list
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/file-jsonl.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-temp-dir)
  (define dir (make-temporary-file "memory-jsonl-test-~a" 'directory))
  dir)

(define (with-temp-backend proc)
  (define dir (make-temp-dir))
  (define backend (make-file-jsonl-backend dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (delete-directory/files dir #:must-exist? #f)
                               (raise e))])
    (proc backend dir)
    (delete-directory/files dir #:must-exist? #f)))

(define (make-test-item #:id [id "test-id"]
                        #:content [content "test content"]
                        #:scope [scope 'session]
                        #:type [type 'semantic]
                        #:tags [tags '("test")])
  (memory-item
   id
   type
   scope
   content
   (hasheq 'project-root "/tmp" 'session-id "s1" 'tags tags 'source "test" 'origin-message-id "test")
   (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
   "2026-06-01T00:00:00Z"
   "2026-06-01T00:00:00Z"))

;; ---------------------------------------------------------------------------
;; Basic store/retrieve
;; ---------------------------------------------------------------------------

(test-case "store and retrieve round-trip"
  (with-temp-backend (lambda (backend dir)
                       (define item (make-test-item #:id "id1" #:content "hello world"))
                       (define store-result (gen:store-memory! backend item))
                       (check-true (memory-result-ok? store-result))
                       (check-equal? (memory-result-value store-result) "id1")
                       ;; Retrieve
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define result (gen:retrieve-memory backend query))
                       (check-true (memory-result-ok? result))
                       (define items (memory-result-value result))
                       (check-equal? (length items) 1)
                       (check-equal? (memory-item-content (car items)) "hello world"))))

(test-case "store persists to file"
  (with-temp-backend (lambda (backend dir)
                       (define item (make-test-item #:id "persist-id" #:content "durable"))
                       (gen:store-memory! backend item)
                       ;; Create a new backend reading the same file
                       (define backend2 (make-file-jsonl-backend dir))
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define result (gen:retrieve-memory backend2 query))
                       (check-true (memory-result-ok? result))
                       (check-equal? (length (memory-result-value result)) 1)
                       (check-equal? (memory-item-content (car (memory-result-value result)))
                                     "durable"))))

(test-case "store multiple items and retrieve all"
  (with-temp-backend (lambda (backend dir)
                       (for ([i (in-range 5)])
                         (gen:store-memory! backend
                                            (make-test-item #:id (format "id~a" i)
                                                            #:content (format "content ~a" i))))
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define result (gen:retrieve-memory backend query))
                       (check-equal? (length (memory-result-value result)) 5))))

;; ---------------------------------------------------------------------------
;; Delete
;; ---------------------------------------------------------------------------

(test-case "delete persists tombstone"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "del-me" #:content "to delete"))
     (define del-result (gen:delete-memory! backend "del-me" #f))
     (check-true (memory-result-ok? del-result))
     ;; Verify gone from same backend
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (check-equal? (length (memory-result-value (gen:retrieve-memory backend query))) 0)
     ;; Verify gone after reload
     (define backend2 (make-file-jsonl-backend dir))
     (check-equal? (length (memory-result-value (gen:retrieve-memory backend2 query))) 0))))

(test-case "delete with scope check"
  (with-temp-backend (lambda (backend dir)
                       (gen:store-memory! backend (make-test-item #:id "scoped" #:scope 'project))
                       ;; Wrong scope should fail
                       (define del-result (gen:delete-memory! backend "scoped" 'session))
                       (check-false (memory-result-ok? del-result))
                       ;; Correct scope should succeed
                       (define del-result2 (gen:delete-memory! backend "scoped" 'project))
                       (check-true (memory-result-ok? del-result2)))))

;; ---------------------------------------------------------------------------
;; Update
;; ---------------------------------------------------------------------------

(test-case "update changes content"
  (with-temp-backend (lambda (backend dir)
                       (gen:store-memory! backend (make-test-item #:id "upd" #:content "original"))
                       (define upd-result
                         (gen:update-memory! backend "upd" (hash 'content "modified")))
                       (check-true (memory-result-ok? upd-result))
                       ;; Verify content changed
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define items (memory-result-value (gen:retrieve-memory backend query)))
                       (check-equal? (length items) 1)
                       (check-equal? (memory-item-content (car items)) "modified"))))

(test-case "update persists after reload"
  (with-temp-backend (lambda (backend dir)
                       (gen:store-memory! backend (make-test-item #:id "upd2" #:content "v1"))
                       (gen:update-memory! backend "upd2" (hash 'content "v2"))
                       ;; Reload
                       (define backend2 (make-file-jsonl-backend dir))
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define items (memory-result-value (gen:retrieve-memory backend2 query)))
                       (check-equal? (memory-item-content (car items)) "v2"))))

;; ---------------------------------------------------------------------------
;; Corrupt lines
;; ---------------------------------------------------------------------------

(test-case "corrupt JSONL line is skipped"
  (with-temp-backend
   (lambda (backend dir)
     ;; Store a valid item
     (gen:store-memory! backend (make-test-item #:id "good" #:content "valid"))
     ;; Append a corrupt line directly to the file
     (define jsonl-path (build-path dir "memory.jsonl"))
     (call-with-output-file jsonl-path
                            (lambda (port) (displayln "THIS IS NOT VALID JSON {{{" port))
                            #:mode 'text
                            #:exists 'append)
     ;; Should still load the valid item
     (define backend2 (make-file-jsonl-backend dir))
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define result (gen:retrieve-memory backend2 query))
     (check-true (memory-result-ok? result))
     (check-equal? (length (memory-result-value result)) 1)
     (check-equal? (memory-item-content (car (memory-result-value result))) "valid"))))

;; ---------------------------------------------------------------------------
;; Ordering
;; ---------------------------------------------------------------------------

(test-case "ordering is stable after reload"
  (with-temp-backend
   (lambda (backend dir)
     ;; Store items with different timestamps
     (for ([i (in-range 5)])
       (gen:store-memory! backend
                          (memory-item (format "ord~a" i)
                                       'semantic
                                       'session
                                       (format "content ~a" i)
                                       (hasheq 'project-root
                                               "/tmp"
                                               'session-id
                                               "s1"
                                               'tags
                                               '()
                                               'source
                                               "test"
                                               'origin-message-id
                                               "test")
                                       (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                                       "2026-06-01T00:00:00Z"
                                       (format "2026-06-0~aT00:00:00Z" (+ i 1)))))
     ;; Reload
     (define backend2 (make-file-jsonl-backend dir))
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define items (memory-result-value (gen:retrieve-memory backend2 query)))
     ;; Should be sorted by updated-at descending
     (check-equal? (length items) 5)
     (define timestamps (map memory-item-updated-at items))
     (check-equal? timestamps (sort timestamps string>?)))))

;; ---------------------------------------------------------------------------
;; Duplicate handling
;; ---------------------------------------------------------------------------

(test-case "duplicate store fails with different content"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "dup" #:content "original"))
     (define result (gen:store-memory! backend (make-test-item #:id "dup" #:content "different")))
     (check-false (memory-result-ok? result)))))

;; ---------------------------------------------------------------------------
;; Available
;; ---------------------------------------------------------------------------

(test-case "available? returns #t"
  (with-temp-backend (lambda (backend dir) (check-true (gen:memory-available? backend)))))

;; ---------------------------------------------------------------------------
;; Manage (no-op)
;; ---------------------------------------------------------------------------

(test-case "manage! returns success"
  (with-temp-backend (lambda (backend dir)
                       (define result (gen:manage-memory! backend (hash)))
                       (check-true (memory-result-ok? result)))))

;; ---------------------------------------------------------------------------
;; Expiry after reload (F16)
;; ---------------------------------------------------------------------------

(test-case "expired items excluded after JSONL reload"
  (with-temp-backend
   (lambda (backend dir)
     (define expired-item
       (memory-item
        "exp1"
        'semantic
        'session
        "expired content"
        (hasheq 'project-root #f 'session-id "s1" 'tags '() 'origin-message-id "m1" 'source 'tool)
        (hasheq 'sensitivity
                'public
                'confidence
                1.0
                'expires-at
                "2020-01-01T00:00:00Z"
                'supersedes
                '())
        "2020-01-01T00:00:00Z"
        "2020-01-01T00:00:00Z"))
     (gen:store-memory! backend expired-item)
     ;; Reload from file
     (define backend2 (make-file-jsonl-backend dir))
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define result (gen:retrieve-memory backend2 query))
     (check-true (memory-result-ok? result))
     (check-equal? (length (memory-result-value result)) 0))))

;; ---------------------------------------------------------------------------
;; Session-id isolation (F18)
;; ---------------------------------------------------------------------------

(test-case "retrieve filters by session-id after reload"
  (with-temp-backend (lambda (backend dir)
                       (define item-s1
                         (memory-item "s1-id"
                                      'semantic
                                      'session
                                      "session 1 content"
                                      (hasheq 'project-root
                                              "/tmp"
                                              'session-id
                                              "sess-1"
                                              'tags
                                              '()
                                              'origin-message-id
                                              "m1"
                                              'source
                                              'tool)
                                      (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                                      "2026-06-01T00:00:00Z"
                                      "2026-06-01T00:00:00Z"))
                       (define item-s2
                         (memory-item "s2-id"
                                      'semantic
                                      'session
                                      "session 2 content"
                                      (hasheq 'project-root
                                              "/tmp"
                                              'session-id
                                              "sess-2"
                                              'tags
                                              '()
                                              'origin-message-id
                                              "m2"
                                              'source
                                              'tool)
                                      (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                                      "2026-06-01T00:00:00Z"
                                      "2026-06-01T00:00:00Z"))
                       (gen:store-memory! backend item-s1)
                       (gen:store-memory! backend item-s2)
                       ;; Reload and query with session-id filter
                       (define backend2 (make-file-jsonl-backend dir))
                       (define query (memory-query #f 'session "/tmp" "sess-1" #f #f 100 #f))
                       (define result (gen:retrieve-memory backend2 query))
                       (check-true (memory-result-ok? result))
                       (check-equal? (length (memory-result-value result)) 1)
                       (check-equal? (memory-item-id (car (memory-result-value result))) "s1-id"))))

;; ---------------------------------------------------------------------------
;; Round-trip valid-memory-item? (F2)
;; ---------------------------------------------------------------------------

(test-case "round-tripped item passes valid-memory-item?"
  (with-temp-backend (lambda (backend dir)
                       (define item
                         (memory-item "rt-id"
                                      'semantic
                                      'session
                                      "round trip"
                                      (hasheq 'project-root
                                              "/tmp"
                                              'session-id
                                              "s1"
                                              'tags
                                              '("test")
                                              'origin-message-id
                                              "m1"
                                              'source
                                              'tool)
                                      (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
                                      "2026-06-01T00:00:00Z"
                                      "2026-06-01T00:00:00Z"))
                       (gen:store-memory! backend item)
                       (define backend2 (make-file-jsonl-backend dir))
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define result (gen:retrieve-memory backend2 query))
                       (check-true (memory-result-ok? result))
                       (check-equal? (length (memory-result-value result)) 1)
                       (define loaded (car (memory-result-value result)))
                       (check-true (valid-memory-item? loaded))
                       ;; Verify sensitivity is symbol, not string (F9)
                       (define sens (hash-ref (memory-item-validity loaded) 'sensitivity))
                       (check-true (symbol? sens))
                       (check-equal? sens 'public))))

;; ---------------------------------------------------------------------------
;; Scope filtering
;; ---------------------------------------------------------------------------

(test-case "retrieve filters by scope"
  (with-temp-backend (lambda (backend dir)
                       (gen:store-memory! backend (make-test-item #:id "s1" #:scope 'session))
                       (gen:store-memory! backend (make-test-item #:id "p1" #:scope 'project))
                       ;; Query session only
                       (define query (memory-query #f 'session #f #f #f #f 100 #f))
                       (define result (gen:retrieve-memory backend query))
                       (check-equal? (length (memory-result-value result)) 1)
                       (check-equal? (memory-item-id (car (memory-result-value result))) "s1"))))

;; ---------------------------------------------------------------------------
;; Tag filtering (P1-1)
;; ---------------------------------------------------------------------------

(define (make-tagged-item #:id [id "tagged"] #:tags [tags '()] #:content [content "tagged content"])
  (memory-item
   id
   'semantic
   'session
   content
   (hasheq 'tags tags 'project-root "/tmp" 'session-id "s1" 'source "test" 'origin-message-id "test")
   (hasheq 'sensitivity 'public 'confidence 0.9 'supersedes '())
   "2026-06-01T00:00:00Z"
   "2026-06-01T00:00:00Z"))

(test-case "retrieve filters by single tag"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-tagged-item #:id "rust-item" #:tags '("rust")))
     (gen:store-memory! backend (make-tagged-item #:id "python-item" #:tags '("python")))
     (gen:store-memory! backend (make-tagged-item #:id "no-tags" #:tags '()))
     ;; Query for rust items
     (define query (memory-query #f #f #f #f #f '("rust") 100 #f))
     (define result (gen:retrieve-memory backend query))
     (check-true (memory-result-ok? result))
     (define items (memory-result-value result))
     (check-equal? (length items) 1)
     (check-equal? (memory-item-id (car items)) "rust-item"))))

(test-case "retrieve filters by multiple tags (AND semantics)"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-tagged-item #:id "rust-item" #:tags '("rust")))
     (gen:store-memory! backend (make-tagged-item #:id "python-item" #:tags '("python")))
     (gen:store-memory! backend (make-tagged-item #:id "both-item" #:tags '("rust" "python")))
     ;; Query for items that have BOTH rust AND python tags
     (define query (memory-query #f #f #f #f #f '("rust" "python") 100 #f))
     (define result (gen:retrieve-memory backend query))
     (check-true (memory-result-ok? result))
     (define items (memory-result-value result))
     ;; Should return only the item that has BOTH tags
     (check-equal? (length items) 1)
     (check-equal? (memory-item-id (car items)) "both-item"))))

(test-case "list filters by tags"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-tagged-item #:id "tagged-a" #:tags '("alpha")))
     (gen:store-memory! backend (make-tagged-item #:id "tagged-b" #:tags '("beta")))
     (define query (memory-query #f #f #f #f #f '("alpha") 100 #f))
     (define result (gen:list-memory backend query))
     (check-true (memory-result-ok? result))
     (check-equal? (length (memory-result-value result)) 1)
     (check-equal? (memory-item-id (car (memory-result-value result))) "tagged-a"))))

(test-case "retrieve with empty/null tags returns all items"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-tagged-item #:id "a" #:tags '("x") #:content "content a"))
     (gen:store-memory! backend (make-tagged-item #:id "b" #:tags '() #:content "content b"))
     ;; tags=#f means no tag filter
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define result (gen:retrieve-memory backend query))
     (check-equal? (length (memory-result-value result)) 2))))

;; ---------------------------------------------------------------------------
;; Path traversal safety (P2-1)
;; ---------------------------------------------------------------------------

(test-case "safe-memory-path rejects directory traversal"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (delete-directory/files dir #:must-exist? #f)
                               (raise e))])
    ;; Direct traversal attempt should raise
    (check-exn exn:fail? (lambda () (safe-memory-path dir "../../etc/passwd")))
    (delete-directory/files dir #:must-exist? #f)))

(test-case "safe-memory-path accepts normal relative path"
  (define dir (make-temp-dir))
  (with-handlers ([exn:fail? (lambda (e)
                               (delete-directory/files dir #:must-exist? #f)
                               (raise e))])
    (define result (safe-memory-path dir "memory.jsonl"))
    (check-true (path? result))
    (delete-directory/files dir #:must-exist? #f)))

;; ---------------------------------------------------------------------------
;; Update patch immutability guard (P3-3)
;; ---------------------------------------------------------------------------

(test-case "update: id in patch is ignored"
  (with-temp-backend (lambda (backend dir)
                       (gen:store-memory! backend (make-test-item #:id "orig-id"))
                       (define r
                         (gen:update-memory! backend "orig-id" (hash 'id "hacked-id" 'content "new")))
                       (check-true (memory-result-ok? r))
                       ;; Reload to verify persisted correctly
                       (define backend2 (make-file-jsonl-backend dir))
                       (define query (memory-query #f #f #f #f #f #f 100 #f))
                       (define items (memory-result-value (gen:retrieve-memory backend2 query)))
                       (check-equal? (length items) 1)
                       (check-equal? (memory-item-id (car items)) "orig-id")
                       (check-equal? (memory-item-content (car items)) "new"))))

(test-case "update: created-at in patch is ignored"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "ts-item"))
     (define r (gen:update-memory! backend "ts-item" (hash 'created-at "1999-01-01T00:00:00Z")))
     (check-true (memory-result-ok? r))
     ;; Reload and verify created-at unchanged
     (define backend2 (make-file-jsonl-backend dir))
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define items (memory-result-value (gen:retrieve-memory backend2 query)))
     (check-equal? (memory-item-created-at (car items)) "2026-06-01T00:00:00Z"))))
