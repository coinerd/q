#lang racket/base
;; tests/test-memory-migration.rkt — Migration utility tests
;;
;; M13-F8: Test export/import/migrate between backends

(require rackunit
         racket/file
         racket/list
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/backends/file-jsonl.rkt"
         "../runtime/memory/migration.rkt"
         "../runtime/memory/service.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (make-test-item id
                        scope
                        content
                        #:supersedes [supersedes '()]
                        #:updated [updated "2025-01-01T00:00:00Z"])
  (memory-item id
               'semantic
               scope
               content
               (hasheq 'tags '() 'source 'test 'project-root #f 'session-id #f 'origin-message-id #f)
               (hasheq 'sensitivity 'public 'confidence 0.9 'expires-at #f 'supersedes supersedes)
               "2025-01-01T00:00:00Z"
               updated))

;; ---------------------------------------------------------------------------
;; Export tests
;; ---------------------------------------------------------------------------

(test-case "export: returns all items from backend"
  (define b (make-memory-hash-backend))
  (parameterize ([current-memory-backend b])
    (gen:store-memory! b (make-test-item "m1" 'session "item 1"))
    (gen:store-memory! b (make-test-item "m2" 'project "item 2"))
    (gen:store-memory! b (make-test-item "m3" 'user "item 3"))
    (define result (export-memory-items b))
    (check-true (memory-result-ok? result))
    (define items (memory-result-value result))
    (check-equal? (length items) 3)))

(test-case "export: filters by scope"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-test-item "m1" 'session "session item"))
  (gen:store-memory! b (make-test-item "m2" 'project "project item"))
  (define result (export-memory-items b #:scope 'session))
  (check-true (memory-result-ok? result))
  (define items (memory-result-value result))
  (check-equal? (length items) 1)
  (check-equal? (memory-item-id (car items)) "m1"))

(test-case "export: rejects invalid backend"
  (define result (export-memory-items #f))
  (check-false (memory-result-ok? result)))

(test-case "export: preserves superseded/history items"
  (define b (make-memory-hash-backend))
  (gen:store-memory! b (make-test-item "old" 'project "old alpha" #:updated "2025-01-01T00:00:00Z"))
  (gen:store-memory!
   b
   (make-test-item "new" 'project "new beta" #:supersedes '("old") #:updated "2025-02-01T00:00:00Z"))
  (define active (gen:retrieve-memory b (memory-query "alpha" 'project #f #f #f #f 10 #f)))
  (check-true (memory-result-ok? active))
  (check-equal? (memory-result-value active) '())
  (define exported (export-memory-items b))
  (check-true (memory-result-ok? exported))
  (check-equal? (sort (map memory-item-id (memory-result-value exported)) string<?) '("new" "old")))

;; ---------------------------------------------------------------------------
;; Import tests
;; ---------------------------------------------------------------------------

(test-case "import: writes items to destination backend"
  (define src (make-memory-hash-backend))
  (define dst (make-memory-hash-backend))
  (gen:store-memory! src (make-test-item "m1" 'session "hello"))
  (gen:store-memory! src (make-test-item "m2" 'project "world"))
  (define export-result (export-memory-items src))
  (define items (memory-result-value export-result))
  (define import-result (import-memory-items! dst items))
  (check-true (memory-result-ok? import-result))
  (check-equal? (memory-result-value import-result) 2)
  ;; Verify items exist in dest
  (define q (memory-query #f 'session #f #f #f #f 10 #f))
  (define retrieved (gen:retrieve-memory dst q))
  (check-true (memory-result-ok? retrieved))
  (check-equal? (length (memory-result-value retrieved)) 1))

(test-case "import: respects scope restriction"
  (define src (make-memory-hash-backend))
  (define dst (make-memory-hash-backend))
  (gen:store-memory! src (make-test-item "m1" 'session "session item"))
  (gen:store-memory! src (make-test-item "m2" 'project "project item"))
  (define export-result (export-memory-items src))
  (define items (memory-result-value export-result))
  ;; Only import session-scoped items
  (define import-result (import-memory-items! dst items #:scope-restriction 'session))
  (check-true (memory-result-ok? import-result))
  (check-equal? (memory-result-value import-result) 1))

(test-case "import: rejects invalid backend"
  (define result (import-memory-items! #f '()))
  (check-false (memory-result-ok? result)))

(test-case "import: rejects non-list items"
  (define b (make-memory-hash-backend))
  (define result (import-memory-items! b "not-a-list"))
  (check-false (memory-result-ok? result)))

;; ---------------------------------------------------------------------------
;; Migrate tests
;; ---------------------------------------------------------------------------

(test-case "migrate: hash to hash"
  (define src (make-memory-hash-backend))
  (define dst (make-memory-hash-backend))
  (gen:store-memory! src (make-test-item "m1" 'session "fact one"))
  (gen:store-memory! src (make-test-item "m2" 'project "fact two"))
  (define result (migrate-memory! src dst))
  (check-true (memory-result-ok? result))
  (check-equal? (memory-result-value result) 2))

(test-case "migrate: with scope filter"
  (define src (make-memory-hash-backend))
  (define dst (make-memory-hash-backend))
  (gen:store-memory! src (make-test-item "m1" 'session "session"))
  (gen:store-memory! src (make-test-item "m2" 'project "project"))
  (define result (migrate-memory! src dst #:scope 'session))
  (check-true (memory-result-ok? result))
  (check-equal? (memory-result-value result) 1))

(test-case "migrate: preserves metadata"
  (define src (make-memory-hash-backend))
  (define dst (make-memory-hash-backend))
  (define item
    (memory-item "meta-test"
                 'semantic
                 'project
                 "content with metadata"
                 (hasheq 'tags
                         '("important")
                         'source
                         'test
                         'project-root
                         "/tmp"
                         'session-id
                         #f
                         'origin-message-id
                         #f)
                 (hasheq 'sensitivity 'internal 'confidence 0.8 'expires-at #f 'supersedes '())
                 "2025-06-01T00:00:00Z"
                 "2025-06-01T00:00:00Z"))
  (gen:store-memory! src item)
  (define result (migrate-memory! src dst))
  (check-true (memory-result-ok? result))
  ;; Verify the item was migrated with metadata
  (define q (memory-query #f 'project "/tmp" #f #f #f 10 #f))
  (define retrieved (gen:retrieve-memory dst q))
  (check-true (memory-result-ok? retrieved))
  (define items (memory-result-value retrieved))
  (check-equal? (length items) 1)
  (define migrated (car items))
  (check-equal? (memory-item-id migrated) "meta-test")
  (check-equal? (hash-ref (memory-item-metadata migrated) 'tags) '("important")))

;; ---------------------------------------------------------------------------
;; Hash → JSONL migration
;; ---------------------------------------------------------------------------

(test-case "migrate: hash to JSONL preserves items"
  (define src (make-memory-hash-backend))
  (define tmpdir (make-temporary-file "migrate-test-~a" 'directory))
  (define dst (make-file-jsonl-backend tmpdir))
  (gen:store-memory! src (make-test-item "j1" 'session "jsonl item 1"))
  (gen:store-memory! src (make-test-item "j2" 'project "jsonl item 2"))
  (define result (migrate-memory! src dst))
  (check-true (memory-result-ok? result))
  (check-equal? (memory-result-value result) 2)
  ;; Verify items in JSONL backend — query project scope with project root
  (define q (memory-query #f 'project #f #f #f #f 10 #f))
  (define retrieved (gen:retrieve-memory dst q))
  (check-true (memory-result-ok? retrieved))
  (check-equal? (length (memory-result-value retrieved)) 1)
  ;; Cleanup
  (delete-directory/files tmpdir))
