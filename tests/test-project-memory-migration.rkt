#lang racket/base

;; tests/test-project-memory-migration.rkt
;; W5C (#8942): Migration from old namespaces to project-keyed storage.
;; Tests that migration is idempotent, resumable, quarantine-safe.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/path
         racket/string
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/file-jsonl.rkt"
         "../runtime/memory/project-identity.rkt"
         "../runtime/memory/project-keyed-factory.rkt"
         "../runtime/memory/project-migration.rkt")

;; Helper: make a test identity
(define (test-id #:key key #:root root)
  (make-project-identity #:key key
                         #:root root
                         #:kind 'git-main
                         #:origin-url "https://github.com/u/r.git"
                         #:common-dir "/some/.git"))

;; Helper: store a simple memory item in a backend
(define (generate-test-id)
  (string-append "id-" (number->string (random 1000000))))

(define (store-item! backend content #:scope [scope 'project])
  (define item
    (memory-item (generate-test-id)
                 'episodic
                 scope
                 content
                 (hasheq 'project-root
                         "/some/p"
                         'session-id
                         "sess-1"
                         'tags
                         '()
                         'source
                         "test"
                         'origin-message-id
                         "msg-1")
                 (hasheq 'sensitivity 'low 'confidence 0.5 'supersedes #f)
                 "2025-01-01T00:00:00Z"
                 "2025-01-01T00:00:00Z"))
  (define result (gen:store-memory! backend item))
  item)

;; Helper: list all items from a backend
(define (list-items backend)
  (define query (memory-query #f #f #f #f #f #f 10000 #t))
  (define result (gen:list-memory backend query))
  (if (memory-result-ok? result)
      (memory-result-value result)
      '()))

(define-test-suite
 project-migration-suite
 ;; ── migrate-from-old-backend! basic operation ──
 (test-case "migrate-from-old-backend! copies all items to new backend"
   (define tmp (make-temporary-file "q-mig1-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (store-item! old-backend "fact-a")
   (store-item! old-backend "fact-b")
   (define id (test-id #:key "mig1key" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   (define report (migrate-from-old-backend! old-backend new-be))
   (check-not-false report)
   (check-true (>= (hash-ref report 'exported 0) 2))
   (check-true (>= (hash-ref report 'imported 0) 2))
   ;; Verify items arrived
   (define items (list-items new-be))
   (check-true (>= (length items) 2))
   (delete-directory/files tmp #:must-exist? #f))
 (test-case "migrate-from-old-backend! is idempotent"
   (define tmp (make-temporary-file "q-mig2-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (store-item! old-backend "fact")
   (define id (test-id #:key "idemkey" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   ;; First migration
   (migrate-from-old-backend! old-backend new-be)
   (define r1 (migrate-from-old-backend! old-backend new-be))
   ;; Second migration should report 0 new imports
   (check-equal? (hash-ref r1 'imported 0) 0)
   (check-equal? (hash-ref r1 'duplicates 0) 1)
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Quarantine ──
 (test-case "dot-namespace items are quarantined"
   (define tmp (make-temporary-file "q-mig3-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (store-item! old-backend "normal" #:scope 'project)
   (store-item! old-backend "bad-scope" #:scope 'session)
   (define id (test-id #:key "quarkey" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   (define report (migrate-from-old-backend! old-backend new-be))
   ;; session-scoped items should be quarantined
   (check-equal? (hash-ref report 'quarantined 0) 1)
   (check-equal? (hash-ref report 'imported 0) 1)
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Empty source ──
 (test-case "migrate-from-old-backend! handles empty source"
   (define tmp (make-temporary-file "q-mig4-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (define id (test-id #:key "emptykey" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   (define report (migrate-from-old-backend! old-backend new-be))
   (check-not-false report)
   (check-equal? (hash-ref report 'exported 0) 0)
   (check-equal? (hash-ref report 'imported 0) 0)
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Error handling: invalid backends ──
 (test-case "migrate-from-old-backend! returns #f for invalid backend"
   (check-false (migrate-from-old-backend!
                 #f
                 (make-file-jsonl-backend (make-temporary-file "q-mig5-~a" 'directory))))
   (check-false (migrate-from-old-backend! (make-file-jsonl-backend (make-temporary-file "q-mig6-~a"
                                                                                         'directory))
                                           #f)))
 ;; ── Re-stamping: items keep their content identity ──
 (test-case "migrated items preserve content"
   (define tmp (make-temporary-file "q-mig7-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (define stored (store-item! old-backend "important" #:scope 'project))
   (define id (test-id #:key "contkey" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   (migrate-from-old-backend! old-backend new-be)
   (define items (list-items new-be))
   (check-equal? (length items) 1)
   (define item (car items))
   (check-equal? (memory-item-content item) (memory-item-content stored))
   (delete-directory/files tmp #:must-exist? #f))
 ;; ── Idempotent resume: partial migration then re-run ──
 (test-case "migrate-from-old-backend! resumes after partial migration"
   (define tmp (make-temporary-file "q-mig8-~a" 'directory))
   (define old-backend (make-file-jsonl-backend tmp))
   (store-item! old-backend "item-a")
   (store-item! old-backend "item-b")
   (define id (test-id #:key "resumekey" #:root "/some/p"))
   (define new-be (make-project-keyed-backend id #:base-data-dir tmp))
   ;; Run once
   (migrate-from-old-backend! old-backend new-be)
   ;; Add a new item to source
   (store-item! old-backend "item-c")
   ;; Run again — should only import the new item
   (define r2 (migrate-from-old-backend! old-backend new-be))
   (check-equal? (hash-ref r2 'imported 0) 1)
   (check-equal? (hash-ref r2 'duplicates 0) 2)
   (define items (list-items new-be))
   (check-equal? (length items) 3)
   (delete-directory/files tmp #:must-exist? #f)))

(run-tests project-migration-suite)
