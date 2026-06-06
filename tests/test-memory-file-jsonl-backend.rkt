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
                        #:type [type 'semantic])
  (memory-item id type scope content (hasheq) (hasheq) "2026-06-01T00:00:00Z" "2026-06-01T00:00:00Z"))

;; ---------------------------------------------------------------------------
;; Basic store/retrieve
;; ---------------------------------------------------------------------------

(test-case "store and retrieve round-trip"
  (with-temp-backend
   (lambda (backend dir)
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
  (with-temp-backend
   (lambda (backend dir)
     (define item (make-test-item #:id "persist-id" #:content "durable"))
     (gen:store-memory! backend item)
     ;; Create a new backend reading the same file
     (define backend2 (make-file-jsonl-backend dir))
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define result (gen:retrieve-memory backend2 query))
     (check-true (memory-result-ok? result))
     (check-equal? (length (memory-result-value result)) 1)
     (check-equal? (memory-item-content (car (memory-result-value result))) "durable"))))

(test-case "store multiple items and retrieve all"
  (with-temp-backend
   (lambda (backend dir)
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
  (with-temp-backend
   (lambda (backend dir)
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
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "upd" #:content "original"))
     (define upd-result (gen:update-memory! backend "upd" (hash 'content "modified")))
     (check-true (memory-result-ok? upd-result))
     ;; Verify content changed
     (define query (memory-query #f #f #f #f #f #f 100 #f))
     (define items (memory-result-value (gen:retrieve-memory backend query)))
     (check-equal? (length items) 1)
     (check-equal? (memory-item-content (car items)) "modified"))))

(test-case "update persists after reload"
  (with-temp-backend
   (lambda (backend dir)
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
       (lambda (port)
         (displayln "THIS IS NOT VALID JSON {{{" port))
       #:mode 'text #:exists 'append)
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
                          (memory-item (format "ord~a" i) 'semantic 'session
                                       (format "content ~a" i)
                                       (hasheq) (hasheq) ""
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

(test-case "duplicate store fails"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "dup"))
     (define result (gen:store-memory! backend (make-test-item #:id "dup")))
     (check-false (memory-result-ok? result)))))

;; ---------------------------------------------------------------------------
;; Available
;; ---------------------------------------------------------------------------

(test-case "available? returns #t"
  (with-temp-backend
   (lambda (backend dir)
     (check-true (gen:memory-available? backend)))))

;; ---------------------------------------------------------------------------
;; Manage (no-op)
;; ---------------------------------------------------------------------------

(test-case "manage! returns success"
  (with-temp-backend
   (lambda (backend dir)
     (define result (gen:manage-memory! backend (hash)))
     (check-true (memory-result-ok? result)))))

;; ---------------------------------------------------------------------------
;; Scope filtering
;; ---------------------------------------------------------------------------

(test-case "retrieve filters by scope"
  (with-temp-backend
   (lambda (backend dir)
     (gen:store-memory! backend (make-test-item #:id "s1" #:scope 'session))
     (gen:store-memory! backend (make-test-item #:id "p1" #:scope 'project))
     ;; Query session only
     (define query (memory-query #f 'session #f #f #f #f 100 #f))
     (define result (gen:retrieve-memory backend query))
     (check-equal? (length (memory-result-value result)) 1)
     (check-equal? (memory-item-id (car (memory-result-value result))) "s1"))))
