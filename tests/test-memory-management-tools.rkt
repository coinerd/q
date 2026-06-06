#lang racket/base
;; tests/test-memory-management-tools.rkt — list_memory and clear_memory tests
;;
;; v0.95.9: Tests for memory management UX tools:
;; - list_memory shows id/type/scope/timestamp/snippet
;; - clear_memory requires scope + confirm flag
;; - Disabled backend returns clear message
;; - Scope filtering works

(require rackunit
         racket/string
         "../tools/builtins/memory-tools.rkt"
         "../runtime/memory/types.rkt"
         "../runtime/memory/protocol.rkt"
         "../runtime/memory/backends/memory-hash.rkt"
         "../runtime/memory/policy.rkt"
         "../runtime/memory/service.rkt"
         "../tools/tool.rkt")

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (with-backend backend thunk)
  (parameterize ([current-memory-backend backend])
    (thunk)))

(define mgmt-test-context
  (make-exec-context #:working-directory "/tmp/q-memory-mgmt"
                     #:session-metadata (hasheq 'session-id "sess-mgmt")))

(define (call-tool tool args)
  (tool args mgmt-test-context))

(define (store-some backend)
  (with-backend
   backend
   (lambda ()
     (call-tool tool-store-memory (hash 'content "session fact" 'scope "session" 'type "semantic"))
     (call-tool tool-store-memory
                (hash 'content "project decision" 'scope "project" 'type "procedural"))
     (call-tool tool-store-memory (hash 'content "another session note" 'scope "session")))))

;; ---------------------------------------------------------------------------
;; list_memory
;; ---------------------------------------------------------------------------

(test-case "list-memory: returns error when backend is #f"
  (parameterize ([current-memory-backend #f])
    (define r (call-tool tool-list-memory (hash)))
    (check-true (tool-result-is-error? r))))

(test-case "list-memory: empty list when no memories"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-list-memory (hash)))
                  (check-false (tool-result-is-error? r))
                  (define items (hash-ref (tool-result-details r) 'items '()))
                  (check-equal? items '()))))

(test-case "list-memory: shows id/type/scope/timestamp/snippet"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-list-memory (hash 'scope "project")))
                  (check-false (tool-result-is-error? r))
                  (define items (hash-ref (tool-result-details r) 'items '()))
                  (check-equal? (length items) 1)
                  (define first-item (car items))
                  ;; Each summary should have required fields
                  (check-true (hash-has-key? first-item 'id))
                  (check-true (hash-has-key? first-item 'type))
                  (check-true (hash-has-key? first-item 'scope))
                  (check-true (hash-has-key? first-item 'updated-at))
                  (check-true (hash-has-key? first-item 'snippet))
                  ;; Also check session scope has 2 items
                  (define rs (call-tool tool-list-memory (hash 'scope "session")))
                  (check-equal? (length (hash-ref (tool-result-details rs) 'items '())) 2))))

(test-case "list-memory: filters by scope"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-list-memory (hash 'scope "project")))
                  (check-false (tool-result-is-error? r))
                  (define items (hash-ref (tool-result-details r) 'items '()))
                  (check-equal? (length items) 1)
                  (check-equal? (hash-ref (car items) 'scope) 'project))))

(test-case "list-memory: respects limit"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-list-memory (hash 'scope "session" 'limit 1)))
                  (check-false (tool-result-is-error? r))
                  (define items (hash-ref (tool-result-details r) 'items '()))
                  (check-equal? (length items) 1))))

(test-case "list-memory: snippet is truncated"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define long-content (make-string 200 #\x))
                  (call-tool tool-store-memory (hash 'content long-content 'scope "project"))
                  (define r (call-tool tool-list-memory (hash 'scope "project")))
                  (define items (hash-ref (tool-result-details r) 'items '()))
                  (check-equal? (string-length (hash-ref (car items) 'snippet)) 80))))

;; ---------------------------------------------------------------------------
;; clear_memory
;; ---------------------------------------------------------------------------

(test-case "clear-memory: returns error when backend is #f"
  (parameterize ([current-memory-backend #f])
    (define r (call-tool tool-clear-memory (hash 'scope "session" 'confirm #t)))
    (check-true (tool-result-is-error? r))))

(test-case "clear-memory: requires scope"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'confirm #t)))
                  (check-true (tool-result-is-error? r)))))

(test-case "clear-memory: requires confirm=true"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'scope "session")))
                  (check-true (tool-result-is-error? r))
                  ;; Also reject confirm=false
                  (define r2 (call-tool tool-clear-memory (hash 'scope "session" 'confirm #f)))
                  (check-true (tool-result-is-error? r2)))))

(test-case "clear-memory: rejects invalid scope"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'scope "global" 'confirm #t)))
                  (check-true (tool-result-is-error? r)))))

(test-case "clear-memory: clears session scope only"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'scope "session" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  (define meta (tool-result-details r))
                  (check-equal? (hash-ref meta 'deleted-count) 2) ; two session items
                  (check-equal? (hash-ref meta 'scope) 'session)
                  ;; Project item should still be there
                  (define lr (call-tool tool-list-memory (hash 'scope "project")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (check-equal? (length items) 1))))

(test-case "clear-memory: clears project scope"
  (define b (make-memory-hash-backend))
  (store-some b)
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'scope "project" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  (define meta (tool-result-details r))
                  (check-equal? (hash-ref meta 'deleted-count) 1)
                  ;; Session items should still be there
                  (define lr (call-tool tool-list-memory (hash 'scope "session")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (check-equal? (length items) 2))))

(test-case "clear-memory: empty scope returns success with 0 deleted"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-clear-memory (hash 'scope "session" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  (check-equal? (hash-ref (tool-result-details r) 'deleted-count) 0))))

;; ---------------------------------------------------------------------------
;; delete-memory emits event data in metadata
;; ---------------------------------------------------------------------------

(test-case "delete-memory: returns id and scope in metadata"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  ;; Store first
                  (call-tool tool-store-memory (hash 'content "to delete" 'scope "project"))
                  ;; Get the id
                  (define lr (call-tool tool-list-memory (hash 'scope "project")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (define item-id (hash-ref (car items) 'id))
                  ;; Delete
                  (define dr (call-tool tool-delete-memory (hash 'id item-id 'scope "project")))
                  (check-false (tool-result-is-error? dr)))))

(test-case "delete-memory: scope mismatch fails"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (call-tool tool-store-memory (hash 'content "scoped" 'scope "project"))
                  (define lr (call-tool tool-list-memory (hash 'scope "project")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (define item-id (hash-ref (car items) 'id))
                  ;; Wrong scope
                  (define dr (call-tool tool-delete-memory (hash 'id item-id 'scope "session")))
                  (check-true (tool-result-is-error? dr)))))

;; ---------------------------------------------------------------------------
;; F19: clear_memory emits per-item deleted events
;; ---------------------------------------------------------------------------

(test-case "clear-memory emits memory.item.deleted per item (F19)"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-mgmt"
                       #:session-metadata (hasheq 'session-id "sess-mgmt")
                       #:event-publisher (lambda (evt) (set-box! events (cons evt (unbox events))))))
  (with-backend b
                (lambda ()
                  (tool-store-memory (hash 'content "item one" 'scope "session") ctx)
                  (tool-store-memory (hash 'content "item two" 'scope "session") ctx)
                  (set-box! events '())
                  (define r (tool-clear-memory (hash 'scope "session" 'confirm #t) ctx))
                  (check-false (tool-result-is-error? r))
                  (define evts (reverse (unbox events)))
                  ;; Should have 2 deleted events
                  (define deleted-events
                    (filter (lambda (evt) (equal? (hash-ref evt 'type #f) "memory.item.deleted"))
                            evts))
                  (check-equal? (length deleted-events) 2))))

;; ---------------------------------------------------------------------------
;; clear_memory 20-item cap (P2-3)
;; ---------------------------------------------------------------------------

(test-case "clear-memory: loops until all items deleted (P2-9)"
  (define b (make-memory-hash-backend))
  ;; Store 25 session-scoped items
  (with-backend b
                (lambda ()
                  (for ([i (in-range 25)])
                    (call-tool tool-store-memory
                               (hash 'content (format "item ~a" i) 'scope "session")))
                  ;; Clear session scope — now loops until empty
                  (define r (call-tool tool-clear-memory (hash 'scope "session" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  ;; All 25 should be deleted
                  (define deleted-count (hash-ref (tool-result-details r) 'deleted-count 0))
                  (check-equal? deleted-count 25)
                  ;; Verify no items remain
                  (define lr (call-tool tool-list-memory (hash 'scope "session")))
                  (define remaining (hash-ref (tool-result-details lr) 'items '()))
                  (check-equal? (length remaining) 0))))

;; ---------------------------------------------------------------------------
;; M13-F5: update_memory tool tests
;; ---------------------------------------------------------------------------

(test-case "update-memory: requires id"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-update-memory (hash)))
                  (check-true (tool-result-is-error? r)))))

(test-case "update-memory: requires existing item in scope"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-update-memory (hash 'id "nonexistent" 'content "new")))
                  (check-true (tool-result-is-error? r)))))

(test-case "update-memory: updates content"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (call-tool tool-store-memory (hash 'content "original content" 'scope "project"))
                  (define lr (call-tool tool-list-memory (hash 'scope "project")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (define item-id (hash-ref (car items) 'id))
                  (define r
                    (call-tool tool-update-memory
                               (hash 'id item-id 'content "updated content" 'scope "project")))
                  (check-false (tool-result-is-error? r))
                  (define lr2 (call-tool tool-list-memory (hash 'scope "project")))
                  (define items2 (hash-ref (tool-result-details lr2) 'items '()))
                  (check-equal? (length items2) 1)
                  ;; Content should be updated
                  (check-true (string-contains? (hash-ref (car items2) 'snippet) "updated")))))

(test-case "update-memory: updates tags"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     (call-tool tool-store-memory (hash 'content "tagged item" 'scope "project"))
     (define lr (call-tool tool-list-memory (hash 'scope "project")))
     (define items (hash-ref (tool-result-details lr) 'items '()))
     (define item-id (hash-ref (car items) 'id))
     (define r (call-tool tool-update-memory (hash 'id item-id 'tags '("updated") 'scope "project")))
     (check-false (tool-result-is-error? r)))))

(test-case "update-memory: emits memory.item.updated event"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-mgmt"
                       #:session-metadata (hasheq 'session-id "sess-mgmt")
                       #:event-publisher (lambda (evt) (set-box! events (cons evt (unbox events))))))
  (with-backend
   b
   (lambda ()
     (tool-store-memory (hash 'content "to update" 'scope "project") ctx)
     (define lr (tool-list-memory (hash 'scope "project") ctx))
     (define items (hash-ref (tool-result-details lr) 'items '()))
     (define item-id (hash-ref (car items) 'id))
     (set-box! events '())
     (define r (tool-update-memory (hash 'id item-id 'content "new content" 'scope "project") ctx))
     (check-false (tool-result-is-error? r))
     (define updated-events
       (filter (lambda (evt) (equal? (hash-ref evt 'type #f) "memory.item.updated"))
               (reverse (unbox events))))
     (check-equal? (length updated-events) 1)
     (check-equal? (hash-ref (car updated-events) 'id) item-id))))

(test-case "update-memory: rejects secret sensitivity before backend update"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     (call-tool tool-store-memory (hash 'content "secret candidate" 'scope "project"))
     (define lr (call-tool tool-list-memory (hash 'scope "project")))
     (define item-id (hash-ref (car (hash-ref (tool-result-details lr) 'items '())) 'id))
     (define r
       (call-tool tool-update-memory (hash 'id item-id 'scope "project" 'sensitivity "secret")))
     (check-true (tool-result-is-error? r))
     (check-true (string-contains? (format "~a" (tool-result-content r)) "Secret sensitivity"))
     (define stored
       (gen:retrieve-memory b (memory-query #f 'project "/tmp/q-memory-mgmt" #f #f #f 10 #f)))
     (check-true (memory-result-ok? stored))
     (check-equal? (hash-ref (memory-item-validity (car (memory-result-value stored))) 'sensitivity)
                   'public))))

(test-case "update-memory: sensitivity plus supersedes preserves validity keys"
  (define b (make-memory-hash-backend))
  (with-backend
   b
   (lambda ()
     (call-tool tool-store-memory (hash 'content "old fact" 'scope "project"))
     (call-tool tool-store-memory (hash 'content "new fact" 'scope "project"))
     (define lr (call-tool tool-list-memory (hash 'scope "project")))
     (define items (hash-ref (tool-result-details lr) 'items '()))
     (define new-id (hash-ref (car items) 'id))
     (define old-id (hash-ref (cadr items) 'id))
     (define r
       (call-tool
        tool-update-memory
        (hash 'id new-id 'scope "project" 'sensitivity "internal" 'supersedes (list old-id))))
     (check-false (tool-result-is-error? r))
     (define stored
       (gen:retrieve-memory b (memory-query #f 'project "/tmp/q-memory-mgmt" #f #f #f 10 #t)))
     (check-true (memory-result-ok? stored))
     (define updated
       (for/first ([item (in-list (memory-result-value stored))]
                   #:when (equal? (memory-item-id item) new-id))
         item))
     (check-true (valid-memory-item? updated))
     (check-equal? (hash-ref (memory-item-validity updated) 'sensitivity) 'internal)
     (check-equal? (hash-ref (memory-item-validity updated) 'supersedes) (list old-id)))))

;; ---------------------------------------------------------------------------
;; M13-F5: cleanup_expired_memory tool tests
;; ---------------------------------------------------------------------------

(test-case "cleanup-expired: requires scope"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (define r (call-tool tool-cleanup-expired-memory (hash 'dry-run #t)))
                  (check-true (tool-result-is-error? r)))))

(test-case "cleanup-expired: dry-run reports expired items"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  ;; Store items, some expired, some not
                  (call-tool tool-store-memory (hash 'content "active item" 'scope "session"))
                  ;; Manually inject an expired item
                  (define expired-item
                    (memory-item "expired-1"
                                 'semantic
                                 'session
                                 "expired content"
                                 (hasheq 'tags
                                         '()
                                         'source
                                         'test
                                         'project-root
                                         #f
                                         'session-id
                                         "sess-mgmt"
                                         'origin-message-id
                                         #f)
                                 (hasheq 'sensitivity
                                         'public
                                         'confidence
                                         0.9
                                         'expires-at
                                         "2020-01-01T00:00:00Z"
                                         'supersedes
                                         '())
                                 "2020-01-01T00:00:00Z"
                                 "2020-01-01T00:00:00Z"))
                  (gen:store-memory! b expired-item)
                  ;; Dry-run
                  (define r
                    (call-tool tool-cleanup-expired-memory (hash 'scope "session" 'dry-run #t)))
                  (check-false (tool-result-is-error? r))
                  (define meta (tool-result-details r))
                  (check-true (>= (hash-ref meta 'expired-count 0) 1))
                  (check-true (>= (hash-ref meta 'total-count 0) 2)))))

(test-case "cleanup-expired: requires confirm to delete"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  (call-tool tool-store-memory (hash 'content "active item" 'scope "session"))
                  (define r (call-tool tool-cleanup-expired-memory (hash 'scope "session")))
                  (check-true (tool-result-is-error? r)))))

(test-case "cleanup-expired: deletes expired items with confirm"
  (define b (make-memory-hash-backend))
  (with-backend b
                (lambda ()
                  ;; Store active item
                  (call-tool tool-store-memory (hash 'content "active item" 'scope "session"))
                  ;; Inject expired item
                  (define expired-item
                    (memory-item "expired-2"
                                 'semantic
                                 'session
                                 "old content"
                                 (hasheq 'tags
                                         '()
                                         'source
                                         'test
                                         'project-root
                                         #f
                                         'session-id
                                         "sess-mgmt"
                                         'origin-message-id
                                         #f)
                                 (hasheq 'sensitivity
                                         'public
                                         'confidence
                                         0.9
                                         'expires-at
                                         "2020-01-01T00:00:00Z"
                                         'supersedes
                                         '())
                                 "2020-01-01T00:00:00Z"
                                 "2020-01-01T00:00:00Z"))
                  (gen:store-memory! b expired-item)
                  ;; Confirm cleanup
                  (define r
                    (call-tool tool-cleanup-expired-memory (hash 'scope "session" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  (define meta (tool-result-details r))
                  (check-equal? (hash-ref meta 'deleted-count) 1)
                  ;; Active item should still be there
                  (define lr (call-tool tool-list-memory (hash 'scope "session")))
                  (define items (hash-ref (tool-result-details lr) 'items '()))
                  (check-equal? (length items) 1))))

(test-case "cleanup-expired: emits deleted events"
  (define b (make-memory-hash-backend))
  (define events (box '()))
  (define ctx
    (make-exec-context #:working-directory "/tmp/q-memory-mgmt"
                       #:session-metadata (hasheq 'session-id "sess-mgmt")
                       #:event-publisher (lambda (evt) (set-box! events (cons evt (unbox events))))))
  (with-backend b
                (lambda ()
                  (define expired-item
                    (memory-item "expired-evt"
                                 'semantic
                                 'session
                                 "expired for events"
                                 (hasheq 'tags
                                         '()
                                         'source
                                         'test
                                         'project-root
                                         #f
                                         'session-id
                                         "sess-mgmt"
                                         'origin-message-id
                                         #f)
                                 (hasheq 'sensitivity
                                         'public
                                         'confidence
                                         0.9
                                         'expires-at
                                         "2020-01-01T00:00:00Z"
                                         'supersedes
                                         '())
                                 "2020-01-01T00:00:00Z"
                                 "2020-01-01T00:00:00Z"))
                  (gen:store-memory! b expired-item)
                  (set-box! events '())
                  (define r (tool-cleanup-expired-memory (hash 'scope "session" 'confirm #t) ctx))
                  (check-false (tool-result-is-error? r))
                  (define deleted-events
                    (filter (lambda (evt) (equal? (hash-ref evt 'type #f) "memory.item.deleted"))
                            (reverse (unbox events))))
                  (check-equal? (length deleted-events) 1))))
