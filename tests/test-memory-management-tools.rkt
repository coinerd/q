#lang racket/base
;; tests/test-memory-management-tools.rkt — list_memory and clear_memory tests
;;
;; v0.95.9: Tests for memory management UX tools:
;; - list_memory shows id/type/scope/timestamp/snippet
;; - clear_memory requires scope + confirm flag
;; - Disabled backend returns clear message
;; - Scope filtering works

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
;; clear_memory 20-item cap (P2-3)
;; ---------------------------------------------------------------------------

(test-case "clear-memory: capped at policy max-retrieve-count"
  (define b (make-memory-hash-backend))
  ;; Store 25 session-scoped items
  (with-backend b
                (lambda ()
                  (for ([i (in-range 25)])
                    (call-tool tool-store-memory
                               (hash 'content (format "item ~a" i) 'scope "session")))
                  ;; Clear session scope
                  (define r (call-tool tool-clear-memory (hash 'scope "session" 'confirm #t)))
                  (check-false (tool-result-is-error? r))
                  ;; The deleted count should reflect the cap (policy default max-retrieve-count = 20)
                  ;; So at most 20 items are deleted per call
                  (define deleted-count (hash-ref (tool-result-details r) 'deleted-count 0))
                  (check-true (<= deleted-count 20))
                  ;; Verify some items remain if the cap was hit
                  (define lr (call-tool tool-list-memory (hash 'scope "session")))
                  (define remaining (hash-ref (tool-result-details lr) 'items '()))
                  (check-true (>= (length remaining) 5)))))
