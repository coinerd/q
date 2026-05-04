#lang racket/base

;; tests/test-working-set-factory.rkt — Tests for closure-encapsulated working-set
;;
;; W0 scaffolding for v0.29.4: Runtime State Encapsulation.
;; These tests define the expected API for make-ws-context, a closure
;; factory that replaces the mutable struct fields in runtime/working-set.rkt.

(require rackunit)

;; Import the closure factory (will be created in W2)
(require (only-in "helpers/ws-test-helpers.rkt"
                  [make-ws-context make-ws-context]))


;; ============================================================
;; 1. Factory returns a callable dispatch function
;; ============================================================

(test-case "make-ws-context returns a procedure"
  (define ws (make-ws-context))
  (check-pred procedure? ws))

(test-case "ws-context rejects unknown actions"
  (define ws (make-ws-context))
  (check-exn exn:fail?
             (lambda () (ws 'unknown-action))))

;; ============================================================
;; 2. Entries get/add/remove
;; ============================================================

(test-case "initial entries are empty"
  (define ws (make-ws-context))
  (check-equal? (ws 'entries) '()))

(test-case "add and retrieve entries"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/foo.rkt" "msg-1" 500)
  (define entries (ws 'entries))
  (check-equal? (length entries) 1)
  (check-equal? (car entries) (hasheq 'path "/tmp/foo.rkt"
                                       'message-id "msg-1"
                                       'token-estimate 500)))

(test-case "add multiple entries"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws 'add! "/tmp/b.rkt" "msg-2" 200)
  (check-equal? (length (ws 'entries)) 2))

(test-case "add refreshes existing entry (move to front)"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/foo.rkt" "msg-1" 100)
  (ws 'add! "/tmp/bar.rkt" "msg-2" 200)
  (ws 'add! "/tmp/foo.rkt" "msg-3" 300)
  (define entries (ws 'entries))
  ;; foo.rkt should be at front (refreshed), bar.rkt second
  (check-equal? (hash-ref (car entries) 'path) "/tmp/foo.rkt")
  (check-equal? (hash-ref (cadr entries) 'path) "/tmp/bar.rkt"))

(test-case "remove entry by path"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/foo.rkt" "msg-1" 100)
  (ws 'add! "/tmp/bar.rkt" "msg-2" 200)
  (ws 'remove! "/tmp/foo.rkt")
  (check-equal? (length (ws 'entries)) 1)
  (check-equal? (hash-ref (car (ws 'entries)) 'path) "/tmp/bar.rkt"))

(test-case "remove nonexistent entry is no-op"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/foo.rkt" "msg-1" 100)
  (ws 'remove! "/tmp/nonexistent.rkt")
  (check-equal? (length (ws 'entries)) 1))

;; ============================================================
;; 3. Reset clears all entries
;; ============================================================

(test-case "reset clears all entries"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/foo.rkt" "msg-1" 100)
  (ws 'add! "/tmp/bar.rkt" "msg-2" 200)
  (ws 'reset!)
  (check-equal? (ws 'entries) '()))

;; ============================================================
;; 4. Token and entry counts
;; ============================================================

(test-case "token-count sums estimates"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws 'add! "/tmp/b.rkt" "msg-2" 200)
  (check-equal? (ws 'token-count) 300))

(test-case "entry-count returns length"
  (define ws (make-ws-context))
  (ws 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws 'add! "/tmp/b.rkt" "msg-2" 200)
  (check-equal? (ws 'entry-count) 2))

;; ============================================================
;; 5. Max entries and max tokens configuration
;; ============================================================

(test-case "default max-entries is 30"
  (define ws (make-ws-context))
  (check-equal? (ws 'max-entries) 30))

(test-case "default max-tokens is 15000"
  (define ws (make-ws-context))
  (check-equal? (ws 'max-tokens) 15000))

(test-case "custom max-entries and max-tokens"
  (define ws (make-ws-context #:max-entries 10 #:max-tokens 5000))
  (check-equal? (ws 'max-entries) 10)
  (check-equal? (ws 'max-tokens) 5000))

;; ============================================================
;; 6. LRU eviction when max-entries exceeded
;; ============================================================

(test-case "evicts oldest entry when max-entries exceeded"
  (define ws (make-ws-context #:max-entries 2))
  (ws 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws 'add! "/tmp/b.rkt" "msg-2" 200)
  (ws 'add! "/tmp/c.rkt" "msg-3" 300)
  ;; a.rkt should be evicted (oldest)
  (check-equal? (ws 'entry-count) 2)
  (define paths (map (lambda (e) (hash-ref e 'path)) (ws 'entries)))
  (check-false (member "/tmp/a.rkt" paths))
  (check-not-false (member "/tmp/c.rkt" paths)))

;; ============================================================
;; 7. Two independent contexts don't interfere
;; ============================================================

(test-case "two ws-contexts are isolated"
  (define ws-a (make-ws-context))
  (define ws-b (make-ws-context))
  (ws-a 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws-b 'add! "/tmp/b.rkt" "msg-2" 200)
  (check-equal? (ws-a 'entry-count) 1)
  (check-equal? (ws-b 'entry-count) 1)
  (check-equal? (hash-ref (car (ws-a 'entries)) 'path) "/tmp/a.rkt")
  (check-equal? (hash-ref (car (ws-b 'entries)) 'path) "/tmp/b.rkt"))

(test-case "reset on one context doesn't affect other"
  (define ws-a (make-ws-context))
  (define ws-b (make-ws-context))
  (ws-a 'add! "/tmp/a.rkt" "msg-1" 100)
  (ws-b 'add! "/tmp/b.rkt" "msg-2" 200)
  (ws-a 'reset!)
  (check-equal? (ws-a 'entry-count) 0)
  (check-equal? (ws-b 'entry-count) 1))

;; ============================================================
;; 8. Validation on mutation
;; ============================================================

(test-case "add validates path is a string"
  (define ws (make-ws-context))
  (check-exn exn:fail?
             (lambda () (ws 'add! 123 "msg-1" 100))))

(test-case "add validates token-estimate is a number"
  (define ws (make-ws-context))
  (check-exn exn:fail?
             (lambda () (ws 'add! "/tmp/foo.rkt" "msg-1" "not-a-number"))))
