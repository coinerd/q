#lang racket/base

;; tests/test-gsd-context-factory.rkt — Tests for closure-encapsulated GSD context
;;
;; W0 scaffolding for v0.29.4: Runtime State Encapsulation.
;; These tests define the expected API for make-gsd-context, a closure
;; factory that replaces module-level boxes in extensions/gsd/session-state.rkt.

(require rackunit
         racket/set)

;; Import the closure factory (will be created in W1)
;; For now, define a placeholder that tests will exercise.
;; Once implemented, this require will work.
(require (only-in "helpers/gsd-test-helpers.rkt"
                  [make-gsd-context make-gsd-context]))


;; ============================================================
;; 1. Factory returns a callable dispatch function
;; ============================================================

(test-case "make-gsd-context returns a procedure"
  (define ctx (make-gsd-context))
  (check-pred procedure? ctx))

(test-case "context dispatch rejects unknown actions"
  (define ctx (make-gsd-context))
  (check-exn exn:fail?
             (lambda () (ctx 'unknown-action))))

;; ============================================================
;; 2. State get/set roundtrip
;; ============================================================

(test-case "state get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-state) #f "initial state is #f")
  (ctx 'set-state 'idle)
  (check-equal? (ctx 'get-state) 'idle))

(test-case "state supports gsd-runtime-state structs"
  (define ctx (make-gsd-context))
  ;; Simulate a full state struct (using a hasheq for now)
  (define fake-state (hasheq 'mode 'executing 'wave 2))
  (ctx 'set-state fake-state)
  (check-equal? (ctx 'get-state) fake-state))

;; ============================================================
;; 3. Plan data get/set roundtrip
;; ============================================================

(test-case "plan get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-plan) #f "initial plan is #f")
  (ctx 'set-plan '((wave 1 "do stuff")))
  (check-equal? (ctx 'get-plan) '((wave 1 "do stuff"))))

;; ============================================================
;; 4. Workflow get/set roundtrip
;; ============================================================

(test-case "workflow get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-workflow) #f "initial workflow is #f")
  (ctx 'set-workflow 'v0.29.4)
  (check-equal? (ctx 'get-workflow) 'v0.29.4))

;; ============================================================
;; 5. Busy flag get/set roundtrip
;; ============================================================

(test-case "busy? get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-false (ctx 'busy?) "initial busy is #f")
  (ctx 'set-busy! #t)
  (check-true (ctx 'busy?)))

;; ============================================================
;; 6. Correlation ID get/set roundtrip
;; ============================================================

(test-case "correlation-id get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-correlation-id) #f)
  (ctx 'set-correlation-id! "abc-123")
  (check-equal? (ctx 'get-correlation-id) "abc-123"))

;; ============================================================
;; 7. Transaction ref/set roundtrip
;; ============================================================

(test-case "transaction-ref/set! roundtrip"
  (define ctx (make-gsd-context))
  (check-exn exn:fail?
             (lambda () (ctx 'transaction-ref 'missing))
             "transaction-ref on missing key should fail")
  (ctx 'transaction-set! 'edit-count 42)
  (check-equal? (ctx 'transaction-ref 'edit-count) 42))

;; ============================================================
;; 8. Two independent contexts don't interfere
;; ============================================================

(test-case "two independent contexts are isolated"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (ctx-a 'set-state 'executing)
  (ctx-b 'set-state 'idle)
  (check-equal? (ctx-a 'get-state) 'executing)
  (check-equal? (ctx-b 'get-state) 'idle)
  ;; Plans also isolated
  (ctx-a 'set-plan '((wave 1)))
  (ctx-b 'set-plan '((wave 2)))
  (check-equal? (ctx-a 'get-plan) '((wave 1)))
  (check-equal? (ctx-b 'get-plan) '((wave 2))))

(test-case "transaction isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (ctx-a 'transaction-set! 'key 'val-a)
  (ctx-b 'transaction-set! 'key 'val-b)
  (check-equal? (ctx-a 'transaction-ref 'key) 'val-a)
  (check-equal? (ctx-b 'transaction-ref 'key) 'val-b))

;; ============================================================
;; 9. Thread safety (basic check)
;; ============================================================

(test-case "concurrent mutations don't corrupt state"
  (define ctx (make-gsd-context))
  (define sem (make-semaphore 0))
  (define iterations 100)
  (define results (box '()))
  ;; Spawn threads that race to set state
  (for ([i (in-range iterations)])
    (thread
     (lambda ()
       (semaphore-wait sem)
       (ctx 'set-state i)
       ;; Don't check value — just ensure no crash
       (semaphore-post sem))))
  ;; Release all threads
  (for ([_ (in-range iterations)])
    (semaphore-post sem))
  ;; Give threads time to finish
  (sleep 0.1)
  ;; State should be an integer (one of the racers won)
  (check-pred integer? (ctx 'get-state)))

;; ============================================================
;; 10. History operations
;; ============================================================

(test-case "history get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-history) '() "initial history is empty")
  (ctx 'set-history! '(a b c))
  (check-equal? (ctx 'get-history) '(a b c)))

(test-case "history update appends correctly"
  (define ctx (make-gsd-context))
  (ctx 'set-history! '(a))
  (ctx 'set-history! (append (ctx 'get-history) '(b)))
  (check-equal? (ctx 'get-history) '(a b)))

;; ============================================================
;; 11. Edit limit get/set roundtrip
;; ============================================================

(test-case "edit-limit get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-edit-limit) 500 "default edit limit")
  (ctx 'set-edit-limit! 1000)
  (check-equal? (ctx 'get-edit-limit) 1000))

;; ============================================================
;; 12. Pinned dir get/set roundtrip
;; ============================================================

(test-case "pinned-dir get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-pinned-dir) #f)
  (ctx 'set-pinned-dir! "/tmp/planning")
  (check-equal? (ctx 'get-pinned-dir) "/tmp/planning"))

;; ============================================================
;; 13. Event bus get/set roundtrip
;; ============================================================

(test-case "event-bus get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx 'get-event-bus) #f)
  (define fake-bus (hasheq 'type 'event-bus))
  (ctx 'set-event-bus! fake-bus)
  (check-equal? (ctx 'get-event-bus) fake-bus))
