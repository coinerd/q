#lang racket/base

;; BOUNDARY: integration

;; tests/test-gsd-context-factory.rkt — Tests for struct-based GSD context
;;
;; v0.29.4 W0: Original tests for closure-encapsulated context.
;; v0.32.5 W1: Rewritten for struct-based gsd-session-ctx.

(require rackunit
         racket/set
         (only-in "../extensions/gsd/runtime-state-types.rkt" gsd-runtime-state?)
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-session-ctx?
                  gsd-session-ctx-state-box
                  gsd-session-ctx-plan-box
                  gsd-session-ctx-pinned-dir-box
                  gsd-session-ctx-edit-limit-box
                  gsd-session-ctx-event-bus-box
                  gsd-session-ctx-history-box
                  gsd-session-ctx-busy-box
                  gsd-session-ctx-correlation-id-box
                  gsd-session-ctx-transaction-box
                  ctx-read
                  ctx-write!
                  ctx-update!))

;; ============================================================
;; 1. Factory returns a struct
;; ============================================================

(test-case "make-gsd-context returns a gsd-session-ctx"
  (define ctx (make-gsd-context))
  (check-pred gsd-session-ctx? ctx))

;; ============================================================
;; 2. State get/set roundtrip
;; ============================================================

(test-case "state get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-pred gsd-runtime-state?
              (ctx-read ctx gsd-session-ctx-state-box)
              "initial state is gsd-runtime-state")
  (ctx-write! ctx gsd-session-ctx-state-box 'idle)
  (check-equal? (ctx-read ctx gsd-session-ctx-state-box) 'idle))

(test-case "state supports arbitrary values"
  (define ctx (make-gsd-context))
  (define fake-state (hasheq 'mode 'executing 'wave 2))
  (ctx-write! ctx gsd-session-ctx-state-box fake-state)
  (check-equal? (ctx-read ctx gsd-session-ctx-state-box) fake-state))

;; ============================================================
;; 3. Plan data get/set roundtrip
;; ============================================================

(test-case "plan get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-plan-box) #f "initial plan is #f")
  (ctx-write! ctx gsd-session-ctx-plan-box '((wave 1 "do stuff")))
  (check-equal? (ctx-read ctx gsd-session-ctx-plan-box) '((wave 1 "do stuff"))))

;; ============================================================
;; 4. Busy flag get/set roundtrip
;; ============================================================

(test-case "busy? get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-false (ctx-read ctx gsd-session-ctx-busy-box) "initial busy is #f")
  (ctx-write! ctx gsd-session-ctx-busy-box #t)
  (check-true (ctx-read ctx gsd-session-ctx-busy-box)))

;; ============================================================
;; 5. Correlation ID get/set roundtrip
;; ============================================================

(test-case "correlation-id get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-correlation-id-box) #f)
  (ctx-write! ctx gsd-session-ctx-correlation-id-box "abc-123")
  (check-equal? (ctx-read ctx gsd-session-ctx-correlation-id-box) "abc-123"))

;; ============================================================
;; 6. Transaction ref/set roundtrip
;; ============================================================

(test-case "transaction update roundtrip"
  (define ctx (make-gsd-context))
  ;; Set a value
  (ctx-update! ctx gsd-session-ctx-transaction-box (lambda (h) (hash-set h 'edit-count 42)))
  (check-equal? (hash-ref (ctx-read ctx gsd-session-ctx-transaction-box) 'edit-count) 42))

;; ============================================================
;; 7. Two independent contexts don't interfere
;; ============================================================

(test-case "two independent contexts are isolated"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (ctx-write! ctx-a gsd-session-ctx-state-box 'executing)
  (ctx-write! ctx-b gsd-session-ctx-state-box 'idle)
  (check-equal? (ctx-read ctx-a gsd-session-ctx-state-box) 'executing)
  (check-equal? (ctx-read ctx-b gsd-session-ctx-state-box) 'idle)
  ;; Plans also isolated
  (ctx-write! ctx-a gsd-session-ctx-plan-box '((wave 1)))
  (ctx-write! ctx-b gsd-session-ctx-plan-box '((wave 2)))
  (check-equal? (ctx-read ctx-a gsd-session-ctx-plan-box) '((wave 1)))
  (check-equal? (ctx-read ctx-b gsd-session-ctx-plan-box) '((wave 2))))

(test-case "transaction isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (ctx-update! ctx-a gsd-session-ctx-transaction-box (lambda (h) (hash-set h 'key 'val-a)))
  (ctx-update! ctx-b gsd-session-ctx-transaction-box (lambda (h) (hash-set h 'key 'val-b)))
  (check-equal? (hash-ref (ctx-read ctx-a gsd-session-ctx-transaction-box) 'key) 'val-a)
  (check-equal? (hash-ref (ctx-read ctx-b gsd-session-ctx-transaction-box) 'key) 'val-b))

;; ============================================================
;; 8. Thread safety (basic check)
;; ============================================================

(test-case "concurrent mutations don't corrupt state"
  (define ctx (make-gsd-context))
  (define iterations 100)
  ;; Spawn threads that race to set state
  (for ([i (in-range iterations)])
    (thread (lambda () (ctx-write! ctx gsd-session-ctx-state-box i))))
  ;; Give threads time to finish
  (sleep 0.1)
  ;; State should be an integer (one of the racers won)
  (check-pred integer? (ctx-read ctx gsd-session-ctx-state-box)))

;; ============================================================
;; 9. History operations
;; ============================================================

(test-case "history get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-history-box) '() "initial history is empty")
  (ctx-write! ctx gsd-session-ctx-history-box '(a b c))
  (check-equal? (ctx-read ctx gsd-session-ctx-history-box) '(a b c)))

(test-case "history update appends correctly"
  (define ctx (make-gsd-context))
  (ctx-write! ctx gsd-session-ctx-history-box '(a))
  (ctx-update! ctx gsd-session-ctx-history-box (lambda (h) (append h '(b))))
  (check-equal? (ctx-read ctx gsd-session-ctx-history-box) '(a b)))

;; ============================================================
;; 10. Edit limit get/set roundtrip
;; ============================================================

(test-case "edit-limit get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-edit-limit-box) 500 "default edit limit")
  (ctx-write! ctx gsd-session-ctx-edit-limit-box 1000)
  (check-equal? (ctx-read ctx gsd-session-ctx-edit-limit-box) 1000))

;; ============================================================
;; 11. Pinned dir get/set roundtrip
;; ============================================================

(test-case "pinned-dir get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-pinned-dir-box) #f)
  (ctx-write! ctx gsd-session-ctx-pinned-dir-box "/tmp/planning")
  (check-equal? (ctx-read ctx gsd-session-ctx-pinned-dir-box) "/tmp/planning"))

;; ============================================================
;; 12. Event bus get/set roundtrip
;; ============================================================

(test-case "event-bus get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (ctx-read ctx gsd-session-ctx-event-bus-box) #f)
  (define fake-bus (hasheq 'type 'event-bus))
  (ctx-write! ctx gsd-session-ctx-event-bus-box fake-bus)
  (check-equal? (ctx-read ctx gsd-session-ctx-event-bus-box) fake-bus))
