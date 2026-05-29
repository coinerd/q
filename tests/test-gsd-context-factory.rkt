#lang racket/base

;; BOUNDARY: integration

;; tests/test-gsd-context-factory.rkt — Tests for struct-based GSD context
;;
;; v0.29.4 W0: Original tests for closure-encapsulated context.
;; v0.32.5 W1: Rewritten for struct-based gsd-session-ctx.
;; v0.69.4 W0: Updated to use gsd-ctx-* named accessors.

(require rackunit
         racket/set
         (only-in "../extensions/gsd/runtime-state-types.rkt" gsd-runtime-state gsd-runtime-state? gsd-runtime-state-mode gsd-runtime-state-current-wave)
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-session-ctx?
                  gsd-ctx-state
                  gsd-ctx-set-state!
                  gsd-ctx-plan
                  gsd-ctx-set-plan!
                  gsd-ctx-pinned-dir
                  gsd-ctx-set-pinned-dir!
                  gsd-ctx-edit-limit
                  gsd-ctx-set-edit-limit!
                  gsd-ctx-event-bus
                  gsd-ctx-set-event-bus!
                  gsd-ctx-history
                  gsd-ctx-set-history!
                  gsd-ctx-busy
                  gsd-ctx-set-busy!
                  gsd-ctx-correlation-id
                  gsd-ctx-set-correlation-id!
                  gsd-ctx-transaction
                  gsd-ctx-set-transaction!
                  gsd-ctx-transaction-update!
                  gsd-ctx-state-update!
                  gsd-ctx-history-update!))

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
  (define initial (gsd-ctx-state ctx))
  (check-pred gsd-runtime-state? initial "initial state is gsd-runtime-state")
  ;; Modify via state-update!
  (gsd-ctx-state-update! ctx (lambda (s) (struct-copy gsd-runtime-state s [mode 'idle])))
  (check-equal? (gsd-runtime-state-mode (gsd-ctx-state ctx)) 'idle))

(test-case "state supports arbitrary values via state-update!"
  (define ctx (make-gsd-context))
  (gsd-ctx-state-update! ctx (lambda (s) (struct-copy gsd-runtime-state s [mode 'executing] [current-wave 2])))
  (check-equal? (gsd-runtime-state-mode (gsd-ctx-state ctx)) 'executing)
  (check-equal? (gsd-runtime-state-current-wave (gsd-ctx-state ctx)) 2))

;; ============================================================
;; 3. Plan data get/set roundtrip
;; ============================================================

(test-case "plan get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-plan ctx) #f "initial plan is #f")
  (gsd-ctx-set-plan! ctx '((wave 1 "do stuff")))
  (check-equal? (gsd-ctx-plan ctx) '((wave 1 "do stuff"))))

;; ============================================================
;; 4. Busy flag get/set roundtrip
;; ============================================================

(test-case "busy? get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-false (gsd-ctx-busy ctx) "initial busy is #f")
  (gsd-ctx-set-busy! ctx #t)
  (check-true (gsd-ctx-busy ctx)))

;; ============================================================
;; 5. Correlation ID get/set roundtrip
;; ============================================================

(test-case "correlation-id get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-correlation-id ctx) #f)
  (gsd-ctx-set-correlation-id! ctx "abc-123")
  (check-equal? (gsd-ctx-correlation-id ctx) "abc-123"))

;; ============================================================
;; 6. Transaction ref/set roundtrip
;; ============================================================

(test-case "transaction update roundtrip"
  (define ctx (make-gsd-context))
  ;; Set a value
  (gsd-ctx-transaction-update! ctx (lambda (h) (hash-set h 'edit-count 42)))
  (check-equal? (hash-ref (gsd-ctx-transaction ctx) 'edit-count) 42))

;; ============================================================
;; 7. Two independent contexts don't interfere
;; ============================================================

(test-case "two independent contexts are isolated"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-state-update! ctx-a (lambda (s) (struct-copy gsd-runtime-state s [mode 'executing])))
  (gsd-ctx-state-update! ctx-b (lambda (s) (struct-copy gsd-runtime-state s [mode 'idle])))
  (check-equal? (gsd-runtime-state-mode (gsd-ctx-state ctx-a)) 'executing)
  (check-equal? (gsd-runtime-state-mode (gsd-ctx-state ctx-b)) 'idle)
  ;; Plans also isolated
  (gsd-ctx-set-plan! ctx-a '((wave 1)))
  (gsd-ctx-set-plan! ctx-b '((wave 2)))
  (check-equal? (gsd-ctx-plan ctx-a) '((wave 1)))
  (check-equal? (gsd-ctx-plan ctx-b) '((wave 2))))

(test-case "transaction isolation between contexts"
  (define ctx-a (make-gsd-context))
  (define ctx-b (make-gsd-context))
  (gsd-ctx-transaction-update! ctx-a (lambda (h) (hash-set h 'key 'val-a)))
  (gsd-ctx-transaction-update! ctx-b (lambda (h) (hash-set h 'key 'val-b)))
  (check-equal? (hash-ref (gsd-ctx-transaction ctx-a) 'key) 'val-a)
  (check-equal? (hash-ref (gsd-ctx-transaction ctx-b) 'key) 'val-b))

;; ============================================================
;; 8. Thread safety (basic check)
;; ============================================================

(test-case "concurrent mutations don't corrupt state"
  (define ctx (make-gsd-context))
  (define iterations 100)
  ;; Spawn threads that race to update state
  (for ([i (in-range iterations)])
    (thread (lambda () (gsd-ctx-state-update! ctx (lambda (s) (struct-copy gsd-runtime-state s [current-wave i]))))))
  ;; Give threads time to finish
  (sleep 0.1)
  ;; State should be a valid gsd-runtime-state with integer current-wave
  (check-pred exact-nonnegative-integer? (gsd-runtime-state-current-wave (gsd-ctx-state ctx))))

;; ============================================================
;; 9. History operations
;; ============================================================

(test-case "history get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-history ctx) '() "initial history is empty")
  (gsd-ctx-set-history! ctx '(a b c))
  (check-equal? (gsd-ctx-history ctx) '(a b c)))

(test-case "history update appends correctly"
  (define ctx (make-gsd-context))
  (gsd-ctx-set-history! ctx '(a))
  (gsd-ctx-history-update! ctx (lambda (h) (append h '(b))))
  (check-equal? (gsd-ctx-history ctx) '(a b)))

;; ============================================================
;; 10. Edit limit get/set roundtrip
;; ============================================================

(test-case "edit-limit get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-edit-limit ctx) 500 "default edit limit")
  (gsd-ctx-set-edit-limit! ctx 1000)
  (check-equal? (gsd-ctx-edit-limit ctx) 1000))

;; ============================================================
;; 11. Pinned dir get/set roundtrip
;; ============================================================

(test-case "pinned-dir get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-pinned-dir ctx) #f)
  (gsd-ctx-set-pinned-dir! ctx "/tmp/planning")
  (check-equal? (gsd-ctx-pinned-dir ctx) "/tmp/planning"))

;; ============================================================
;; 12. Event bus get/set roundtrip
;; ============================================================

(test-case "event-bus get/set roundtrip"
  (define ctx (make-gsd-context))
  (check-equal? (gsd-ctx-event-bus ctx) #f)
  (define fake-bus (hasheq 'type 'event-bus))
  (gsd-ctx-set-event-bus! ctx fake-bus)
  (check-equal? (gsd-ctx-event-bus ctx) fake-bus))
