#lang racket/base

;; @speed fast  ;; @suite tui

;; BOUNDARY: io

;; tests/tui/test-event-reducer-registry.rkt -- Event reducer registry tests (W-07)

(require rackunit
         "../../tui/state-events.rkt"
         "../../tui/state-types.rkt"
         "../../util/message/protocol-types.rkt")

(define (make-test-event ev-type payload #:time [time 1000])
  (event 1 ev-type time "test-session" "turn-1" payload))

;; ============================================================
;; Registry infrastructure
;; ============================================================

(test-case "event-reducer-registered? returns #t for known types"
  (check-true (event-reducer-registered? "assistant.message.completed"))
  (check-true (event-reducer-registered? "tool.call.started"))
  (check-true (event-reducer-registered? "runtime.error"))
  (check-true (event-reducer-registered? "model.stream.delta"))
  (check-true (event-reducer-registered? "turn.started"))
  (check-true (event-reducer-registered? "turn.completed")))

(test-case "event-reducer-registered? returns #f for unknown types"
  (check-false (event-reducer-registered? "unknown.event.type"))
  (check-false (event-reducer-registered? ""))
  (check-false (event-reducer-registered? "not.real")))

(test-case "register-event-reducer! adds new handler"
  (define custom-called (box #f))
  (register-event-reducer! "test.custom.event"
                           (lambda (state evt)
                             (set-box! custom-called #t)
                             state))
  (check-true (event-reducer-registered? "test.custom.event"))
  (define st0 (initial-ui-state))
  (apply-event-to-state st0 (make-test-event "test.custom.event" (hash)))
  (check-true (unbox custom-called)))

(test-case "apply-event-to-state falls through for unregistered types"
  (define st0 (initial-ui-state))
  (define st1 (apply-event-to-state st0 (make-test-event "totally.unknown" (hasheq 'foo 'bar))))
  ;; Should return state unchanged
  (check-eq? st0 st1))

;; ============================================================
;; Dispatch correctness (spot-check key handlers)
;; ============================================================

(test-case "model.stream.delta accumulates streaming text"
  (define st0 (initial-ui-state))
  (define st1
    (apply-event-to-state st0 (make-test-event "model.stream.delta" (hasheq 'delta "Hello"))))
  (check-equal? (ui-state-streaming-text st1) "Hello")
  (check-true (ui-state-busy? st1))
  (define st2
    (apply-event-to-state st1 (make-test-event "model.stream.delta" (hasheq 'delta " World"))))
  (check-equal? (ui-state-streaming-text st2) "Hello World"))

(test-case "turn.started sets busy flags"
  (define st0 (initial-ui-state))
  (define st1 (apply-event-to-state st0 (make-test-event "turn.started" (hash))))
  (check-true (ui-state-busy? st1))
  (check-false (ui-state-streaming-text st1)))

(test-case "event-reducer-registry: turn.cancelled clears busy and streaming"
  (define st0 (set-streaming-text (set-busy (initial-ui-state) #t) "partial text"))
  (define st1 (apply-event-to-state st0 (make-test-event "turn.cancelled" (hash))))
  (check-false (ui-state-busy? st1))
  (check-false (ui-state-streaming-text st1)))

(test-case "model.request.started sets busy"
  (define st0 (initial-ui-state))
  (define st1 (apply-event-to-state st0 (make-test-event "model.request.started" (hash))))
  (check-true (ui-state-busy? st1)))

(test-case "context.built sets context-tokens"
  (define st0 (initial-ui-state))
  (define st1 (apply-event-to-state st0 (make-test-event "context.built" (hasheq 'tokenCount 5000))))
  (check-equal? (ui-state-context-tokens st1) 5000))

(test-case "queue.status-update stores payload"
  (define st0 (initial-ui-state))
  (define payload (hasheq 'steering 2 'followup 1))
  (define st1 (apply-event-to-state st0 (make-test-event "queue.status-update" payload)))
  (check-equal? (ui-state-queue-counts st1) payload))

(test-case "compaction lifecycle: started/complete toggles status"
  (define st0 (initial-ui-state))
  (define st1 (apply-event-to-state st0 (make-test-event "compaction.started" (hash))))
  (check-equal? (ui-state-status-message st1) "Compacting...")
  (define st2 (apply-event-to-state st1 (make-test-event "compaction.completed" (hash))))
  (check-false (ui-state-status-message st2)))

(test-case "context.pressure sets pressure level and percent"
  (define st0 (initial-ui-state))
  (define st1
    (apply-event-to-state st0
                          (make-test-event "context.pressure"
                                           (hasheq 'level 'yellow 'usage-percent 72.5))))
  (check-equal? (ui-state-context-pressure-level st1) 'yellow)
  (check-= (ui-state-context-pressure-percent st1) 72.5 0.01))

(test-case "context.pressure registered"
  (check-true (event-reducer-registered? "context.pressure")))
