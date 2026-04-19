#lang racket

(require rackunit
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

;; Helper: create a test event
(define (make-test-event ev payload)
  (event "1" ev 1000 "test-session" "test-turn" payload))

;; Helper: create state with streaming text set
(define (state-with-streaming [text "partial response..."])
  (struct-copy ui-state
               (initial-ui-state)
               [streaming-text text]
               [streaming-thinking "thinking..."]
               [busy? #t]
               [pending-tool-name #f]))

(test-case "auto-retry.start clears streaming-text"
  (define state (state-with-streaming "Hello world"))
  (define evt (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3)))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-text next) "auto-retry.start should clear streaming-text"))

(test-case "auto-retry.start clears streaming-thinking"
  (define state (state-with-streaming))
  (define evt (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3)))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-thinking next) "auto-retry.start should clear streaming-thinking"))

(test-case "auto-retry.start preserves busy state"
  (define state (state-with-streaming))
  (define evt (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3)))
  (define next (apply-event-to-state state evt))
  (check-true (ui-state-busy? next) "auto-retry.start should preserve busy state"))

(test-case "error-then-retry: streaming fully cleared"
  ;; Simulate: streaming → runtime.error → auto-retry.start
  (define state (state-with-streaming "Some streamed text"))
  (define err-evt (make-test-event "runtime.error" (hasheq 'error "timeout")))
  (define after-error (apply-event-to-state state err-evt))
  ;; Error already clears streaming, but retry should keep it clear
  (define retry-evt (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3)))
  (define after-retry (apply-event-to-state after-error retry-evt))
  (check-false (ui-state-streaming-text after-retry)
               "streaming-text should remain clear after retry"))

(test-case "runtime.error clears streaming-text (existing behavior)"
  (define state (state-with-streaming "Hello"))
  (define evt (make-test-event "runtime.error" (hasheq 'error "HTTP timeout")))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-text next)))

(test-case "turn.started clears streaming state (existing behavior)"
  (define state (state-with-streaming "Hello"))
  (define evt (make-test-event "turn.started" (hasheq)))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-text next))
  (check-false (ui-state-streaming-thinking next)))

;; ═══════════════════════════════════════════════════════════
;; Wave 2: Regression — error recovery edge cases
;; ═══════════════════════════════════════════════════════════

(test-case "multiple consecutive retries clear streaming each time"
  (define state0 (state-with-streaming "Streaming text 1"))
  (define retry1 (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3)))
  (define state1 (apply-event-to-state state0 retry1))
  (check-false (ui-state-streaming-text state1))
  (define delta1 (make-test-event "model.stream.delta" (hasheq 'delta "Retry stream")))
  (define state2 (apply-event-to-state state1 delta1))
  (check-equal? (ui-state-streaming-text state2) "Retry stream")
  (define retry2 (make-test-event "auto-retry.start" (hasheq 'attempt 2 'max-retries 3)))
  (define state3 (apply-event-to-state state2 retry2))
  (check-false (ui-state-streaming-text state3))
  (define delta2 (make-test-event "model.stream.delta" (hasheq 'delta "Retry 2 stream")))
  (define state4 (apply-event-to-state state3 delta2))
  (check-equal? (ui-state-streaming-text state4) "Retry 2 stream")
  (define retry3 (make-test-event "auto-retry.start" (hasheq 'attempt 3 'max-retries 3)))
  (define state5 (apply-event-to-state state4 retry3))
  (check-false (ui-state-streaming-text state5))
  (check-false (ui-state-streaming-thinking state5)))

(test-case "error during streaming clears all state"
  (define state (state-with-streaming "Active streaming"))
  (define err (make-test-event "runtime.error" (hasheq 'error "Connection reset")))
  (define next (apply-event-to-state state err))
  (check-false (ui-state-streaming-text next))
  (check-false (ui-state-streaming-thinking next))
  (check-false (ui-state-busy? next))
  (check-false (ui-state-pending-tool-name next)))

(test-case "error + retry + immediate user input: state is clean"
  (define s0 (state-with-streaming "text"))
  (define s1 (apply-event-to-state s0 (make-test-event "runtime.error" (hasheq 'error "timeout"))))
  (define s2
    (apply-event-to-state s1 (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3))))
  (define s3 (apply-event-to-state s2 (make-test-event "turn.completed" (hasheq))))
  (check-false (ui-state-streaming-text s3))
  (check-false (ui-state-streaming-thinking s3))
  (check-false (ui-state-pending-tool-name s3))
  (check-false (ui-state-busy? s3)))

(test-case "turn.cancelled clears streaming state"
  (define state (state-with-streaming "Active"))
  (define evt (make-test-event "turn.cancelled" (hasheq)))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-text next))
  (check-false (ui-state-streaming-thinking next))
  (check-false (ui-state-busy? next)))

(test-case "model.stream.completed clears streaming state"
  (define state (state-with-streaming "Streaming"))
  (define evt (make-test-event "model.stream.completed" (hasheq)))
  (define next (apply-event-to-state state evt))
  (check-false (ui-state-streaming-text next))
  (check-false (ui-state-streaming-thinking next)))

(test-case "auto-retry.start appends retry entry to transcript"
  (define state (initial-ui-state))
  (define evt (make-test-event "auto-retry.start" (hasheq 'attempt 2 'max-retries 3)))
  (define next (apply-event-to-state state evt))
  (define entries (ui-state-transcript next))
  (check-equal? (length entries) 1)
  (check-equal? (transcript-entry-kind (car entries)) 'system)
  (check-true (string-contains? (transcript-entry-text (car entries)) "attempt 2/3")))

(test-case "stress: many entries + streaming + retry does not corrupt state"
  (define s0
    (for/fold ([s (initial-ui-state)]) ([i (in-range 100)])
      (apply-event-to-state s
                            (make-test-event "assistant.message.completed"
                                             (hasheq 'content (format "Message ~a" i))))))
  (define s1 (apply-event-to-state s0 (make-test-event "model.stream.delta" (hasheq 'delta "Hello"))))
  (check-equal? (ui-state-streaming-text s1) "Hello")
  (define s2
    (apply-event-to-state s1 (make-test-event "auto-retry.start" (hasheq 'attempt 1 'max-retries 3))))
  (check-false (ui-state-streaming-text s2))
  (check-false (ui-state-streaming-thinking s2))
  (check-equal? (length (ui-state-transcript s2)) 101))
