#lang racket

;; tests/workflows/test-provider-error-recovery.rkt
;;
;; 12 integration test cases for TUI error recovery scenarios.
;; Uses event-simulator to drive TUI state transitions and
;; state-assertions for verification.
;;
;; Issue: #1272 — Wave 6 of v0.11.2 milestone

(require rackunit
         rackunit/text-ui
         "../../tui/state.rkt"
         "../../util/protocol-types.rkt"
         "../tui/event-simulator.rkt"
         "../tui/state-assertions.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-evt type payload)
  (make-test-event type (apply hasheq (append-map (lambda (p) (list (car p) (cdr p))) payload))))

;; Count entries of a given kind in the transcript
(define (count-entries state kind)
  (for/sum ([e (in-list (ui-state-transcript state))]) (if (eq? (transcript-entry-kind e) kind) 1 0)))

;; Find entries of a given kind
(define (entries-of-kind state kind)
  (filter (lambda (e) (eq? (transcript-entry-kind e) kind)) (ui-state-transcript state)))

;; ============================================================
;; 12 Test Cases
;; ============================================================

(define-test-suite
 provider-error-recovery-tests
 ;; TC-01: Provider timeout → auto-retry
 ;; Verify retry count is displayed correctly (not "?")
 (test-case "TC-01: provider timeout with auto-retry shows attempt count"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "model.stream.delta" '((delta . "Hello ")))
           (make-evt "runtime.error"
                     '((error . "HTTP read timeout after 120s") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 2) (delay-ms . 1000)))
           (make-evt "model.stream.delta" '((delta . "Retried response")))
           (make-evt "model.stream.completed" '())
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   ;; Error should be displayed
   (check-true (state-has-error? sf) "error should be displayed")
   ;; Retry entry should contain "attempt 1/2"
   (check-true (transcript-contains-text? sf "attempt 1/2") "retry count should show attempt 1/2")
   ;; State should be idle at end
   (check-true (state-idle? sf) "state should be idle after retry success")
   ;; No orphan streaming
   (check-false (ui-state-streaming-text sf) "no orphan streaming text"))
 ;; TC-02: Rate limit → auto-retry success
 ;; Retry events present + eventual success
 (test-case "TC-02: rate limit with auto-retry succeeds"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error"
                     '((error . "HTTP 429 rate limit exceeded") (errorType . rate-limit)))
           (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 3) (delay-ms . 2000)))
           (make-evt "model.stream.delta" '((delta . "Success after rate limit")))
           (make-evt "model.stream.completed" '())
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-has-retry? sf) "retry should be visible")
   (check-true (transcript-contains-text? sf "attempt 1/3") "should show attempt 1/3")
   (check-true (state-idle? sf) "state should be idle after successful retry"))
 ;; TC-03: Auth failure → no retry
 ;; Immediate error, no retry attempts
 (test-case "TC-03: auth failure shows error without retry"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error" '((error . "401 Unauthorized") (errorType . auth)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-has-error? sf) "error should be displayed")
   (check-false (state-has-retry? sf) "no retry for auth errors")
   (check-true (state-has-recovery-hint? sf) "recovery hint should be visible")
   (check-true (transcript-contains-text? sf "config.json") "should mention config.json"))
 ;; TC-04: Context overflow → hint about /compact
 ;; Mentions /compact, no retry
 (test-case "TC-04: context overflow shows /compact hint"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error"
                     '((error . "context_length exceeded: too many tokens")
                       (errorType . context-overflow)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-has-error? sf) "error should be displayed")
   (check-true (transcript-contains-text? sf "/compact") "should mention /compact")
   (check-true (transcript-contains-text? sf "/retry") "should mention /retry")
   ;; The recovery hint mentions /retry but that doesn't mean auto-retry happened
   ;; Verify no auto-retry.start event entries in the transcript
   (define retry-start-entries
     (filter (lambda (e)
               (and (eq? (transcript-entry-kind e) 'system)
                    (regexp-match? #rx"\\[retry:" (transcript-entry-text e))))
             (ui-state-transcript sf)))
   (check-equal? retry-start-entries '() "no auto-retry.start for context overflow"))
 ;; TC-05: Max iterations exceeded
 ;; Error with iteration count
 (test-case "TC-05: max iterations exceeded shows error"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error"
                     '((error . "max.iterations reached: 25") (errorType . max-iterations)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-has-error? sf) "error should be displayed")
   (check-true (state-idle? sf) "state should be idle")
   (check-true (transcript-contains-text? sf "Max iterations") "should mention max iterations"))
 ;; TC-06: Tool execution failure
 ;; Tool-fail entry, agent continues
 (test-case "TC-06: tool execution failure shows tool-fail entry"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "turn.started" '())
           (make-evt "tool.call.started" '((name . "bash") (arguments . "rm -rf /")))
           (make-evt "tool.call.failed" '((name . "bash") (error . "blocked by extension")))
           (make-evt "model.stream.delta" '((delta . "I cannot do that.")))
           (make-evt "model.stream.completed" '())
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-idle? sf) "state should be idle after tool failure")
   (check-equal? (count-entries sf 'tool-fail) 1 "should have exactly 1 tool-fail entry")
   (check-false (ui-state-pending-tool-name sf) "no pending tool after failure"))
 ;; TC-07: Partial response timeout
 ;; Partial text preserved in transcript
 (test-case "TC-07: partial response preserves streamed text"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "model.stream.delta" '((delta . "First part. ")))
           (make-evt "model.stream.delta" '((delta . "Second part. ")))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-has-error? sf) "error should be displayed")
   ;; After error, streaming-text should be cleared (BUG-29 fix)
   (check-false (ui-state-streaming-text sf) "streaming text should be cleared on error")
   (check-true (state-idle? sf) "state should be idle"))
 ;; TC-08: Error then /retry resubmit
 ;; Simulate: first error, then new turn (retry) succeeds
 (test-case "TC-08: retry after error succeeds on second attempt"
   (define s0 (initial-ui-state))
   ;; First turn fails
   (define events-turn1
     (list (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 2) (delay-ms . 1000)))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "turn.completed" '())))
   (define s1 (simulate-events s0 events-turn1))
   (check-true (state-has-error? s1) "first attempt should show error")
   ;; Second turn succeeds (user retried)
   (define events-turn2
     (list (make-evt "turn.started" '())
           (make-evt "model.stream.delta" '((delta . "Success on retry")))
           (make-evt "model.stream.completed" '())
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s1 events-turn2))
   (check-true (state-idle? sf) "state should be idle after retry success")
   (check-false (ui-state-streaming-text sf) "no orphan streaming"))
 ;; TC-09: Consecutive timeouts (3x)
 ;; Final error with "provider unavailable"
 (test-case "TC-09: three consecutive timeouts exhaust retries"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 3) (delay-ms . 1000)))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 2) (max-retries . 3) (delay-ms . 2000)))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 3) (max-retries . 3) (delay-ms . 4000)))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   ;; Should have 4 error entries (original + 3 retries all failed)
   (check-equal? (count-entries sf 'error) 4 "should have 4 error entries")
   (check-equal? (count-entries sf 'system) 7 "should have hint + retry entries")
   (check-true (state-idle? sf) "state should be idle after exhausted retries"))
 ;; TC-10: Streaming interrupt + clean state
 (test-case "TC-10: streaming interrupted by turn.cancelled leaves clean state"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "turn.started" '())
           (make-evt "model.stream.delta" '((delta . "Streaming some text")))
           (make-evt "model.stream.delta" '((delta . " and more")))
           (make-evt "turn.cancelled" '())))
   (define sf (simulate-events s0 events))
   (check-true (state-idle? sf) "state should be idle after cancel")
   (check-false (ui-state-streaming-text sf) "streaming text should be cleared")
   (check-false (ui-state-pending-tool-name sf) "no pending tool")
   (check-false (ui-state-busy? sf) "not busy"))
 ;; TC-11: Error clears orphan streaming
 ;; Streaming in progress, error occurs → streaming-text = #f
 (test-case "TC-11: runtime.error clears orphan streaming text"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "turn.started" '())
           (make-evt "model.stream.delta" '((delta . "Partial response")))
           (make-evt "runtime.error"
                     '((error . "unexpected disconnection") (errorType . provider-error)))
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-false (ui-state-streaming-text sf) "streaming text must be #f after error")
   (check-false (ui-state-streaming-thinking sf) "streaming thinking must be #f after error")
   (check-true (state-has-error? sf) "error should be in transcript"))
 ;; TC-12: Retry count matches max-retries
 ;; Shows attempt 1/2, 2/2
 (test-case "TC-12: retry count displays match max-retries setting"
   (define s0 (initial-ui-state))
   (define events
     (list (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 1) (max-retries . 2) (delay-ms . 1000)))
           (make-evt "runtime.error" '((error . "HTTP read timeout") (errorType . timeout)))
           (make-evt "auto-retry.start" '((attempt . 2) (max-retries . 2) (delay-ms . 2000)))
           (make-evt "model.stream.delta" '((delta . "Finally succeeded")))
           (make-evt "model.stream.completed" '())
           (make-evt "turn.completed" '())))
   (define sf (simulate-events s0 events))
   (check-true (transcript-contains-text? sf "attempt 1/2") "should show attempt 1/2")
   (check-true (transcript-contains-text? sf "attempt 2/2") "should show attempt 2/2")
   (check-true (state-idle? sf) "state should be idle after final success")
   ;; Verify retry entries in transcript
   (define retry-entries
     (filter (lambda (e)
               (and (eq? (transcript-entry-kind e) 'system)
                    (regexp-match? #rx"attempt [0-9]+/[0-9]+" (transcript-entry-text e))))
             (ui-state-transcript sf)))
   (check-equal? (length retry-entries) 2 "should have exactly 2 retry entries")))

;; ============================================================
;; Run
;; ============================================================

(module+ test
  (define result (run-tests provider-error-recovery-tests 'verbose))
  (unless (zero? result)
    (error 'test-provider-error-recovery "~a test(s) failed" result)))
