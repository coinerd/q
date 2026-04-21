#lang racket

;; tests/test-tui-exploration-events.rkt — Tests for v0.14.1 TUI event handlers
;; Covers: TC1 findings (iteration.soft-warning, exploration.progress,
;;   context.mid-turn-over-budget, auto-retry.start with errorType)

(require rackunit
         rackunit/text-ui
         "../tui/state.rkt"
         "../util/protocol-types.rkt")

(define (make-test-event ev-type
                         payload
                         #:time [time 1000]
                         #:session-id [sid "test-session"]
                         #:turn-id [tid "turn-1"])
  (event 1 ev-type time sid tid payload))

(define-simple-check (check-contains? str substr) (string-contains? str substr))

(define-test-suite
 test-tui-exploration-events
 ;; ============================================================
 ;; iteration.soft-warning
 ;; ============================================================
 (test-case "soft-warning: adds system entry with iteration count"
   (define s0 (initial-ui-state))
   (define evt (make-test-event "iteration.soft-warning" (hash 'iteration 15 'remaining 5)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds an entry")
   (define entry (last entries))
   (check-equal? (transcript-entry-kind entry) 'system)
   (check-contains? (transcript-entry-text entry) "15")
   (check-contains? (transcript-entry-text entry) "5"))
 (test-case "soft-warning: handles missing fields gracefully"
   (define s0 (initial-ui-state))
   (define evt (make-test-event "iteration.soft-warning" (hash)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds entry even with missing fields"))
 ;; ============================================================
 ;; exploration.progress
 ;; ============================================================
 (test-case "exploration.progress: shows tool count and names"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "exploration.progress"
                      (hash 'consecutive-tools 3 'tool-names '("read" "edit" "bash"))))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds an entry")
   (define entry (last entries))
   (check-equal? (transcript-entry-kind entry) 'system)
   (check-contains? (transcript-entry-text entry) "3")
   (check-contains? (transcript-entry-text entry) "read")
   (check-contains? (transcript-entry-text entry) "bash"))
 (test-case "exploration.progress: handles empty tool names"
   (define s0 (initial-ui-state))
   (define evt (make-test-event "exploration.progress" (hash 'consecutive-tools 0 'tool-names '())))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds entry even with empty tools"))
 ;; ============================================================
 ;; context.mid-turn-over-budget
 ;; ============================================================
 (test-case "mid-turn-over-budget: shows token estimate and budget"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "context.mid-turn-over-budget" (hash 'estimated-tokens 95000 'budget 90000)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds an entry")
   (define entry (last entries))
   (check-equal? (transcript-entry-kind entry) 'system)
   (check-contains? (transcript-entry-text entry) "95000")
   (check-contains? (transcript-entry-text entry) "90000"))
 (test-case "mid-turn-over-budget: handles missing fields gracefully"
   (define s0 (initial-ui-state))
   (define evt (make-test-event "context.mid-turn-over-budget" (hash)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0 "adds entry even with missing fields"))
 ;; ============================================================
 ;; auto-retry.start with errorType
 ;; ============================================================
 (test-case "auto-retry.start: timeout error type label"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "auto-retry.start" (hash 'attempt 1 'max-retries 3 'errorType 'timeout)))
   (define s1 (apply-event-to-state s0 evt))
   (define entries (ui-state-transcript s1))
   (check > (length entries) 0)
   (define entry (last entries))
   (check-equal? (transcript-entry-kind entry) 'system)
   (check-contains? (transcript-entry-text entry) "LLM timeout"))
 (test-case "auto-retry.start: rate-limit error type label"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "auto-retry.start" (hash 'attempt 2 'max-retries 3 'errorType 'rate-limit)))
   (define s1 (apply-event-to-state s0 evt))
   (define entry (last (ui-state-transcript s1)))
   (check-contains? (transcript-entry-text entry) "rate limited"))
 (test-case "auto-retry.start: context-overflow error type label"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "auto-retry.start"
                      (hash 'attempt 1 'max-retries 3 'errorType 'context-overflow)))
   (define s1 (apply-event-to-state s0 evt))
   (define entry (last (ui-state-transcript s1)))
   (check-contains? (transcript-entry-text entry) "context too large"))
 (test-case "auto-retry.start: provider-error error type label"
   (define s0 (initial-ui-state))
   (define evt
     (make-test-event "auto-retry.start" (hash 'attempt 1 'max-retries 3 'errorType 'provider-error)))
   (define s1 (apply-event-to-state s0 evt))
   (define entry (last (ui-state-transcript s1)))
   (check-contains? (transcript-entry-text entry) "server error"))
 (test-case "auto-retry.start: no errorType falls back to generic"
   (define s0 (initial-ui-state))
   (define evt (make-test-event "auto-retry.start" (hash 'attempt 1 'max-retries 3)))
   (define s1 (apply-event-to-state s0 evt))
   (define entry (last (ui-state-transcript s1)))
   ;; Generic fallback: "[retry: attempt 1/3]" — no type-specific label like "LLM timeout"
   (check-contains? (transcript-entry-text entry) "attempt 1/3")
   (check-false (string-contains? (transcript-entry-text entry) "LLM timeout"))
   (check-false (string-contains? (transcript-entry-text entry) "rate limited")))
 (test-case "auto-retry.start: clears streaming state"
   (define s0
     (struct-copy ui-state
                  (initial-ui-state)
                  [streaming-text "partial..."]
                  [streaming-thinking "thinking..."]))
   (define evt (make-test-event "auto-retry.start" (hash 'attempt 1 'max-retries 3)))
   (define s1 (apply-event-to-state s0 evt))
   (check-false (ui-state-streaming-text s1))
   (check-false (ui-state-streaming-thinking s1))))

(run-test test-tui-exploration-events)
