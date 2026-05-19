#lang racket

;; BOUNDARY: integration
;; v0.45.11 W1: Tests for busy-state watchdog logic
;; v0.45.12 L4: Updated to test the extracted check-busy-watchdog function
;; directly instead of replicating logic.

(require rackunit
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../tui/state-ui.rkt"
         ;; v0.45.12 L4: Import the actual watchdog function
         (only-in "../tui/tui-render-loop.rkt" check-busy-watchdog current-busy-watchdog-ms)
         ;; v0.45.14: event constructor for apply-event-to-state tests
         (only-in "tui/event-simulator.rkt" make-test-event))

;; Helper: create a minimal ui-state for testing
(define (make-test-state #:busy? [busy? #f] #:busy-since [since #f])
  (define base (initial-ui-state #:session-id "test-session" #:model-name "test-model"))
  (define with-busy
    (if busy?
        (set-busy (set-busy-since base since) #t)
        base))
  with-busy)

;; ============================================================
;; L4: Tests using the extracted check-busy-watchdog function
;; ============================================================

(test-case "watchdog: non-busy state returns #f"
  (define st (make-test-state #:busy? #f))
  (define result (check-busy-watchdog st (current-inexact-milliseconds) (* 30 60 1000)))
  (check-false result))

(test-case "watchdog: busy with recent timestamp returns #f"
  (define now (current-inexact-milliseconds))
  (define st (make-test-state #:busy? #t #:busy-since now))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  (check-false result))

(test-case "watchdog: busy with expired timestamp returns cleared state"
  (define now (current-inexact-milliseconds))
  (define thirty-one-min-ago (- now (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result)
  ;; Verify busy? cleared
  (check-false (ui-state-busy? result))
  ;; Verify status message set
  (check-equal? (ui-state-status-message result) "watchdog: busy timeout")
  ;; Verify streaming text cleared
  (check-false (ui-state-streaming-text result))
  (check-false (ui-state-streaming-thinking result))
  ;; Verify pending-tool-name cleared
  (check-false (ui-state-pending-tool-name result)))

(test-case "watchdog: cleared state has watchdog transcript entry"
  (define now (current-inexact-milliseconds))
  (define thirty-one-min-ago (- now (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result)
  (define entries (ui-state-transcript result))
  (define watchdog-entry
    (findf (lambda (e)
             (and (eq? (transcript-entry-kind e) 'system)
                  (hash-ref (transcript-entry-meta e) 'watchdog #f)))
           entries))
  (check-not-false watchdog-entry))

(test-case "watchdog: busy?=#t but busy-since=#f returns #f (edge case)"
  ;; busy-since is #f but busy? is #t — should NOT fire
  (define st (make-test-state #:busy? #t #:busy-since #f))
  (define now (current-inexact-milliseconds))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  (check-false result))

(test-case "watchdog: exactly at threshold does NOT fire"
  ;; elapsed == threshold → (> elapsed threshold) is false → no fire
  (define threshold (* 30 60 1000))
  (define now (+ 1000 threshold)) ;; now - since = threshold exactly
  (define st (make-test-state #:busy? #t #:busy-since 1000))
  (define result (check-busy-watchdog st now threshold))
  (check-false result))

(test-case "watchdog: 1ms past threshold fires"
  (define threshold (* 30 60 1000))
  (define since 1000)
  (define now (+ since threshold 1)) ;; exactly 1ms past
  (define st (make-test-state #:busy? #t #:busy-since since))
  (define result (check-busy-watchdog st now threshold))
  (check-not-false result))

(test-case "watchdog: idempotent — calling twice on already-cleared state returns #f"
  (define now (current-inexact-milliseconds))
  (define thirty-one-min-ago (- now (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  (define result1 (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result1)
  ;; Result has busy?=#f, so calling again returns #f
  (define result2 (check-busy-watchdog result1 now (* 30 60 1000)))
  (check-false result2))

(test-case "watchdog: current-busy-watchdog-ms parameter has correct default"
  (check-equal? (current-busy-watchdog-ms) (* 30 60 1000)))

(test-case "watchdog: parameter can be overridden"
  (parameterize ([current-busy-watchdog-ms 1000])
    (check-equal? (current-busy-watchdog-ms) 1000)))

;; ============================================================
;; v0.45.13 M2: Transcript entry text and timestamp assertions
;; ============================================================

(test-case "v0.45.13 M2: watchdog transcript entry has correct text and timestamp"
  (define now (+ (current-inexact-milliseconds) (* 31 60 1000)))
  (define st (set-busy-since (set-busy (initial-ui-state) #t) (- now (* 31 60 1000))))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  ;; Verify transcript entry exists
  (define entries (ui-state-transcript result))
  (define watchdog-entry
    (for/first ([e (in-list entries)]
                #:when (and (eq? (transcript-entry-kind e) 'system)
                            (hash-ref (transcript-entry-meta e) 'watchdog #f)))
      e))
  (check-not-false watchdog-entry "watchdog transcript entry exists")
  ;; Verify the text content matches expected message
  (check-equal? (transcript-entry-text watchdog-entry)
                "[Watchdog: busy state timed out — force-cleared after 30 min]")
  ;; Verify timestamp matches the 'now' argument passed to check-busy-watchdog
  (check-equal? (transcript-entry-timestamp watchdog-entry) now)
  ;; Verify busy? is cleared
  (check-false (ui-state-busy? result) "busy? is cleared after watchdog fires")
  ;; Document: busy-since is intentionally NOT cleared (benign — busy?=#f gates the check)
  (check-true (number? (ui-state-busy-since result))
              "busy-since retains old value (not cleared — gated by busy?)"))

(test-case "v0.45.13 M2: existing transcript entries preserved after watchdog"
  ;; Add a pre-existing transcript entry, then fire watchdog, verify both exist
  (define now (+ (current-inexact-milliseconds) (* 31 60 1000)))
  (define base-st (set-busy-since (set-busy (initial-ui-state) #t) (- now (* 31 60 1000))))
  ;; Add a pre-existing entry manually
  (define pre-entry (make-entry 'user "existing message" 1000 (hasheq)))
  (define st (add-transcript-entry base-st pre-entry))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result)
  ;; Should have 2 entries: watchdog entry (newest first) then pre-existing
  (define entries (ui-state-transcript result))
  (check-equal? (length entries) 2)
  ;; Watchdog entry is first (newest, cons'd last)
  (check-equal? (transcript-entry-kind (first entries)) 'system)
  (check-true (hash-ref (transcript-entry-meta (first entries)) 'watchdog #f))
  ;; Pre-existing entry is second
  (check-equal? (transcript-entry-text (second entries)) "existing message"))

;; ============================================================
;; v0.45.14 W0: Rapid-iteration busy-state stuck fix
;; ============================================================

(test-case "v0.45.14: turn-completed always clears busy? regardless of elapsed time"
  ;; Even with busy-since very recent (< 500ms), handle-turn-completed must clear busy?
  (define now (current-inexact-milliseconds))
  (define st
    (set-busy (set-busy-since (initial-ui-state #:session-id "test" #:model-name "m") now) #t))
  ;; Create a turn.completed event
  (define evt (make-test-event "turn.completed" (hasheq) #:time (+ now 100)))
  (define result (apply-event-to-state st evt))
  ;; busy? MUST be #f even though only 100ms elapsed
  (check-false (ui-state-busy? result))
  (check-false (ui-state-busy-since result))
  (check-false (ui-state-pending-tool-name result)))

(test-case "v0.45.14: rapid iteration pattern does not stick busy"
  ;; Simulate 10 rapid turn.started/turn.completed pairs with short elapsed times
  (define base (initial-ui-state #:session-id "test" #:model-name "m"))
  (define (turn-pair st idx)
    (define start-time (+ (* idx 200) 1000000))
    (define end-time (+ start-time 150)) ;; 150ms — well under old 500ms threshold
    (define start-evt (make-test-event "turn.started" (hasheq) #:time start-time))
    (define after-start (apply-event-to-state st start-evt))
    ;; Simulate tool activity
    (define tool-evt
      (make-test-event "tool.call.started" (hasheq 'name "read") #:time (+ start-time 10)))
    (define after-tool (apply-event-to-state after-start tool-evt))
    ;; Complete the turn
    (define end-evt (make-test-event "turn.completed" (hasheq) #:time end-time))
    (apply-event-to-state after-tool end-evt))
  (define final-st
    (for/fold ([st base]) ([i (in-range 10)])
      (turn-pair st i)))
  ;; After 10 rapid iterations, busy? MUST be #f
  (check-false (ui-state-busy? final-st))
  (check-false (ui-state-busy-since final-st)))

(test-case "v0.45.14: watchdog does not fire during active streaming"
  ;; Even with expired busy-since, watchdog returns #f if streaming text is present
  (define now (+ (current-inexact-milliseconds) (* 31 60 1000)))
  (define base (set-busy (set-busy-since (initial-ui-state) (- now (* 31 60 1000))) #t))
  ;; Set streaming text — agent is clearly alive and streaming
  (define st (set-streaming-text base "partial response text..."))
  (define result (check-busy-watchdog st now (* 30 60 1000)))
  ;; Watchdog must NOT fire
  (check-false result))

(test-case "v0.45.14: turn-cancelled clears busy-since"
  (define now (current-inexact-milliseconds))
  (define st
    (set-busy (set-busy-since (initial-ui-state #:session-id "test" #:model-name "m") now) #t))
  (define evt (make-test-event "turn.cancelled" (hasheq) #:time (+ now 100)))
  (define result (apply-event-to-state st evt))
  (check-false (ui-state-busy? result))
  (check-false (ui-state-busy-since result)))
