#lang racket

;; @speed fast  ;; @suite tui

;; BOUNDARY: integration
;; v0.45.11 W1: Tests for busy-state watchdog logic
;; v0.45.12 L4: Updated to test the extracted check-busy-watchdog function
;; directly instead of replicating logic.

(require rackunit
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../tui/state-ui.rkt"
         ;; v0.45.12 L4: Import the actual watchdog function
         (only-in "../tui/tui-render-loop.rkt"
                  apply-busy-watchdog!
                  check-busy-watchdog
                  current-busy-watchdog-ms)
         (only-in "../tui/context.rkt" make-tui-ctx tui-ctx-goal-cancel-box tui-ctx-ui-state-box)
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

(test-case "watchdog: loop application cancels active goal"
  (define ctx (make-tui-ctx))
  (define now (current-inexact-milliseconds))
  (define expired-since (- now (* 31 60 1000)))
  (set-box! (tui-ctx-ui-state-box ctx) (make-test-state #:busy? #t #:busy-since expired-since))
  (check-false (unbox (tui-ctx-goal-cancel-box ctx)))
  (define fired? (apply-busy-watchdog! ctx now (* 30 60 1000)))
  (check-true fired?)
  (check-true (unbox (tui-ctx-goal-cancel-box ctx)))
  (check-false (ui-state-busy? (unbox (tui-ctx-ui-state-box ctx)))))

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
  ;; busy-since is now cleared to #f when watchdog fires (v0.85.x fix:
  ;; prevents watchdog spam when tool events re-set busy?=#t with stale timestamp)
  (check-false (ui-state-busy-since result) "busy-since cleared to #f after watchdog fires"))

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

;; ============================================================
;; v0.85.x: Watchdog spam fix — set-busy clears/resets busy-since
;; ============================================================

(test-case "v0.85.x: set-busy #f clears busy-since"
  ;; When busy is cleared, busy-since must also be cleared to prevent
  ;; watchdog from re-triggering when tool events set busy?=#t again
  (define now (current-inexact-milliseconds))
  (define st (set-busy-since (set-busy (initial-ui-state) #t) now))
  (check-true (ui-state-busy? st))
  (check-equal? (ui-state-busy-since st) now)
  ;; Clear busy
  (define cleared (set-busy st #f))
  (check-false (ui-state-busy? cleared))
  (check-false (ui-state-busy-since cleared)))

(test-case "v0.85.x: set-busy #t sets busy-since when currently #f"
  ;; When setting busy?=#t and busy-since is #f, set-busy must initialize
  ;; busy-since to the current time (so watchdog can track elapsed duration)
  (define st (set-busy (initial-ui-state) #t))
  (check-true (ui-state-busy? st))
  (check-true (number? (ui-state-busy-since st)))
  ;; busy-since should be recent (within last 5 seconds)
  (check-true (< (- (current-inexact-milliseconds) (ui-state-busy-since st)) 5000)))

(test-case "v0.85.x: set-busy #t preserves existing busy-since"
  ;; When setting busy?=#t and busy-since already has a value, preserve it
  ;; (e.g. handle-turn-started set it to the event timestamp)
  (define ts 1234567890)
  (define st (set-busy (set-busy-since (initial-ui-state) ts) #t))
  (check-true (ui-state-busy? st))
  (check-equal? (ui-state-busy-since st) ts))

(test-case "v0.85.x: watchdog spam prevention — cleared busy-since prevents re-fire"
  ;; This is the core bug scenario: watchdog fires, clears busy? and busy-since,
  ;; then a tool event re-sets busy?=#t. Since set-busy now sets a FRESH
  ;; busy-since, the watchdog must NOT fire on the next check.
  (define now (current-inexact-milliseconds))
  (define thirty-one-min-ago (- now (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  ;; Watchdog fires
  (define result1 (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result1)
  (check-false (ui-state-busy? result1))
  (check-false (ui-state-busy-since result1))
  ;; Tool event re-sets busy (simulates goal thread still running)
  (define re-busy (set-busy result1 #t))
  (check-true (ui-state-busy? re-busy))
  ;; busy-since should now be fresh, not the 31-min-ago timestamp
  (check-true (number? (ui-state-busy-since re-busy)))
  (check-true (> (ui-state-busy-since re-busy) (- now 5000)))
  ;; Watchdog must NOT fire on the new state (busy-since is recent)
  (define result2 (check-busy-watchdog re-busy now (* 30 60 1000)))
  (check-false result2 "watchdog must not re-fire after tool event sets fresh busy-since"))

(test-case "v0.85.x: watchdog fires only once per busy period"
  ;; After watchdog fires and clears state, calling check-busy-watchdog again
  ;; on the already-cleared state must return #f (no duplicate entries)
  (define now (current-inexact-milliseconds))
  (define thirty-one-min-ago (- now (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  ;; First fire
  (define result1 (check-busy-watchdog st now (* 30 60 1000)))
  (check-not-false result1)
  ;; Count watchdog entries in result1
  (define watchdog-count-1
    (length (filter (lambda (e) (hash-ref (transcript-entry-meta e) 'watchdog #f))
                    (ui-state-transcript result1))))
  (check-equal? watchdog-count-1 1)
  ;; Second check on cleared state — must NOT add another entry
  (define result2 (check-busy-watchdog result1 now (* 30 60 1000)))
  (check-false result2))
