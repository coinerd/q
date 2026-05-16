#lang racket

;; v0.45.11 W1: Tests for busy-state watchdog logic
;; v0.45.12 L4: Updated to test the extracted check-busy-watchdog function
;; directly instead of replicating logic.

(require rackunit
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../tui/state-ui.rkt"
         ;; v0.45.12 L4: Import the actual watchdog function
         (only-in "../tui/tui-render-loop.rkt" check-busy-watchdog current-busy-watchdog-ms))

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
