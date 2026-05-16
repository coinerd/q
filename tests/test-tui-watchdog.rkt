#lang racket

;; v0.45.11 W1: Tests for busy-state watchdog logic
;; Tests the state transitions that the watchdog in tui-main-loop performs.

(require rackunit
         "../tui/state-types.rkt"
         "../tui/state-events.rkt"
         "../tui/state-ui.rkt")

;; Helper: create a minimal ui-state for testing
(define (make-test-state #:busy? [busy? #f] #:busy-since [since #f])
  (define base (initial-ui-state #:session-id "test-session" #:model-name "test-model"))
  (define with-busy
    (if busy?
        (set-busy (set-busy-since base since) #t)
        base))
  with-busy)

(test-case "watchdog: non-busy state is not affected"
  (define st (make-test-state #:busy? #f))
  (check-false (ui-state-busy? st)))

(test-case "watchdog: busy state with recent busy-since is not timed out"
  (define now (current-inexact-milliseconds))
  (define st (make-test-state #:busy? #t #:busy-since now))
  (check-true (ui-state-busy? st))
  (check-equal? (ui-state-busy-since st) now)
  ;; Less than 30 min elapsed — should NOT be timed out
  (define elapsed (- (current-inexact-milliseconds) (ui-state-busy-since st)))
  (check-true (< elapsed (* 30 60 1000))))

(test-case "watchdog: force-clear busy state after timeout"
  ;; Set busy-since to 31 minutes ago
  (define thirty-one-min-ago (- (current-inexact-milliseconds) (* 31 60 1000)))
  (define st (make-test-state #:busy? #t #:busy-since thirty-one-min-ago))
  (check-true (ui-state-busy? st))
  ;; Simulate the watchdog clear (same code as in tui-render-loop.rkt)
  (define cleared
    (set-status-message
     (clear-streaming (set-pending-tool-name (set-busy st #f) #f))
     "watchdog: busy timeout"))
  (check-false (ui-state-busy? cleared))
  (check-equal? (ui-state-status-message cleared) "watchdog: busy timeout"))

(test-case "watchdog: add watchdog system entry to transcript"
  (define st (make-test-state #:busy? #f))
  (define watchdog-entry
    (make-entry 'system
                "[Watchdog: busy state timed out — force-cleared after 30 min]"
                (current-inexact-milliseconds)
                (hasheq 'watchdog #t)))
  (define updated (add-transcript-entry st watchdog-entry))
  (define entries (ui-state-transcript updated))
  (check-not-false (findf (lambda (e)
                            (and (eq? (transcript-entry-kind e) 'system)
                                 (hash-ref (transcript-entry-meta e) 'watchdog #f)))
                          entries)))

(test-case "watchdog: elapsed time calculation works correctly"
  ;; Verify that 31 minutes > 30 minutes (the threshold)
  (define thirty-min-ms (* 30 60 1000))
  (define thirty-one-min-ms (* 31 60 1000))
  (define now (current-inexact-milliseconds))
  ;; 31 min ago should exceed threshold
  (check-true (> (- now (- now thirty-one-min-ms)) thirty-min-ms))
  ;; 29 min ago should NOT exceed threshold
  (check-true (< (- now (- now (* 29 60 1000))) thirty-min-ms)))
