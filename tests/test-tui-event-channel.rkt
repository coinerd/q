#lang racket/base

;; tests/test-tui-event-channel.rkt — BF1-ROOT regression test
;;
;; Verifies that the TUI event channel no longer has a bounded counter
;; that silently drops events after 1000. The counter was removed in
;; v0.99.4 (BF1-ROOT fix).

(require rackunit
         rackunit/text-ui
         racket/async-channel
         (only-in racket/list last)
         "../util/event/event.rkt"
         "../util/event/event-bus.rkt")

;; Helper: Subscribe to bus, putting all events into an async-channel
;; (mirrors the fix in tui/tui-init.rkt subscribe-runtime-events!)
(define (make-bus+channel)
  (define bus (make-event-bus))
  (define ch (make-async-channel))
  (subscribe! bus (lambda (evt) (async-channel-put ch evt)))
  (values bus ch))

;; Helper: Drain all events currently in the async-channel
(define (drain-channel ch)
  (let loop ([acc '()])
    (define evt (async-channel-try-get ch))
    (if evt
        (loop (cons evt acc))
        (reverse acc))))

;; Helper: Make a simple test event
(define (make-test-event [n 0])
  (make-event n (current-inexact-milliseconds) #f #f (hasheq 'index n)))

(define suite
  (test-suite "BF1-ROOT: TUI Event Channel (no bounded counter)"

    ;; ── Core fix: events above old 1000 threshold are NOT dropped ──

    (test-case "2000 events all received (was capped at 1000)"
      (define-values (bus ch) (make-bus+channel))
      (for ([i (in-range 2000)])
        (publish! bus (make-test-event i)))
      (sleep 0.1) ; let subscribers process
      (define events (drain-channel ch))
      (check-equal? (length events)
                    2000
                    "All 2000 events must be received — old code would cap at 1000"))

    (test-case "500 events all received (under old threshold)"
      (define-values (bus ch) (make-bus+channel))
      (for ([i (in-range 500)])
        (publish! bus (make-test-event i)))
      (sleep 0.1)
      (define events (drain-channel ch))
      (check-equal? (length events) 500))

    (test-case "5000 events all received"
      (define-values (bus ch) (make-bus+channel))
      (for ([i (in-range 5000)])
        (publish! bus (make-test-event i)))
      (sleep 0.2)
      (define events (drain-channel ch))
      (check-equal? (length events) 5000))

    ;; ── Streaming simulation: completion event received after deltas ──

    (test-case "completion event received after 999 deltas"
      (define-values (bus ch) (make-bus+channel))
      (for ([i (in-range 999)])
        (publish!
         bus
         (make-event 'model.stream.delta (current-inexact-milliseconds) #f #f (hasheq 'delta "x"))))
      ;; The 1000th event is the critical completion event
      (publish! bus (make-event 'stream.turn.completed (current-inexact-milliseconds) #f #f (hasheq)))
      (sleep 0.2)
      (define events (drain-channel ch))
      (check-equal? (length events) 1000)
      ;; The last event must be the completion event
      (define last-evt (list-ref events 999))
      (check-equal? (event-ev last-evt) 'stream.turn.completed))

    (test-case "full streaming simulation (deltas + thinking + completion)"
      (define-values (bus ch) (make-bus+channel))
      ;; Simulate 800 delta events
      (for ([i (in-range 800)])
        (publish! bus
                  (make-event 'model.stream.delta
                              (current-inexact-milliseconds)
                              #f
                              #f
                              (hasheq 'delta "chunk"))))
      ;; 300 thinking events (total now 1100, above old threshold)
      (for ([i (in-range 300)])
        (publish! bus
                  (make-event 'model.stream.thinking
                              (current-inexact-milliseconds)
                              #f
                              #f
                              (hasheq 'delta "think"))))
      ;; Completion
      (publish! bus (make-event 'stream.turn.completed (current-inexact-milliseconds) #f #f (hasheq)))
      (sleep 0.2)
      (define events (drain-channel ch))
      (check-equal? (length events) 1101)
      (define last-evt (last events))
      (check-equal? (event-ev last-evt) 'stream.turn.completed))

    ;; ── Interspersed draining ──

    (test-case "interspersed draining across cycles"
      (define-values (bus ch) (make-bus+channel))
      (define total-received 0)
      ;; Publish 100 at a time, drain between batches
      (for ([batch (in-range 10)])
        (for ([i (in-range 100)])
          (publish! bus (make-test-event (+ (* batch 100) i))))
        (sleep 0.05)
        (set! total-received (+ total-received (length (drain-channel ch)))))
      (check-equal? total-received 1000))

    ;; ── Concurrent publish ──

    (test-case "concurrent publish from 3 threads"
      (define-values (bus ch) (make-bus+channel))
      (define threads
        (for/list ([t (in-range 3)])
          (thread (lambda ()
                    (for ([i (in-range 500)])
                      (publish! bus (make-test-event (+ (* t 500) i))))))))
      (for-each thread-wait threads)
      (sleep 0.2)
      (define events (drain-channel ch))
      (check-equal? (length events) 1500))

    ;; ── Memory check ──

    (test-case "10K events drainable without error"
      (define-values (bus ch) (make-bus+channel))
      (for ([i (in-range 10000)])
        (publish! bus (make-test-event i)))
      (sleep 0.3)
      (define events (drain-channel ch))
      (check-equal? (length events) 10000))))

(run-tests suite)
