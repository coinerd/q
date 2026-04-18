#lang racket/base

;; extensions/telemetry.rkt — Extension performance telemetry (#1224)
;;
;; Provides timing and metrics collection for extension operations.
;; Extensions can use this to measure hook handler execution time,
;; tool call durations, and track performance across sessions.
;;
;; Usage:
;;   (define timer (telemetry-start "my-operation"))
;;   ... do work ...
;;   (telemetry-end timer) ; => telemetry-event with elapsed-ms
;;   (telemetry-report events) ; => summary hash

(require racket/match)

(provide ;; Telemetry event struct
         (struct-out telemetry-event)
         ;; Timing functions
         telemetry-start
         telemetry-end
         ;; Metrics
         telemetry-report
         telemetry-summary)

;; ============================================================
;; Structs
;; ============================================================

;; A recorded telemetry event
(struct telemetry-event
  (operation   ; string — name of the operation
   start-time  ; exact-positive-integer — start timestamp (ms)
   end-time    ; exact-positive-integer — end timestamp (ms)
   elapsed-ms  ; positive-real — elapsed time in ms
   metadata    ; hash — optional metadata
   )
  #:transparent)

;; ============================================================
;; Timing functions
;; ============================================================

;; Start a telemetry timer. Returns a hash with start info.
(define (telemetry-start operation #:metadata [metadata (hasheq)])
  (hasheq 'operation operation
          'start-time (current-inexact-milliseconds)
          'metadata metadata))

;; End a telemetry timer. Takes the hash from telemetry-start.
;; Returns a telemetry-event.
(define (telemetry-end timer)
  (define end-time (current-inexact-milliseconds))
  (define start-time (hash-ref timer 'start-time))
  (define elapsed (- end-time start-time))
  (telemetry-event (hash-ref timer 'operation)
                   start-time
                   end-time
                   elapsed
                   (hash-ref timer 'metadata (hasheq))))

;; ============================================================
;; Metrics
;; ============================================================

;; Generate a report from a list of telemetry events.
;; Groups by operation name and returns per-operation stats.
(define (telemetry-report events)
  (define groups
    (for/fold ([acc (hasheq)])
              ([e (in-list events)])
      (define op (telemetry-event-operation e))
      (define existing (hash-ref acc op '()))
      (hash-set acc op (cons e existing))))
  ;; Compute per-operation stats
  (for/hash ([(op evts) (in-hash groups)])
    (define elapsed-list (map telemetry-event-elapsed-ms evts))
    (define count (length elapsed-list))
    (define total (apply + elapsed-list))
    (values op
            (hasheq 'count count
                    'total-ms total
                    'avg-ms (if (> count 0) (/ total count) 0)
                    'min-ms (apply min elapsed-list)
                    'max-ms (apply max elapsed-list)))))

;; Generate a one-line summary string for a telemetry event.
(define (telemetry-summary event)
  (format "[telemetry] ~a: ~ams"
          (telemetry-event-operation event)
          (telemetry-event-elapsed-ms event)))
