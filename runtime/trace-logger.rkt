#lang racket/base

;; runtime/trace-logger.rkt — v0.15.0
;;
;; Structured trace logger for request-cycle diagnostics.
;; Subscribes to the event bus and writes trace.jsonl entries
;; with sequence numbers, ISO timestamps, and event data.

(require racket/date
         json
         racket/file
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt")

(provide make-trace-logger
         trace-logger?
         start-trace-logger!
         stop-trace-logger!)

;; ============================================================
;; Trace logger struct
;; ============================================================

(struct trace-logger
        (bus session-dir enabled? [seq #:mutable] [sub-id #:mutable] [out-port #:mutable])
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-trace-logger bus session-dir #:enabled? [enabled? #t])
  (trace-logger bus session-dir enabled? 0 #f #f))

;; ============================================================
;; Start / Stop
;; ============================================================

(define (start-trace-logger! logger)
  (when (trace-logger-enabled? logger)
    (define bus (trace-logger-bus logger))
    (define session-dir (trace-logger-session-dir logger))
    (define trace-path (build-path session-dir "trace.jsonl"))
    ;; Ensure parent dir exists
    (make-directory* session-dir)
    ;; Open output port in append mode
    (define out (open-output-file trace-path #:exists 'append))
    (set-trace-logger-out-port! logger out)
    ;; Subscribe to all events
    (define sub-id (subscribe! bus (lambda (evt) (handle-event! logger evt))))
    (set-trace-logger-sub-id! logger sub-id)))

(define (stop-trace-logger! logger)
  (when (trace-logger-enabled? logger)
    (define sub-id (trace-logger-sub-id logger))
    (when sub-id
      (unsubscribe! (trace-logger-bus logger) sub-id)
      (set-trace-logger-sub-id! logger #f))
    (define out (trace-logger-out-port logger))
    (when out
      (close-output-port out)
      (set-trace-logger-out-port! logger #f))))

;; ============================================================
;; Event handler
;; ============================================================

(define (handle-event! logger evt)
  (define out (trace-logger-out-port logger))
  (when out
    (define seq (add1 (trace-logger-seq logger)))
    (set-trace-logger-seq! logger seq)
    (define entry
      (hasheq 'ts
              (seconds->iso8601 (event-time evt))
              'seq
              seq
              'phase
              (event-ev evt)
              'sessionId
              (event-session-id evt)
              'turnId
              (or (event-turn-id evt) 'null)
              'data
              (event-payload evt)))
    (write-json entry out)
    (newline out)
    (flush-output out)))

;; ============================================================
;; Helpers
;; ============================================================

(define (seconds->iso8601 secs)
  (define d (seconds->date secs #f))
  (format "~a-~a-~aT~a:~a:~aZ"
          (date-year d)
          (~pad2 (date-month d))
          (~pad2 (date-day d))
          (~pad2 (date-hour d))
          (~pad2 (date-minute d))
          (~pad2 (date-second d))))

(define (~pad2 n)
  (if (< n 10)
      (format "0~a" n)
      (number->string n)))
