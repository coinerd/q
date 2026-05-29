#lang racket/base

;; runtime/trace-logger.rkt — v0.15.0
;;
;; Structured trace logger for request-cycle diagnostics.
;; Subscribes to the event bus and writes trace.jsonl entries
;; with sequence numbers, ISO timestamps, and event data.

(require racket/contract
         racket/date
         json
         racket/file
         racket/class
         "../agent/event-bus.rkt"
         "../util/protocol-types.rkt"
         "trace-sink.rkt")

(provide (contract-out
          [make-trace-logger
           (->* (event-bus? path-string?)
                (#:enabled? boolean? #:sink (or/c (is-a?/c trace-sink<%>) #f) #:async? boolean?)
                trace-logger?)]
          [trace-logger? (-> any/c boolean?)]
          [start-trace-logger! (->* (trace-logger?) (#:port (or/c output-port? #f)) void?)]
          [stop-trace-logger! (-> trace-logger? void?)]
          [flush-trace-logger! (-> trace-logger? void?)]))

;; ============================================================
;; Trace logger struct
;; ============================================================

(struct trace-logger
        (bus session-dir
             enabled?
             [seq #:mutable]
             [sub-id #:mutable]
             [out-port #:mutable]
             [sink #:mutable]
             [async? #:mutable])
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-trace-logger bus
                           session-dir
                           #:enabled? [enabled? #t]
                           #:sink [sink #f]
                           #:async? [async? #f])
  (trace-logger bus session-dir enabled? 0 #f #f sink async?))

;; ============================================================
;; Start / Stop
;; ============================================================

(define (start-trace-logger! logger #:port [out-port #f])
  (when (trace-logger-enabled? logger)
    (define bus (trace-logger-bus logger))
    (define session-dir (trace-logger-session-dir logger))
    ;; Close existing port if re-starting (prevents double-start leak)
    (define old-out (trace-logger-out-port logger))
    (when old-out
      (close-output-port old-out)
      (set-trace-logger-out-port! logger #f))
    (define out
      (or out-port
          (let ([trace-path (build-path session-dir "trace.jsonl")])
            ;; Ensure parent dir exists
            (make-directory* session-dir)
            ;; Open output port in append mode
            (open-output-file trace-path #:exists 'append))))
    (set-trace-logger-out-port! logger out)
    ;; v0.70.4: wrap sink in async-trace-sink% when async? is enabled
    (when (and (trace-logger-async? logger) (not (trace-logger-sink logger)))
      ;; Close the port we just opened — async sink manages its own file handle
      (close-output-port out)
      (set-trace-logger-out-port! logger #f)
      (define trace-path (build-path (trace-logger-session-dir logger) "trace.jsonl"))
      (define file-sink (new json-file-trace-sink% [path trace-path]))
      (set-trace-logger-sink! logger (new async-trace-sink% [inner-sink file-sink])))
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

;; T02: Synchronous flush for deterministic testing.
;; Ensures all buffered trace writes are written to disk.
(define (flush-trace-logger! logger)
  (define sink (trace-logger-sink logger))
  (when (and sink (object? sink))
    (send sink trace-flush!))
  (define out (trace-logger-out-port logger))
  (when out
    (flush-output out)))

;; ============================================================
;; Event handler
;; ============================================================

(define (handle-event! logger evt)
  (define out (trace-logger-out-port logger))
  (define sink (trace-logger-sink logger))
  (when (or out (and sink (object? sink)))
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
              (sanitize-for-json (event-payload evt))))
    ;; v0.15.1: Wrap write-json in error handler to prevent
    ;; partial writes from corrupting the JSONL file.
    (cond
      [(and sink (object? sink))
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning "trace-logger: sink write error seq=~a: ~a"
                                                 seq
                                                 (exn-message e)))])
         (send sink trace-write! entry))]
      [out
       (with-handlers ([exn:fail? (lambda (e)
                                    (log-warning
                                     "trace-logger: skipping non-serializable event seq=~a: ~a"
                                     seq
                                     (exn-message e)))])
         (write-json entry out)
         (newline out)
         (flush-output out))])))

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

;; v0.15.1: Sanitize values for JSON serialization.
;; Recursively converts non-jsexpr values (structs, etc.) to safe representations
;; to prevent write-json partial writes that corrupt the JSONL file.
(define (sanitize-for-json v)
  (cond
    [(jsexpr? v) v]
    [(event? v)
     (hasheq 'phase (event-ev v) 'seq "nested-event" 'data (sanitize-for-json (event-payload v)))]
    [(struct? v) (format "<~a>" (object-name v))]
    [(procedure? v) "<procedure>"]
    [(hash? v)
     (for/hasheq ([(k val) (in-hash v)])
       (values k (sanitize-for-json val)))]
    [(list? v) (map sanitize-for-json v)]
    [(pair? v) (cons (sanitize-for-json (car v)) (sanitize-for-json (cdr v)))]
    [else (format "<unsupported:~a>" v)]))
