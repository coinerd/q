#lang racket

;; tests/test-trace-logger.rkt — v0.15.0 Wave 0
;;
;; TDD tests for the trace logger core module.

(require rackunit
         racket/file
         racket/port
         racket/string
         json
         "../agent/event-bus.rkt"
         "../runtime/trace-logger.rkt"
         "../util/protocol-types.rkt")

(define (make-temp-dir)
  (define tmp (find-system-path 'temp-dir))
  (define dir (make-temporary-file "trace-test-~a" 'directory tmp))
  dir)

(define (read-trace-jsonl dir)
  (define path (build-path dir "trace.jsonl"))
  (if (file-exists? path)
      (filter-map (lambda (line)
                    (if (and (string? line) (> (string-length line) 0))
                        (string->jsexpr line)
                        #f))
                  (file->lines path))
      '()))

;; ============================================================
;; Core: make-trace-logger subscribes to bus and writes JSONL
;; ============================================================

(test-case "trace-logger writes events to trace.jsonl"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  ;; Publish a test event
  (publish! bus (make-event "test.event" (current-seconds) "sess-1" #f (hasheq 'key "value")))
  (flush-trace-logger! logger) ; allow async write
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1 "should have at least one trace entry")
  (define entry (car entries))
  (check-equal? (hash-ref entry 'phase "test.event") "test.event")
  (check-equal? (hash-ref entry 'sessionId #f) "sess-1")
  (delete-directory/files dir))

(test-case "trace entries have sequence numbers"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  (publish! bus (make-event "evt.1" (current-seconds) "s1" #f (hasheq)))
  (publish! bus (make-event "evt.2" (current-seconds) "s1" #f (hasheq)))
  (publish! bus (make-event "evt.3" (current-seconds) "s1" #f (hasheq)))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 3)
  ;; Sequence numbers should be 1, 2, 3
  (check-equal? (hash-ref (first entries) 'seq #f) 1)
  (check-equal? (hash-ref (second entries) 'seq #f) 2)
  (check-equal? (hash-ref (third entries) 'seq #f) 3)
  (delete-directory/files dir))

(test-case "trace entries have ISO timestamps"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  (publish! bus (make-event "test.ts" (current-seconds) "s1" #f (hasheq)))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1)
  (define ts (hash-ref (car entries) 'ts #f))
  (check-not-false ts "should have ts field")
  ;; ISO format check: starts with year
  (check-not-false (regexp-match #rx"^202[0-9]-" ts))
  (delete-directory/files dir))

(test-case "disabled logger does not write any file"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #f))
  (start-trace-logger! logger)
  (publish! bus (make-event "test.disabled" (current-seconds) "s1" #f (hasheq)))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define trace-path (build-path dir "trace.jsonl"))
  (check-false (file-exists? trace-path) "disabled logger should not create file")
  (delete-directory/files dir))

(test-case "trace entry preserves payload data"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  (publish!
   bus
   (make-event "tool.call.started" (current-seconds) "s1" #f (hasheq 'tool "write" 'arguments '())))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1)
  (define data (hash-ref (car entries) 'data #f))
  (check-not-false data)
  (check-equal? (hash-ref data 'tool #f) "write")
  (delete-directory/files dir))

;; ============================================================
;; v0.15.1: Sanitization tests
;; ============================================================

(test-case "sanitize-for-json: struct payload produces valid JSON"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  ;; Publish event with a nested event struct in the payload
  ;; (this is what causes malformed JSONL in production)
  (define inner-evt (make-event "nested.event" (current-seconds) "s2" #f (hasheq 'x 1)))
  (publish!
   bus
   (make-event "outer.event" (current-seconds) "s1" #f (hasheq 'nested inner-evt 'plain "text")))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1)
  (define data (hash-ref (car entries) 'data #f))
  (check-not-false data)
  ;; The nested event should be sanitized to a hash, not cause corruption
  (check-equal? (hash-ref data 'plain #f) "text")
  (define nested (hash-ref data 'nested #f))
  (check-not-false nested "nested event should be sanitized to a hash")
  (check-equal? (hash-ref nested 'phase #f) "nested.event")
  (delete-directory/files dir))

(test-case "sanitize-for-json: 100 rapid events produce 100 valid JSON lines"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  (for ([i (in-range 100)])
    (publish! bus (make-event (format "rapid.~a" i) (current-seconds) "s1" #f (hasheq 'i i))))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check-equal? (length entries) 100 "all 100 events should produce valid JSON")
  (delete-directory/files dir))

(test-case "sanitize-for-json: non-jsexpr values are converted to strings"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  ;; Publish with a procedure value in payload (not jsexpr)
  (publish!
   bus
   (make-event "test.procedure" (current-seconds) "s1" #f (hasheq 'fn (lambda (x) x) 'valid "yes")))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1)
  (define data (hash-ref (car entries) 'data #f))
  (check-not-false data)
  ;; fn should be sanitized to a string, not cause corruption
  (check-not-false (hash-ref data 'fn #f))
  (check-equal? (hash-ref data 'valid #f) "yes")
  (delete-directory/files dir))

(test-case "circuit breaker does not disable trace logger after sanitize fix"
  ;; Simulate what happens in production: many events with struct payloads
  ;; should NOT trip the circuit breaker after sanitization
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define logger (make-trace-logger bus dir #:enabled? #t))
  (start-trace-logger! logger)
  (for ([i (in-range 20)])
    (define inner-evt (make-event "inner" (current-seconds) "s" #f (hasheq)))
    (publish!
     bus
     (make-event (format "stress.~a" i) (current-seconds) "s1" #f (hasheq 'nested inner-evt 'idx i))))
  (flush-trace-logger! logger)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check-equal? (length entries) 20 "all 20 events with nested structs should be logged")
  (delete-directory/files dir))
