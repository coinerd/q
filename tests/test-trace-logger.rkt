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
  (sleep 0.1) ; allow async write
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
  (sleep 0.1)
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
  (sleep 0.1)
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
  (sleep 0.1)
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
  (sleep 0.1)
  (stop-trace-logger! logger)
  (define entries (read-trace-jsonl dir))
  (check >= (length entries) 1)
  (define data (hash-ref (car entries) 'data #f))
  (check-not-false data)
  (check-equal? (hash-ref data 'tool #f) "write")
  (delete-directory/files dir))
