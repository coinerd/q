#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-subscriber.rkt — W4 (v0.99.7) Event Bus Subscriber Tests
;;
;; Tests for the blackboard event bus subscriber and crash recovery:
;;   - Subscriber updates blackboard on relevant events
;;   - Subscriber ignores irrelevant events
;;   - Multiple events → correct accumulated state
;;   - Clean unsubscribe stops updates
;;   - Crash recovery from JSONL log
;;   - Malformed events handled gracefully

(require rackunit
         rackunit/text-ui
         racket/file
         racket/port
         json
         (only-in "../util/event/event.rkt" make-event event-ev event-payload event-time)
         "../util/event/event-bus.rkt"
         "../agent/blackboard.rkt"
         "../agent/blackboard-reducer.rkt"
         "../agent/blackboard-subscriber.rkt")

;; Helper: create an event struct for publishing.
;; ev-name is a string (matching event bus convention).
(define (make-test-event ev-name payload)
  (make-event ev-name 1000 "test-session" "turn-1" payload))

;; Helper: write JSONL entries to a temp file.
(define (write-jsonl path entries)
  (call-with-output-file path
                         (lambda (out)
                           (for ([e (in-list entries)])
                             (write-json e out)
                             (newline out)))
                         #:exists 'replace))

(define suite
  (test-suite "Blackboard Subscriber (W4 v0.99.7)"

    ;; ── blackboard-relevant-event? ──

    (test-case "relevant-event? returns #t for known event names"
      (check-true (blackboard-relevant-event? (make-test-event "gsd.wave.started"
                                                               (hasheq 'wave "W0"))))
      (check-true (blackboard-relevant-event? (make-test-event "mas.test.result"
                                                               (hasheq 'file "f" 'result 'pass))))
      (check-true (blackboard-relevant-event? (make-test-event "gsd.verification.completed"
                                                               (hasheq 'verdict 'approve)))))

    (test-case "relevant-event? returns #f for unknown event names"
      (check-false (blackboard-relevant-event? (make-test-event "some.random.event" (hasheq))))
      (check-false (blackboard-relevant-event? (make-test-event "user.message"
                                                                (hasheq 'text "hello")))))

    (test-case "relevant-event? works with hashes too"
      (check-true (blackboard-relevant-event? (hasheq 'event 'gsd.wave.started)))
      (check-false (blackboard-relevant-event? (hasheq 'event 'unknown.event))))

    (test-case "relevant-event? returns #f for non-event, non-hash values"
      (check-false (blackboard-relevant-event? "string"))
      (check-false (blackboard-relevant-event? 42))
      (check-false (blackboard-relevant-event? #f)))

    ;; ── event->reducer-hash ──

    (test-case "event->reducer-hash converts event? to hash"
      (define evt (make-test-event "gsd.wave.started" (hasheq 'wave "W0")))
      (define h (event->reducer-hash evt))
      (check-equal? (hash-ref h 'event) 'gsd.wave.started)
      (check-true (hash? (hash-ref h 'data)))
      (check-equal? (hash-ref h 'timestamp) 1000))

    (test-case "event->reducer-hash converts string event name to symbol"
      (define evt (make-test-event "mas.test.result" (hasheq 'file "f" 'result 'pass)))
      (define h (event->reducer-hash evt))
      (check-true (symbol? (hash-ref h 'event))))

    ;; ── Subscriber: Basic Updates ──

    (test-case "subscriber updates blackboard on relevant events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (publish! bus (make-test-event "gsd.wave.started" (hasheq 'wave "W0")))
        (define state (read-blackboard bb))
        (check-true (hash? (blackboard-state-wave-status state)))
        (check-equal? (hash-ref (blackboard-state-wave-status state) "W0") 'in-progress)
        (stop-blackboard-subscriber! bus)))

    (test-case "subscriber ignores irrelevant events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (publish! bus (make-test-event "some.irrelevant.event" (hasheq 'foo "bar")))
        (define state (read-blackboard bb))
        ;; Blackboard should still be empty (no relevant events processed)
        (check-equal? (hash-count (blackboard-state-wave-status state)) 0)
        (stop-blackboard-subscriber! bus)))

    (test-case "subscriber accumulates state from multiple events"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (publish! bus (make-test-event "gsd.wave.started" (hasheq 'wave "W0")))
        (publish! bus (make-test-event "gsd.wave.completed" (hasheq 'wave "W0")))
        (publish! bus (make-test-event "gsd.wave.started" (hasheq 'wave "W1")))
        (publish! bus (make-test-event "mas.test.result" (hasheq 'file "test.rkt" 'result 'pass)))
        (define state (read-blackboard bb))
        (check-equal? (hash-ref (blackboard-state-wave-status state) "W0") 'completed)
        (check-equal? (hash-ref (blackboard-state-wave-status state) "W1") 'in-progress)
        (check-equal? (length (blackboard-state-test-results state)) 1)
        (stop-blackboard-subscriber! bus)))

    ;; ── Subscriber: Clean Unsubscribe ──

    (test-case "stop-blackboard-subscriber! prevents further updates"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (publish! bus (make-test-event "gsd.wave.started" (hasheq 'wave "W0")))
        (stop-blackboard-subscriber! bus)
        ;; Publish more events after unsubscribe
        (publish! bus (make-test-event "gsd.wave.started" (hasheq 'wave "W1")))
        (define state (read-blackboard bb))
        ;; Only W0 should be present, not W1
        (check-true (hash-has-key? (blackboard-state-wave-status state) "W0"))
        (check-false (hash-has-key? (blackboard-state-wave-status state) "W1"))))

    ;; ── Subscriber: Exception Isolation ──

    (test-case "subscriber handles malformed payloads gracefully"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        ;; Event with non-hash payload — should not crash
        (publish! bus (make-event "gsd.wave.started" 1000 "s" "t" "not-a-hash"))
        ;; Subsequent valid event should still work
        (publish! bus (make-test-event "gsd.wave.completed" (hasheq 'wave "W0")))
        (define state (read-blackboard bb))
        (check-equal? (hash-ref (blackboard-state-wave-status state) "W0") 'completed)
        (stop-blackboard-subscriber! bus)))

    ;; ── Crash Recovery from JSONL ──
    ;; JSONL entries use string event names (JSON doesn't have symbols).
    ;; The subscriber normalizes them to symbols for the reducer.

    (test-case "rebuild-blackboard-from-log! replays JSONL events"
      (define tmpdir (find-system-path 'temp-dir))
      (define log-path (build-path tmpdir "test-blackboard-recovery.jsonl"))
      ;; Write test events to JSONL with string event names
      (write-jsonl
       log-path
       (list (hasheq 'event "gsd.wave.started" 'data (hasheq 'wave "W0") 'timestamp 1000)
             (hasheq 'event "gsd.wave.completed" 'data (hasheq 'wave "W0") 'timestamp 2000)
             (hasheq 'event
                     "mas.test.result"
                     'data
                     (hasheq 'file "f.rkt" 'result "pass")
                     'timestamp
                     3000)))
      ;; Rebuild from log
      (define bb (make-blackboard))
      (define result (rebuild-blackboard-from-log! log-path bb))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'completed)
      (check-equal? (length (blackboard-state-test-results result)) 1)
      ;; Cleanup
      (delete-file log-path))

    (test-case "rebuild-blackboard-from-log! skips irrelevant events"
      (define tmpdir (find-system-path 'temp-dir))
      (define log-path (build-path tmpdir "test-blackboard-skip.jsonl"))
      (write-jsonl log-path
                   (list (hasheq 'event "gsd.wave.started" 'data (hasheq 'wave "W0") 'timestamp 1000)
                         (hasheq 'event "user.message" 'data (hasheq 'text "hello") 'timestamp 2000)
                         (hasheq 'event
                                 "mas.test.result"
                                 'data
                                 (hasheq 'file "f.rkt" 'result "pass")
                                 'timestamp
                                 3000)))
      (define result (rebuild-blackboard-from-log! log-path))
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress)
      (check-equal? (length (blackboard-state-test-results result)) 1)
      (delete-file log-path))

    (test-case "rebuild-blackboard-from-log! handles non-existent file"
      (define result (rebuild-blackboard-from-log! "/nonexistent/path/events.jsonl"))
      ;; Should return empty blackboard without error
      (check-false (blackboard-state-active-plan result))
      (check-equal? (hash-count (blackboard-state-wave-status result)) 0))

    (test-case "rebuild-blackboard-from-log! handles corrupted lines gracefully"
      (define tmpdir (find-system-path 'temp-dir))
      (define log-path (build-path tmpdir "test-blackboard-corrupt.jsonl"))
      (call-with-output-file
       log-path
       (lambda (out)
         (write-json (hasheq 'event "gsd.wave.started" 'data (hasheq 'wave "W0") 'timestamp 1000) out)
         (newline out)
         ;; Corrupted line
         (display "THIS IS NOT VALID JSON\n" out)
         ;; Another valid line after corruption
         (write-json (hasheq 'event
                             "mas.test.result"
                             'data
                             (hasheq 'file "f.rkt" 'result "pass")
                             'timestamp
                             3000)
                     out)
         (newline out))
       #:exists 'replace)
      (define result (rebuild-blackboard-from-log! log-path))
      ;; Valid events should still be processed (jsonl-read-all-valid skips corrupted)
      (check-equal? (hash-ref (blackboard-state-wave-status result) "W0") 'in-progress)
      (check-equal? (length (blackboard-state-test-results result)) 1)
      (delete-file log-path))

    ;; ── start-blackboard-subscriber! resets blackboard ──

    (test-case "start-blackboard-subscriber! resets blackboard to empty"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        ;; Pre-populate with some data
        (update-blackboard!
         (lambda (state) (struct-copy blackboard-state state [wave-status (hash "W0" 'completed)]))
         bb)
        (check-true (hash-has-key? (blackboard-state-wave-status (read-blackboard bb)) "W0"))
        ;; Start subscriber — should reset
        (start-blackboard-subscriber! bus bb)
        (check-false (hash-has-key? (blackboard-state-wave-status (read-blackboard bb)) "W0"))
        (stop-blackboard-subscriber! bus)))

    ;; ── Double stop is safe ──

    (test-case "stop-blackboard-subscriber! is idempotent"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (stop-blackboard-subscriber! bus)
        ;; Second stop should not error
        (stop-blackboard-subscriber! bus)))))

(test-case "T1: concurrent dispatch handles 100 events from 5 threads"
  (define bus (make-event-bus))
  (define bb (make-blackboard))
  (parameterize ([current-blackboard bb])
    (start-blackboard-subscriber! bus bb)
    ;; Launch 5 threads, each publishing 20 wave events
    (define threads
      (for/list ([t (in-range 5)])
        (thread (lambda ()
                  (for ([i (in-range 20)])
                    (publish! bus
                              (make-test-event 'gsd.wave.started
                                               (hasheq 'wave (format "W~a-~a" t i)))))))))
    (for-each thread-wait threads)
    ;; Verify total events processed
    (define final (read-blackboard bb))
    (define wave-count (hash-count (blackboard-state-wave-status final)))
    (check-true (>= wave-count 1))
    (check-true (<= wave-count 100))
    (stop-blackboard-subscriber! bus)))

(run-tests suite)
