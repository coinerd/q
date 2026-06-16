#lang racket/base

;; tests/test-blackboard-follower.rkt — Tests for blackboard-follower.rkt
;; v0.99.13 W1 (G-2): Extraction tests for the follower module.
;;
;; These tests verify:
;; 1. blackboard-relevant-event? correctly filters relevant/irrelevant events
;; 2. normalize-jsonl-entry converts string event names to symbols
;; 3. rebuild-blackboard-from-log! replays JSONL events correctly
;; 4. The follower module works independently of blackboard-subscriber.rkt

(require rackunit
         rackunit/text-ui
         racket/file
         (only-in "../util/json/jsonl.rkt" jsonl-append!)
         (only-in "../agent/blackboard.rkt" make-blackboard)
         "../agent/blackboard-follower.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-temp-trace-path)
  (build-path (find-system-path 'temp-dir)
              (format "test-follower-~a.jsonl" (current-inexact-milliseconds))))

;; ============================================================
;; Test Suite
;; ============================================================

(define follower-suite
  (test-suite "blackboard-follower tests (v0.99.13 W1 / G-2)"

    ;; ── Test 1: blackboard-relevant-event? for known events ──────
    (test-case "blackboard-relevant-event? returns #t for relevant hash events"
      (for ([name relevant-event-names])
        (define evt (hasheq 'event name 'data (hasheq)))
        (check-true (blackboard-relevant-event? evt) (format "expected ~a to be relevant" name))))

    ;; ── Test 2: blackboard-relevant-event? rejects unknown events ─
    (test-case "blackboard-relevant-event? returns #f for irrelevant events"
      (check-false (blackboard-relevant-event? (hasheq 'event 'unknown.event)))
      (check-false (blackboard-relevant-event? (hasheq 'event 'random.thing)))
      (check-false (blackboard-relevant-event? "not-an-event"))
      (check-false (blackboard-relevant-event? 42))
      (check-false (blackboard-relevant-event? (hasheq 'no-event-key #t))))

    ;; ── Test 3: blackboard-relevant-event? handles string event names ──
    (test-case "blackboard-relevant-event? handles string event names from JSON"
      ;; JSON round-trips symbols as strings — the follower must still match.
      (check-true (blackboard-relevant-event? (hasheq 'event "gsd.wave.started")))
      (check-true (blackboard-relevant-event? (hasheq 'event "mas.test.result")))
      (check-false (blackboard-relevant-event? (hasheq 'event "unknown.event"))))

    ;; ── Test 4: normalize-jsonl-entry converts string event names ──
    (test-case "normalize-jsonl-entry converts string event names to symbols"
      (define entry (hasheq 'event "gsd.wave.started" 'data (hasheq 'wave 1)))
      (define normalized (normalize-jsonl-entry entry))
      (check-equal? (hash-ref normalized 'event) 'gsd.wave.started))

    (test-case "normalize-jsonl-entry leaves non-hash entries unchanged"
      (check-equal? (normalize-jsonl-entry "string") "string")
      (check-equal? (normalize-jsonl-entry 42) 42)
      (check-equal? (normalize-jsonl-entry '()) '()))

    (test-case "normalize-jsonl-entry leaves symbol event names unchanged"
      (define entry (hasheq 'event 'gsd.wave.completed))
      (define normalized (normalize-jsonl-entry entry))
      (check-equal? (hash-ref normalized 'event) 'gsd.wave.completed))

    (test-case "normalize-jsonl-entry handles string-keyed event field"
      ;; JSON data from jsonl-read-all-valid uses equal?-based hashes.
      ;; Use hash (not hasheq) so string keys are matched by equal?.
      (define entry (hash "event" "mas.test.result"))
      (define normalized (normalize-jsonl-entry entry))
      (check-equal? (hash-ref normalized 'event #f) 'mas.test.result))

    ;; ── Test 5: rebuild-blackboard-from-log! replays events ───────
    (test-case "rebuild-blackboard-from-log! replays JSONL and applies relevant events"
      (define trace-path (make-temp-trace-path))
      (jsonl-append! trace-path (hash 'event "gsd.wave.started" 'data (hash 'wave "W0")))
      (jsonl-append! trace-path (hash 'event "ui.render.requested" 'data (hash 'panel "main")))
      (jsonl-append! trace-path (hash 'event "gsd.wave.completed" 'data (hash 'wave "W0")))
      ;; Replay into a real blackboard container.
      (define bb (make-blackboard))
      (define state (rebuild-blackboard-from-log! trace-path bb))
      (check-not-false state "rebuild returns a non-#f blackboard state")
      (delete-file trace-path))

    (test-case "rebuild-blackboard-from-log! filters out irrelevant events"
      (define trace-path (make-temp-trace-path))
      (jsonl-append! trace-path (hash 'event "gsd.wave.started" 'data (hash 'wave "W-filter")))
      (jsonl-append! trace-path (hash 'event "user.typed.something" 'data (hash 'text "hi")))
      (define bb (make-blackboard))
      (define state (rebuild-blackboard-from-log! trace-path bb))
      (check-not-false state "returns a valid state even with mixed events")
      (delete-file trace-path))

    ;; ── Test 6: relevant-event-names is a proper list ────────────
    (test-case "relevant-event-names is a non-empty list of symbols"
      (check-true (list? relevant-event-names))
      (check-true (> (length relevant-event-names) 5))
      (for ([name relevant-event-names])
        (check-true (symbol? name))))))

(run-tests follower-suite)
