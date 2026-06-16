#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-lifecycle.rkt — W6 (v0.99.7) Session Lifecycle Wiring Tests
;;
;; Tests for:
;;   - blackboard-enabled? config query (defaults to false)
;;   - Subscriber starts when config enabled
;;   - Subscriber does NOT start when config disabled
;;   - Crash recovery rebuilds from trace.jsonl
;;   - Injection parameter wiring

(require rackunit
         rackunit/text-ui
         racket/file
         (only-in "../runtime/settings-query.rkt" blackboard-enabled?)
         (only-in "../runtime/settings-core.rkt" q-settings)
         (only-in "../agent/blackboard.rkt"
                  make-blackboard
                  current-blackboard
                  read-blackboard
                  reset-blackboard!
                  blackboard-state-wave-status
                  empty-blackboard)
         (only-in "../agent/blackboard-subscriber.rkt"
                  start-blackboard-subscriber!
                  stop-blackboard-subscriber!
                  rebuild-blackboard-from-log!
                  current-subscription)
         (only-in "../runtime/context-assembly/state-aware-builder.rkt"
                  current-blackboard-injection-enabled)
         (only-in "../util/event/event-bus.rkt" make-event-bus publish! event-bus?)
         (only-in "../util/event/event.rkt" make-event)
         (only-in "../util/json/jsonl.rkt" jsonl-append!))

;; ── Helper: build a q-settings with a given merged hash ──
(define (make-settings merged-hash)
  (q-settings (hash) (hash) merged-hash))

;; ── Helper: temp file path for trace ──
(define (make-temp-trace-path)
  (build-path (find-system-path 'temp-dir)
              (format "test-trace-~a.jsonl" (current-inexact-milliseconds))))

(define suite
  (test-suite "Blackboard Lifecycle Wiring (W6 v0.99.7)"

    ;; ── W6-T2: Config Schema ──

    (test-case "blackboard-enabled? defaults to #t (v0.99.14)"
      (define settings (make-settings (hash)))
      (check-true (blackboard-enabled? settings)))

    (test-case "blackboard-enabled? returns #t when mas.blackboard.enabled is true"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled #t)))))
      (check-true (blackboard-enabled? settings)))

    (test-case "blackboard-enabled? accepts string 'true'"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled "true")))))
      (check-true (blackboard-enabled? settings)))

    (test-case "blackboard-enabled? returns #f for string 'false'"
      (define settings (make-settings (hash 'mas (hash 'blackboard (hash 'enabled "false")))))
      (check-false (blackboard-enabled? settings)))

    (test-case "blackboard-enabled? handles missing mas key (defaults to #t)"
      (define settings (make-settings (hash 'other-key 'val)))
      (check-true (blackboard-enabled? settings)))

    ;; ── W6-T1: Subscriber Lifecycle ──

    (test-case "subscriber starts when started on a bus"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (define sub-id (start-blackboard-subscriber! bus bb))
        (check-true (exact-nonnegative-integer? sub-id))
        (check-true (and (unbox current-subscription) #t))
        ;; Clean up
        (stop-blackboard-subscriber! bus)
        (check-false (unbox current-subscription))))

    (test-case "subscriber does not start when not called (default state)"
      ;; Ensure current-subscription is cleared
      (stop-blackboard-subscriber!)
      (check-false (unbox current-subscription)))

    (test-case "publishing event updates blackboard via subscriber"
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        ;; Publish a wave.started event
        (publish! bus
                  (make-event 'gsd.wave.started
                              (current-inexact-milliseconds)
                              "test-session"
                              #f
                              (hash 'wave "W0")))
        ;; Give subscriber a moment to process (synchronous in this impl)
        (define state (read-blackboard bb))
        (check-true (and state #t))
        (define waves (blackboard-state-wave-status state))
        (check-true (hash-has-key? waves "W0"))
        ;; Clean up
        (stop-blackboard-subscriber! bus)))

    ;; ── W6-T1: Injection Parameter ──

    (test-case "injection parameter defaults to #f"
      (check-false (current-blackboard-injection-enabled)))

    (test-case "injection parameter can be set to #t"
      (parameterize ([current-blackboard-injection-enabled #t])
        (check-true (current-blackboard-injection-enabled)))
      ;; Should be restored to #f after parameterize
      (check-false (current-blackboard-injection-enabled)))

    ;; ── W6-T1: Crash Recovery ──

    (test-case "rebuild-blackboard-from-log! replays trace.jsonl"
      (define trace-path (make-temp-trace-path))
      ;; Write events to trace file (JSON requires string keys/values)
      (jsonl-append! trace-path (hash 'event "gsd.wave.started" 'data (hash 'wave "W0")))
      (jsonl-append! trace-path (hash 'event "gsd.wave.completed" 'data (hash 'wave "W0")))
      (jsonl-append! trace-path (hash 'event "gsd.wave.started" 'data (hash 'wave "W1")))
      ;; Rebuild
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (define state (rebuild-blackboard-from-log! trace-path bb))
        (check-true (and state #t))
        (define waves (blackboard-state-wave-status state))
        (check-true (hash-has-key? waves "W0"))
        (check-true (hash-has-key? waves "W1")))
      ;; Clean up
      (delete-file trace-path))

    (test-case "rebuild-blackboard-from-log! ignores irrelevant events"
      (define trace-path (make-temp-trace-path))
      (jsonl-append! trace-path (hash 'event "ui.render.requested" 'data (hash 'panel "main")))
      (jsonl-append! trace-path (hash 'event "gsd.wave.started" 'data (hash 'wave "W2")))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (define state (rebuild-blackboard-from-log! trace-path bb))
        (define waves (blackboard-state-wave-status state))
        ;; Only the wave.started event should be reflected
        (check-true (hash-has-key? waves "W2"))
        (check-false (hash-has-key? waves "W0")))
      (delete-file trace-path))

    ;; ── v0.99.14 W1: Session Teardown Cleanup ──

    (test-case "stop-blackboard-subscriber! clears subscription after start"
      ;; Start a subscriber on a fresh event bus
      (define bus (make-event-bus))
      (define bb (make-blackboard))
      (parameterize ([current-blackboard bb])
        (start-blackboard-subscriber! bus bb)
        (check-true (pair? (unbox current-subscription)))
        ;; Stop it — should clear the subscription box
        (stop-blackboard-subscriber!)
        (check-false (unbox current-subscription))))

    (test-case "calling stop-blackboard-subscriber! twice is safe (idempotent)"
      (set-box! current-subscription #f)
      (check-not-exn (lambda () (stop-blackboard-subscriber!)))
      (check-not-exn (lambda () (stop-blackboard-subscriber!)))
      (check-false (unbox current-subscription)))

    (test-case "stop-blackboard-subscriber! with no active subscription is safe no-op"
      ;; Simulate session close when blackboard was never enabled
      (set-box! current-subscription #f)
      (check-not-exn (lambda () (stop-blackboard-subscriber!)))
      (check-false (unbox current-subscription)))))

(run-tests suite)
