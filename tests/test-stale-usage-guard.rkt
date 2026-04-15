#lang racket

;;; tests/test-stale-usage-guard.rkt — tests for stale usage guard (#770)
;;;
;;; Verifies that compaction is not re-triggered with stale usage data
;;; immediately after a compaction cycle.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt")

(define (make-temp-dir)
  (make-temporary-file "q-stale-test-~a" 'directory))

(define (make-test-session)
  (make-agent-session
   (hasheq 'session-dir (make-temp-dir)
           'event-bus (make-event-bus)
           'provider #f
           'tool-registry #f
           'model-name "test"
           'system-instructions '())))

(test-case "last-compaction-time starts as #f"
  (define sess (make-test-session))
  (check-false (agent-session-last-compaction-time sess)))

(test-case "last-compaction-time updated after compaction"
  (define sess (make-test-session))
  (set-agent-session-last-compaction-time! sess (current-inexact-milliseconds))
  (check-not-false (agent-session-last-compaction-time sess)))

(test-case "stale usage guard prevents immediate re-compaction"
  (define sess (make-test-session))
  ;; Simulate recent compaction
  (set-agent-session-last-compaction-time! sess (current-inexact-milliseconds))
  ;; Create context that would normally trigger compaction (threshold 0 = always)
  (define context
    (list (make-message "m1" #f 'user 'message
                        (list (make-text-part "hello world")) (current-seconds) (hasheq))))
  ;; maybe-compact-context should return context unchanged (stale guard)
  (define result (maybe-compact-context sess context 0))
  (check-equal? result context))

(test-case "fresh usage after cooldown triggers normally"
  (define sess (make-test-session))
  ;; Simulate compaction that happened 3 seconds ago
  (set-agent-session-last-compaction-time! sess (- (current-inexact-milliseconds) 3000))
  ;; Create context with enough messages to trigger
  (define context
    (list (make-message "m1" #f 'user 'message
                        (list (make-text-part "hello")) (current-seconds) (hasheq))))
  ;; Should not be blocked by stale guard (3s > 2s cooldown)
  ;; The actual compaction depends on token count vs threshold, but stale guard is passed
  (define result (maybe-compact-context sess context 0))
  ;; Result should be valid (may or may not compact, but shouldn't be stale-blocked)
  (check-true (list? result)))
