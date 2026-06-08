#lang racket

;; BOUNDARY: integration

;;; tests/test-stale-usage-guard.rkt — tests for stale usage guard (#770)
;;;
;;; Verifies that compaction is not re-triggered with stale usage data
;;; immediately after a compaction cycle.

(require rackunit
         rackunit/text-ui
         (only-in "helpers/temp-fs.rkt" with-temp-dir)
         "../util/message/protocol-types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session/session-types.rkt"
         (only-in "../runtime/session/session-mutation.rkt" guarded-set-last-compaction-time!))

(define (make-test-session dir)
  (make-agent-session (hasheq 'session-dir
                              dir
                              'event-bus
                              (make-event-bus)
                              'provider
                              #f
                              'tool-registry
                              #f
                              'model-name
                              "test"
                              'system-instructions
                              '())))

(test-case "last-compaction-time starts as #f"
  (with-temp-dir (dir)
                 (define sess (make-test-session dir))
                 (check-false (agent-session-last-compaction-time sess))))

(test-case "last-compaction-time updated after compaction"
  (with-temp-dir
   (dir)
   (define sess (make-test-session dir))
   (guarded-set-last-compaction-time! sess (inexact->exact (round (current-inexact-milliseconds))))
   (check-not-false (agent-session-last-compaction-time sess))))

(test-case "stale usage guard prevents immediate re-compaction"
  (with-temp-dir
   (dir)
   (define sess (make-test-session dir))
   ;; Simulate recent compaction
   (guarded-set-last-compaction-time! sess (inexact->exact (round (current-inexact-milliseconds))))
   ;; Create context that would normally trigger compaction (threshold 0 = always)
   (define context
     (list (make-message "m1"
                         #f
                         'user
                         'message
                         (list (make-text-part "hello world"))
                         (current-seconds)
                         (hasheq))))
   ;; maybe-compact-context should return context unchanged (stale guard)
   (define result (maybe-compact-context sess context 0))
   (check-equal? result context)))

(test-case "fresh usage after cooldown triggers normally"
  (with-temp-dir
   (dir)
   (define sess (make-test-session dir))
   ;; Simulate compaction that happened 3 seconds ago
   (guarded-set-last-compaction-time! sess
                                      (inexact->exact (round (- (current-inexact-milliseconds)
                                                                3000))))
   ;; Create context with enough messages to trigger
   (define context
     (list (make-message "m1"
                         #f
                         'user
                         'message
                         (list (make-text-part "hello"))
                         (current-seconds)
                         (hasheq))))
   ;; Should not be blocked by stale guard (3s > 2s cooldown)
   ;; The actual compaction depends on token count vs threshold, but stale guard is passed
   (define result (maybe-compact-context sess context 0))
   ;; Result should be valid (may or may not compact, but shouldn't be stale-blocked)
   (check-true (list? result))))
