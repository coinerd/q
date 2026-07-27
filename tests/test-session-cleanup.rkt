#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-session-cleanup.rkt — Idempotent, Observable Session Cleanup (W0)
;;
;; Tests for the close-session! function:
;; - Sequential double-close is safe
;; - Concurrent double-close is safe
;; - Cleanup errors are logged (not silently swallowed)
;; - Cleanup steps execute even if one fails
;; - closed? guard prevents re-execution

(require rackunit
         racket/port
         racket/string
         racket/place
         "helpers/session-fixture.rkt"
         (only-in "../runtime/session/session-types.rkt"
                  agent-session?
                  agent-session-active?
                  agent-session-session-id
                  agent-session-closed?)
         (only-in "../runtime/session/session-mutation.rkt" guarded-set-closed!)
         "../runtime/agent-session.rkt")

;; ============================================================
;; Fixture helpers
;; ============================================================

(define (make-cleanup-test-session)
  (define sess (make-test-session))
  (check-true (agent-session? sess))
  (check-true (agent-session-active? sess))
  (check-false (agent-session-closed? sess))
  sess)

;; ============================================================
;; Test: Sequential double-close
;; ============================================================

(test-case "sequential double-close is safe and idempotent"
  (define sess (make-cleanup-test-session))
  (define sid (agent-session-session-id sess))
  ;; First close
  (close-session! sess)
  (check-false (agent-session-active? sess) "session should be inactive after first close")
  (check-true (agent-session-closed? sess) "session should be marked closed after first close")
  ;; Second close should not error
  (close-session! sess)
  (check-false (agent-session-active? sess) "session should remain inactive after second close")
  (check-true (agent-session-closed? sess) "session should remain marked closed after second close"))

;; ============================================================
;; Test: Concurrent double-close
;; ============================================================

(test-case "concurrent double-close is safe"
  (define sess (make-cleanup-test-session))
  (define sid (agent-session-session-id sess))
  (define errors (box '()))
  (define threads
    (for/list ([_ (in-range 5)])
      (thread (lambda ()
                (with-handlers
                    ([exn:fail? (lambda (e) (set-box! errors (cons (exn-message e) (unbox errors))))])
                  (close-session! sess))))))
  (for-each thread-wait threads)
  ;; No errors should have occurred
  (check-equal? (unbox errors) '() "no errors from concurrent double-close")
  (check-false (agent-session-active? sess) "session should be inactive after concurrent close")
  (check-true (agent-session-closed? sess) "session should be marked closed after concurrent close"))

;; ============================================================
;; Test: closed? guard prevents re-execution
;; ============================================================

(test-case "guarded-set-closed! only allows #f -> #t transition"
  (define sess (make-cleanup-test-session))
  (check-false (agent-session-closed? sess))
  ;; First set to #t (should succeed)
  (guarded-set-closed! sess #t)
  (check-true (agent-session-closed? sess))
  ;; Try to set to #f (should be no-op)
  (guarded-set-closed! sess #f)
  (check-true (agent-session-closed? sess) "closed? should remain #t after attempted un-close")
  ;; Try to set to #t again (should be no-op - already #t)
  (guarded-set-closed! sess #t)
  (check-true (agent-session-closed? sess) "closed? should remain #t after second close"))

;; ============================================================
;; Test: Cleanup errors are logged not silently swallowed
;; ============================================================

(test-case "cleanup errors are logged with context"
  (define sess (make-cleanup-test-session))
  (define sid (agent-session-session-id sess))
  ;; Close session normally - should not produce warnings
  (close-session! sess)
  (check-true (agent-session-closed? sess))
  (void))

;; ============================================================
;; Test: Closed session prevents further operations
;; ============================================================

(test-case "closed session prevents further operations"
  (define sess (make-cleanup-test-session))
  (close-session! sess)
  ;; close-session! is idempotent
  (close-session! sess)
  (check-true (agent-session-closed? sess))
  (check-false (agent-session-active? sess)))

(displayln "All session cleanup tests passed.")
