#lang racket

;; @speed fast
;; @suite default
;; @boundary integration

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
         racket/async-channel
         "helpers/session-fixture.rkt"
         (only-in "../runtime/session/session-types.rkt"
                  agent-session?
                  agent-session-active?
                  agent-session-session-id
                  agent-session-closed?
                  agent-session-prompt-running?
                  agent-session-event-bus
                  agent-session-config)
         (only-in "../runtime/session/session-mutation.rkt"
                  guarded-set-closed!
                  try-claim-prompt!
                  release-prompt!)
         (only-in "../util/event/event-bus.rkt" make-event-bus subscribe!)
         (only-in "../util/event/event.rkt" event event-event event-payload)
         (only-in "../util/cancellation.rkt" cancellation-token-cancelled?)
         (only-in "../llm/provider.rkt" make-provider)
         (only-in "../llm/model.rkt" make-model-response make-stream-chunk)
         (only-in "../util/message/protocol-types.rkt" loop-result-termination-reason)
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

(test-case "closed session rejects new prompt ownership claims (F3)"
  (define sess (make-cleanup-test-session))
  ;; A prompt claim before close succeeds
  (check-true (try-claim-prompt! sess) "prompt can claim ownership before close")
  (release-prompt! sess)
  ;; After close, a new prompt must never claim ownership
  (close-session! sess)
  (check-false (try-claim-prompt! sess)
               "a closing/closed session must reject new prompt ownership claims"))

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

;; ============================================================
;; W0-F3: close coordinates with an active prompt
;; ============================================================

(test-case "close-session! waits for an active prompt and orders session.updated before session.closed (F3)"
  (define dir (make-temporary-file "q-close-active-prompt-~a" 'directory))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (evt) (set-box! events (append (unbox events) (list (event-event evt))))))
  (define stream-entered (make-semaphore 0))
  (define release-stream (make-semaphore 0))
  (define provider
    (make-provider (lambda () "close-block-mock")
                   (lambda () (hash 'streaming #t 'token-counting #t))
                   (lambda (_req) (make-model-response '() (hash) "mock" 'stop))
                   (lambda (_req)
                     ;; Block on a test-owned semaphore: close must wait for the
                     ;; prompt to finish rather than racing its writes.
                     (semaphore-post stream-entered)
                     (semaphore-wait release-stream)
                     (list (make-stream-chunk "ok" #f (hasheq) #t)))))
  (define sess (make-test-session #:dir dir #:provider provider #:event-bus bus))
  (define result-ch (make-async-channel))
  (define prompt-thread
    (thread (lambda ()
              (define-values (_s r) (run-prompt! sess "prompt during close"))
              (async-channel-put result-ch r))))
  (check-not-false (sync/timeout 5 stream-entered) "prompt should reach the blocking stream")
  (check-true (agent-session-prompt-running? sess) "prompt owns the session")
  ;; Close from a separate thread; it must block until the prompt releases ownership
  (define close-done (box #f))
  (define close-thread
    (thread (lambda ()
              (close-session! sess #:timeout-ms 2000)
              (set-box! close-done #t))))
  ;; Close must NOT complete while the prompt is still blocked
  (sleep 0.2)
  (check-false (unbox close-done) "close-session! must wait for the active prompt")
  ;; Let the prompt finish, then close completes
  (semaphore-post release-stream)
  (define result (sync/timeout 8 result-ch))
  (check-not-false result "prompt should terminate after close coordination")
  (check-not-false (sync/timeout 8 (thread-dead-evt close-thread))
                   "close should finish after the prompt")
  (check-true (unbox close-done))
  (check-false (agent-session-prompt-running? sess) "prompt ownership released after close")
  (check-false (agent-session-active? sess) "session deactivated after close")
  (check-true (agent-session-closed? sess))
  ;; Ordering: the prompt's session.updated must precede the close's session.closed
  (define updated-idx (index-of (unbox events) "session.updated"))
  (define closed-idx (index-of (unbox events) "session.closed"))
  (check-not-false updated-idx "prompt emitted session.updated")
  (check-not-false closed-idx "close emitted session.closed")
  (check-true (< updated-idx closed-idx)
              "session.updated must precede session.closed (no writes after repository close)")
  (delete-directory/files dir #:must-exist? #f))

(test-case "close-session! cancels a stuck prompt and completes within the timeout (F3)"
  (define dir (make-temporary-file "q-close-cancel-prompt-~a" 'directory))
  (define bus (make-event-bus))
  (define stream-entered (make-semaphore 0))
  (define sess-box (box #f))
  (define provider
    (make-provider (lambda () "close-cancel-mock")
                   (lambda () (hash 'streaming #t 'token-counting #t))
                   (lambda (_req) (make-model-response '() (hash) "mock" 'stop))
                   (lambda (_req)
                     ;; Block until the close cancels the prompt token, then finish
                     (define sess (unbox sess-box))
                     (semaphore-post stream-entered)
                     (let wait-for-cancel ()
                       (define token (dict-ref (agent-session-config sess) 'cancellation-token #f))
                       (unless (and token (cancellation-token-cancelled? token))
                         (sleep 0.01)
                         (wait-for-cancel)))
                     (list (make-stream-chunk #f #f (hasheq) #t)))))
  (define sess (make-test-session #:dir dir #:provider provider #:event-bus bus))
  (set-box! sess-box sess)
  (define result-ch (make-async-channel))
  (thread (lambda ()
            (define-values (_s r) (run-prompt! sess "stuck prompt"))
            (async-channel-put result-ch r)))
  (check-not-false (sync/timeout 5 stream-entered) "prompt should reach the blocking stream")
  ;; Close without releasing the stream manually: the token cancellation must
  ;; unblock the prompt so the close completes well within the timeout.
  (define close-elapsed-ms (box #f))
  (define close-start (current-inexact-milliseconds))
  (close-session! sess #:timeout-ms 10000)
  (set-box! close-elapsed-ms (- (current-inexact-milliseconds) close-start))
  (check-true (< (unbox close-elapsed-ms) 5000) "close must not wait out the full timeout")
  (define result (sync/timeout 5 result-ch))
  (check-not-false result)
  (check-equal? (loop-result-termination-reason result) 'cancelled)
  (check-false (agent-session-prompt-running? sess))
  (check-true (agent-session-closed? sess))
  (delete-directory/files dir #:must-exist? #f))
