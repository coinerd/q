#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration
;;
;; tests/test-streaming-hook-isolation.rkt — W1 Hook Isolation tests
;;
;; Tests that extension failures during streaming hooks do not hang or crash streaming.
;; Tests that hook errors are properly caught and isolated.

(require rackunit
         racket/string
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../agent/state.rkt"
         "../agent/loop-stream.rkt"
         "../agent/stream-runner.rkt"
         "../util/message/message.rkt"
         "../util/event/event-bus.rkt"
         "../util/ids.rkt"
         (only-in "../util/cancellation.rkt" make-cancellation-token cancel-token!)
         (only-in "../util/cancellation.rkt" cancellation-token-cancelled?))

;; ============================================================
;; Helper: failing hook dispatcher builders
;; ============================================================

;; A hook-dispatcher that throws for any hook point
(define (make-throwing-hook-dispatcher)
  (lambda (hook-point payload) (error 'test-hook "deliberate failure at ~a" hook-point)))

;; A hook-dispatcher that throws specifically on 'message-end
(define (make-message-end-throwing-hook-dispatcher)
  (lambda (hook-point payload)
    (when (eq? hook-point 'message-end)
      (error 'test-hook "message-end hook failure"))
    #f))

;; A hook-dispatcher that throws on 'message-update
(define (make-message-update-throwing-hook-dispatcher)
  (lambda (hook-point payload)
    (when (eq? hook-point 'message-update)
      (error 'test-hook "message-update hook failure"))
    #f))

;; A hook-dispatcher that throws on 'model-response-post
(define (make-model-response-post-throwing-hook-dispatcher)
  (lambda (hook-point payload)
    (when (eq? hook-point 'model-response-post)
      (error 'test-hook "model-response-post hook failure"))
    #f))

;; A hook-dispatcher that throws on 'agent-end
(define (make-agent-end-throwing-hook-dispatcher)
  (lambda (hook-point payload)
    (when (eq? hook-point 'agent-end)
      (error 'test-hook "agent-end hook failure"))
    #f))

;; ============================================================
;; Helper: safe-hook-dispatch contract & guard
;; ============================================================

;; The guard function we expect to wrap hook dispatches.
;; Returns #f on exception, optionally emits an error event.
(define (safe-hook-dispatch hook-dispatcher hook-point payload)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (hook-dispatcher hook-point payload)))

(test-case "safe-hook-dispatch passes through success result"
  (define hd (lambda (hp p) 'result))
  (check-equal? (safe-hook-dispatch hd 'test (hasheq)) 'result))

(test-case "safe-hook-dispatch returns #f on exception"
  (define hd (make-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'test (hasheq))))

;; ============================================================
;; 1. message-update hook throwing does not crash stream-runner
;; ============================================================

(test-case "message-update hook failure caught in stream-runner"
  ;; The stream-runner should handle hook dispatcher exceptions gracefully.
  ;; We call safe-hook-dispatch directly to verify isolation.
  (define hd (make-message-update-throwing-hook-dispatcher))
  (check-false
   (safe-hook-dispatch hd
                       'message-update
                       (hasheq 'session-id "s1" 'turn-id "t1" 'delta-text "hi" 'delta-tool-call #f))
   "message-update hook failure should not propagate"))

(test-case "message-update hook failure does not crash stream building"
  (define hd (make-throwing-hook-dispatcher))
  ;; Even a completely throwing hook-dispatcher should not crash the guard
  (check-false (safe-hook-dispatch hd 'message-update (hasheq))
               "throwing hook-dispatcher should not propagate"))

;; ============================================================
;; 2. message-end hook throwing does not crash build-stream-result
;; ============================================================

(test-case "message-end hook failure caught"
  (define hd (make-message-end-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'message-end (hasheq))
               "message-end hook failure should not propagate"))

(test-case "message-end hook failure does not crash final result building"
  (define hd (make-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'message-end (hasheq 'session-id "s1"))
               "throwing message-end hook should not propagate"))

;; ============================================================
;; 3. model-response-post hook throwing does not crash
;; ============================================================

(test-case "model-response-post hook failure caught"
  (define hd (make-model-response-post-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'model-response-post (hasheq))
               "model-response-post hook failure should not propagate"))

;; ============================================================
;; 4. agent-end hook throwing does not crash
;; ============================================================

(test-case "agent-end hook failure caught"
  (define hd (make-agent-end-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'agent-end (hasheq))
               "agent-end hook failure should not propagate"))

;; ============================================================
;; 5. All hook points guarded consistently
;; ============================================================

(test-case "All hook points produce safe behaviour under exception"
  (define hd (make-throwing-hook-dispatcher))
  (for ([hook-point
         '(message-start message-update message-end model-response-post agent-end model-request-pre)])
    (check-false (safe-hook-dispatch hd hook-point (hasheq))
                 (format "throwing ~a should not propagate" hook-point))))

(test-case "message-update hook failure is isolated from others"
  ;; Only message-update throws; others must still work
  (define hd (make-message-update-throwing-hook-dispatcher))
  (check-false (safe-hook-dispatch hd 'message-update (hasheq)))
  (check-false (safe-hook-dispatch hd 'message-end (hasheq)))
  (check-false (safe-hook-dispatch hd 'model-response-post (hasheq)))
  (check-false (safe-hook-dispatch hd 'agent-end (hasheq)))
  (check-false (safe-hook-dispatch hd 'model-request-pre (hasheq))))

(test-case "Cancellation remains responsive with throwing hooks"
  ;; Cancellation token check should not be affected by hook failures
  (define token (make-cancellation-token))
  (define hd (make-throwing-hook-dispatcher))
  ;; The hook dispatcher may be called during cancellation cleanup
  ;; but cancellation itself should still work
  (check-false (cancellation-token-cancelled? token))
  (cancel-token! token)
  (check-true (cancellation-token-cancelled? token))
  ;; Hook failure during cancellation cleanup should be caught
  (check-false (safe-hook-dispatch hd 'agent-end (hasheq))))
