#lang racket

;; tests/test-hooks-complete.rkt — Tests for all 7 hook dispatch points (R2-7)

(require rackunit
         rackunit/text-ui
         "../agent/types.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../agent/state.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../runtime/iteration.rkt"
         (only-in "../tools/scheduler.rkt" run-tool-batch scheduler-result)
         (only-in "../tools/tool.rkt"
                  make-tool make-tool-registry make-exec-context
                  register-tool! tool-name tool-schema tool-execute
                  tool-call tool-call? make-tool-call
                  tool-call-id tool-call-name tool-call-arguments
                  make-error-result make-success-result)
         "../util/ids.rkt"
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-user-message text)
  (make-message (generate-id) #f 'user 'user
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (make-test-extension-registry . point-handler-pairs)
  (define reg (make-extension-registry))
  (for ([pair (in-list point-handler-pairs)])
    (define point (car pair))
    (define handler (cadr pair))
    (define ext (extension (format "test-ext-~a" point)
                           "0.1.0"
                           "1.0"
                           (hasheq point handler)))
    (register-extension! reg ext))
  reg)

(define (make-hook-dispatcher reg)
  (lambda (hook-point payload)
    (dispatch-hooks hook-point payload reg)))

;; ============================================================
;; Test suite
;; ============================================================

(define hooks-tests
  (test-suite
   "Hook Dispatch Points (R2-7)"

   ;; ---- Hook 1: model-request-pre (in agent loop) ----
   (test-case "model-request-pre hook is dispatched before streaming"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     
     (define content-parts (list (hasheq 'type "text" 'text "Response")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     
     (define ctx (list (make-user-message "Hello")))
     (run-agent-turn ctx mock-prov bus
                     #:session-id "test"
                     #:turn-id "t1"
                     #:hook-dispatcher hook-dispatcher)
     
     (check-true (unbox hook-called?) "model-request-pre hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'message-count 0) 1
                   "payload should contain message count"))

   ;; ---- Hook 2: model-response-post (in agent loop) ----
   (test-case "model-response-post hook is dispatched after response"
     (define bus (make-event-bus))
     (define hook-called? (box #f))
     (define captured-payload (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-response-post
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-payload payload)
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     
     (define content-parts (list (hasheq 'type "text" 'text "Response text")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     
     (define ctx (list (make-user-message "Hello")))
     (run-agent-turn ctx mock-prov bus
                     #:session-id "test"
                     #:turn-id "t1"
                     #:hook-dispatcher hook-dispatcher)
     
     (check-true (unbox hook-called?) "model-response-post hook should be called")
     (check-equal? (hash-ref (unbox captured-payload) 'response-content "")
                   "Response text"
                   "payload should contain response content"))

   ;; ---- Hook 3: tool-call-pre (in scheduler) ----
   (test-case "tool-call-pre hook is dispatched before tool execution"
     (define hook-called? (box #f))
     (define captured-name (box #f))
     (define reg (make-test-extension-registry
                  (list 'tool-call-pre
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-name (hash-ref payload 'tool-name #f))
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     
     (define test-tool
       (make-tool "test-tool" "A test tool"
                  (hasheq 'type "object" 'properties (hasheq))
                  (lambda (args exec-ctx)
                    (make-success-result "tool output"))))
     
     (define tc (make-tool-call "tc-1" "test-tool" (hasheq)))
     (define registry (make-tool-registry))
     (register-tool! registry test-tool)
     
     (define results (run-tool-batch (list tc) registry #:hook-dispatcher hook-dispatcher))
     
     (check-true (unbox hook-called?) "tool-call-pre hook should be called")
     (check-equal? (unbox captured-name) "test-tool" "payload should contain tool name"))

   ;; ---- Hook 4: tool-result-post (in scheduler) ----
   (test-case "tool-result-post hook is dispatched after tool execution"
     (define hook-called? (box #f))
     (define captured-result (box #f))
     (define reg (make-test-extension-registry
                  (list 'tool-result-post
                        (lambda (payload)
                          (set-box! hook-called? #t)
                          (set-box! captured-result (hash-ref payload 'result #f))
                          (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     
     (define test-tool
       (make-tool "result-tool" "A test tool"
                  (hasheq 'type "object" 'properties (hasheq))
                  (lambda (args exec-ctx)
                    (make-success-result "done"))))
     
     (define tc (make-tool-call "tc-2" "result-tool" (hasheq)))
     (define registry (make-tool-registry))
     (register-tool! registry test-tool)
     
     (define results (run-tool-batch (list tc) registry #:hook-dispatcher hook-dispatcher))
     
     (check-true (unbox hook-called?) "tool-result-post hook should be called")
     (check-not-false (unbox captured-result) "payload should contain result"))

   ;; ---- Hook 5: hook-pass action works correctly ----
   (test-case "hook-pass action allows normal execution"
     (define bus (make-event-bus))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload) (hook-pass payload)))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     
     (define content-parts (list (hasheq 'type "text" 'text "OK")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     
     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher hook-dispatcher))
     
     (check-equal? (loop-result-termination-reason result) 'completed
                   "pass action should allow normal completion"))

   ;; ---- Hook 6: hook-amend modifies payload ----
   (test-case "hook-amend action modifies payload"
     (define amended? (box #f))
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (set-box! amended? #t)
                          (hook-amend (hash-set payload 'amended #t))))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define result (hook-dispatcher 'model-request-pre (hasheq 'x 1)))
     (check-true (unbox amended?) "handler was called")
     (check-equal? (hook-result-action result) 'amend "action is amend"))

   ;; ---- Hook 7: hook-block stops dispatch ----
   (test-case "hook-block action stops dispatch"
     (define reg (make-test-extension-registry
                  (list 'model-request-pre
                        (lambda (payload)
                          (hook-block "blocked for testing")))))
     (define hook-dispatcher (make-hook-dispatcher reg))
     (define result (hook-dispatcher 'model-request-pre (hasheq)))
     (check-equal? (hook-result-action result) 'block "action is block")
     (check-equal? (hook-result-payload result) "blocked for testing"
                   "block reason preserved"))

   ;; ---- Hook 8: no hook dispatcher — no errors ----
   (test-case "missing hook dispatcher does not cause errors"
     (define bus (make-event-bus))
     (define content-parts (list (hasheq 'type "text" 'text "OK")))
     (define mock-prov (make-mock-provider
                        (make-model-response content-parts (hash) "mock" #f)))
     
     (define result
       (run-agent-turn (list (make-user-message "Hi")) mock-prov bus
                       #:session-id "test" #:turn-id "t1"
                       #:hook-dispatcher #f))
     
     (check-equal? (loop-result-termination-reason result) 'completed
                   "no hook dispatcher should work normally"))
   ))

;; Run tests
(module+ main
  (run-tests hooks-tests))

(module+ test
  (run-tests hooks-tests))
