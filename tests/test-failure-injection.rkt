#lang racket

;; tests/test-failure-injection.rkt — Failure injection tests
;;
;; Tests that the system handles failures gracefully:
;; - Provider stream errors (network, auth, malformed)
;; - Error classification in dispatch-iteration
;; - Tool execution errors
;; - IO/disk errors during session persistence
;; - Extension hook failures
;;
;; Issue #607: Failure injection test harness

(require rackunit
         rackunit/text-ui
         json
         "../llm/model.rkt"
         "../llm/provider.rkt"
         (only-in "../tools/tool.rkt"
                  make-tool-registry register-tool! make-tool
                  make-exec-context
                  make-success-result make-error-result
                  tool-result? tool-result-is-error?)
         "../tools/scheduler.rkt"
         "../agent/event-bus.rkt"
         (only-in "../util/protocol-types.rkt"
                  make-event event? event-event event-payload
                  loop-result? loop-result-termination-reason
                  loop-result-messages)
         "../extensions/api.rkt"
         "../util/jsonl.rkt"
         (prefix-in sdk: "../interfaces/sdk.rkt")
         "helpers/mock-provider.rkt")

(require (only-in "../main.rkt"
                  register-default-tools!
                  make-event-bus))

;; ============================================================
;; Failure injection helpers
;; ============================================================

(define (make-temp-dir)
  (make-temporary-file "q-fail-~a" 'directory))

(define (cleanup-dir dir)
  (when (directory-exists? dir)
    (delete-directory/files dir)))

;; Provider that raises on nth call
(define (make-failing-provider #:on-call [n 1]
                               #:error [err (exn:fail "injected provider failure"
                                                      (current-continuation-marks))])
  (define call-count (box 0))
  (make-provider
   (lambda () "failing-mock")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (if (= (unbox call-count) n)
         (raise err)
         (make-model-response
          (list (hash 'type "text" 'text "ok"))
          (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
          "fail-mock" 'stop)))
   (lambda (req)
     (set-box! call-count (add1 (unbox call-count)))
     (if (= (unbox call-count) n)
         (raise err)
         (list (make-stream-chunk "ok" #f
                             (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
                             #t))))))

;; Provider that returns malformed response (wrong shape)
(define (make-malformed-provider)
  (make-provider
   (lambda () "malformed-mock")
   (lambda () (hash 'streaming #t 'token-counting #t))
   ;; send returns a hash instead of model-response
   (lambda (req) (hasheq 'wrong "shape"))
   ;; stream returns empty list
   (lambda (req) '())))

;; Provider that fails mid-stream (first chunk ok, second raises)
(define (make-midstream-failure-provider)
  (define sent-first? (box #f))
  (make-provider
   (lambda () "midstream-fail")
   (lambda () (hash 'streaming #t 'token-counting #t))
   (lambda (req)
     (raise (exn:fail "midstream failure" (current-continuation-marks))))
   (lambda (req)
     (if (unbox sent-first?)
         (raise (exn:fail "midstream failure" (current-continuation-marks)))
         (begin
           (set-box! sent-first? #t)
           (list (make-stream-chunk "partial " #f #f #f)
                 (make-stream-chunk #f #f #f #t)))))))

;; ============================================================
;; Test: Provider errors don't crash session
;; ============================================================

(test-case "fail-inject: provider error on first call"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (e)
                    (set-box! events (cons (event-event e) (unbox events)))))
  (define prov (make-failing-provider #:on-call 1))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "trigger error"))
  ;; Session should survive
  (check-true (sdk:runtime? rt3)
              "runtime should survive provider error")
  ;; Should have started a turn
  (check-not-false (member "turn.started" (reverse (unbox events)))
                   "should emit turn.started before error")
  (cleanup-dir dir))

(test-case "fail-inject: provider error on second call (mid-session)"
  (define dir (make-temp-dir))
  (define prov (make-failing-provider #:on-call 2))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)))
  (define rt2 (sdk:open-session rt))
  ;; First call succeeds
  (define-values (rt3 result1) (sdk:run-prompt! rt2 "first prompt"))
  (check-pred loop-result? result1)
  (check-equal? (loop-result-termination-reason result1) 'completed)
  ;; Second call fails — should not crash
  (define-values (rt4 result2) (sdk:run-prompt! rt3 "second prompt"))
  (check-true (sdk:runtime? rt4)
              "runtime should survive mid-session provider error")
  ;; First turn's data should still be in session
  (define info (sdk:session-info rt4))
  (check >= (hash-ref info 'history-length) 2
         "first turn data should persist after error")
  (cleanup-dir dir))

(test-case "fail-inject: malformed provider response"
  (define dir (make-temp-dir))
  (define prov (make-malformed-provider))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "trigger malformed"))
  ;; Should not crash — malformed response handled gracefully
  (check-true (sdk:runtime? rt3)
              "runtime should survive malformed provider response")
  (cleanup-dir dir))

(test-case "fail-inject: mid-stream failure captures partial output"
  (define dir (make-temp-dir))
  (define prov (make-midstream-failure-provider))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "trigger midstream"))
  ;; Should survive and may have partial output
  (check-true (sdk:runtime? rt3)
              "runtime should survive mid-stream failure")
  (cleanup-dir dir))

;; ============================================================
;; Test: Tool execution errors
;; ============================================================

(test-case "fail-inject: tool raises exception during execution"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool! reg
    (make-tool "crash-tool" "Crashes on execution"
               (hasheq 'type "object"
                       'properties (hasheq))
               (lambda (args ctx)
                 (raise (exn:fail "tool crash!"
                                  (current-continuation-marks))))))
  (define prov (make-tool-call-mock-provider "crash-tool" (hasheq) "recovered"))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "use crash tool"))
  ;; Should survive tool crash
  (check-true (sdk:runtime? rt3)
              "runtime should survive tool execution exception")
  (cleanup-dir dir))

(test-case "fail-inject: tool returns error result"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  (register-tool! reg
    (make-tool "error-tool" "Returns error"
               (hasheq 'type "object"
                       'properties (hasheq))
               (lambda (args ctx)
                 (make-error-result "deliberate error"))))
  (define prov (make-tool-call-mock-provider "error-tool" (hasheq) "handled error"))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "use error tool"))
  (check-true (sdk:runtime? rt3))
  (check-pred loop-result? result)
  ;; Should complete: error result is a normal flow, not an exception
  (check-equal? (loop-result-termination-reason result) 'completed
                "tool error result should allow completion")
  (cleanup-dir dir))

;; ============================================================
;; Test: Missing tool
;; ============================================================

(test-case "fail-inject: tool-call for unregistered tool"
  (define dir (make-temp-dir))
  (define reg (make-tool-registry))
  ;; No tools registered — provider asks for a non-existent tool
  (define prov (make-tool-call-mock-provider "nonexistent" (hasheq) "handled"))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry reg
                               #:max-iterations 5))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 result) (sdk:run-prompt! rt2 "call missing tool"))
  (check-true (sdk:runtime? rt3)
              "runtime should survive missing tool")
  (cleanup-dir dir))

;; ============================================================
;; Test: Session persistence under stress
;; ============================================================

(test-case "fail-inject: multiple prompts with intermittent failures"
  (define dir (make-temp-dir))
  (define call-idx (box 0))
  (define flaky-prov
    (make-provider
     (lambda () "flaky")
     (lambda () (hash 'streaming #t 'token-counting #t))
     (lambda (req)
       (set-box! call-idx (add1 (unbox call-idx)))
       (if (odd? (unbox call-idx))
           (make-model-response
            (list (hash 'type "text" 'text "ok"))
            (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
            "flaky" 'stop)
           (raise (exn:fail "intermittent failure"
                            (current-continuation-marks)))))
     (lambda (req)
       (set-box! call-idx (add1 (unbox call-idx)))
       (if (odd? (unbox call-idx))
           (list (make-stream-chunk "ok" #f
                               (hasheq 'prompt-tokens 5 'completion-tokens 2 'total-tokens 7)
                               #t))
           (raise (exn:fail "intermittent failure"
                            (current-continuation-marks)))))))
  (define rt (sdk:make-runtime #:provider flaky-prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)))
  (define rt2 (sdk:open-session rt))
  ;; Run 4 prompts — even ones fail, odd ones succeed
  (define-values (rt3 _1) (sdk:run-prompt! rt2 "p1"))
  (define-values (rt4 _2) (sdk:run-prompt! rt3 "p2"))
  (define-values (rt5 _3) (sdk:run-prompt! rt4 "p3"))
  (define-values (rt6 _4) (sdk:run-prompt! rt5 "p4"))
  ;; Final runtime should be valid
  (check-true (sdk:runtime? rt6)
              "runtime should survive intermittent failures")
  ;; Session should have some persisted data
  (define info (sdk:session-info rt6))
  (check >= (hash-ref info 'history-length) 2
         "some successful turns should be persisted")
  (cleanup-dir dir))

;; ============================================================
;; Test: Event coherence under failures
;; ============================================================

(test-case "fail-inject: turn.started/turn.completed pairing under error"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (e)
                    (set-box! events (cons (event-event e) (unbox events)))))
  (define prov (make-failing-provider #:on-call 1))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (define-values (rt3 _) (sdk:run-prompt! rt2 "trigger error"))
  (define evts (reverse (unbox events)))
  ;; turn.started should be emitted
  (check-not-false (member "turn.started" evts)
                   "turn.started should be emitted before error")
  ;; NOTE: Current behavior does NOT always pair turn.started with
  ;; turn.completed on provider errors. This test documents the gap.
  ;; A future fix should ensure pairing (turn.failed or turn.completed).
  (define start-count (length (filter (lambda (e) (equal? e "turn.started")) evts)))
  (check-equal? start-count 1 "exactly one turn.started should be emitted")
  (cleanup-dir dir))

(test-case "fail-inject: session.started emitted even when turn fails"
  (define dir (make-temp-dir))
  (define bus (make-event-bus))
  (define events (box '()))
  (subscribe! bus (lambda (e)
                    (set-box! events (cons (event-event e) (unbox events)))))
  (define prov (make-failing-provider #:on-call 1))
  (define rt (sdk:make-runtime #:provider prov
                               #:session-dir dir
                               #:tool-registry (make-tool-registry)
                               #:event-bus bus))
  (define rt2 (sdk:open-session rt))
  (sdk:run-prompt! rt2 "trigger error")
  (define evts (reverse (unbox events)))
  (check-not-false (member "session.started" evts)
                   "session.started should be emitted even before a failing turn")
  (cleanup-dir dir))
