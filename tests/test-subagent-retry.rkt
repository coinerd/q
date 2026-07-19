#lang racket

;; @speed fast
;; @suite tools

;; tests/test-subagent-retry.rkt
;; v0.99.26 W1b: Verify subagent auto-retry (F-1a) and max-turns default (F-1b).

(require rackunit
         rackunit/text-ui
         "../tools/builtins/spawn-subagent.rkt"
         "../tools/tool.rkt"
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../util/ids.rkt")

;; ── Mock provider that fails once then succeeds ──
;; The first call raises a rate-limit error; subsequent calls succeed.
;; This verifies that run-subagent-loop retries on transient errors.

(define (make-retry-provider)
  (define calls-box (box 0))
  (define provider
    (make-provider
     (lambda () "retry-test-mock")
     (lambda () (hasheq 'streaming #t))
     (lambda (req)
       (define n (unbox calls-box))
       (set-box! calls-box (add1 n))
       (if (= n 0)
           ;; First call: simulate transient connection error (fast retry)
           (raise (exn:fail "connection reset by peer" (current-continuation-marks)))
           ;; Second call: success
           (make-model-response (list (hasheq 'type "text" 'text "Task completed after retry."))
                                (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                                "mock-model"
                                'stop)))
     (lambda (req) (error 'retry-test-mock "streaming not supported"))))
  (values provider calls-box))

;; ── Mock provider that always succeeds ──

(define (make-always-success-provider)
  (make-mock-provider
   (make-model-response (list (hasheq 'type "text" 'text "Success on first try."))
                        (hasheq 'prompt-tokens 10 'completion-tokens 5 'total-tokens 15)
                        "mock-model"
                        'stop)
   #:name "mock-success"))

(define (make-test-exec-ctx provider)
  (make-exec-context #:working-directory (current-directory)
                     #:runtime-settings (hasheq 'provider provider 'model "mock-model")))

(define suite
  (test-suite "Subagent Reliability Fixes (v0.99.26 W1b)"

    ;; ---- F-1b: Default max-turns = 10 ----
    (test-case "parse-subagent-config default max-turns is 10"
      (define cfg (parse-subagent-config (hasheq 'task "test task" 'capabilities '(read-only))))
      (check-equal? (subagent-config-max-turns cfg) 10))

    (test-case "parse-subagent-config explicit max-turns respected"
      (define cfg
        (parse-subagent-config (hasheq 'task "test task" 'max-turns 3 'capabilities '(read-only))))
      (check-equal? (subagent-config-max-turns cfg) 3))

    ;; ---- F-1a: Subagent retries on transient provider error ----
    ;; Uses "connection reset" which triggers fast (1s) retry backoff.
    (test-case "subagent retries on transient provider error"
      (define-values (provider calls-box) (make-retry-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define cfg (subagent-config "Simple task" "assistant" 5 #f #f '(read-only)))
      (define result (run-subagent-with-config cfg exec-ctx))
      (check-false (tool-result-is-error? result) "result should not be an error after retry")
      (check-true (> (unbox calls-box) 1) "provider-send should have been called more than once"))

    (test-case "subagent succeeds without retry when no errors"
      (define provider (make-always-success-provider))
      (define exec-ctx (make-test-exec-ctx provider))
      (define cfg (subagent-config "Simple task" "assistant" 5 #f #f '(read-only)))
      (define result (run-subagent-with-config cfg exec-ctx))
      (check-false (tool-result-is-error? result)))))

(run-tests suite)
