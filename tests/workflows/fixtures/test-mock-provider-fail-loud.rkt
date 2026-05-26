#lang racket

;; tests/workflows/fixtures/test-mock-provider-fail-loud.rkt — Fail-loud contract (#5469)
;;
;; Verifies that the scripted mock provider fails loudly on script exhaustion
;; and does not silently return placeholder responses.

(require rackunit
         rackunit/text-ui
         "mock-provider.rkt"
         "../../../llm/provider.rkt"
         "../../../llm/model.rkt")

(define dummy-req (make-model-request '() #f #f))

(define fail-loud-tests
  (test-suite "Mock provider fail-loud contract (#5469)"

    (test-case "default exhaustion behavior is error"
      ;; Create a provider with a single-entry script, call it twice
      (define p (make-scripted-provider (list (text-response "first"))))
      ;; First call should work
      (define resp1 (provider-send p dummy-req))
      (check-true (model-response? resp1) "first call returns a response")
      ;; Second call should error because script is exhausted
      (check-exn exn:fail?
                 (lambda () (provider-send p dummy-req))
                 "script exhaustion must error by default"))

    (test-case "explicit exhaustion 'error behaves same as default"
      (define p (make-scripted-provider (list (text-response "only")) #:exhaustion-behavior 'error))
      (provider-send p dummy-req)
      (check-exn exn:fail?
                 (lambda () (provider-send p dummy-req))
                 "explicit 'error must also fail on exhaustion"))

    (test-case "explicit exhaustion 'done returns silent response"
      (define p (make-scripted-provider (list (text-response "first")) #:exhaustion-behavior 'done))
      (provider-send p dummy-req)
      ;; Second call should NOT error — returns "done" text
      (define resp2 (provider-send p dummy-req))
      (check-true (model-response? resp2) "exhaustion 'done returns a response, not an error"))

    (test-case "empty script errors immediately on default"
      (define p (make-scripted-provider '()))
      (check-exn exn:fail?
                 (lambda () (provider-send p dummy-req))
                 "empty script must error immediately"))

    (test-case "stream also errors on exhaustion"
      (define p (make-scripted-provider (list (text-response "first"))))
      (provider-stream p dummy-req)
      (check-exn exn:fail?
                 (lambda () (provider-stream p dummy-req))
                 "stream exhaustion must also error"))

    (test-case "response constructors produce correct types"
      (define tr (text-response "hello"))
      (check-equal? (hash-ref tr 'type) "text")
      (define tcr (tool-call-response "tc-1" "read" (hash 'path "foo")))
      (check-equal? (hash-ref tcr 'type) "tool-call")
      (check-equal? (hash-ref tcr 'name) "read")
      (define dr (done-response))
      (check-equal? (hash-ref dr 'type) "done")
      (define er (error-response "oops"))
      (check-equal? (hash-ref er 'type) "error")
      (check-equal? (hash-ref er 'message) "oops"))

    (test-case "provider name is configurable"
      (define p (make-scripted-provider '() #:name "test-provider"))
      (check-equal? (provider-name p) "test-provider"))))

(module+ main
  (run-tests fail-loud-tests))
