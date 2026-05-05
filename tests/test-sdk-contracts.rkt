#lang racket

;; tests/test-sdk-contracts.rkt — Contract blame verification for v0.30.0
;;
;; Verifies that tightened contracts in sdk-core.rkt, sdk-public.rkt,
;; event-bus.rkt, and extensions/events.rkt raise proper blame when
;; given wrong-type arguments.

(require rackunit
         rackunit/text-ui
         (only-in "../interfaces/sdk-public.rkt" make-runtime runtime? publish!)
         (only-in "../agent/event-bus.rkt" make-event-bus subscribe! unsubscribe!)
         (only-in "../extensions/events.rkt" ext-publish!)
         (only-in "../llm/provider.rkt" make-mock-provider)
         (only-in "../llm/model.rkt" make-model-response))

(define mock-provider (make-mock-provider (make-model-response '() #f "mock" 'done)))

(define sdk-contract-tests
  (test-suite "SDK contract blame tests"

    (test-case "make-runtime rejects non-provider for #:provider"
      (check-exn #rx"expected: provider\\?" (lambda () (make-runtime #:provider "not-a-provider"))))

    (test-case "make-runtime rejects non-tool-registry for #:tool-registry"
      (check-exn #rx"tool-registry\\?"
                 (lambda () (make-runtime #:provider mock-provider #:tool-registry "bad"))))

    (test-case "make-runtime rejects non-boolean for #:register-default-tools?"
      (check-exn #rx"expected: boolean\\?"
                 (lambda () (make-runtime #:provider mock-provider #:register-default-tools? "yes"))))

    (test-case "make-runtime rejects non-boolean for #:auto-load-extensions?"
      (check-exn #rx"expected: boolean\\?"
                 (lambda () (make-runtime #:provider mock-provider #:auto-load-extensions? "yes"))))

    (test-case "event-bus subscribe! rejects non-procedure handler"
      (define bus (make-event-bus))
      (check-exn #rx"expected: procedure\\?" (lambda () (subscribe! bus "not-a-proc"))))

    (test-case "event-bus unsubscribe! rejects non-integer sub-id"
      (define bus (make-event-bus))
      (check-exn #rx"natural\\?" (lambda () (unsubscribe! bus "not-an-id"))))

    (test-case "event-bus publish! rejects non-event payload"
      (define bus (make-event-bus))
      (check-exn #rx"expected: event\\?" (lambda () (publish! bus "not-an-event"))))

    (test-case "ext-publish! rejects non-event payload"
      (define bus (make-event-bus))
      (check-exn #rx"expected: event\\?" (lambda () (ext-publish! bus "not-an-event"))))

    (test-case "make-runtime accepts valid optional arguments"
      (define rt (make-runtime #:provider mock-provider))
      (check-true (runtime? rt)))))

(module+ main
  (run-tests sdk-contract-tests 'verbose))
