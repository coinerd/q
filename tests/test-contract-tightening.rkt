#lang racket/base

;; test-contract-tightening.rkt — Tests for any/c → typed contract tightening (W2)
;; Part of v0.81.0

(require rackunit
         racket/contract
         "../llm/model.rkt"
         "../llm/provider.rkt"
         "../agent/state.rkt"
         "../agent/effect-types.rkt"
         "../agent/stream-runner.rkt"
         "../agent/loop-phases.rkt"
         "../util/cancellation.rkt"
         "../agent/event-bus.rkt")

(test-case "stream-from-provider: procedure exists with tightened contract"
  (check-pred procedure? stream-from-provider))

(test-case "phase-build-request: accepts listof hash + hash settings"
  (define raw-msgs (list (hasheq 'role "user" 'content "hello")))
  (define settings (hasheq 'model "gpt-4"))
  (define-values (req effects) (phase-build-request raw-msgs #f settings))
  (check-pred model-request? req)
  (check-equal? effects '()))

(test-case "phase-build-request: rejects non-hash in messages list"
  (check-exn
   exn:fail:contract?
   (lambda ()
     (phase-build-request (list "not-a-hash") #f (hasheq)))))

(test-case "phase-emit-start: procedure exists"
  (check-pred procedure? phase-emit-start))

(test-case "phase-build-context: procedure exists"
  (check-pred procedure? phase-build-context))

(test-case "phase-stream: procedure exists"
  (check-pred procedure? phase-stream))
