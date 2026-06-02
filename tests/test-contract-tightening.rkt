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
         "../util/message.rkt"
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
  (check-exn exn:fail:contract? (lambda () (phase-build-request (list "not-a-hash") #f (hasheq)))))

(test-case "phase-emit-start: accepts listof message and returns listof message"
  (define msgs (list (make-message "id1" #f 'user 'user '("hello") 1000 (hasheq))))
  (define st (make-loop-state "s" "t"))
  (define-values (ctx1 effects) (phase-emit-start "s" "t" st msgs))
  (check-equal? ctx1 msgs)
  (check-pred list? effects))

(test-case "phase-emit-start: rejects non-message in context list"
  (check-exn exn:fail:contract?
             (lambda ()
               (define st (make-loop-state "s" "t"))
               (phase-emit-start "s" "t" st (list (hasheq 'role "user"))))))

(test-case "phase-pre-hook: returns #f when no hook dispatcher"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define req (make-model-request msgs #f (hasheq)))
  (define mock-prov
    (make-provider (lambda () "mock") (lambda () (hasheq)) (lambda (r) (hasheq)) (lambda (r) '())))
  (define-values (result effects) (phase-pre-hook #f mock-prov msgs req "s" "t"))
  (check-false result)
  (check-equal? effects '()))

(test-case "phase-msg-hook: returns #f when no hook dispatcher"
  (define msgs (list (hasheq 'role "user" 'content "hello")))
  (define req (make-model-request msgs #f (hasheq)))
  (define mock-prov
    (make-provider (lambda () "mock") (lambda () (hasheq)) (lambda (r) (hasheq)) (lambda (r) '())))
  (define st (make-loop-state "s" "t"))
  (define-values (result effects) (phase-msg-hook #f mock-prov req msgs "s" "t" st))
  (check-false result)
  (check-equal? effects '()))

(test-case "phase-build-context: procedure exists"
  (check-pred procedure? phase-build-context))

(test-case "phase-stream: procedure exists"
  (check-pred procedure? phase-stream))
