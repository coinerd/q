#lang racket/base

;; tests/helpers/gsd-test-helpers.rkt — Test helper for GSD context factory
;;
;; W0 scaffolding: provides a basic make-gsd-context that satisfies
;; the tests in test-gsd-context-factory.rkt.
;; In W1, the real implementation will be in extensions/gsd/session-state.rkt.

(provide make-gsd-context)

(define (make-gsd-context)
  (let ([state #f]
        [plan-data #f]
        [workflow #f]
        [busy #f]
        [correlation-id #f]
        [transaction (make-hasheq)]
        [history '()]
        [edit-limit 500]
        [pinned-dir #f]
        [event-bus #f]
        [sem (make-semaphore 1)])
    (lambda (action . args)
      (call-with-semaphore
       sem
       (lambda ()
         (case action
           [(get-state) state]
           [(set-state) (set! state (car args))]
           [(get-plan) plan-data]
           [(set-plan) (set! plan-data (car args))]
           [(get-workflow) workflow]
           [(set-workflow) (set! workflow (car args))]
           [(busy?) busy]
           [(set-busy!) (set! busy (car args))]
           [(get-correlation-id) correlation-id]
           [(set-correlation-id!) (set! correlation-id (car args))]
           [(transaction-ref) (hash-ref transaction (car args))]
           [(transaction-set!) (hash-set! transaction (car args) (cadr args))]
           [(get-history) history]
           [(set-history!) (set! history (car args))]
           [(get-edit-limit) edit-limit]
           [(set-edit-limit!) (set! edit-limit (car args))]
           [(get-pinned-dir) pinned-dir]
           [(set-pinned-dir!) (set! pinned-dir (car args))]
           [(get-event-bus) event-bus]
           [(set-event-bus!) (set! event-bus (car args))]
           [else (error 'gsd-context "unknown action: ~a" action)]))))))
