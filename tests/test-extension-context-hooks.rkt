#lang racket

;; tests/test-extension-context-hooks.rkt — Extension context in hooks (#1325)
;;
;; Tests that maybe-dispatch-hooks accepts and passes #:ctx parameter.

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/context.rkt"
         "../runtime/iteration.rkt"
         "../util/hook-types.rkt")

(define (make-test-ctx #:session-id sid #:model-name model [reg #f])
  (make-extension-ctx #:session-id sid
                      #:session-dir "/tmp"
                      #:event-bus (make-event-bus)
                      #:extension-registry (or reg (make-extension-registry))
                      #:model-name model))

(define ext-ctx-hooks-suite
  (test-suite "extension-context-hooks: context in dispatch calls"

    (test-case "dispatch-hooks accepts #:ctx keyword"
      (define reg (make-extension-registry))
      (define ctx (make-test-ctx #:session-id "test-s1" #:model-name "test-model"))
      (define result (dispatch-hooks 'test-hook (hasheq 'key 'val) reg #:ctx ctx))
      (check-true (hook-result? result)))

    (test-case "maybe-dispatch-hooks accepts #:ctx keyword"
      (define reg (make-extension-registry))
      (define ctx (make-test-ctx #:session-id "test-s2" #:model-name "test-model"))
      (define-values (payload hr) (maybe-dispatch-hooks reg 'test-hook (hasheq) #:ctx ctx))
      (check-equal? payload (hasheq)))

    (test-case "maybe-dispatch-hooks with #f registry returns payload"
      (define-values (payload hr) (maybe-dispatch-hooks #f 'test-hook (hasheq 'x 1)))
      (check-equal? payload (hasheq 'x 1))
      (check-false hr))

    (test-case "extension receives context in handler"
      (define reg (make-extension-registry))
      (define received-ctx (box #f))
      (register-extension! reg
                           (extension "test-ext"
                                      "1.0.0"
                                      1
                                      (hasheq 'any-hook
                                              (lambda (ctx payload)
                                                (set-box! received-ctx ctx)
                                                (hook-result 'continue payload)))))
      (define ctx (make-test-ctx #:session-id "ctx-test" #:model-name "claude-3" reg))
      (dispatch-hooks 'any-hook (hasheq) reg #:ctx ctx)
      (check-not-false (unbox received-ctx) "handler received context")
      (check-equal? (ctx-session-id (unbox received-ctx)) "ctx-test")
      (check-equal? (ctx-model (unbox received-ctx)) "claude-3"))))

(run-tests ext-ctx-hooks-suite)
