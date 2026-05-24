#lang racket

;; BOUNDARY: integration

;; tests/test-extension-context-hooks.rkt — Extension context in hooks (#1325)
;;
;; Tests that maybe-dispatch-hooks accepts and passes #:ctx parameter.

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/api.rkt"
         "../extensions/context.rkt"
         (only-in "../runtime/runtime-helpers.rkt" maybe-dispatch-hooks)
         "../util/hook-types.rkt"
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  current-gsd-ctx
                  gsd-default-ctx
                  gsd-session-ctx?)
         (only-in "../extensions/gsd/state-machine.rkt" gsm-ctx-current gsm-ctx-transition-to!))

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
                                      "1"
                                      (hasheq 'any-hook
                                              (lambda (ctx payload)
                                                (set-box! received-ctx ctx)
                                                (hook-result 'continue payload)))))
      (define ctx (make-test-ctx #:session-id "ctx-test" #:model-name "claude-3" reg))
      (dispatch-hooks 'any-hook (hasheq) reg #:ctx ctx)
      (check-not-false (unbox received-ctx) "handler received context")
      (check-equal? (ctx-session-id (unbox received-ctx)) "ctx-test")
      (check-equal? (ctx-model (unbox received-ctx)) "claude-3"))

    ;; W8: Runtime extension context wiring for GSD ctx
    (test-case "extension-ctx gsd-ctx field preserves per-session context"
      (define gsd-ctx (make-gsd-context))
      (gsm-ctx-transition-to! gsd-ctx 'executing)
      (define ext-ctx
        (make-extension-ctx #:session-id "sess-gsd"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)
                            #:gsd-ctx gsd-ctx))
      (check-eq? (ctx-gsd-ctx ext-ctx) gsd-ctx "gsd-ctx field preserved")
      (check-eq? (gsm-ctx-current (ctx-gsd-ctx ext-ctx)) 'executing "state accessible"))

    (test-case "extension-ctx gsd-ctx defaults to #f when not provided"
      (define ext-ctx
        (make-extension-ctx #:session-id "sess-no-gsd"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)))
      (check-false (ctx-gsd-ctx ext-ctx) "gsd-ctx defaults to #f"))

    (test-case "per-session gsd-ctx isolates state from default"
      (define gsd-ctx (make-gsd-context))
      (gsm-ctx-transition-to! gsd-ctx 'exploring)
      (define ext-ctx
        (make-extension-ctx #:session-id "sess-iso"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)
                            #:gsd-ctx gsd-ctx))
      (check-eq? (gsm-ctx-current (ctx-gsd-ctx ext-ctx)) 'exploring "session ctx exploring")
      (check-eq? (gsm-ctx-current gsd-default-ctx) 'idle "default still idle"))

    (test-case "current-gsd-ctx parameterize dispatches to session ctx"
      (define gsd-ctx (make-gsd-context))
      (gsm-ctx-transition-to! gsd-ctx 'plan-written)
      (parameterize ([current-gsd-ctx gsd-ctx])
        (check-eq? (gsm-ctx-current (current-gsd-ctx)) 'plan-written "parameter works")))

    (test-case "two sessions have independent gsd-ctx"
      (define gsd-a (make-gsd-context))
      (define gsd-b (make-gsd-context))
      (gsm-ctx-transition-to! gsd-a 'executing)
      (gsm-ctx-transition-to! gsd-b 'verifying)
      (define ext-a
        (make-extension-ctx #:session-id "a"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)
                            #:gsd-ctx gsd-a))
      (define ext-b
        (make-extension-ctx #:session-id "b"
                            #:session-dir #f
                            #:event-bus (make-event-bus)
                            #:extension-registry (make-extension-registry)
                            #:gsd-ctx gsd-b))
      (check-eq? (gsm-ctx-current (ctx-gsd-ctx ext-a)) 'executing "a executing")
      (check-eq? (gsm-ctx-current (ctx-gsd-ctx ext-b)) 'verifying "b verifying"))))

(run-tests ext-ctx-hooks-suite)
