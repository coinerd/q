#lang racket/base

;; examples/sdk/08-full-control.rkt — Full dependency injection

(require "../../interfaces/sdk.rkt"
         "../../tools/tool.rkt"
         "../../runtime/session-manager.rkt")

(define bus (make-event-bus))
(define registry (make-tool-registry))
(define mgr (make-in-memory-session-manager))

(define rt
  (create-agent-session #:provider (hasheq 'type 'test)
                        #:event-bus bus
                        #:tool-registry registry
                        #:session-manager mgr
                        #:max-iterations 20
                        #:system-instructions '("You are an expert coder.")
                        #:token-budget-threshold 80000))

(printf "Full control session: ~a~n" (session-info rt))
