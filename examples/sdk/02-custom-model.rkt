#lang racket/base

;; examples/sdk/02-custom-model.rkt — Select model and thinking level

(require "../../interfaces/sdk.rkt")

(define rt
  (create-agent-session #:provider (hasheq 'type 'test)
                        #:model-name "claude-sonnet-4-20250514"
                        #:thinking-level 'high))

(define info (session-info rt))
(printf "Model: ~a~n" (hash-ref info 'model-name))
