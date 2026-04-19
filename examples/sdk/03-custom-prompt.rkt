#lang racket/base

;; examples/sdk/03-custom-prompt.rkt — Modify system prompt

(require "../../interfaces/sdk.rkt")

(define rt
  (create-agent-session #:provider (hasheq 'type 'test)
                        #:system-instructions
                        '("You are a helpful coding assistant." "Always write tests first.")))

(define info (session-info rt))
(printf "System instructions: ~a~n" (hash-ref info 'system-instructions))
