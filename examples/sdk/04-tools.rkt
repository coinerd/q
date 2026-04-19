#lang racket/base

;; examples/sdk/04-tools.rkt — Custom tool registration

(require "../../interfaces/sdk.rkt"
         "../../tools/tool.rkt")

(define registry (make-tool-registry))

;; Register a custom tool
(register-tool! registry (hash 'name 'my-tool 'description "A custom tool" 'parameters '()))

(define rt (create-agent-session #:provider (hasheq 'type 'test) #:tool-registry registry))

(printf "Tool registry configured~n")
