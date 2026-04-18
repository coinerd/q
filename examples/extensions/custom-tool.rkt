#lang racket/base

;; examples/extensions/custom-tool.rkt — custom tool registration example (#1213)
;;
;; Demonstrates how to register an LLM-callable tool via the 'register-tools hook.
;; The tool takes a "greeting" parameter and returns a structured result.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/custom-tool.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; A simple tool implementation.
;; In production, tools would interact with the filesystem, APIs, etc.
(define (run-greet-tool args)
  (define name (hash-ref args 'name "world"))
  (define greeting (format "Hello, ~a! Welcome to q." name))
  ;; Tools return a hash with 'content and optionally 'is-error
  (hasheq 'content greeting 'is-error #f))

;; The register-tools hook receives an empty payload and can return
;; an amended payload with a 'tools key containing tool definitions.
(define the-extension
  (extension "custom-tool"
             "1.0.0"
             "1"
             (hasheq 'register-tools
                     (lambda (payload)
                       (hook-amend
                        (hasheq 'tools
                                (list (hasheq 'name 'greet
                                              'description "Generate a personalized greeting"
                                              'parameters
                                              (hasheq 'type "object"
                                                      'properties
                                                      (hasheq 'name
                                                              (hasheq 'type "string"
                                                                      'description "Name to greet"))
                                                      'required (list "name"))
                                              ;; The execute function — called when the LLM invokes this tool
                                              'execute run-greet-tool))))))))

;; Key concepts:
;;   1. 'register-tools hook lets extensions add tools to the LLM's tool list
;;   2. Return (hook-amend payload-with-tools) to provide tools
;;   3. Tool definitions follow the OpenAI function-calling schema format
;;   4. Each tool has: name, description, parameters (JSON Schema), execute function
;;   5. The execute function receives a hash of arguments and returns a result hash
