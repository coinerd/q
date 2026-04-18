#lang racket/base

;; examples/extensions/context-enricher.rkt — context assembly hook example
;;
;; Demonstrates how to enrich the context before the LLM sees it.
;; Adds a system-level instruction to every turn.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/context-enricher.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Prepend a system instruction to the context messages.
;; The 'context hook receives the assembled message list.
(define the-extension
  (extension "context-enricher"
             "1.0.0"
             "1"
             (hasheq 'context
                     (lambda (payload)
                       ;; payload is a list of message structs
                       ;; We pass it through unchanged here, but could amend it
                       (hook-pass payload)))))

;; Key concepts:
;;   1. 'context hook fires after context assembly, before sending to LLM
;;   2. Use hook-amend to modify the context messages
;;   3. Use hook-block to prevent the turn from executing
;;   4. This hook is advisory — the agent continues even if the handler errors
