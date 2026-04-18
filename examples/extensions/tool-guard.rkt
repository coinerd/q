#lang racket/base

;; examples/extensions/tool-guard.rkt — tool call validation example
;;
;; Demonstrates how to block or amend tool calls using the 'tool-call hook.
;; Blocks calls to tools on a denylist.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/tool-guard.rkt")

(require racket/string
         "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Tools that are not allowed to execute.
(define blocked-tools '("dangerous_tool" "rm_rf"))

;; Check each tool call against the denylist.
;; If any match, block the entire batch.
(define the-extension
  (extension "tool-guard"
             "1.0.0"
             "1"
             (hasheq 'tool-call
                     (lambda (payload)
                       ;; payload is a list of tool-call structs
                       (define names (map (lambda (tc) (symbol->string (if (hash? tc) (hash-ref tc 'name 'unknown) 'unknown)))
                                          (if (list? payload) payload '())))
                       (define blocked (filter (lambda (n) (member n blocked-tools)) names))
                       (if (null? blocked)
                           (hook-pass payload)
                           (hook-block (format "Blocked tools: ~a" (string-join blocked ", "))))))))

;; Key concepts:
;;   1. 'tool-call is a CRITICAL hook — defaults to block on error (safety-first)
;;   2. Return hook-block to prevent tool execution entirely
;;   3. Return hook-amend to modify the tool calls (e.g., remove some, change args)
;;   4. Return hook-pass to allow execution unchanged
;;   5. Critical hooks protect the system — if your handler crashes, tools are blocked
