#lang racket/base

;; examples/extensions/custom-command.rkt — slash command example (#1214)
;;
;; Demonstrates a slash command extension with argument parsing.
;; The command /echo repeats back arguments provided by the user.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/custom-command.rkt")

(require racket/string
         "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Command handler: receives a hash with 'args (the text after /echo)
;; Returns a formatted response string.
(define (handle-echo-command args-text)
  (define parts (string-split (or args-text "") " "))
  (define uppercased (map string-upcase parts))
  (string-join uppercased " "))

;; The 'input hook intercepts user input before it reaches the agent.
;; If the input starts with /echo, we handle it as a command.
;; Otherwise, we pass it through unchanged.
(define the-extension
  (extension "custom-command"
             "1.0.0"
             "1"
             (hasheq 'input
                     (lambda (payload)
                       ;; payload is the user input string
                       (define input (if (string? payload) payload ""))
                       (cond
                         [(string-prefix? input "/echo ")
                          (define args-text (substring input 6)) ; skip "/echo "
                          (define response (handle-echo-command args-text))
                          ;; Amend the payload to replace user input with our response
                          (hook-amend response)]
                         [(equal? input "/echo")
                          (hook-amend "Echo mode: provide text after /echo")]
                         [else
                          ;; Not our command — pass through
                          (hook-pass payload)])))))

;; Key concepts:
;;   1. 'input hook intercepts user input before the agent processes it
;;   2. Check for your command prefix (/echo) to identify your commands
;;   3. Use hook-amend to replace the input with your response
;;   4. Use hook-pass to let input through unchanged (for non-matching input)
;;   5. hook-block would prevent the input from reaching the agent entirely
