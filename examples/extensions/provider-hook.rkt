#lang racket/base

;; examples/extensions/provider-hook.rkt — provider interaction hook example
;;
;; Demonstrates hooks around LLM provider calls:
;;   - before-provider-request: observe/modify the request before sending
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/provider-hook.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "provider-hook"
             "1.0.0"
             "1"
             (hasheq 'before-provider-request
                     (lambda (payload)
                       ;; payload has: session-id, turn-id
                       (log-info (format "provider-hook: Sending request to LLM (session ~a, turn ~a)"
                                         (hash-ref payload 'session-id "?")
                                         (hash-ref payload 'turn-id "?")))
                       (hook-pass payload)))))

;; Key concepts:
;;   1. 'before-provider-request fires before each LLM API call
;;   2. Can amend payload to inject parameters or block the request
;;   3. Use for rate limiting, request logging, parameter injection
;;   4. 'provider.response.after (not shown) fires after the response
