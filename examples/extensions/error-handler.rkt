#lang racket/base

;; examples/extensions/error-handler.rkt — error resilience patterns
;;
;; Demonstrates best practices for writing resilient extension handlers:
;;   - Use with-handlers to catch exceptions
;;   - Return safe defaults on error
;;   - Log errors for debugging
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/error-handler.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; Critical hook handler: tool-call
;; For critical hooks, errors should default to block (safety-first)
(define (handle-tool-call payload)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-warning (format "error-handler: tool-call handler failed: ~a"
                                          (exn-message e)))
                     (hook-block (format "Error in tool-call handler: ~a"
                                         (exn-message e))))])
    (hook-pass payload)))

;; Advisory hook handler: turn-end
;; For advisory hooks, errors should default to pass (liveness-first)
(define (handle-turn-end payload)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-warning (format "error-handler: turn-end handler failed: ~a"
                                          (exn-message e)))
                     (hook-pass payload))])
    (hook-pass payload)))

(define the-extension
  (extension "error-handler"
             "1.0.0"
             "1"
             (hasheq 'tool-call handle-tool-call
                     'turn-end handle-turn-end)))

;; Key concepts:
;;   1. Critical hooks (tool-call, session-before-switch, input) default to BLOCK on error
;;   2. Advisory hooks (turn-end, context, register-tools) default to PASS on error
;;   3. Always wrap handler logic in with-handlers for resilience
;;   4. Log errors with log-warning for debugging
;;   5. The dispatch system has its own timeout and error recovery, but explicit handling is best practice
