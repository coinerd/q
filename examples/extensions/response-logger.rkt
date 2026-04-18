#lang racket/base

;; examples/extensions/response-logger.rkt — turn-end observation example
;;
;; Demonstrates how to observe agent responses after each turn.
;; Logs a summary of each turn's output.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/response-logger.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "response-logger"
             "1.0.0"
             "1"
             (hasheq 'turn-end
                     (lambda (payload)
                       ;; payload is the loop-result from the completed turn
                       ;; We just observe and pass through
                       (log-info "response-logger: Turn completed")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. 'turn-end fires after each agent turn completes
;;   2. This is an advisory hook — only 'pass and 'amend are valid actions
;;   3. Use this for logging, metrics, analytics
;;   4. The payload contains the turn's messages and metadata
