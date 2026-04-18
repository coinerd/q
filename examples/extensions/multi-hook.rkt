#lang racket/base

;; examples/extensions/multi-hook.rkt — multiple hooks in one extension
;;
;; Demonstrates registering multiple hook points in a single extension.
;; Shows the recommended pattern for extensions that need to coordinate
;; across several lifecycle events.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/multi-hook.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "multi-hook"
             "1.0.0"
             "1"
             (hasheq 'turn-start
                     (lambda (payload)
                       (log-info "multi-hook: Turn starting")
                       (hook-pass payload))

                     'turn-end
                     (lambda (payload)
                       (log-info "multi-hook: Turn ending")
                       (hook-pass payload))

                     'tool-call
                     (lambda (payload)
                       (log-info "multi-hook: Tool call intercepted")
                       (hook-pass payload))

                     'context
                     (lambda (payload)
                       (log-info "multi-hook: Context assembled")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. A single extension can register handlers for multiple hook points
;;   2. All handlers share the same extension name for logging/debugging
;;   3. This is the recommended pattern for related functionality
;;   4. Keep unrelated concerns in separate extensions for modularity
