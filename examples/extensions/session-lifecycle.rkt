#lang racket/base

;; examples/extensions/session-lifecycle.rkt — session event hooks example
;;
;; Demonstrates hooks for session lifecycle events:
;;   - before-agent-start: observe/configure agent startup
;;   - session-before-switch: guard session resume
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/session-lifecycle.rkt")

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

(define the-extension
  (extension "session-lifecycle"
             "1.0.0"
             "1"
             (hasheq 'before-agent-start
                     (lambda (payload)
                       ;; payload has: session-id, max-iterations, context-message-count
                       (log-info (format "session-lifecycle: Agent starting, context has ~a messages"
                                         (hash-ref payload 'context-message-count 0)))
                       (hook-pass payload))

                     'session-before-switch
                     (lambda (payload)
                       ;; This is a CRITICAL hook — can block session resume
                       ;; Use for safety checks before resuming sessions
                       (log-info "session-lifecycle: Session switch approved")
                       (hook-pass payload)))))

;; Key concepts:
;;   1. 'before-agent-start fires once at the beginning of the agent loop
;;   2. 'session-before-switch fires when resuming a saved session
;;   3. session-before-switch is CRITICAL — errors default to block
;;   4. Use these hooks for initialization, validation, audit logging
