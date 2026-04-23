#lang racket/base

;; util/hook-types.rkt — canonical hook result types and per-hook validation
;; Shared between extensions layer and agent core.
;; ARCH-01: extracted from extensions/hooks.rkt to eliminate layer violations.
;;
;; FEAT-63: Per-hook-point result validation schemas.
;; Each hook-point has documented expected actions and payload constraints.
;; Invalid results are caught early and logged as warnings.

;; Hook result struct and action constructors
(provide (struct-out hook-result)
         hook-pass
         hook-amend
         hook-block
         ;; FEAT-63: Per-hook-point validation
         valid-hook-actions-for
         validate-hook-result
         ;; Hook point names (#1148)
         hook-point-names
         ;; #1149: expose schemas for testing
         hook-action-schemas
         ;; #1210: hook name validation
         valid-hook-name?)

(struct hook-result (action payload) #:transparent)

(define (hook-pass [payload #f])
  (hook-result 'pass payload))

(define (hook-amend payload)
  (hook-result 'amend payload))

(define (hook-block [reason #f])
  (hook-result 'block reason))

;; ============================================================
;; FEAT-63: Per-hook-point result validation
;; ============================================================

;; Maps hook-point symbols to their valid actions.
;; Extensions returning actions outside this set are warned.
(define hook-action-schemas
  ;; Model request hooks
  (hasheq 'model-request-pre
          '(pass amend block)
          'before-provider-request
          '(pass amend block)
          ;; Message hooks
          'message-start
          '(pass amend block)
          'message-end
          '(pass amend block)
          ;; #1208: message-update alias (hyphen form of message.update)
          'message-update
          '(amend)
          ;; Tool execution hooks
          'tool-call
          '(pass amend block)
          'tool-result
          '(pass amend block)
          ;; #1208: tool-call-pre / tool-result-post (dispatched from scheduler.rkt)
          'tool-call-pre
          '(pass amend block)
          'tool-result-post
          '(pass amend block)
          ;; Turn lifecycle hooks
          'turn-start
          '(pass amend block)
          'turn-end
          '(pass amend)
          ;; Agent lifecycle hooks
          'before-agent-start
          '(pass amend block)
          ;; Context assembly hooks
          'context
          '(pass amend block)
          'context.assembly
          '(pass amend block)
          ;; Session hooks
          'session-before-switch
          '(pass block)
          'session-before-fork
          '(pass block)
          'session-before-compact
          '(pass block)
          ;; Input hooks
          'input
          '(pass amend block)
          ;; Extension registration hooks
          'register-tools
          '(pass amend)
          ;; Steering/queue hooks
          'before-send
          '(pass amend)
          ;; Resource discovery hooks (#1148)
          'resources-discover
          '(pass amend block)
          ;; ============================================================
          ;; #1149: Fine-grained lifecycle events
          ;; ============================================================
          ;; Tool lifecycle (fine-grained)
          'tool.execution.start
          '(pass)
          'tool.execution.update
          '(amend)
          'tool.execution.end
          '(pass)
          ;; Message lifecycle (fine-grained)
          'message.update
          '(amend)
          'message.stream.delta
          '(pass)
          ;; Context events
          'context.window.changed
          '(pass)
          'context.tokens.used
          '(amend)
          ;; Provider events
          'provider.response.after
          '(amend)
          'provider.stream.delta
          '(pass)
          'provider.error
          '(pass)
          ;; Model events
          'model.selected
          '(pass)
          'model.changed
          '(pass)
          ;; Session lifecycle additions
          'session.created
          '(pass)
          'session.loaded
          '(pass)
          'session.compacted
          '(pass)
          ;; Extension lifecycle
          'extension.loaded
          '(pass)
          'extension.unloaded
          '(pass)
          ;; Shortcut registration
          'register-shortcuts
          '(pass amend)
          ;; Command dispatch
          'execute-command
          '(pass amend)
          ;; Agent lifecycle
          'agent.started
          '(pass)
          'agent.idle
          '(pass)
          'agent.error
          '(pass)
          ;; #1310: New hook points
          'agent.end
          '(pass)
          'session-shutdown
          '(pass)
          'user-bash
          '(pass amend block)
          'session-before-tree
          '(pass block)
          'session-tree
          '(pass)
          'session-rebind
          '(pass)))

;; valid-hook-actions-for : symbol? -> (listof symbol?)
;; Returns the valid actions for a given hook-point.
;; Defaults to '(pass amend block) for unknown hook-points (permissive).
(define (valid-hook-actions-for hook-point)
  (hash-ref hook-action-schemas hook-point '(pass amend block)))

;; validate-hook-result : symbol? hook-result? -> boolean?
;; Checks whether a hook-result's action is valid for the given hook-point.
;; Returns #t if valid, #f if invalid. Logs a warning on invalid.
(define (validate-hook-result hook-point result)
  (define valid-actions (valid-hook-actions-for hook-point))
  (define action (hook-result-action result))
  (if (member action valid-actions)
      #t
      (begin
        (log-warning
         (format "Hook validation: ~a returned invalid action '~a' for hook '~a. Valid: ~a"
                 result
                 action
                 hook-point
                 valid-actions))
        #f)))

;; ============================================================
;; Hook point names (#1148)
;; ============================================================

;; Returns a list of all registered hook-point symbols.
;; Useful for testing and introspection.
(define (hook-point-names)
  (hash-keys hook-action-schemas))

;; ============================================================
;; #1210: Hook name validation
;; ============================================================

;; valid-hook-name? : symbol? -> boolean?
;; Returns #t if the given symbol is a registered hook-point name.
;; Used at extension registration time to catch typos early.
(define (valid-hook-name? name)
  (hash-has-key? hook-action-schemas name))
