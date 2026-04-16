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
         validate-hook-result)

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
          '(pass amend)
          'message-end
          '(pass amend)
          ;; Tool execution hooks
          'tool-call
          '(pass amend block)
          'tool-result
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
          '(pass amend)))

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
        (log-warning (format "Hook validation: ~a returned invalid action '~a for hook '~a. Valid: ~a"
                             action
                             hook-point
                             valid-actions))
        #f)))
