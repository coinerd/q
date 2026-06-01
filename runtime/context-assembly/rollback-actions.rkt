#lang racket/base

;; runtime/context-assembly/rollback-actions.rkt — Bounded rollback action model
;; STABILITY: evolving
;;
;; Represents rollback actions as pure data. Actions are prioritized:
;; warn-only < expand-context < force-distill < revert-state.
;; At most one action is executed per turn to prevent recursive loops.

(require racket/contract
         racket/list
         racket/string)

;; ── Action Types ──

(define-struct rollback-action
               (type ; symbol: warn-only, expand-context, force-distill, revert-state
                reason ; string: human-readable reason
                severity ; integer: 0-3
                metadata) ; hash: additional context
  #:transparent)

(define valid-action-types '(warn-only expand-context force-distill revert-state))

(define (rollback-action-type? v)
  (and (symbol? v) (memq v valid-action-types) #t))

;; ── Constructors ──

(define (make-warn-action reason)
  (rollback-action 'warn-only reason 0 (hasheq)))

(define (make-expand-context-action reason metadata)
  (rollback-action 'expand-context reason 1 metadata))

(define (make-force-distill-action reason metadata)
  (rollback-action 'force-distill reason 2 metadata))

(define (make-revert-state-action reason metadata)
  (rollback-action 'revert-state reason 3 metadata))

;; ── Prioritization ──

;; Select the highest-severity action from a list (at most one).
(define (select-highest-priority-action actions)
  (cond
    [(null? actions) #f]
    [else (car (sort actions > #:key rollback-action-severity))]))

;; ── Execution Guard ──

;; Feature flag for action execution (disabled by default — warn-only mode)
(define current-rollback-action-execution? (make-parameter #f))

;; Execute a rollback action if execution is enabled.
;; Returns the action type if executed, #f otherwise.
;; Never executes 'revert-state unless explicitly enabled.
(define (maybe-execute-action action)
  (cond
    [(not action) #f]
    [(not (current-rollback-action-execution?)) #f]
    [(eq? (rollback-action-type action) 'revert-state) #f] ; too risky
    [else (rollback-action-type action)]))

;; ── Trigger to Action Mapping ──

;; Convert rollback trigger warnings to recommended actions.
;; Returns a list of rollback-action.
(define (warnings->actions warnings)
  (for/list ([w (in-list warnings)])
    (cond
      [(string-contains? w "amnesia") (make-force-distill-action w (hasheq 'trigger 'amnesia))]
      [(string-contains? w "excessive")
       (make-expand-context-action w (hasheq 'trigger 'excessive-savings))]
      [(string-contains? w "repeat") (make-warn-action w)]
      [else (make-warn-action w)])))

;; ── Exports ──

(provide (struct-out rollback-action)
         current-rollback-action-execution?
         rollback-action-type?
         (contract-out [make-warn-action (-> string? rollback-action?)]
                       [make-expand-context-action (-> string? hash? rollback-action?)]
                       [make-force-distill-action (-> string? hash? rollback-action?)]
                       [make-revert-state-action (-> string? hash? rollback-action?)]
                       [select-highest-priority-action
                        (-> (listof rollback-action?) (or/c rollback-action? #f))]
                       [maybe-execute-action (-> (or/c rollback-action? #f) (or/c symbol? #f))]
                       [warnings->actions (-> (listof string?) (listof rollback-action?))]))
