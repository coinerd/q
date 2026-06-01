#lang racket/base

;; runtime/context-assembly/rollback-actions.rkt — Bounded rollback action model
;; STABILITY: evolving
;;
;; Represents rollback actions as pure data. Actions are prioritized:
;; warn-only < expand-context < force-distill < revert-state.
;; At most one action is executed per turn to prevent recursive loops.
;;
;; v0.77.10 M2: Real execution via injectable callbacks.
;; Default callbacks are no-ops. The runtime (state-aware-builder) wires
;; real callbacks that set feature flags and budgets.

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

;; v0.77.10 M2: Injectable execution callbacks.
;; Default to no-op. Runtime wires real implementations.
;; Signature: (-> rollback-action? void?)
(define current-force-distill-fn (make-parameter #f))
(define current-expand-context-fn (make-parameter #f))
(define current-revert-state-fn (make-parameter #f))

;; v0.77.10 M2: Action execution log for observability.
;; Each entry is a hash with 'type, 'reason, 'timestamp.
(define current-rollback-action-log (make-parameter '()))

;; v0.77.10 M2: Execute force-distill action.
;; Calls the injectable callback if available, then logs.
(define (execute-force-distill! action)
  (define fn (current-force-distill-fn))
  (when fn
    (fn action))
  (log-action! action))

;; v0.77.10 M2: Execute expand-context action.
;; Calls the injectable callback if available, then logs.
(define (execute-expand-context! action)
  (define fn (current-expand-context-fn))
  (when fn
    (fn action))
  (log-action! action))

;; v0.79.1 GAP-6: Execute revert-state action.
;; Calls the injectable callback if available, then logs.
;; Only executes when current-revert-state-fn is wired.
(define (execute-revert-state! action)
  (define fn (current-revert-state-fn))
  (when fn
    (fn action))
  (log-action! action))

;; Log an executed action to the rollback action log.
(define (log-action! action)
  (current-rollback-action-log (append (current-rollback-action-log)
                                       (list (hasheq 'type
                                                     (rollback-action-type action)
                                                     'reason
                                                     (rollback-action-reason action)
                                                     'timestamp
                                                     (current-seconds))))))

;; Execute a rollback action if execution is enabled.
;; v0.77.10 M2: Now dispatches to real execution functions.
;; Returns the action type if executed, #f otherwise.
;; Never executes 'revert-state unless explicitly enabled.
(define (maybe-execute-action action)
  (cond
    [(not action) #f]
    [(not (current-rollback-action-execution?)) #f]
    [(eq? (rollback-action-type action) 'revert-state)
     (if (current-revert-state-fn)
         (begin
           (execute-revert-state! action)
           'revert-state)
         #f)]
    [(eq? (rollback-action-type action) 'force-distill)
     (execute-force-distill! action)
     'force-distill]
    [(eq? (rollback-action-type action) 'expand-context)
     (execute-expand-context! action)
     'expand-context]
    [(eq? (rollback-action-type action) 'warn-only)
     (log-action! action)
     'warn-only]
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
         current-rollback-action-log
         current-force-distill-fn
         current-expand-context-fn
         current-revert-state-fn
         rollback-action-type?
         (contract-out [make-warn-action (-> string? rollback-action?)]
                       [make-expand-context-action (-> string? hash? rollback-action?)]
                       [make-force-distill-action (-> string? hash? rollback-action?)]
                       [make-revert-state-action (-> string? hash? rollback-action?)]
                       [select-highest-priority-action
                        (-> (listof rollback-action?) (or/c rollback-action? #f))]
                       [maybe-execute-action (-> (or/c rollback-action? #f) (or/c symbol? #f))]
                       [warnings->actions (-> (listof string?) (listof rollback-action?))]))
