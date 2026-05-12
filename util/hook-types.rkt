#lang typed/racket

;; util/hook-types.rkt — canonical hook result types and per-hook validation
;; Shared between extensions layer and agent core.
;; Migrated to #lang typed/racket in v0.28.9 W1 (TR expansion).
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR boundary system.

(provide (struct-out hook-result)
         hook-pass
         hook-amend
         hook-block
         valid-hook-actions-for
         validate-hook-result
         hook-point-names
         hook-action-schemas
         valid-hook-name?
         HOOK-SCHEMA-VERSION
         hook-schema-version)

;; v0.28.8: Hook schema version
(: HOOK-SCHEMA-VERSION Integer)
(define HOOK-SCHEMA-VERSION 1)

(: hook-schema-version (-> Integer))
(define (hook-schema-version)
  HOOK-SCHEMA-VERSION)

;; Hook result struct
(struct hook-result ([action : Symbol] [payload : Any]) #:transparent)

(: hook-pass (->* () (Any) hook-result))
(define (hook-pass [payload #f])
  (hook-result 'pass payload))

(: hook-amend (-> Any hook-result))
(define (hook-amend payload)
  (hook-result 'amend payload))

(: hook-block (->* () (Any) hook-result))
(define (hook-block [reason #f])
  (hook-result 'block reason))

;; ============================================================
;; FEAT-63: Per-hook-point result validation
;; ============================================================

(: hook-action-schemas (HashTable Symbol (Listof Symbol)))
(define hook-action-schemas
  (hasheq 'model-request-pre
          '(pass amend block)
          'before-provider-request
          '(pass amend block)
          'message-start
          '(pass amend block)
          'message-end
          '(pass amend block)
          'message-update
          '(amend)
          'tool-call
          '(pass amend block)
          'tool-result
          '(pass amend block)
          'tool-call-pre
          '(pass amend block)
          'tool-result-post
          '(pass amend block)
          'turn-start
          '(pass amend block)
          'turn-end
          '(pass amend)
          'before-agent-start
          '(pass amend block)
          'context
          '(pass amend block)
          'context.assembly
          '(pass amend block)
          'session-before-switch
          '(pass block)
          'session-before-fork
          '(pass block)
          'session-before-compact
          '(pass block)
          'input
          '(pass amend block)
          'register-tools
          '(pass amend)
          'before-send
          '(pass amend)
          'resources-discover
          '(pass amend block)
          'tool.execution.started
          '(pass)
          'tool.execution.update
          '(amend)
          'tool.execution.completed
          '(pass)
          'message.update
          '(amend)
          'message.stream.delta
          '(pass)
          'context.window.changed
          '(pass)
          'context.tokens.used
          '(amend)
          'provider.response.after
          '(amend)
          'provider.stream.delta
          '(pass)
          'provider.error
          '(pass)
          'model.selected
          '(pass)
          'model.changed
          '(pass)
          'session.created
          '(pass)
          'session.loaded
          '(pass)
          'session.compacted
          '(pass)
          'extension.loaded
          '(pass)
          'extension.unloaded
          '(pass)
          'register-shortcuts
          '(pass amend)
          'execute-command
          '(pass amend)
          'agent.started
          '(pass)
          'agent.idle
          '(pass)
          'agent.error
          '(pass)
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

(: default-actions (Listof Symbol))
(define default-actions '(pass amend block))

(: valid-hook-actions-for (-> Symbol (Listof Symbol)))
(define (valid-hook-actions-for hook-point)
  (hash-ref hook-action-schemas hook-point (lambda () default-actions)))

(: validate-hook-result (->* (Symbol hook-result) (Integer) Boolean))
(define (validate-hook-result hook-point result [expected-version HOOK-SCHEMA-VERSION])
  (define valid-actions (valid-hook-actions-for hook-point))
  (define action (hook-result-action result))
  (cond
    [(not (= expected-version HOOK-SCHEMA-VERSION))
     (log-warning (format "Hook validation: schema version mismatch ~a vs ~a for hook '~a"
                          expected-version
                          HOOK-SCHEMA-VERSION
                          hook-point))
     #f]
    [(member action valid-actions) #t]
    [else
     (log-warning (format "Hook validation: ~a returned invalid action '~a' for hook '~a. Valid: ~a"
                          result
                          action
                          hook-point
                          valid-actions))
     #f]))

(: hook-point-names (-> (Listof Symbol)))
(define (hook-point-names)
  (hash-keys hook-action-schemas))

(: valid-hook-name? (-> Symbol Boolean))
(define (valid-hook-name? name)
  (hash-has-key? hook-action-schemas name))
