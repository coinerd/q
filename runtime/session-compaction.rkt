#lang racket/base

;; runtime/session-compaction.rkt — session-aware context compaction
;; STABILITY: internal
;;
;; Extracted from agent-session.rkt (ARCH-05).
;; Token budget checking and compaction triggering.

(require racket/contract
         (only-in racket/dict dict-ref)
         "../llm/token-budget.rkt"
         "../runtime/compactor.rkt"
         (only-in "../util/protocol-types.rkt" message-role message-content content-part->jsexpr)
         (only-in "../util/hook-types.rkt" hook-result-action)
         (only-in "runtime-helpers.rkt" emit-session-event! maybe-dispatch-hooks)
         "../agent/event-emitter.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "session-types.rkt")

(provide maybe-compact-context
         compact-context-mid-turn)

;; ============================================================
;; Compaction dispatch
;; ============================================================

;;; maybe-compact-context : agent-session? (listof message?) integer?
;;;                        -> (listof message?)
;;;
;;; Checks if context token count exceeds budget threshold. If so (and
;;; not in recursive-compaction guard or stale-usage cooldown), dispatches
;;; 'session-before-compact hook, runs compact-history, emits compaction
;;; events, and returns compacted message list. Otherwise returns input
;;; unchanged.
(define (maybe-compact-context sess context-with-system token-budget-threshold)
  (define bus (agent-session-event-bus sess))
  (define sid (agent-session-session-id sess))

  (define raw-messages
    (for/list ([msg (in-list context-with-system)])
      (hasheq 'role
              (symbol->string (message-role msg))
              'content
              (map content-part->jsexpr (message-content msg)))))
  (define token-count (estimate-context-tokens raw-messages))
  (cond
    [(not (should-compact? token-count token-budget-threshold)) context-with-system]
    [else
     ;; #770: Stale usage guard — skip if compaction just completed
     (define last-compact (agent-session-last-compaction-time sess))
     (define now-ms (current-inexact-milliseconds))
     (cond
       [(and last-compact (< (- now-ms last-compact) 2000))
        context-with-system] ; too soon after last compaction
       [(agent-session-compacting? sess) context-with-system] ; recursive compaction guard
       [else
        (set-agent-session-compacting?! sess #t)
        (emit-typed-event! bus
                           (make-compaction-event #:session-id sid
                                                  #:turn-id #f
                                                  #:timestamp (current-inexact-milliseconds)
                                                  #:reason "budget-exceeded"
                                                  #:tokens-before token-count
                                                  #:tokens-after token-count))
        (dynamic-wind
         (lambda () (void))
         (lambda ()
           (maybe-compact-context-internal sess
                                           context-with-system
                                           token-count
                                           token-budget-threshold
                                           bus
                                           sid))
         (lambda ()
           (set-agent-session-compacting?! sess #f)
           (set-agent-session-last-compaction-time! sess (current-inexact-milliseconds))
           (emit-typed-event! bus
                              (make-compaction-event #:session-id sid
                                                     #:turn-id #f
                                                     #:timestamp (current-inexact-milliseconds)
                                                     #:reason "compaction-complete"
                                                     #:tokens-before token-count
                                                     #:tokens-after token-count))))])]))

;; Internal compaction logic (extracted for dynamic-wind)
(define (maybe-compact-context-internal sess
                                        context-with-system
                                        token-count
                                        token-budget-threshold
                                        bus
                                        sid)
  ;; Dispatch 'session-before-compact hook
  (define compact-payload
    (hasheq 'session-id
            sid
            'token-count
            token-count
            'budget-threshold
            token-budget-threshold
            'message-count
            (length context-with-system)))
  (define-values (_amended-compact compact-hook-res)
    (maybe-dispatch-hooks (agent-session-extension-registry sess)
                          'session-before-compact
                          compact-payload))
  (cond
    [(and compact-hook-res (eq? (hook-result-action compact-hook-res) 'block)) context-with-system]
    [else
     (emit-session-event! bus
                          sid
                          "compaction.warning"
                          (hasheq 'tokenCount token-count 'budgetThreshold token-budget-threshold))
     (define compact-result (compact-history context-with-system))
     (emit-session-event! bus
                          sid
                          "compaction.completed"
                          (hasheq 'removedCount
                                  (compaction-result-removed-count compact-result)
                                  'keptCount
                                  (length (compaction-result-kept-messages compact-result))
                                  'tokenCount
                                  token-count))
     (compaction-result->message-list compact-result)]))

;; ============================================================
;; Mid-turn compaction (v0.28.21 W3)
;; ============================================================

;;; compact-context-mid-turn : agent-session? (listof message?) -> (listof message?)
;;;
;;; Lightweight mid-turn compaction. Reuses maybe-compact-context
;;; but is designed to be called from check-mid-turn-budget! when
;;; context exceeds 90% budget during tool-call loops.
;;; Returns compacted context or original if compaction not possible.
(define (compact-context-mid-turn sess context)
  (define max-tokens (dict-ref (agent-session-config sess) 'max-context-tokens 128000))
  (define budget-threshold (inexact->exact (floor (* max-tokens 0.9))))
  (maybe-compact-context sess context budget-threshold))
