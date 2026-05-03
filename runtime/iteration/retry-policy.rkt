#lang racket/base

;; runtime/iteration/retry-policy.rkt — overflow recovery, budget checking, error detection
;;
;; Pure policy functions for retry and recovery.

(require racket/list
         (only-in racket/string string-join)
         (only-in "../auto-retry.rkt" context-overflow-error?)
         (only-in "../compactor.rkt"
                  compaction-result
                  compaction-result-removed-count
                  compaction-result-kept-messages)
         (only-in "../../util/protocol-types.rkt"
                  message?
                  message-id
                  message-role
                  message-content
                  text-part?
                  text-part-text
                  tool-result-part?
                  tool-result-part-is-error?)
         (only-in "../../util/event-contracts.rkt"
                  budget-payload/c
                  compact-result-payload/c
                  error-detail-payload/c)
         (only-in "../runtime-helpers.rkt" emit-session-event!)
         (only-in "../session-compaction.rkt" compact-context-mid-turn)
         (only-in "loop-state.rkt" resolve-estimate-tokens))

(provide check-mid-turn-budget!
         call-with-overflow-recovery)

;; Check if context exceeds mid-turn token budget.
;; When session is provided and over budget, triggers mid-turn compaction.
;; Returns (listof message?) if session provided (possibly compacted),
;; or integer token estimate if no session.
(define (check-mid-turn-budget! ctx
                                bus
                                session-id
                                config
                                #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)]
                                #:session [sess #f])
  (define max-tokens (hash-ref config 'max-context-tokens 128000))
  (define budget-threshold (inexact->exact (floor (* max-tokens 0.9))))
  (define texts
    (for/list ([msg (in-list ctx)])
      (define content (message-content msg))
      (cond
        [(string? content) content]
        [(list? content)
         (apply string-append
                (for/list ([part (in-list content)]
                           #:when (text-part? part))
                  (text-part-text part)))]
        [else ""])))
  (define estimated (for/sum ([t (in-list texts)]) (estimate-tokens (list (hasheq 'content t)))))
  (cond
    ;; Under budget: return ctx if session, else estimate
    [(<= estimated budget-threshold) (if sess ctx estimated)]
    [else
     ;; Over budget
     (when (and bus session-id)
       (emit-session-event!
        bus
        session-id
        "context.mid-turn-over-budget"
        (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
     (if sess
         ;; v0.28.21 W3: Trigger mid-turn compaction when session available
         (compact-context-mid-turn sess ctx)
         estimated)]))

;; Handle context overflow by compacting the context and retrying once.
;; Catches both context-overflow-error? and plain exn:fail with overflow messages.
(define (call-with-overflow-recovery thunk ctx bus session-id #:compact-proc [compact-proc #f])
  (define do-compact
    (or compact-proc
        (lambda (msgs)
          (define half (max 1 (quotient (length msgs) 2)))
          (compaction-result #f (- (length msgs) half) (take-right msgs half)))))
  (with-handlers ([(lambda (e) (context-overflow-error? e))
                   (lambda (e)
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.detected"
                                          (hasheq 'error (exn-message e)))
                     (define compact-result (do-compact ctx))
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.compacted"
                                          (hasheq 'original-size
                                                  (length ctx)
                                                  'removed-count
                                                  (compaction-result-removed-count compact-result)
                                                  'kept-count
                                                  (length (compaction-result-kept-messages
                                                           compact-result))))
                     (thunk))]
                  [exn:fail? (lambda (e) (raise e))])
    (thunk)))
