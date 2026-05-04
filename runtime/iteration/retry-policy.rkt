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

(provide compute-mid-turn-estimate
         check-mid-turn-budget!
         estimate-mid-turn-tokens
         maybe-compact-mid-turn
         call-with-overflow-recovery
         detect-exploration-loop)

;; ── Shared token estimation logic ──
;; Returns (values estimated budget-threshold max-tokens).
(define (compute-mid-turn-estimate ctx config estimate-tokens)
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
  (values estimated budget-threshold max-tokens))

;; ── Estimate token count for mid-turn context ──
;; Returns integer token estimate. Emits context.mid-turn-over-budget event
;; when context exceeds 90% of max budget.
(define (estimate-mid-turn-tokens ctx
                                  bus
                                  session-id
                                  config
                                  #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)])
  (define-values (estimated budget-threshold max-tokens)
    (compute-mid-turn-estimate ctx config estimate-tokens))
  (when (and (> estimated budget-threshold) bus session-id)
    (emit-session-event!
     bus
     session-id
     "context.mid-turn-over-budget"
     (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
  estimated)

;; ── Compact context mid-turn if over budget ──
;; Returns (listof message?) — compacted context if over 90% budget,
;; or original context if under budget.
(define (maybe-compact-mid-turn sess
                                ctx
                                bus
                                session-id
                                config
                                #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)])
  (define-values (estimated budget-threshold max-tokens)
    (compute-mid-turn-estimate ctx config estimate-tokens))
  (cond
    [(<= estimated budget-threshold) ctx]
    [else
     (when (and bus session-id)
       (emit-session-event!
        bus
        session-id
        "context.mid-turn-over-budget"
        (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
     (compact-context-mid-turn sess ctx)]))

;; ── Backward-compat wrapper ──
;; Returns (listof message?) if session provided, or integer? if not.
(define (check-mid-turn-budget! ctx
                                bus
                                session-id
                                config
                                #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)]
                                #:session [sess #f])
  (if sess
      (maybe-compact-mid-turn sess ctx bus session-id config #:estimate-tokens estimate-tokens)
      (estimate-mid-turn-tokens ctx bus session-id config #:estimate-tokens estimate-tokens)))

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

;; ============================================================
;; v0.28.21 W6: Exploration loop detection
;; ============================================================

;;; detect-exploration-loop : (listof string?) integer? -> (or/c #f string?)
;;;
;;; Checks recent tool names for repeating patterns that indicate
;;; an exploration loop (e.g., read-grep-read-grep cycles).
;;; Returns #f if no loop detected, or a string describing the loop.
(define (detect-exploration-loop recent-tool-names [min-repeats 3])
  (define n (length recent-tool-names))
  (cond
    [(< n (* min-repeats 2)) #f]
    [else
     ;; Check for repeating 2-tool patterns in the last N tool calls
     (define recent (take-at-most recent-tool-names (* min-repeats 4)))
     (define pairs
       (for/list ([i (in-range (sub1 (length recent)))])
         (list (list-ref recent i) (list-ref recent (add1 i)))))
     (define pair-counts (count-occurrences pairs))
     ;; Find any pair repeated 3+ times
     (define looping-pair
       (for/first ([pair (in-hash-keys pair-counts)]
                   #:when (>= (hash-ref pair-counts pair) min-repeats))
         pair))
     (cond
       [looping-pair
        (format "exploration loop detected: ~a repeated ~a times"
                looping-pair
                (hash-ref pair-counts looping-pair))]
       [else #f])]))

;; Helper: count occurrences in a list of items
(define (count-occurrences items)
  (define counts (make-hash))
  (for ([item (in-list items)])
    (hash-update! counts item add1 0))
  counts)

;; Helper: take at most N from list
(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))
