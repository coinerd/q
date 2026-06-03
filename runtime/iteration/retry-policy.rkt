#lang typed/racket

;; runtime/iteration/retry-policy.rkt — overflow recovery, budget checking, error detection
;;
;; Pure policy functions for retry and recovery.
;; v0.30.2 W1: Migrated to Typed Racket (TR beachhead).
;;
;; TR BOUNDARY:
;; This is a #lang typed/racket module. Untyped consumers receive
;; auto-generated contracts from TR boundary system.

(require racket/list
         racket/string
         racket/match)

(provide compute-mid-turn-estimate
         check-mid-turn-budget!
         estimate-mid-turn-tokens
         maybe-compact-mid-turn
         call-with-overflow-recovery
         detect-exploration-loop
         count-occurrences)

;; ── Typed imports from untyped modules ──────────────────────────

(require/typed racket/dict [dict-ref (->* ((HashTable Symbol Any) Symbol) (Any) Any)])
(require/typed "../auto-retry.rkt" [context-overflow-error? (-> Any Boolean)])

(require/typed "../compaction/compactor.rkt"
               [#:struct compaction-result
                ([summary-message : (U String #f)] [removed-count : Integer]
                                                   [kept-messages : (Listof Any)])])

(require/typed "../../util/message/message.rkt" [message-content (-> Any (U String (Listof Any)))])
(require/typed "../../util/content/content-parts.rkt"
               [text-part? (-> Any Boolean)]
               [text-part-text (-> Any String)])

;; v0.33.5 W0a: Removed emit-session-event! import — opaque event-bus? cannot
;; cross TR boundary as Any (any-wrap/c limitation). Instead, callers pass an
;; emit-event callback that wraps the event bus internally.

;; v0.33.7 W0a (N-A01): Removed dead compact-context-mid-turn import.
;; Was only used as default for #:compact-proc before v0.33.5 W0a replaced
;; the default with raise-arguments-error. No callers reference it.

(require/typed "../../llm/token-budget.rkt"
               [estimate-context-tokens (-> (Listof Any) Nonnegative-Integer)])

;; ── Shared token estimation logic ──
;; Returns (values estimated budget-threshold max-tokens).
(: compute-mid-turn-estimate
   (-> (Listof Any)
       (HashTable Symbol Any)
       (-> (Listof Any) Nonnegative-Integer)
       (Values Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)))
(define (compute-mid-turn-estimate ctx config estimate-tokens)
  (define max-tokens
    :
    Nonnegative-Integer
    (let ([v (dict-ref config 'max-context-tokens #f)]) (if (exact-nonnegative-integer? v) v 128000)))
  (define budget-threshold
    :
    Nonnegative-Integer
    (cast (exact-floor (* max-tokens 9/10)) Nonnegative-Integer))
  (define texts
    (for/list :
      (Listof String)
      ([msg (in-list ctx)])
      (define content (message-content msg))
      (cond
        [(string? content) content]
        [(list? content)
         (apply string-append
                (for/list :
                  (Listof String)
                  ([part (in-list content)] #:when (text-part? part))
                  (text-part-text part)))]
        [else ""])))
  (define estimated
    (for/sum :
             Nonnegative-Integer
             ([t (in-list texts)])
             (estimate-tokens (list (hasheq 'content t)))))
  (values estimated budget-threshold max-tokens))

;; ── Estimate token count for mid-turn context ──
(: estimate-mid-turn-tokens
   (->* ((Listof Any) (U String #f) (HashTable Symbol Any))
        (#:emit-event (U (-> String Any Any) #f)
                      #:estimate-tokens (-> (Listof Any) Nonnegative-Integer))
        Nonnegative-Integer))
(define (estimate-mid-turn-tokens ctx
                                  session-id
                                  config
                                  #:emit-event [emit-event #f]
                                  #:estimate-tokens [estimate-tokens estimate-context-tokens])
  (define-values (estimated budget-threshold max-tokens)
    (compute-mid-turn-estimate ctx config estimate-tokens))
  (when (and (> estimated budget-threshold) emit-event session-id)
    (emit-event "context.mid-turn-over-budget"
                (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
  estimated)

;; ── Compact context mid-turn if over budget ──
(: maybe-compact-mid-turn
   (->* (Any (Listof Any) (U String #f) (HashTable Symbol Any))
        (#:emit-event (U (-> String Any Any) #f)
                      #:compact-proc (U (-> (Listof Any) (Listof Any)) #f)
                      #:estimate-tokens (-> (Listof Any) Nonnegative-Integer))
        (Listof Any)))
(define (maybe-compact-mid-turn sess
                                ctx
                                session-id
                                config
                                #:emit-event [emit-event #f]
                                #:compact-proc [compact-proc #f]
                                #:estimate-tokens [estimate-tokens estimate-context-tokens])
  (define-values (estimated budget-threshold max-tokens)
    (compute-mid-turn-estimate ctx config estimate-tokens))
  (cond
    [(<= estimated budget-threshold) ctx]
    [else
     (when (and emit-event session-id)
       (emit-event
        "context.mid-turn-over-budget"
        (hasheq 'estimated-tokens estimated 'budget budget-threshold 'max-tokens max-tokens)))
     (if compact-proc
         (compact-proc ctx)
         (raise-arguments-error
          'maybe-compact-mid-turn
          "#:compact-proc is required; default fallback removed to avoid any-wrap/c issues"
          "sess"
          sess))]))

;; ── Backward-compat wrapper ──
;; v0.33.7 W0a (N-A02): Added #:compact-proc passthrough so the #:session
;; path doesn't silently fail when compact-proc is needed.
(: check-mid-turn-budget!
   (->* ((Listof Any) (U String #f) (HashTable Symbol Any))
        (#:emit-event (U (-> String Any Any) #f)
                      #:estimate-tokens (-> (Listof Any) Nonnegative-Integer)
                      #:session (U Any #f)
                      #:compact-proc (U (-> (Listof Any) (Listof Any)) #f))
        Any))
(define (check-mid-turn-budget! ctx
                                session-id
                                config
                                #:emit-event [emit-event #f]
                                #:estimate-tokens [estimate-tokens estimate-context-tokens]
                                #:session [sess #f]
                                #:compact-proc [compact-proc #f])
  (if sess
      (maybe-compact-mid-turn sess
                              ctx
                              session-id
                              config
                              #:emit-event emit-event
                              #:compact-proc compact-proc
                              #:estimate-tokens estimate-tokens)
      (estimate-mid-turn-tokens ctx
                                session-id
                                config
                                #:emit-event emit-event
                                #:estimate-tokens estimate-tokens)))

;; Handle context overflow by compacting the context and retrying once.
(: call-with-overflow-recovery
   (->* ((-> Any) (Listof Any) String)
        (#:emit-event (U (-> String Any Any) #f)
                      #:compact-proc (U (-> (Listof Any) compaction-result) #f))
        Any))
(define (call-with-overflow-recovery thunk
                                     ctx
                                     session-id
                                     #:emit-event [emit-event #f]
                                     #:compact-proc [compact-proc #f])
  (define do-compact
    :
    (-> (Listof Any) compaction-result)
    (or compact-proc
        (lambda ([msgs : (Listof Any)])
          (define half (max 1 (quotient (length msgs) 2)))
          (compaction-result #f (- (length msgs) half) (take-right msgs half)))))
  (with-handlers
      ([(lambda ([e : Any]) (context-overflow-error? e))
        (lambda ([e : Any])
          (when emit-event
            (emit-event "context.overflow.detected" (hasheq 'error (exn-message (cast e exn)))))
          (define compact-result (do-compact ctx))
          (when emit-event
            (emit-event "context.overflow.compacted"
                        (hasheq 'original-size
                                (length ctx)
                                'removed-count
                                (compaction-result-removed-count compact-result)
                                'kept-count
                                (length (compaction-result-kept-messages compact-result)))))
          (thunk))]
       [exn:fail? (lambda ([e : Any]) (raise (cast e exn:fail)))])
    (thunk)))

;; ============================================================
;; v0.28.21 W6: Exploration loop detection
;; ============================================================

(: detect-exploration-loop (->* ((Listof String)) (Nonnegative-Integer) (U String #f)))
(define (detect-exploration-loop recent-tool-names [min-repeats 3])
  (define n (length recent-tool-names))
  (cond
    [(< n (* min-repeats 2)) #f]
    [else
     ;; Check for repeating 2-tool patterns in the last N tool calls
     (define recent (take-at-most recent-tool-names (* min-repeats 4)))
     (define pairs
       (for/list :
         (Listof (List String String))
         ([i (in-range (sub1 (length recent)))])
         (list (list-ref recent i) (list-ref recent (add1 i)))))
     (define pair-counts (count-occurrences pairs))
     ;; Find any pair repeated 3+ times
     (define looping-pair
       (for/or :
         (U (List String String) #f)
         ([pair : (List String String) (in-list pairs)])
         (if (>= (hash-ref pair-counts pair (lambda () 0)) min-repeats) pair #f)))
     (cond
       [looping-pair
        (format "exploration loop detected: ~a repeated ~a times"
                looping-pair
                (hash-ref pair-counts looping-pair))]
       [else #f])]))

;; Helper: count occurrences in a list of items
(: count-occurrences (-> (Listof Any) (Immutable-HashTable Any Nonnegative-Integer)))
(define (count-occurrences items)
  ;; v0.35.3 (I-06): Pure for/fold replaces mutable hash
  (for/fold ([counts
              :
              (Immutable-HashTable Any Nonnegative-Integer)
              (hash)])
            ([item (in-list items)])
    (hash-set counts item (add1 (hash-ref counts item (lambda () 0))))))

;; Helper: take at most N from list
(: take-at-most (All (A) (-> (Listof A) Nonnegative-Integer (Listof A))))
(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))
