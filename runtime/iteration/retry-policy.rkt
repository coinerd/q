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
         detect-exploration-loop)

;; ── Typed imports from untyped modules ──────────────────────────

(require/typed "../auto-retry.rkt"
               [context-overflow-error? (-> Any Boolean)])

(require/typed "../compactor.rkt"
               [#:struct compaction-result
                         ([summary-message : (U String #f)]
                          [removed-count : Integer]
                          [kept-messages : (Listof Any)])])

(require/typed "../../util/protocol-types.rkt"
               [message-content (-> Any Any)]
               [text-part? (-> Any Boolean)]
               [text-part-text (-> Any String)])

(require/typed "../runtime-helpers.rkt"
               [emit-session-event! (-> Any String String Any Any)])

(require/typed "../session-compaction.rkt"
               [compact-context-mid-turn (-> Any (Listof Any) (Listof Any))])

(require/typed "loop-state.rkt"
               [resolve-estimate-tokens (-> (-> (Listof Any) Nonnegative-Integer))])

;; ── Shared token estimation logic ──
;; Returns (values estimated budget-threshold max-tokens).
(: compute-mid-turn-estimate
   (-> (Listof Any)
       (HashTable Symbol Any)
       (-> (Listof Any) Nonnegative-Integer)
       (Values Nonnegative-Integer Nonnegative-Integer Nonnegative-Integer)))
(define (compute-mid-turn-estimate ctx config estimate-tokens)
  (define max-tokens : Nonnegative-Integer
    (let ([v (hash-ref config 'max-context-tokens #f)])
      (if (exact-nonnegative-integer? v) v 128000)))
  (define budget-threshold : Nonnegative-Integer
    (cast (exact-floor (* max-tokens 9/10)) Nonnegative-Integer))
  (define texts
    (for/list : (Listof String)
              ([msg (in-list ctx)])
      (define content (message-content msg))
      (cond
        [(string? content) content]
        [(list? content)
         (apply string-append
                (for/list : (Listof String)
                          ([part (in-list content)]
                           #:when (text-part? part))
                  (text-part-text part)))]
        [else ""])))
  (define estimated
    (for/sum : Nonnegative-Integer
             ([t (in-list texts)])
      (estimate-tokens (list (hasheq 'content t)))))
  (values estimated budget-threshold max-tokens))

;; ── Estimate token count for mid-turn context ──
(: estimate-mid-turn-tokens
   (->* ((Listof Any) Any (U String #f) (HashTable Symbol Any))
        (#:estimate-tokens (-> (Listof Any) Nonnegative-Integer))
        Nonnegative-Integer))
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
     (hasheq 'estimated-tokens estimated
             'budget budget-threshold
             'max-tokens max-tokens)))
  estimated)

;; ── Compact context mid-turn if over budget ──
(: maybe-compact-mid-turn
   (->* (Any (Listof Any) Any (U String #f) (HashTable Symbol Any))
        (#:estimate-tokens (-> (Listof Any) Nonnegative-Integer))
        (Listof Any)))
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
        (hasheq 'estimated-tokens estimated
                'budget budget-threshold
                'max-tokens max-tokens)))
     (compact-context-mid-turn sess ctx)]))

;; ── Backward-compat wrapper ──
(: check-mid-turn-budget!
   (->* ((Listof Any) Any (U String #f) (HashTable Symbol Any))
        (#:estimate-tokens (-> (Listof Any) Nonnegative-Integer)
         #:session (U Any #f))
        Any))
(define (check-mid-turn-budget! ctx
                                bus
                                session-id
                                config
                                #:estimate-tokens [estimate-tokens (resolve-estimate-tokens)]
                                #:session [sess #f])
  (if sess
      (maybe-compact-mid-turn sess ctx bus session-id config
                              #:estimate-tokens estimate-tokens)
      (estimate-mid-turn-tokens ctx bus session-id config
                               #:estimate-tokens estimate-tokens)))

;; Handle context overflow by compacting the context and retrying once.
(: call-with-overflow-recovery
   (->* ((-> Any) (Listof Any) Any String)
        (#:compact-proc (U (-> (Listof Any) compaction-result) #f))
        Any))
(define (call-with-overflow-recovery
         thunk ctx bus session-id
         #:compact-proc [compact-proc #f])
  (define do-compact
    : (-> (Listof Any) compaction-result)
    (or compact-proc
        (lambda ([msgs : (Listof Any)])
          (define half (max 1 (quotient (length msgs) 2)))
          (compaction-result #f
                             (- (length msgs) half)
                             (take-right msgs half)))))
  (with-handlers ([(lambda ([e : Any]) (context-overflow-error? e))
                   (lambda ([e : Any])
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.detected"
                                          (hasheq 'error (exn-message (cast e exn))))
                     (define compact-result (do-compact ctx))
                     (emit-session-event! bus
                                          session-id
                                          "context.overflow.compacted"
                                          (hasheq 'original-size
                                                  (length ctx)
                                                  'removed-count
                                                  (compaction-result-removed-count
                                                   compact-result)
                                                  'kept-count
                                                  (length (compaction-result-kept-messages
                                                           compact-result))))
                     (thunk))]
                  [exn:fail? (lambda ([e : Any]) (raise (cast e exn:fail)))])
    (thunk)))

;; ============================================================
;; v0.28.21 W6: Exploration loop detection
;; ============================================================

(: detect-exploration-loop (->* ((Listof String)) (Nonnegative-Integer)
                                (U String #f)))
(define (detect-exploration-loop recent-tool-names [min-repeats 3])
  (define n (length recent-tool-names))
  (cond
    [(< n (* min-repeats 2)) #f]
    [else
     ;; Check for repeating 2-tool patterns in the last N tool calls
     (define recent (take-at-most recent-tool-names (* min-repeats 4)))
     (define pairs
       (for/list : (Listof (List String String))
                 ([i (in-range (sub1 (length recent)))])
         (list (list-ref recent i) (list-ref recent (add1 i)))))
     (define pair-counts (count-occurrences pairs))
     ;; Find any pair repeated 3+ times
     (define looping-pair
       (for/or : (U (List String String) #f)
               ([pair : (List String String) (in-list pairs)])
         (if (>= (hash-ref pair-counts pair (lambda () 0)) min-repeats)
             pair
             #f)))
     (cond
       [looping-pair
        (format "exploration loop detected: ~a repeated ~a times"
                looping-pair
                (hash-ref pair-counts looping-pair))]
       [else #f])]))

;; Helper: count occurrences in a list of items
(: count-occurrences (-> (Listof Any) (HashTable Any Nonnegative-Integer)))
(define (count-occurrences items)
  (define counts : (HashTable Any Nonnegative-Integer) (make-hash))
  (for ([item (in-list items)])
    (hash-set! counts item (add1 (hash-ref counts item (lambda () 0)))))
  counts)

;; Helper: take at most N from list
(: take-at-most (All (A) (-> (Listof A) Nonnegative-Integer (Listof A))))
(define (take-at-most lst n)
  (if (> (length lst) n)
      (take lst n)
      lst))
