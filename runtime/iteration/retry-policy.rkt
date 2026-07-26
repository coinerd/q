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
         count-occurrences
         current-loop-cooldown-left)

;; ── Typed imports from untyped modules ──────────────────────────

(require/typed racket/dict [dict-ref (->* (Any Any) (Any) Any)])
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
;; v0.28.21 W6 / v0.99.68 W4: Exploration loop detection
;; W4: min-repeats 3→6, argument-aware detection, cooldown
;; ============================================================

;; Each entry can be either:
;;   - A string (tool name only, backward compat)
;;   - A (List String ...) where car is tool name, cadr is first argument
;; Argument-aware detection uses (tool . first-arg) as the pair key
;; to distinguish (read foo, read foo) from (read a, read b).

(: tool-name-key (-> Any String))
(define (tool-name-key entry)
  (cond
    [(string? entry) entry]
    [(and (list? entry) (pair? entry) (string? (car entry))) (car entry)]
    [else (format "~a" entry)]))

(: tool-argument-key (-> Any (U String #f)))
(define (tool-argument-key entry)
  (cond
    [(and (list? entry) (pair? entry) (pair? (cdr entry)) (string? (cadr entry))) (cadr entry)]
    [(string? entry) #f]
    [else #f]))

(: make-pair-key (-> Any Any String))
(define (make-pair-key a b)
  (define a-name (tool-name-key a))
  (define b-name (tool-name-key b))
  (define a-arg (tool-argument-key a))
  (define b-arg (tool-argument-key b))
  (cond
    ;; Full argument-aware key when both have arguments
    [(and a-arg b-arg) (format "~a:~a -> ~a:~a" a-name a-arg b-name b-arg)]
    ;; Fall back to tool-name-only when arguments unavailable
    [else (format "~a -> ~a" a-name b-name)]))

;; v0.99.68 W4: Cooldown parameter — after firing, suppress for N subsequent calls
(define current-loop-cooldown-left (make-parameter 0))

(: detect-exploration-loop (->* ((Listof Any)) (Nonnegative-Integer) (U String #f)))
(define (detect-exploration-loop recent-tool-names [min-repeats 6])
  ;; Check cooldown first
  (define cooldown (current-loop-cooldown-left))
  (cond
    [(positive? cooldown)
     (current-loop-cooldown-left (sub1 cooldown))
     #f]
    [else
     (define n (length recent-tool-names))
     (cond
       [(< n (* min-repeats 2)) #f]
       [else
        (define recent (take-at-most recent-tool-names (* min-repeats 4)))
        (define pair-keys
          (for/list :
            (Listof String)
            ([i (in-range (sub1 (length recent)))])
            (make-pair-key (list-ref recent i) (list-ref recent (add1 i)))))
        (define pair-counts (count-occurrences pair-keys))
        (define max-count
          (for/fold ([best
                      :
                      Nonnegative-Integer
                      0])
                    ([c
                      :
                      Nonnegative-Integer
                      (in-hash-values pair-counts)])
            (max best c)))
        (cond
          [(>= max-count min-repeats)
           (current-loop-cooldown-left min-repeats)
           (format "exploration loop detected: pair repeated ~a times (threshold: ~a)"
                   max-count
                   min-repeats)]
          [else #f])])]))

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
