#lang racket/base

;; runtime/context-assembly/selection.rkt — message partitioning, fitting, context building
;;
;; Selection logic: what messages to include/exclude, pinning, pair-preserving fit.
;;
;; TR MIGRATION DEFERRED (v0.30.9 investigation):
;;   Blocker: local mutable hash `token-memo` used as memoization cache.
;;   Resolution path: extract memo into a function parameter, making build-assembled-context
;;   pure. Then wrap with a thin impure shell that creates the memo. This would allow
;;   the core logic to be typed. Estimated effort: ~2 waves. Deferred to backlog.

(require racket/contract
         racket/match
         racket/list
         racket/set
         (only-in "../../util/protocol-types.rkt"
                  message
                  message-id
                  message-kind
                  message-role
                  message-parent-id
                  message-content
                  make-message
                  make-text-part
                  compaction-summary-entry?)
         (only-in "../context-policy.rkt"
                  estimate-message-tokens
                  ensure-first-user-pinned
                  fit-messages-with-importance-rescue)
         (only-in "../context-pinning.rkt" partition-messages/working-set)
         (only-in "../working-set.rkt" working-set? working-set-resolve-messages)
         "budgeting.rkt"
         (only-in "../context-summary.rkt"
                  context-summary?
                  context-summary-text
                  context-summary-entry-count
                  context-summary-from-id
                  context-summary-to-id)
         (only-in "../../util/errors.rkt" warn-deprecated!))

;; R10: Call-options struct bundling keyword args for build-assembled-context
(struct context-assembly-call-options
        (cache provider
               model-name
               trace-callback
               working-set
               generate-summary-proc
               generate-catalog-proc
               estimate-text-proc)
  #:transparent)

(define (make-context-assembly-call-options #:cache [cache #f]
                                            #:provider [provider #f]
                                            #:model-name [model-name #f]
                                            #:trace-callback [trace #f]
                                            #:working-set [ws #f]
                                            #:generate-summary-proc [summary-proc #f]
                                            #:generate-catalog-proc [catalog-proc #f]
                                            #:estimate-text-proc [estimate-proc #f])
  (context-assembly-call-options cache
                                 provider
                                 model-name
                                 trace
                                 ws
                                 summary-proc
                                 catalog-proc
                                 estimate-proc))

(provide (struct-out context-assembly-call-options)
         (contract-out [make-context-assembly-call-options (-> any/c)]
                       [build-assembled-context (-> any/c any/c any/c)]
                       [build-assembled-context/raw (-> any/c any/c any/c #:memo any/c any/c)]
                       [build-assembled-context/v2 (-> any/c any/c any/c any/c)]
                       [build-session-context (-> any/c any/c)]
                       [select-messages
                        (-> list? list? exact-nonnegative-integer? procedure? (values list? list?))]))

;; F23: Pure selection function — testable without infrastructure.
;; Takes pinned messages, removable messages, a budget, and an estimation function.
;; Returns (values selected excluded) where selected fits within budget.
(define (select-messages pinned removable budget estimate-fn)
  (define pinned-tokens (for/sum ([m (in-list pinned)]) (estimate-fn m)))
  (define remaining-budget (- budget pinned-tokens))
  (if (<= remaining-budget 0)
      (values pinned removable)
      (let ()
        (define kept (fit-messages-with-importance-rescue removable remaining-budget estimate-fn))
        (define kept-ids
          (for/hash ([m (in-list kept)])
            (values (message-id m) #t)))
        (define excluded
          (filter (lambda (m) (not (hash-has-key? kept-ids (message-id m)))) removable))
        (values (append pinned kept) excluded))))

(define (build-assembled-context idx
                                 config
                                 #:cache [cache #f]
                                 #:provider [provider #f]
                                 #:model-name [model-name #f]
                                 #:trace-callback [trace #f]
                                 #:working-set [ws #f]
                                 #:generate-summary-proc [generate-summary-proc #f]
                                 #:generate-catalog-proc [generate-catalog-proc #f]
                                 #:estimate-text-proc [estimate-text-proc #f])
  (build-assembled-context/v2 idx
                              config
                              (make-context-assembly-call-options
                               #:cache cache
                               #:provider provider
                               #:model-name model-name
                               #:trace-callback trace
                               #:working-set ws
                               #:generate-summary-proc generate-summary-proc
                               #:generate-catalog-proc generate-catalog-proc
                               #:estimate-text-proc estimate-text-proc)))

;; R10: Simplified v2 API taking call-options struct
(define (build-assembled-context/v2 idx config opts)
  (define trace (context-assembly-call-options-trace-callback opts))
  (define (emit-trace phase data)
    (when trace
      (trace phase data)))
  (define ws (context-assembly-call-options-working-set opts))
  (define raw-messages (build-session-context idx))
  (emit-trace 'start (hash 'raw-count (length raw-messages)))
  (if (null? raw-messages)
      (begin
        (emit-trace 'empty (hash))
        (context-result '() 0 0 0 0 #f '() #f))
      (build-assembled-context/raw
       raw-messages
       config
       ws
       #:memo (make-hash)
       #:estimate-text-proc (context-assembly-call-options-estimate-text-proc opts)
       #:generate-summary-proc (context-assembly-call-options-generate-summary-proc opts)
       #:generate-catalog-proc (context-assembly-call-options-generate-catalog-proc opts)
       #:provider (context-assembly-call-options-provider opts)
       #:model-name (context-assembly-call-options-model-name opts)
       #:cache (context-assembly-call-options-cache opts)
       #:trace trace)))

;; ============================================================
;; build-assembled-context/raw — pure core with explicit memo
;; ============================================================
;; FD-02: Extracted from build-assembled-context to enable TR migration
;; and stage-level testing. The public API creates memo internally.

(define (build-assembled-context/raw raw-messages
                                     config
                                     ws
                                     #:memo memo
                                     #:estimate-text-proc [estimate-text-proc #f]
                                     #:generate-summary-proc [generate-summary-proc #f]
                                     #:generate-catalog-proc [generate-catalog-proc #f]
                                     #:provider [provider #f]
                                     #:model-name [model-name #f]
                                     #:cache [cache #f]
                                     #:trace [trace #f])
  (warn-deprecated! 'build-assembled-context/raw
                    "v0.47.0"
                    "Use build-tiered-context from context-assembly/serialization.rkt")
  (define (emit-trace phase data)
    (when trace
      (trace phase data)))
  (define memo-hit-box (or (and (hash? config) (hash-ref config 'memo-hit-counter #f)) (box 0)))
  (define base-estimate (or estimate-text-proc estimate-message-tokens))
  (define (memoized-estimate msg)
    (define id (message-id msg))
    (cond
      [(hash-has-key? memo id)
       (set-box! memo-hit-box (add1 (unbox memo-hit-box)))
       (hash-ref memo id)]
      [else
       (define est (base-estimate msg))
       (hash-set! memo id est)
       est]))

  (define max-tokens (context-assembly-config-recent-tokens config))
  (define ws-messages
    (if ws
        (working-set-resolve-messages ws raw-messages message-id)
        '()))
  (define ws-message-ids
    (if ws
        (map message-id ws-messages)
        '()))
  (define-values (pinned removable) (partition-messages/working-set raw-messages ws-message-ids))
  (define pinned-tokens (for/sum ([m (in-list pinned)]) (memoized-estimate m)))
  (define pinned-count (length pinned))
  (define removable-count (length removable))
  (emit-trace
   'phase1-pinned
   (hash 'pinned-count pinned-count 'removable-count removable-count 'pinned-tokens pinned-tokens))

  (define remaining-budget (- max-tokens pinned-tokens))
  (define-values (recent excluded)
    (if (<= remaining-budget 0)
        (values '() removable)
        (let ()
          (define kept
            (fit-messages-with-importance-rescue removable remaining-budget memoized-estimate))
          (define kept-ids
            (for/hash ([m (in-list kept)])
              (values (message-id m) #t)))
          (define exc (filter (lambda (m) (not (hash-has-key? kept-ids (message-id m)))) removable))
          (values kept exc))))
  (define recent-count (length recent))
  (define excluded-count (length excluded))
  (emit-trace 'phase3-fitted
              (hash 'recent-count
                    recent-count
                    'excluded-count
                    excluded-count
                    'remaining-budget
                    remaining-budget))

  ;; Phase 2: Generate summary for excluded entries
  (define summary-obj
    (and generate-summary-proc (generate-summary-proc excluded provider model-name cache)))
  (emit-trace 'phase2-summary
              (hash 'has-summary?
                    (and summary-obj #t)
                    'entry-count
                    (and summary-obj (context-summary-entry-count summary-obj))))

  ;; CA-01 FIX: Convert summary to a pinned user message so it survives budget fitting
  (define summary-msg
    (and summary-obj
         (let* ([summary-text (context-summary-text summary-obj)]
                [summary-tokens (base-estimate summary-text)])
           ;; Only inject if summary doesn't exceed 10% of budget
           (and (<= summary-tokens (* 0.1 max-tokens))
                (make-message (format "ctx-summary-~a-~a"
                                      (context-summary-from-id summary-obj)
                                      (context-summary-to-id summary-obj))
                              #f
                              'user
                              'context-assembly-summary
                              (list (make-text-part summary-text))
                              (current-seconds)
                              (hasheq 'pinned #t 'context-summary #t))))))
  ;; Phase 4: Generate catalog
  (define catalog
    (if generate-catalog-proc
        (generate-catalog-proc excluded
                               (context-assembly-config-max-catalog-entries config)
                               (context-assembly-config-max-catalog-tokens config))
        '()))

  (emit-trace 'phase4-catalog
              (hash 'catalog-entries (length catalog) 'has-catalog? (not (null? catalog))))

  ;; Phase 5: Reassemble in original order
  (define pinned-ids
    (for/hash ([m (in-list pinned)])
      (values (message-id m) #t)))
  (define recent-ids
    (for/hash ([m (in-list recent)])
      (values (message-id m) #t)))
  (define result-messages
    (for/list ([m (in-list raw-messages)]
               #:when (or (hash-has-key? pinned-ids (message-id m))
                          (hash-has-key? recent-ids (message-id m))))
      m))

  (define result-with-pin (ensure-first-user-pinned result-messages raw-messages))
  (define result-with-pin-and-summary
    (if summary-msg
        (cons summary-msg result-with-pin)
        result-with-pin))
  (define total-tokens (for/sum ([m (in-list result-with-pin-and-summary)]) (memoized-estimate m)))
  (define over-budget? (> total-tokens max-tokens))

  (emit-trace 'done
              (hash 'total-tokens
                    total-tokens
                    'over-budget?
                    over-budget?
                    'result-count
                    (length result-with-pin-and-summary)
                    'pinned-count
                    pinned-count
                    'recent-count
                    recent-count
                    'excluded-count
                    excluded-count
                    'memo-hits
                    (unbox memo-hit-box)))
  (context-result result-with-pin-and-summary
                  total-tokens
                  pinned-count
                  recent-count
                  excluded-count
                  over-budget?
                  catalog
                  summary-obj))

;; CA-05: Import shared session walk logic from session-walk.rkt
(require "session-walk.rkt") ;; provides build-session-context, assemble-context, etc.
