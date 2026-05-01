#lang racket/base

;; runtime/context-assembly/selection.rkt — message partitioning, fitting, context building
;;
;; Selection logic: what messages to include/exclude, pinning, pair-preserving fit.

(require racket/list
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
                  fit-messages-pair-preserving)
         (only-in "../context-pinning.rkt" partition-messages/working-set)
         (only-in "../working-set.rkt"
                  working-set?
                  working-set-resolve-messages)
         "budgeting.rkt")

(provide build-assembled-context
         build-session-context)

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
  (define (emit-trace phase data)
    (when trace (trace phase data)))
  (define raw-messages (build-session-context idx))
  (emit-trace 'start (hash 'raw-count (length raw-messages)))
  (if (null? raw-messages)
      (begin
        (emit-trace 'empty (hash))
        (context-result '() 0 0 0 0 #f '() #f))
      (let ()
        (define token-memo (make-hash))
        (define (memoized-estimate msg)
          (hash-ref! token-memo (message-id msg) (lambda () (estimate-message-tokens msg))))

        (define max-tokens (context-assembly-config-recent-tokens config))
        (define ws-messages
          (if ws (working-set-resolve-messages ws raw-messages message-id) '()))
        (define ws-message-ids (if ws (map message-id ws-messages) '()))
        (define-values (pinned removable)
          (partition-messages/working-set raw-messages ws-message-ids))
        (define pinned-tokens (for/sum ([m (in-list pinned)]) (memoized-estimate m)))
        (define pinned-count (length pinned))
        (define removable-count (length removable))
        (emit-trace 'phase1-pinned
                    (hash 'pinned-count pinned-count
                          'removable-count removable-count
                          'pinned-tokens pinned-tokens))

        (define remaining-budget (- max-tokens pinned-tokens))
        (define-values (recent excluded)
          (if (<= remaining-budget 0)
              (values '() removable)
              (let ()
                (define kept (fit-messages-pair-preserving removable remaining-budget memoized-estimate))
                (define kept-ids (for/hash ([m (in-list kept)]) (values (message-id m) #t)))
                (define exc (filter (lambda (m) (not (hash-has-key? kept-ids (message-id m)))) removable))
                (values kept exc))))
        (define recent-count (length recent))
        (define excluded-count (length excluded))
        (emit-trace 'phase3-fitted
                    (hash 'recent-count recent-count
                          'excluded-count excluded-count
                          'remaining-budget remaining-budget))

        ;; Phase 2: Generate summary for excluded entries
        (define summary-obj
          (and generate-summary-proc
               (generate-summary-proc excluded provider model-name cache)))
        (emit-trace 'phase2-summary
                    (hash 'has-summary? (and summary-obj #t)
                          'entry-count (and summary-obj
                                            ((hash-ref summary-obj 'entry-count (lambda () 0))))))

        (define summary-msg #f) ;; Summary msg construction delegated to caller
        ;; Phase 4: Generate catalog
        (define catalog
          (if generate-catalog-proc
              (generate-catalog-proc excluded
                                     (context-assembly-config-max-catalog-entries config)
                                     (context-assembly-config-max-catalog-tokens config))
              '()))

        ;; Phase 5: Reassemble in original order
        (define pinned-ids (for/hash ([m (in-list pinned)]) (values (message-id m) #t)))
        (define recent-ids (for/hash ([m (in-list recent)]) (values (message-id m) #t)))
        (define result-messages
          (for/list ([m (in-list raw-messages)]
                     #:when (or (hash-has-key? pinned-ids (message-id m))
                                (hash-has-key? recent-ids (message-id m))))
            m))

        (define result-with-pin (ensure-first-user-pinned result-messages raw-messages))
        (define total-tokens (for/sum ([m (in-list result-with-pin)]) (memoized-estimate m)))
        (define over-budget? (> total-tokens max-tokens))

        (context-result result-with-pin
                        total-tokens
                        pinned-count
                        recent-count
                        excluded-count
                        over-budget?
                        catalog
                        summary-obj))))

;; Build session context from index (tree walk)
(define (build-session-context idx)
  (define leaf (active-leaf idx))
  (cond
    [(not leaf) '()]
    [else
     (define path (get-branch idx (message-id leaf)))
     (cond
       [(not path) '()]
       [else (assemble-context path)])]))

(define (assemble-context path)
  (define-values (pre-compaction post-compaction) (split-at-compaction path))
  (define relevant-entries (if post-compaction post-compaction path))
  (filter-map entry->context-message relevant-entries))

(define (split-at-compaction path)
  (define compaction-idx
    (for/last ([entry (in-list path)]
               [i (in-naturals)]
               #:when (compaction-summary-entry? entry))
      i))
  (cond
    [(not compaction-idx) (values path #f)]
    [else (define-values (pre post) (split-at path compaction-idx))
          (values pre post)]))

(define (entry->context-message entry)
  (define kind (message-kind entry))
  (cond
    [(memq kind '(message)) entry]
    [(eq? kind 'compaction-summary) (transform-summary-to-user entry)]
    [(eq? kind 'branch-summary) (transform-summary-to-user entry)]
    [(memq kind '(session-info model-change thinking-level-change)) #f]
    [(memq kind '(tool-result bash-execution)) entry]
    [(eq? kind 'system-instruction) entry]
    [(eq? kind 'custom-message) entry]
    [else entry]))

(define (transform-summary-to-user entry)
  (struct-copy message entry [role 'user]))

;; Re-export needed from session-index
(require (only-in "../session-index.rkt" active-leaf get-branch))
