#lang racket/base

;; runtime/context-assembly/token-metrics.rkt — Token measurement for state-aware assembly
;; v0.76.2 W0: Measure token savings from context filtering.

(require racket/list
         racket/string
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../../util/protocol-types.rkt"
                  message?
                  message-content
                  message-role
                  text-part?
                  text-part-text)
         "context-floor.rkt"
         "state-aware-builder.rkt")

(provide context-metrics
         context-metrics?
         context-metrics-task-state
         context-metrics-before-tokens
         context-metrics-after-tokens
         context-metrics-savings-tokens
         context-metrics-savings-pct
         context-metrics-category-breakdown
         context-metrics-timestamp
         measure-context-size
         compute-savings
         category-breakdown
         measure-context-assembly)

;; ── Struct ──

(struct context-metrics
        (task-state before-tokens
                    after-tokens
                    savings-tokens
                    savings-pct
                    category-breakdown
                    timestamp)
  #:transparent)

;; ── Token counting ──

;; measure-context-size : (listof message?) -> exact-nonnegative-integer?
(define (measure-context-size messages)
  (estimate-context-tokens messages))

;; compute-savings : exact-nonnegative-integer? exact-nonnegative-integer?
;;                -> (values exact-integer? real?)
;; Returns (values savings-tokens savings-percentage)
(define (compute-savings before after)
  (define savings (- before after))
  (define pct
    (if (> before 0)
        (* 100.0 (/ savings before))
        0.0))
  (values savings pct))

;; category-breakdown : tiered-context? -> hash?
;; Returns token counts per tier and for working-set/conclusions if present.
(define (category-breakdown tc)
  (define tier-a-tokens (measure-context-size (tiered-context-tier-a tc)))
  (define tier-b-tokens (measure-context-size (tiered-context-tier-b tc)))
  (define tier-c-tokens (measure-context-size (tiered-context-tier-c tc)))
  (hasheq 'tier-a
          tier-a-tokens
          'tier-b
          tier-b-tokens
          'tier-c
          tier-c-tokens
          'total
          (+ tier-a-tokens tier-b-tokens tier-c-tokens)))

;; ── Assembly wrapper ──

;; measure-context-assembly :
;;   (listof message?) #:task-state any/c #:conclusions (listof any/c) ->
;;   (values tiered-context? context-metrics?)
;;
;; Wraps build-tiered-context/state-aware and returns both the context
;; and metrics measuring before/after token counts.
(define (measure-context-assembly messages
                                  #:task-state [task-state #f]
                                  #:conclusions [conclusions '()]
                                  #:tier-b-count [tier-b-count #f]
                                  #:tier-c-count [tier-c-count 4]
                                  #:working-set-messages [ws-messages '()]
                                  #:trace [trace-cb #f])
  ;; Measure input before filtering
  (define before-tokens (measure-context-size messages))

  ;; Build state-aware context
  (define tc
    (build-tiered-context/state-aware messages
                                      #:tier-b-count tier-b-count
                                      #:tier-c-count tier-c-count
                                      #:working-set-messages ws-messages
                                      #:task-state task-state
                                      #:conclusions conclusions
                                      #:trace trace-cb))

  ;; Measure output after filtering
  (define after-tokens (measure-context-size (tiered-context->message-list tc)))

  ;; Compute savings
  (define-values (savings-tokens savings-pct) (compute-savings before-tokens after-tokens))

  ;; Category breakdown
  (define breakdown (category-breakdown tc))

  ;; Build metrics
  (define state-name
    (cond
      [(not task-state) 'none]
      [(symbol? task-state) task-state]
      [else 'unknown]))

  (define metrics
    (context-metrics state-name
                     before-tokens
                     after-tokens
                     savings-tokens
                     savings-pct
                     breakdown
                     (current-seconds)))

  (values tc metrics))
