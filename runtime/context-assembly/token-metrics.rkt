#lang racket/base

;; runtime/context-assembly/token-metrics.rkt — Token measurement for state-aware assembly
;; v0.76.2 W0: Measure token savings from context filtering.

(require racket/list
         racket/string
         (only-in "../../llm/token-budget.rkt" estimate-context-tokens)
         (only-in "../../util/content-parts.rkt" text-part?)
         (only-in "../../util/message.rkt" message? message-content message-role)
         (only-in "../../util/protocol-types.rkt" text-part-text)
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
         context-token-telemetry
         context-token-telemetry?
         context-token-telemetry-conclusion-budget-remaining
         context-token-telemetry-tier-a-tokens
         context-token-telemetry-tier-b-tokens
         context-token-telemetry-tier-c-tokens
         context-token-telemetry-conclusion-tokens
         context-token-telemetry-working-set-tokens
         context-token-telemetry-recent-tokens
         context-token-telemetry-total-tokens
         context-token-telemetry-timestamp
         measure-context-size
         compute-savings
         category-breakdown
         measure-context-token-telemetry
         compute-conclusion-coverage
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

;; Observation-only tier/category token telemetry.
;; The category fields intentionally measure caller-supplied slices rather than
;; mutating the tiered context; later v0.77 waves reuse this as a baseline.
(struct context-token-telemetry
        (tier-a-tokens tier-b-tokens
                       tier-c-tokens
                       conclusion-tokens
                       working-set-tokens
                       recent-tokens
                       total-tokens
                       timestamp
                       conclusion-budget-remaining)
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

;; measure-context-token-telemetry :
;;   tiered-context?
;;   #:conclusion-messages (listof message?)
;;   #:working-set-messages (listof message?)
;;   #:recent-messages (listof message?)
;;   -> context-token-telemetry?
;; Reports tier A/B/C token estimates plus category estimates without changing
;; assembly behavior.
(define (measure-context-token-telemetry tc
                                         #:conclusion-messages [conclusion-messages '()]
                                         #:working-set-messages [working-set-messages '()]
                                         #:recent-messages [recent-messages '()])
  (define tier-a-tokens (measure-context-size (tiered-context-tier-a tc)))
  (define tier-b-tokens (measure-context-size (tiered-context-tier-b tc)))
  (define tier-c-tokens (measure-context-size (tiered-context-tier-c tc)))
  (define conc-tokens (measure-context-size conclusion-messages))
  (define budget (current-conclusion-token-budget))
  (define remaining
    (if (> budget 0)
        (- budget conc-tokens)
        -1))
  (context-token-telemetry tier-a-tokens
                           tier-b-tokens
                           tier-c-tokens
                           conc-tokens
                           (measure-context-size working-set-messages)
                           (measure-context-size recent-messages)
                           (+ tier-a-tokens tier-b-tokens tier-c-tokens)
                           (current-seconds)
                           remaining))

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

;; ── Conclusion coverage ──

;; compute-conclusion-coverage :
;;   (listof string?) exact-nonnegative-integer? -> real?
;;
;; Computes ratio of record_conclusion calls to total tool calls.
;; tool-names: list of tool names called in the last N turns.
;; Returns coverage ratio (0.0–1.0+).
(define (compute-conclusion-coverage tool-names)
  (define total (length tool-names))
  (if (zero? total)
      0.0
      (let ([conclusion-calls
             (count
              (lambda (name)
                (member name '("record_conclusion" "save-conclusion" "save_conclusion") string=?))
              tool-names)])
        (/ conclusion-calls total))))
