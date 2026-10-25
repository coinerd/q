#lang racket/base

;; runtime/context-assembly/conclusion-ranker.rkt — Deterministic conclusion ranking
;; STABILITY: evolving
;; CONSUMERS: state-aware-builder, tests
;;
;; Scores conclusions by state match, recency, dependency overlap, category priority,
;; and explicit tags. Deterministic — no LLM calls. Used to enforce a hard token budget.

(require racket/contract
         racket/list
         "task-conclusion.rkt")

;; ── Category Priority ──

(define category-priority
  '((error-cause . 10) (decision . 8)
                       (test-result . 7)
                       (pattern .
                         5)
                       (fact . 3)))

(define (category-score c)
  (define cat (task-conclusion-category c))
  (cdr (or (assq cat category-priority) '(0 . 0))))

;; ── Scoring ──

;; Score a single conclusion against the current task state and active files.
;; Higher score = more relevant. Factors:
;;   - State match: +50 if fsm-state-origin matches current state
;;   - Category priority: 0-10
;;   - Recency: max 20 points, based on age in seconds
;;   - Tag overlap: +5 per matching tag
;;   - Has dependencies: +3 (indicates curated conclusion)
;; R99a: provenance-kind entries (recognized legacy deterministic fallbacks and
;; future internal 'provenance records) always score 0 — they receive no
;; state/recency/tag/dependency boosts and rank below every insight. Ties are
;; broken deterministically in rank-and-budget (insight before provenance).
(define (score-conclusion c current-state active-tags [now (current-seconds)])
  (if (provenance-kind? c)
      0
      (let* ([state-score
              (if (and current-state (eq? (task-conclusion-fsm-state-origin c) current-state)) 50 0)]
             [cat-score (category-score c)]
             [age (- now (task-conclusion-timestamp c))]
             [recency-score (max 0 (- 20 (quotient age 300)))] ; decay over 5min intervals
             [c-tags (task-conclusion-relevance-tags c)]
             [tag-score (* 5 (for/sum ([t (in-list c-tags)]) (if (member t active-tags) 1 0)))]
             [dep-score (if (pair? (task-conclusion-dependencies c)) 3 0)])
        (+ state-score cat-score recency-score tag-score dep-score))))

;; ── Ranking ──

;; Rank conclusions by score (highest first), applying a token budget.
;; Returns a bounded list of conclusions whose total estimated tokens
;; stay within max-conclusion-tokens.
(define (rank-and-budget conclusions
                         #:current-state [current-state #f]
                         #:active-tags [active-tags '()]
                         #:max-conclusion-tokens [max-tokens 2000]
                         #:token-estimate
                         [token-estimate
                          (lambda (c) (* 4 (+ 10 (string-length (task-conclusion-text c)))))])
  (define now (current-seconds))
  ;; R99a: decorate with input index for a total deterministic order:
  ;; score desc → insight before provenance → original input order.
  (define scored
    (for/list ([c (in-list conclusions)]
               [i (in-naturals)])
      (cons i (cons c (score-conclusion c current-state active-tags now)))))
  (define sorted
    (sort scored
          (lambda (a b)
            (define sa (cddr a))
            (define sb (cddr b))
            (cond
              [(> sa sb) #t]
              [(< sa sb) #f]
              ;; tie: insights (provenance-kind? = #f) first
              [(not (provenance-kind? (cadr a))) (provenance-kind? (cadr b))]
              [(not (provenance-kind? (cadr b))) #f]
              ;; full tie: stable input order
              [else (< (car a) (car b))]))))
  ;; Greedy fill within budget
  (define-values (selected _tokens)
    (for/fold ([acc '()]
               [used 0])
              ([sc (in-list sorted)]
               #:break (> used max-tokens))
      (define c (cadr sc))
      (define cost (token-estimate c))
      (if (<= (+ used cost) max-tokens)
          (values (cons c acc) (+ used cost))
          (values acc used))))
  (reverse selected))

;; ── Exports ──

(provide (contract-out [score-conclusion
                        (->* (task-conclusion? (or/c symbol? #f) (listof symbol?))
                             (exact-integer?)
                             exact-nonnegative-integer?)]
                       [rank-and-budget
                        (->* ((listof task-conclusion?))
                             (#:current-state (or/c symbol? #f)
                              #:active-tags (listof symbol?)
                              #:max-conclusion-tokens exact-nonnegative-integer?
                              #:token-estimate (-> task-conclusion? exact-nonnegative-integer?))
                             (listof task-conclusion?))]))
