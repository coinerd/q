#lang racket/base

;; runtime/context-assembly/conclusion-ranker.rkt — Deterministic conclusion ranking
;; STABILITY: evolving
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
(define (score-conclusion c current-state active-tags [now (current-seconds)])
  (define state-score
    (if (and current-state (eq? (task-conclusion-fsm-state-origin c) current-state)) 50 0))
  (define cat-score (category-score c))
  (define age (- now (task-conclusion-timestamp c)))
  (define recency-score (max 0 (- 20 (quotient age 300)))) ; decay over 5min intervals
  (define c-tags (task-conclusion-relevance-tags c))
  (define tag-score (* 5 (for/sum ([t (in-list c-tags)]) (if (member t active-tags) 1 0))))
  (define dep-score (if (pair? (task-conclusion-dependencies c)) 3 0))
  (+ state-score cat-score recency-score tag-score dep-score))

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
  (define scored
    (for/list ([c (in-list conclusions)])
      (cons c (score-conclusion c current-state active-tags now))))
  ;; Sort by score descending
  (define sorted (sort scored > #:key cdr))
  ;; Greedy fill within budget
  (define-values (selected _tokens)
    (for/fold ([acc '()]
               [used 0])
              ([sc (in-list sorted)]
               #:break (> used max-tokens))
      (define c (car sc))
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
