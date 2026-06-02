#lang racket/base

;; q/runtime/goal-codec.rkt — Serialization for goal state types
;;
;; Provides hash round-trip functions for goal-state, goal-check,
;; evaluation-result, and check-result.

(require racket/contract
         "goal-types.rkt")

(provide (contract-out
          [goal-state->hash (-> goal-state? hash?)]
          [hash->goal-state (-> hash? goal-state?)]
          [goal-check->hash (-> goal-check? hash?)]
          [hash->goal-check (-> hash? goal-check?)]
          [evaluation-result->hash (-> evaluation-result? hash?)]
          [hash->evaluation-result (-> hash? evaluation-result?)]
          [check-result->hash (-> check-result? hash?)]
          [hash->check-result (-> hash? check-result?)]))

;; --------------------------------------------------
;; Constants (re-imported for default values)
;; --------------------------------------------------

(define DEFAULT-GOAL-MAX-TURNS 8)

;; --------------------------------------------------
;; goal-check
;; --------------------------------------------------

(define (goal-check->hash gc)
  (hasheq 'command
          (goal-check-command gc)
          'expected-exit
          (goal-check-expected-exit gc)
          'label
          (goal-check-label gc)))

(define (hash->goal-check h)
  (goal-check (hash-ref h 'command) (hash-ref h 'expected-exit 0) (hash-ref h 'label "")))

;; --------------------------------------------------
;; check-result
;; --------------------------------------------------

(define (check-result->hash cr)
  (hasheq 'label
          (check-result-label cr)
          'exit-code
          (check-result-exit-code cr)
          'stdout
          (check-result-stdout cr)
          'stderr
          (check-result-stderr cr)
          'timed-out?
          (check-result-timed-out? cr)
          'elapsed-ms
          (check-result-elapsed-ms cr)))

(define (hash->check-result h)
  (check-result (hash-ref h 'label "")
                (hash-ref h 'exit-code 1)
                (hash-ref h 'stdout "")
                (hash-ref h 'stderr "")
                (hash-ref h 'timed-out? #f)
                (hash-ref h 'elapsed-ms 0)))

;; --------------------------------------------------
;; evaluation-result
;; --------------------------------------------------

(define (evaluation-result->hash er)
  (hasheq 'achieved?
          (evaluation-result-achieved? er)
          'reason
          (evaluation-result-reason er)
          'check-results
          (map check-result->hash (evaluation-result-check-results er))
          'model-used
          (evaluation-result-model-used er)
          'token-cost
          (evaluation-result-token-cost er)))

(define (hash->evaluation-result h)
  (evaluation-result (hash-ref h 'achieved? #f)
                     (hash-ref h 'reason "")
                     (map hash->check-result (hash-ref h 'check-results '()))
                     (hash-ref h 'model-used "")
                     (hash-ref h 'token-cost 0)))

;; --------------------------------------------------
;; goal-state
;; --------------------------------------------------

(define (goal-state->hash gs)
  (hasheq 'id
          (goal-state-id gs)
          'goal-text
          (goal-state-goal-text gs)
          'status
          (symbol->string (goal-state-status gs))
          'turns-used
          (goal-state-turns-used gs)
          'max-turns
          (goal-state-max-turns gs)
          'evaluator-model
          (goal-state-evaluator-model gs)
          'evaluator-mode
          (symbol->string (goal-state-evaluator-mode gs))
          'checks
          (map goal-check->hash (goal-state-checks gs))
          'evaluations
          (map evaluation-result->hash (goal-state-evaluations gs))
          'last-evaluation
          (let ([le (goal-state-last-evaluation gs)]) (and le (evaluation-result->hash le)))
          'started-at
          (goal-state-started-at gs)
          'updated-at
          (goal-state-updated-at gs)
          'meta
          (goal-state-meta gs)))

(define (hash->goal-state h)
  (goal-state (hash-ref h 'id "")
              (hash-ref h 'goal-text "")
              (let ([s (hash-ref h 'status "active")])
                (if (symbol? s) s (string->symbol s)))
              (hash-ref h 'turns-used 0)
              (hash-ref h 'max-turns DEFAULT-GOAL-MAX-TURNS)
              (hash-ref h 'evaluator-model "auto")
              (let ([m (hash-ref h 'evaluator-mode "transcript")])
                (if (symbol? m) m (string->symbol m)))
              (map hash->goal-check (hash-ref h 'checks '()))
              (map hash->evaluation-result (hash-ref h 'evaluations '()))
              (let ([le (hash-ref h 'last-evaluation #f)]) (and le (hash->evaluation-result le)))
              (hash-ref h 'started-at 0)
              (hash-ref h 'updated-at 0)
              (hash-ref h 'meta #f)))
