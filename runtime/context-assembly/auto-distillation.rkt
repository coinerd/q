#lang racket/base

;; runtime/context-assembly/auto-distillation.rkt — Automatic fallback conclusions
;; STABILITY: evolving
;;
;; Identifies uncovered working-set entries (no matching conclusion origin-message-ids)
;; and produces deterministic fallback conclusions. Optional LLM distillation is injectable
;; but disabled by default. Timeout/failure returns deterministic fallback.

(require racket/contract
         racket/list
         (only-in "task-conclusion.rkt"
                  task-conclusion
                  task-conclusion?
                  task-conclusion-id
                  task-conclusion-origin-message-ids
                  task-conclusion-text
                  task-conclusion-category
                  task-conclusion-fsm-state-origin
                  task-conclusion-timestamp
                  task-conclusion-relevance-tags
                  task-conclusion-dependencies))

;; ── Uncovered Entry Detection ──

;; Find WS entries (represented as message IDs) that have no covering conclusion.
;; A message ID is "covered" if any conclusion's origin-message-ids contains it.
(define (find-uncovered-entries ws-message-ids conclusions)
  (define covered-ids
    (for/fold ([s (hash)])
              ([c (in-list conclusions)]
               #:when (task-conclusion? c))
      (for/fold ([acc s]) ([mid (in-list (task-conclusion-origin-message-ids c))])
        (hash-set acc mid #t))))
  (filter (lambda (id) (not (hash-has-key? covered-ids id))) ws-message-ids))

;; ── Deterministic Fallback ──

;; Generate a deterministic fallback conclusion for an uncovered entry.
;; Uses the message ID and a generic template.
(define (make-deterministic-fallback msg-id current-state)
  (task-conclusion (format "auto-~a" msg-id)
                   (format "[Auto] Previously read file (origin: ~a)" msg-id)
                   'fact
                   (or current-state 'idle)
                   (list msg-id)
                   (current-seconds)
                   '(auto-distilled)
                   '()))

;; Generate fallback conclusions for all uncovered entries.
(define (generate-fallback-conclusions uncovered-ids current-state)
  (for/list ([id (in-list uncovered-ids)])
    (make-deterministic-fallback id current-state)))

;; ── Optional LLM Distillation Interface ──

;; LLM distillation function type: (listof string?) symbol? -> (listof task-conclusion?)
;; When provided, called instead of deterministic fallback.
;; On timeout/error, falls back to deterministic.

(define (distill-with-llm uncovered-ids current-state llm-distill-fn timeout-secs)
  (with-handlers ([exn:fail? (lambda (e)
                               (generate-fallback-conclusions uncovered-ids current-state))])
    (define result
      (sync/timeout timeout-secs (thread (lambda () (llm-distill-fn uncovered-ids current-state)))))
    (cond
      [(not result) (generate-fallback-conclusions uncovered-ids current-state)]
      [(and (list? result) (andmap task-conclusion? result)) result]
      [else (generate-fallback-conclusions uncovered-ids current-state)])))

;; ── Main Entry Point ──

;; Feature flag for auto-distillation
(define current-auto-distillation-enabled? (make-parameter #f))

;; Optional LLM distillation function (parameter)
(define current-llm-distill-fn (make-parameter #f))

;; Generate auto-conclusions for uncovered WS entries.
;; Returns (listof task-conclusion?) — either LLM-distilled or deterministic fallbacks.
(define (auto-distill ws-message-ids conclusions current-state)
  (define uncovered (find-uncovered-entries ws-message-ids conclusions))
  (cond
    [(null? uncovered) '()]
    [(not (current-auto-distillation-enabled?)) '()]
    [(current-llm-distill-fn) (distill-with-llm uncovered current-state (current-llm-distill-fn) 5)]
    [else (generate-fallback-conclusions uncovered current-state)]))

;; ── Exports ──

(provide current-auto-distillation-enabled?
         current-llm-distill-fn
         (contract-out [find-uncovered-entries
                        (-> (listof string?) (listof task-conclusion?) (listof string?))]
                       [generate-fallback-conclusions
                        (-> (listof string?) (or/c symbol? #f) (listof task-conclusion?))]
                       [auto-distill
                        (-> (listof string?)
                            (listof task-conclusion?)
                            (or/c symbol? #f)
                            (listof task-conclusion?))]))
