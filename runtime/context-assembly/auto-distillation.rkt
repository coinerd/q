#lang racket/base

;; runtime/context-assembly/auto-distillation.rkt — Automatic fallback conclusions
;; STABILITY: evolving
;; CONSUMERS: state-aware-builder, loop-phases
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
                  task-conclusion-dependencies
                  ;; R99a: shared frozen recognizer re-exported for consumers of
                  ;; this module. R99b (owner-authorized): the deterministic
                  ;; fallback now emits INTERNAL 'provenance records; the frozen
                  ;; recognizer keeps classifying only old-signature records.
                  legacy-deterministic-fallback?))

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
;; v0.79.2 GAP-3: Accepted an optional content summary for richer fallback text.
;; v0.95.18 F9: Blank/whitespace content treated as absent.
;; R99b (BUG-0099, owner-authorized writer activation): the fallback is an
;; INTERNAL 'provenance record — an honest provenance marker, never a
;; fact-labeled raw excerpt. The text is a fixed sentence naming only the
;; origin message id. content-summary is KEPT IN THE ARITY for call-site
;; compatibility but is deliberately unused: no source excerpt, no summary
;; and no "previously read file" claim may leak into the record. Records stay
;; discoverable via the 'auto-distilled tag and the stable auto-<origin> id,
;; and carry the explicit 'deterministic-fallback tag. They are provenance
;; kind by CATEGORY (the frozen legacy recognizer keeps classifying only
;; old-signature [Auto] records stored before this change).
(define (make-deterministic-fallback msg-id current-state [content-summary #f])
  ;; R99b: intentionally unused for the text (arity compatibility only).
  (void content-summary)
  (task-conclusion (format "auto-~a" msg-id)
                   (format "Source message ~a retained as provenance; no distilled insight available." msg-id)
                   'provenance
                   (or current-state 'idle)
                   (list msg-id)
                   (current-seconds)
                   '(auto-distilled deterministic-fallback)
                   '()))

;; Generate fallback conclusions for all uncovered entries.
;; v0.79.2 GAP-3: Accept optional content-summary hash (msg-id -> string).
(define (generate-fallback-conclusions uncovered-ids current-state [content-summaries (hash)])
  (for/list ([id (in-list uncovered-ids)])
    (make-deterministic-fallback id current-state (hash-ref content-summaries id #f))))

;; ── Optional LLM Distillation Interface ──

;; LLM distillation function type: (listof string?) symbol? hash? -> (listof task-conclusion?)
;; When provided, called instead of deterministic fallback.
;; On timeout/error, falls back to deterministic.

(define (distill-with-llm uncovered-ids
                          current-state
                          llm-distill-fn
                          timeout-secs
                          [content-summaries (hash)])
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (generate-fallback-conclusions uncovered-ids current-state content-summaries))])
    (define result-ch (make-channel))
    ;; GAP-P v0.97.12: Use custodian for cleaner thread cleanup.
    (define thd-cust (make-custodian))
    (define thd
      (parameterize ([current-custodian thd-cust])
        (thread (lambda ()
                  (with-handlers ([exn:fail? (lambda (e) (channel-put result-ch 'error))])
                    (channel-put result-ch
                                 (llm-distill-fn uncovered-ids current-state content-summaries)))))))
    (define result (sync/timeout timeout-secs result-ch))
    (cond
      [(not result)
       (custodian-shutdown-all thd-cust)
       (generate-fallback-conclusions uncovered-ids current-state content-summaries)]
      [(eq? result 'error)
       (generate-fallback-conclusions uncovered-ids current-state content-summaries)]
      [(and (list? result) (andmap task-conclusion? result)) result]
      [else (generate-fallback-conclusions uncovered-ids current-state content-summaries)])))

;; ── Main Entry Point ──

;; Feature flag for auto-distillation
(define current-auto-distillation-enabled? (make-parameter #f))

;; Optional LLM distillation function (parameter)
(define current-llm-distill-fn (make-parameter #f))

;; Generate auto-conclusions for uncovered WS entries.
;; Returns (listof task-conclusion?) — either LLM-distilled or deterministic fallbacks.
;; v0.79.2 GAP-3: Accept optional content-summary hash for richer fallback text.
(define (auto-distill ws-message-ids conclusions current-state [content-summaries (hash)])
  (define uncovered (find-uncovered-entries ws-message-ids conclusions))
  (cond
    [(null? uncovered) '()]
    [(current-llm-distill-fn)
     (distill-with-llm uncovered current-state (current-llm-distill-fn) 5 content-summaries)]
    [else (generate-fallback-conclusions uncovered current-state content-summaries)]))

;; ── Exports ──

(provide current-auto-distillation-enabled?
         current-llm-distill-fn
         ;; R99a: frozen legacy deterministic-fallback recognizer (single shared
         ;; definition lives in task-conclusion.rkt).
         legacy-deterministic-fallback?
         (contract-out [find-uncovered-entries
                        (-> (listof string?) (listof task-conclusion?) (listof string?))]
                       [generate-fallback-conclusions
                        (->* ((listof string?) (or/c symbol? #f)) (hash?) (listof task-conclusion?))]
                       [auto-distill
                        (->* ((listof string?) (listof task-conclusion?) (or/c symbol? #f))
                             (hash?)
                             (listof task-conclusion?))]))
