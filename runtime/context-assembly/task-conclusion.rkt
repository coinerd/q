#lang racket/base

;; runtime/context-assembly/task-conclusion.rkt — Conclusion type for task-state context optimization
;; STABILITY: evolving
;; CONSUMERS: state-aware-builder, save/record-conclusion tools
;;
;; A task-conclusion captures a distilled insight from the agent's work,
;; tagged with the FSM state when it was recorded and relevance tags for
;; state-aware context assembly. Replaces raw file contents in the prompt
;; when the agent has moved past exploration.

(require racket/contract
         racket/string)

;; ── Struct ──

(struct task-conclusion
        (id ; string — unique ID
         text ; string — the conclusion text
         category ; symbol — 'fact | 'decision | 'pattern | 'error-cause | 'test-result
         fsm-state-origin ; symbol — task-state when conclusion was recorded
         origin-message-ids ; (listof string) — provenance: which messages led to this
         timestamp ; integer — epoch seconds
         relevance-tags ; (listof symbol) — tags for state-relevance matching
         dependencies)
  #:transparent)

;; ── Predicates ──

;; ── R99a (BUG-0099): public vs stored category split ──
;; Public (writer-facing) categories are EXACTLY the five LLM insight
;; categories. The tools surface (save-conclusion / record-conclusion) keeps
;; using task-conclusion-category?/valid-categories, so 'provenance is never
;; publicly writable.
(define valid-categories '(fact decision pattern error-cause test-result))

;; Stored records additionally accept the INTERNAL 'provenance category for
;; FUTURE provenance records. The R99b deterministic writer that would create
;; them is HELD — R99a is readers/public-predicate separation only, with no
;; new-category writer active anywhere.
(define stored-categories '(fact decision pattern error-cause test-result provenance))

(define (task-conclusion-category? v)
  (and (symbol? v) (memq v valid-categories) #t))

(define (stored-task-conclusion-category? v)
  (and (symbol? v) (memq v stored-categories) #t))

;; ── R99a: FROZEN legacy deterministic-fallback recognizer ──
;; The historical deterministic fallback (auto-distillation.rkt) produced
;; records with ALL THREE of:
;;   1. the 'auto-distilled relevance tag,
;;   2. the stable "auto-<origin>" id shape (non-empty origin suffix),
;;   3. text starting with the known "[Auto] " prefix.
;; ONLY the full three-part signature is recognized. Tag alone, known prefix
;; alone, or auto-id alone is NOT recognized: ambiguous/partial matches keep
;; their existing type and uncertainty, and genuine LLM conclusions are
;; untouched. This recognizer is shared by every reader and the memory bridge;
;; DO NOT widen it without a new reviewed decision (it gates display labels,
;; ranking penalties, and bridge exclusion).
(define auto-distilled-tag 'auto-distilled)
(define auto-id-prefix "auto-")
(define auto-text-prefix "[Auto] ")

(define (legacy-deterministic-fallback? c)
  (and (task-conclusion? c)
       (memq auto-distilled-tag (task-conclusion-relevance-tags c))
       (let ([id (task-conclusion-id c)])
         (and (string? id)
              (> (string-length id) (string-length auto-id-prefix))
              (string-prefix? id auto-id-prefix)))
       (let ([text (task-conclusion-text c)])
         (and (string? text) (string-prefix? text auto-text-prefix)))))

;; ── R99a: shared provenance/insight classification ──
;; provenance-kind?: future internal 'provenance records AND recognized legacy
;; deterministic fallbacks. These render as UNVERIFIED provenance, rank below
;; insights, and are excluded from high-value memory export.
;; insight-kind?: records with a public five-category that are not
;; provenance-kind — the ordinary learned-insight surface.
(define (provenance-kind? c)
  (or (and (task-conclusion? c) (eq? (task-conclusion-category c) 'provenance))
      (legacy-deterministic-fallback? c)))

(define (insight-kind? c)
  (and (task-conclusion? c)
       (task-conclusion-category? (task-conclusion-category c))
       (not (provenance-kind? c))))

;; ── Serialization ──

(define (conclusion->hash c)
  (hash 'id
        (task-conclusion-id c)
        'text
        (task-conclusion-text c)
        'category
        (task-conclusion-category c)
        'fsm-state-origin
        (task-conclusion-fsm-state-origin c)
        'origin-message-ids
        (task-conclusion-origin-message-ids c)
        'timestamp
        (task-conclusion-timestamp c)
        'relevance-tags
        (task-conclusion-relevance-tags c)
        'dependencies
        (task-conclusion-dependencies c)))

;; GAP-J v0.97.12: Added field validation for malformed data.
(define (hash->conclusion h)
  (define the-id (hash-ref h 'id))
  (define the-text (hash-ref h 'text))
  (define the-category (hash-ref h 'category))
  (unless (string? the-id)
    (error 'hash->conclusion "expected string for 'id, got: ~a" the-id))
  (unless (string? the-text)
    (error 'hash->conclusion "expected string for 'text, got: ~a" the-text))
  (unless (stored-task-conclusion-category? the-category)
    (error 'hash->conclusion
           "expected stored category symbol for 'category, got: ~a (stored-valid: ~a)"
           the-category
           stored-categories))
  (task-conclusion the-id
                   the-text
                   the-category
                   (hash-ref h 'fsm-state-origin)
                   (hash-ref h 'origin-message-ids '())
                   (hash-ref h 'timestamp 0)
                   (hash-ref h 'relevance-tags '())
                   (hash-ref h 'dependencies '())))

;; ── Exports ──

(provide task-conclusion
         task-conclusion?
         struct:task-conclusion
         task-conclusion-id
         task-conclusion-text
         task-conclusion-category
         task-conclusion-fsm-state-origin
         task-conclusion-origin-message-ids
         task-conclusion-timestamp
         task-conclusion-relevance-tags
         task-conclusion-dependencies
         task-conclusion-category?
         valid-categories
         ;; R99a: stored-type acceptance (internal 'provenance) + shared
         ;; frozen legacy recognizer and provenance/insight classification.
         stored-categories
         stored-task-conclusion-category?
         legacy-deterministic-fallback?
         provenance-kind?
         insight-kind?
         auto-distilled-tag
         auto-id-prefix
         auto-text-prefix
         (contract-out [conclusion->hash (-> task-conclusion? hash?)]
                       [hash->conclusion (-> hash? task-conclusion?)]))
