#lang racket/base

;; runtime/context-assembly/task-conclusion.rkt — Conclusion type for task-state context optimization
;; STABILITY: evolving
;;
;; A task-conclusion captures a distilled insight from the agent's work,
;; tagged with the FSM state when it was recorded and relevance tags for
;; state-aware context assembly. Replaces raw file contents in the prompt
;; when the agent has moved past exploration.

(require racket/contract)

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

(define valid-categories '(fact decision pattern error-cause test-result))

(define (task-conclusion-category? v)
  (and (symbol? v) (memq v valid-categories) #t))

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

(define (hash->conclusion h)
  (task-conclusion (hash-ref h 'id)
                   (hash-ref h 'text)
                   (hash-ref h 'category)
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
         (contract-out [conclusion->hash (-> task-conclusion? hash?)]
                       [hash->conclusion (-> hash? task-conclusion?)]))
