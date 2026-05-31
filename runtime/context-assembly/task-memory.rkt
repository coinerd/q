#lang racket/base

;; runtime/context-assembly/task-memory.rkt — In-memory store for task conclusions
;; STABILITY: evolving
;;
;; Stores conclusions ordered by timestamp. Provides state-based filtering
;; and bounded eviction to keep memory usage under control.

(require racket/contract
         racket/list
         "task-conclusion.rkt"
         (only-in "task-state.rkt" task-state?))

;; ── Struct ──

(struct task-memory
        (conclusions ; (listof task-conclusion?) — ordered by timestamp, newest last
         max-conclusions ; integer — default 50
         )
  #:transparent)

;; ── Constructor ──

(define (make-task-memory [max-conclusions 50])
  (task-memory '() max-conclusions))

;; ── Mutators (functional — return new task-memory) ──

(define (add-conclusion mem c)
  (define capped
    (take-at-most (append (task-memory-conclusions mem) (list c)) (task-memory-max-conclusions mem)))
  (struct-copy task-memory mem [conclusions capped]))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (drop lst (- (length lst) n))
      lst))

;; ── Filtering ──

;; Return conclusions whose fsm-state-origin matches one of the given states.
(define (conclusions-for-states mem states)
  (define state-set (list->set states))
  (filter (λ (c) (set-member? state-set (task-conclusion-fsm-state-origin c)))
          (task-memory-conclusions mem)))

;; Return conclusions tagged with any of the given relevance tags.
(define (conclusions-with-tags mem tags)
  (define tag-set (list->set tags))
  (filter (λ (c)
            (for/or ([t (in-list (task-conclusion-relevance-tags c))])
              (set-member? tag-set t)))
          (task-memory-conclusions mem)))

;; ── Eviction ──

;; Evict oldest conclusions that don't match any of the keep-states.
;; Keeps at least (- max-conclusions reserve) conclusions.
(define (evict-stale-conclusions mem [keep-states '()] [reserve 10])
  (define concs (task-memory-conclusions mem))
  (define max-c (task-memory-max-conclusions mem))
  (cond
    [(<= (length concs) max-c) mem] ; under limit, no eviction needed
    [else
     (define keep-set (list->set keep-states))
     (define-values (keep rest)
       (partition (λ (c) (set-member? keep-set (task-conclusion-fsm-state-origin c))) concs))
     (define evictable (take-at-most rest (- max-c reserve)))
     (struct-copy task-memory mem [conclusions (append evictable keep)])]))

;; ── Helpers ──

(define (list->set lst)
  (for/hash ([v (in-list lst)])
    (values v #t)))

(define (set-member? ht v)
  (hash-has-key? ht v))

;; ── Serialization ──

(define (task-memory->hash mem)
  (hash 'conclusions
        (map conclusion->hash (task-memory-conclusions mem))
        'max-conclusions
        (task-memory-max-conclusions mem)))

(define (hash->task-memory h)
  (task-memory (map hash->conclusion (hash-ref h 'conclusions '())) (hash-ref h 'max-conclusions 50)))

;; ── Exports ──

(provide (struct-out task-memory)
         (contract-out
          [make-task-memory (->* () (integer?) task-memory?)]
          [add-conclusion (-> task-memory? task-conclusion? task-memory?)]
          [conclusions-for-states (-> task-memory? (listof symbol?) (listof task-conclusion?))]
          [conclusions-with-tags (-> task-memory? (listof symbol?) (listof task-conclusion?))]
          [evict-stale-conclusions (->* (task-memory?) ((listof symbol?) integer?) task-memory?)]
          [task-memory->hash (-> task-memory? hash?)]
          [hash->task-memory (-> hash? task-memory?)]))
