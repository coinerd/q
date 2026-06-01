#lang racket/base

;; runtime/context-assembly/conclusion-graph.rkt — Pure DAG for conclusion dependencies
;; STABILITY: evolving
;;
;; Builds a directed graph from task-conclusion dependencies.
;; Supports cycle detection, topological sort, and seed-based subgraph selection.
;; Pure functions only — no mutation, no I/O, no side effects.

(require racket/contract
         racket/list
         "task-conclusion.rkt")

;; ── Struct ──

(struct conclusion-graph
        (nodes ; (hash/c string? task-conclusion?) — id → conclusion
         edges ; (hash/c string? (listof string?)) — id → dependency ids
         reverse) ; (hash/c string? (listof string?)) — id → dependents
  #:transparent)

;; ── Constructor ──

(define (build-conclusion-graph conclusions)
  (define nodes
    (for/hash ([c (in-list conclusions)])
      (values (task-conclusion-id c) c)))
  (define edges
    (for/hash ([c (in-list conclusions)])
      (values (task-conclusion-id c)
              (filter (λ (dep-id) (hash-has-key? nodes dep-id)) (task-conclusion-dependencies c)))))
  (define reverse
    (let ([rev (make-hash)])
      (for ([(id deps) (in-hash edges)])
        (for ([dep (in-list deps)])
          (hash-update! rev dep (λ (lst) (cons id lst)) '())))
      (for/hash ([(k v) (in-hash rev)])
        (values k (reverse-list v)))))
  (conclusion-graph nodes edges reverse))

(define (reverse-list lst)
  (for/fold ([acc '()]) ([x (in-list lst)])
    (cons x acc)))

;; ── Accessors ──

(define (graph-conclusion-count g)
  (hash-count (conclusion-graph-nodes g)))

(define (graph-edge-count g)
  (for/sum ([deps (in-hash-values (conclusion-graph-edges g))]) (length deps)))

;; ── Cycle Detection ──

;; Returns list of cyclic node IDs (nodes involved in any cycle).
(define (graph-detect-cycles g)
  (define edges (conclusion-graph-edges g))
  (define all-ids (hash-keys edges))
  (define visited (make-hash))
  (define rec-stack (make-hash))
  (define cycles '())
  (define (dfs id)
    (hash-set! visited id #t)
    (hash-set! rec-stack id #t)
    (for ([dep (in-list (hash-ref edges id '()))])
      (cond
        [(hash-ref rec-stack dep #f) (set! cycles (cons id cycles))]
        [(not (hash-ref visited dep #f)) (dfs dep)]))
    (hash-set! rec-stack id #f))
  (for ([id (in-list all-ids)])
    (unless (hash-ref visited id #f)
      (dfs id)))
  (remove-duplicates cycles))

;; ── Topological Sort ──

;; Returns list of IDs in dependency order (dependencies first).
;; Undefined for cyclic graphs — call graph-detect-cycles first.
(define (graph-topological-sort g)
  (define edges (conclusion-graph-edges g))
  (define all-ids (hash-keys edges))
  (define visited (make-hash))
  (define result '())
  (define (visit id)
    (unless (hash-ref visited id #f)
      (hash-set! visited id #t)
      (for ([dep (in-list (hash-ref edges id '()))])
        (visit dep))
      (set! result (cons id result))))
  (for ([id (in-list all-ids)])
    (visit id))
  (reverse result))

;; ── Seed Selection ──

;; Given seed IDs, return all reachable IDs (transitive closure via dependencies).
;; Seeds not in the graph are silently ignored.
(define (graph-select-by-seeds g seed-ids)
  (define nodes (conclusion-graph-nodes g))
  (define edges (conclusion-graph-edges g))
  (define visited (make-hash))
  (define (bfs queue)
    (cond
      [(null? queue) '()]
      [else
       (define id (car queue))
       (cond
         [(hash-ref visited id #f) (bfs (cdr queue))]
         [else
          (hash-set! visited id #t)
          (define deps (hash-ref edges id '()))
          (bfs (append (cdr queue) deps))])]))
  ;; Only start from seeds that exist in graph
  (define valid-seeds (filter (λ (s) (hash-has-key? nodes s)) seed-ids))
  (bfs valid-seeds)
  (hash-keys visited))

;; ── Degraded Fallback ──

;; When graph selection fails (empty seeds, no graph, cycles),
;; fall back to bounded recency selection from a flat conclusion list.
(define (fallback-select-conclusions conclusions max-count [states '()])
  (define filtered
    (if (null? states)
        conclusions
        (let ([state-set (for/hash ([s (in-list states)])
                           (values s #t))])
          (filter (λ (c) (hash-has-key? state-set (task-conclusion-fsm-state-origin c)))
                  conclusions))))
  ;; Sort by timestamp descending, take most recent
  (define sorted (sort filtered > #:key task-conclusion-timestamp))
  (take-at-most sorted max-count))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (drop lst (- (length lst) n))
      lst))

;; ── Exports ──

(provide (struct-out conclusion-graph)
         (contract-out [build-conclusion-graph (-> (listof task-conclusion?) conclusion-graph?)]
                       [graph-conclusion-count (-> conclusion-graph? exact-nonnegative-integer?)]
                       [graph-edge-count (-> conclusion-graph? exact-nonnegative-integer?)]
                       [graph-detect-cycles (-> conclusion-graph? (listof string?))]
                       [graph-topological-sort (-> conclusion-graph? (listof string?))]
                       [graph-select-by-seeds
                        (-> conclusion-graph? (listof string?) (listof string?))]
                       [fallback-select-conclusions
                        (->* ((listof task-conclusion?) exact-nonnegative-integer?)
                             ((listof symbol?))
                             (listof task-conclusion?))]))
