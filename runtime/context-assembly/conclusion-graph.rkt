#lang racket/base

;; runtime/context-assembly/conclusion-graph.rkt — Pure DAG for conclusion dependencies
;; STABILITY: evolving
;; CONSUMERS: state-aware-builder, tests
;;
;; Builds a directed graph from task-conclusion dependencies.
;; Supports cycle detection, topological sort, and seed-based subgraph selection.
;; Pure functions only — no mutation, no I/O, no side effects.

(require racket/contract
         racket/list
         "task-conclusion.rkt"
         (only-in "conclusion-ranker.rkt" rank-and-budget)
         (only-in "config.rkt" current-conclusion-token-budget))

;; ── Struct ──

(struct conclusion-graph
        (nodes ; (hash/c string? task-conclusion?) — id → conclusion
         edges ; (hash/c string? (listof string?)) — id → dependency ids
         reverse) ; (hash/c string? (listof string?)) — id → dependents
  #:transparent)

;; ── Constructor ──

;; GAP-J v0.97.12: Warn on duplicate IDs (for/hash silently overwrites).
;; H4 v0.97.13: Converted from make-hash/hash-set!/hash-update! to immutable for/fold.
(define (build-conclusion-graph conclusions)
  ;; Check for duplicates and warn (pure scan)
  (for/fold ([seen (hash)]) ([c (in-list conclusions)])
    (define cid (task-conclusion-id c))
    (when (hash-has-key? seen cid)
      (log-warning "conclusion-graph: duplicate conclusion ID overwritten: ~a" cid))
    (hash-set seen cid #t))
  (define nodes
    (for/hash ([c (in-list conclusions)])
      (values (task-conclusion-id c) c)))
  (define edges
    (for/hash ([c (in-list conclusions)])
      (values (task-conclusion-id c)
              (filter (λ (dep-id) (hash-has-key? nodes dep-id)) (task-conclusion-dependencies c)))))
  ;; Build reverse edge table immutably
  (define reverse-raw
    (for/fold ([rev (hash)]) ([(id deps) (in-hash edges)])
      (for/fold ([r rev]) ([dep (in-list deps)])
        (hash-update r dep (λ (lst) (cons id lst)) '()))))
  (define reverse-clean
    (for/hash ([(k v) (in-hash reverse-raw)])
      (values k (reverse-list v))))
  (conclusion-graph nodes edges reverse-clean))

(define (reverse-list lst)
  (for/fold ([acc '()]) ([x (in-list lst)])
    (cons x acc)))

;; ── Accessors ──

(define (graph-conclusion-count g)
  (hash-count (conclusion-graph-nodes g)))

(define (graph-edge-count g)
  (for/sum ([deps (in-hash-values (conclusion-graph-edges g))]) (length deps)))

;; ── Cycle Detection ──

;; H4 v0.97.13: Converted from make-hash/hash-set!/set! to immutable for/fold.
;; Returns list of cyclic node IDs (nodes involved in any cycle).
(define (graph-detect-cycles g)
  (define edges (conclusion-graph-edges g))
  (define all-ids (hash-keys edges))
  (define (dfs id visited rec-stack cycles)
    (let* ([visited (hash-set visited id #t)]
           [rec-stack (hash-set rec-stack id #t)])
      (for/fold ([v visited]
                 [rs rec-stack]
                 [cy cycles])
                ([dep (in-list (hash-ref edges id '()))])
        (cond
          [(hash-ref rs dep #f) (values v rs (cons id cy))]
          [(not (hash-ref v dep #f))
           (define-values (v2 rs2 cy2) (dfs dep v rs cy))
           (values v2 rs2 cy2)]
          [else (values v rs cy)]))))
  (define-values (final-visited _final-stack final-cycles)
    (for/fold ([visited (hash)]
               [rec-stack (hash)]
               [cycles '()])
              ([id (in-list all-ids)])
      (if (hash-ref visited id #f)
          (values visited rec-stack cycles)
          (let-values ([(v rs cy) (dfs id visited rec-stack cycles)])
            (values v rs cy)))))
  (remove-duplicates final-cycles))

;; ── Topological Sort ──

;; H4 v0.97.13: Converted from make-hash/hash-set!/set! to immutable for/fold.
;; Returns list of IDs in dependency order (dependencies first).
;; Undefined for cyclic graphs — call graph-detect-cycles first.
(define (graph-topological-sort g)
  (define edges (conclusion-graph-edges g))
  (define all-ids (hash-keys edges))
  (define (visit id visited result)
    (if (hash-ref visited id #f)
        (values visited result)
        (let ([visited+ (hash-set visited id #t)])
          (define-values (v2 r2)
            (for/fold ([v visited+]
                       [r result])
                      ([dep (in-list (hash-ref edges id '()))])
              (visit dep v r)))
          (values v2 (cons id r2)))))
  (define-values (_final-visited final-result)
    (for/fold ([visited (hash)]
               [result '()])
              ([id (in-list all-ids)])
      (visit id visited result)))
  (reverse final-result))

;; ── Seed Selection ──

;; H4 v0.97.13: Converted from make-hash/hash-set! to immutable for/fold.
;; Given seed IDs, return all reachable IDs (transitive closure via dependencies).
;; Seeds not in the graph are silently ignored.
(define (graph-select-by-seeds g seed-ids)
  (define nodes (conclusion-graph-nodes g))
  (define edges (conclusion-graph-edges g))
  ;; Only start from seeds that exist in graph
  (define valid-seeds (filter (λ (s) (hash-has-key? nodes s)) seed-ids))
  (define final-visited
    (let loop ([queue valid-seeds]
               [visited (hash)])
      (cond
        [(null? queue) visited]
        [else
         (define id (car queue))
         (if (hash-ref visited id #f)
             (loop (cdr queue) visited)
             (let ([visited (hash-set visited id #t)]
                   [deps (hash-ref edges id '())])
               (loop (append (cdr queue) deps) visited)))])))
  (hash-keys final-visited))

;; ── Degraded Fallback ──

;; GAP-A v0.97.10: Unified ranking strategy.
;; When graph selection fails (empty seeds, no graph, cycles),
;; delegate to rank-and-budget for multi-factor scoring instead of
;; pure recency. Same conclusions ranked consistently across all paths.
(define (fallback-select-conclusions conclusions max-count [states '()])
  (define filtered
    (if (null? states)
        conclusions
        (let ([state-set (for/hash ([s (in-list states)])
                           (values s #t))])
          (filter (λ (c) (hash-has-key? state-set (task-conclusion-fsm-state-origin c)))
                  conclusions))))
  ;; Delegate to rank-and-budget for consistent multi-factor scoring
  (define budget (or (current-conclusion-token-budget) (* max-count 200)))
  (define ranked
    (rank-and-budget filtered
                     #:current-state (and (pair? states) (car states))
                     #:max-conclusion-tokens budget))
  (take-at-most (or (and (list? ranked) ranked) '()) max-count))

(define (take-at-most lst n)
  (if (> (length lst) n)
      (drop lst (- (length lst) n))
      lst))

;; v0.77.10 M4: Convenience wrapper — returns conclusion objects directly.
;; Wraps graph-select-by-seeds + ID→conclusion lookup.
(define (graph-select-conclusions g seed-ids)
  (define nodes (conclusion-graph-nodes g))
  (define selected-ids (graph-select-by-seeds g seed-ids))
  (for/list ([id (in-list selected-ids)]
             #:when (hash-has-key? nodes id))
    (hash-ref nodes id)))

;; ── Exports ──

(provide conclusion-graph
         conclusion-graph?
         struct:conclusion-graph
         conclusion-graph-nodes
         conclusion-graph-edges
         conclusion-graph-reverse
         (contract-out
          [build-conclusion-graph (-> (listof task-conclusion?) conclusion-graph?)]
          [graph-conclusion-count (-> conclusion-graph? exact-nonnegative-integer?)]
          [graph-edge-count (-> conclusion-graph? exact-nonnegative-integer?)]
          [graph-detect-cycles (-> conclusion-graph? (listof string?))]
          [graph-topological-sort (-> conclusion-graph? (listof string?))]
          [graph-select-by-seeds (-> conclusion-graph? (listof string?) (listof string?))]
          [graph-select-conclusions (-> conclusion-graph? (listof string?) (listof task-conclusion?))]
          [fallback-select-conclusions
           (->* ((listof task-conclusion?) exact-nonnegative-integer?)
                ((listof symbol?))
                (listof task-conclusion?))]))
