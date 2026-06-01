#lang racket/base

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/task-conclusion.rkt"
                  task-conclusion
                  task-conclusion-id
                  task-conclusion-dependencies)
         (only-in "../runtime/context-assembly/conclusion-graph.rkt"
                  build-conclusion-graph
                  conclusion-graph?
                  graph-select-by-seeds
                  graph-topological-sort
                  graph-detect-cycles
                  graph-conclusion-count
                  graph-edge-count
                  fallback-select-conclusions)
         (only-in "../runtime/context-assembly/task-memory.rkt"
                  make-task-memory
                  add-conclusion
                  conclusions-for-context))

(define c1 (task-conclusion "c1" "text1" 'fact 'idle '() 100 '() '("c2")))
(define c2 (task-conclusion "c2" "text2" 'fact 'idle '() 200 '() '()))
(define c3 (task-conclusion "c3" "text3" 'fact 'idle '() 300 '() '("c1")))
(define c-self (task-conclusion "cs" "self" 'fact 'idle '() 400 '() '("cs")))

(define (index-of lst v)
  (for/first ([x (in-list lst)]
              [i (in-naturals)]
              #:when (equal? x v))
    i))

(define suite
  (test-suite "conclusion-graph"
    (test-case "build empty graph"
      (define g (build-conclusion-graph '()))
      (check-true (conclusion-graph? g))
      (check-equal? (graph-conclusion-count g) 0)
      (check-equal? (graph-edge-count g) 0))

    (test-case "build graph with no deps"
      (define g (build-conclusion-graph (list c2)))
      (check-equal? (graph-conclusion-count g) 1)
      (check-equal? (graph-edge-count g) 0))

    (test-case "build graph with deps"
      (define g (build-conclusion-graph (list c1 c2 c3)))
      (check-equal? (graph-conclusion-count g) 3)
      (check-equal? (graph-edge-count g) 2))

    (test-case "topological sort is valid order"
      (define g (build-conclusion-graph (list c1 c2 c3)))
      (define sorted (graph-topological-sort g))
      (check-equal? (length sorted) 3)
      (check-true (< (index-of sorted "c2") (index-of sorted "c1")))
      (check-true (< (index-of sorted "c1") (index-of sorted "c3"))))

    (test-case "detect cycle"
      (define g (build-conclusion-graph (list c-self)))
      (define cycles (graph-detect-cycles g))
      (check-true (pair? cycles)))

    (test-case "no cycles in acyclic graph"
      (define g (build-conclusion-graph (list c1 c2)))
      (check-equal? (graph-detect-cycles g) '()))

    (test-case "select by seeds returns transitive closure"
      (define g (build-conclusion-graph (list c1 c2 c3)))
      (define selected (graph-select-by-seeds g '("c3")))
      (check-equal? (length selected) 3)
      (check-not-false (member "c1" selected))
      (check-not-false (member "c2" selected))
      (check-not-false (member "c3" selected)))

    (test-case "select by unknown seed returns empty"
      (define g (build-conclusion-graph (list c1 c2)))
      (check-equal? (graph-select-by-seeds g '("unknown")) '()))

    (test-case "fallback selects most recent by state"
      (define concs (list c1 c2 c3))
      (define selected (fallback-select-conclusions concs 2))
      (check-equal? (length selected) 2))

    (test-case "fallback with state filter"
      (define concs (list c1 c2 c3))
      (define selected (fallback-select-conclusions concs 10 '(idle)))
      (check-equal? (length selected) 3))

    (test-case "conclusions-for-context with seeds"
      (define mem (make-task-memory))
      (define mem2 (add-conclusion mem c1))
      (define mem3 (add-conclusion mem2 c2))
      (define mem4 (add-conclusion mem3 c3))
      (define result (conclusions-for-context mem4 '(idle) '("c3")))
      (check-true (>= (length result) 1)))

    (test-case "conclusions-for-context empty seeds falls back"
      (define mem (make-task-memory))
      (define mem2 (add-conclusion mem c1))
      (define mem3 (add-conclusion mem2 c2))
      (define result (conclusions-for-context mem3 '(idle) '()))
      (check-true (>= (length result) 1)))

    (test-case "conclusions-for-context empty memory returns empty"
      (define mem (make-task-memory))
      (check-equal? (conclusions-for-context mem '() '()) '()))
    (test-case "fallback with empty input"
      (check-equal? (fallback-select-conclusions '() 5) '()))))

(run-tests suite)
