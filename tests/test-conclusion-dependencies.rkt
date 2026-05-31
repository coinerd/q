#lang racket/base

;; tests/test-conclusion-dependencies.rkt — M6: Conclusion dependency tracking
;; v0.76.5: Conclusions store file paths they depend on.

(require rackunit
         "../runtime/context-assembly/task-conclusion.rkt")

;; ── Dependency field exists and defaults to empty ──

(test-case "new conclusion has empty dependencies by default"
  (define c (task-conclusion "c1" "Found a bug" 'fact 'exploration '() 1000 '() '()))
  (check-equal? (task-conclusion-dependencies c) '()))

(test-case "conclusion stores dependency paths"
  (define c (task-conclusion "c1"
                             "Module X depends on Y"
                             'fact
                             'exploration
                             '()
                             1000
                             '(module)
                             '("src/module-x.rkt" "src/module-y.rkt")))
  (check-equal? (task-conclusion-dependencies c)
                '("src/module-x.rkt" "src/module-y.rkt")))

;; ── Serialization round-trip with dependencies ──

(test-case "conclusion->hash includes dependencies"
  (define c (task-conclusion "c1"
                             "text"
                             'fact
                             'exploration
                             '()
                             1000
                             '()
                             '("file.rkt")))
  (define h (conclusion->hash c))
  (check-equal? (hash-ref h 'dependencies) '("file.rkt")))

(test-case "hash->conclusion reads dependencies"
  (define h (hash 'id "c1"
                  'text "text"
                  'category 'fact
                  'fsm-state-origin 'exploration
                  'origin-message-ids '()
                  'timestamp 1000
                  'relevance-tags '()
                  'dependencies '("a.rkt" "b.rkt")))
  (define c (hash->conclusion h))
  (check-equal? (task-conclusion-dependencies c) '("a.rkt" "b.rkt")))

(test-case "hash->conclusion defaults dependencies to empty when missing"
  (define h (hash 'id "c1"
                  'text "text"
                  'category 'fact
                  'fsm-state-origin 'exploration
                  'origin-message-ids '()
                  'timestamp 1000
                  'relevance-tags '()))
  (define c (hash->conclusion h))
  (check-equal? (task-conclusion-dependencies c) '()))

;; ── Multiple dependencies ──

(test-case "conclusion with many dependencies"
  (define deps (for/list ([i 10]) (format "src/file~a.rkt" i)))
  (define c (task-conclusion "c1" "overview" 'fact 'exploration '() 1000 '() deps))
  (check-equal? (task-conclusion-dependencies c) deps)
  (check-equal? (length (task-conclusion-dependencies c)) 10))

;; ── Cycle-free by construction ──

(test-case "dependencies are just file paths, no cycle risk"
  ;; The tagging system is acyclic by construction — dependencies are
  ;; file path strings, not references to other conclusions.
  (define c1 (task-conclusion "c1" "fact about A" 'fact 'exploration '() 1000 '() '("a.rkt")))
  (define c2 (task-conclusion "c2" "fact about B" 'fact 'exploration '() 1000 '() '("b.rkt")))
  ;; c1's dependencies don't reference c2 and vice versa
  (check-false (member "b.rkt" (task-conclusion-dependencies c1)))
  (check-false (member "a.rkt" (task-conclusion-dependencies c2))))
