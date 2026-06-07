#lang racket/base

;;; test-memory-reflection-w5.rkt — W5: Deterministic memory reflection tests

(require rackunit
         racket/list
         racket/string
         (only-in "../runtime/memory/reflection.rkt"
                  reflect-session-memories!
                  current-reflection-min-group-size
                  current-reflection-overlap-threshold
                  jaccard-similarity
                  shared-tags?
                  items-related?
                  group-items
                  merge-group-items)
         (only-in "../runtime/memory/backends/memory-hash.rkt" make-memory-hash-backend)
         (only-in "../runtime/memory/types.rkt"
                  memory-item
                  memory-item-id
                  memory-item-content
                  memory-item-scope
                  memory-item-type
                  memory-item-metadata
                  memory-item-validity
                  memory-query
                  memory-result-ok?
                  memory-result-value)
         (only-in "../runtime/memory/protocol.rkt" gen:store-memory! gen:retrieve-memory))

(define (make-test-item id content [tags '()] [scope 'session])
  (memory-item
   id
   'semantic
   scope
   content
   (hasheq 'source 'test 'session-id "s1" 'project-root "." 'tags tags 'origin-tool-call-id "tc1")
   (hasheq 'sensitivity 'public 'confidence 0.7 'supersedes '())
   "2026-01-01T00:00:00Z"
   "2026-01-01T00:00:00Z"))

(define (all-items backend)
  (define r (gen:retrieve-memory backend (memory-query "" #f #f #f #f #f 100 #t)))
  (if (memory-result-ok? r)
      (memory-result-value r)
      '()))

;; ---------------------------------------------------------------------------
;; Unit tests: Jaccard similarity
;; ---------------------------------------------------------------------------

(test-case "W5: jaccard-similarity identical sets = 1.0"
  (check-= (jaccard-similarity '("a" "b" "c") '("a" "b" "c")) 1.0 0.01))

(test-case "W5: jaccard-similarity disjoint sets = 0.0"
  (check-= (jaccard-similarity '("a" "b") '("c" "d")) 0.0 0.01))

(test-case "W5: jaccard-similarity partial overlap"
  ;; {a,b} ∩ {b,c} = {b}, {a,b,c} => 1/3 ≈ 0.33
  (check-= (jaccard-similarity '("a" "b") '("b" "c")) (/ 1 3) 0.01))

;; ---------------------------------------------------------------------------
;; Unit tests: shared-tags?
;; ---------------------------------------------------------------------------

(test-case "W5: shared-tags? with common tags"
  (define a (make-test-item "a" "x" '("racket" "memory")))
  (define b (make-test-item "b" "y" '("memory" "test")))
  (check-true (shared-tags? a b)))

(test-case "W5: shared-tags? with disjoint tags"
  (define a (make-test-item "a" "x" '("racket")))
  (define b (make-test-item "b" "y" '("python")))
  (check-false (shared-tags? a b)))

;; ---------------------------------------------------------------------------
;; Unit tests: items-related?
;; ---------------------------------------------------------------------------

(test-case "W5: items-related? by shared tags"
  (define a (make-test-item "a" "completely different content" '("memory")))
  (define b (make-test-item "b" "totally unrelated text" '("memory")))
  (check-true (items-related? a b 0.3)))

(test-case "W5: items-related? by token overlap"
  (define a (make-test-item "a" "the memory system stores facts" '()))
  (define b (make-test-item "b" "the memory system retrieves facts" '()))
  (check-true (items-related? a b 0.3)))

(test-case "W5: items not related"
  (define a (make-test-item "a" "alpha beta gamma" '()))
  (define b (make-test-item "b" "delta epsilon zeta" '()))
  (check-false (items-related? a b 0.3)))

;; ---------------------------------------------------------------------------
;; Unit tests: group-items
;; ---------------------------------------------------------------------------

(test-case "W5: group-items groups related items together"
  (define items
    (list (make-test-item "m1" "memory stores data" '("mem"))
          (make-test-item "m2" "memory retrieves data" '("mem"))
          (make-test-item "m3" "completely unrelated thing" '())))
  (define groups (group-items items 0.3))
  ;; m1 and m2 should be grouped together (shared tag "mem")
  ;; m3 should be alone
  (check-equal? (length groups) 2)
  (define big-group (argmax length groups))
  (check-equal? (length big-group) 2))

(test-case "W5: group-items deterministic output"
  (define items
    (list (make-test-item "m1" "alpha" '("t"))
          (make-test-item "m2" "beta" '("t"))
          (make-test-item "m3" "gamma" '("t"))))
  (define g1 (group-items items 0.3))
  (define g2 (group-items items 0.3))
  (check-equal? g1 g2))

;; ---------------------------------------------------------------------------
;; Integration tests: reflect-session-memories!
;; ---------------------------------------------------------------------------

(test-case "W5: reflect-session-memories! no-op with < min items"
  (define backend (make-memory-hash-backend))
  (gen:store-memory! backend (make-test-item "m1" "only one item"))
  (define results
    (reflect-session-memories! backend #:session-id "s1" #:project-root "." #:min-group-size 2))
  (check-equal? results '()))

(test-case "W5: reflect-session-memories! creates reflection from grouped items"
  (define backend (make-memory-hash-backend))
  (gen:store-memory! backend (make-test-item "m1" "stores session data" '("memory")))
  (gen:store-memory! backend (make-test-item "m2" "retrieves session data" '("memory")))
  (define results
    (reflect-session-memories! backend #:session-id "s1" #:project-root "." #:min-group-size 2))
  (check-equal? (length results) 1)
  (define refl (car results))
  (check-equal? (memory-item-scope refl) 'project)
  (check-equal? (memory-item-type refl) 'semantic)
  (check-true (string-contains? (memory-item-content refl) "[reflection]"))
  ;; Supersedes both sources
  (define superseded (hash-ref (memory-item-validity refl) 'supersedes '()))
  (check-equal? (sort superseded string<?) '("m1" "m2")))

(test-case "W5: reflect-session-memories! no-op when no groups meet min-size"
  (define backend (make-memory-hash-backend))
  (gen:store-memory! backend (make-test-item "m1" "alpha beta" '()))
  (gen:store-memory! backend (make-test-item "m2" "gamma delta" '()))
  ;; min-group-size=3 means the 2-item group won't qualify
  (define results
    (reflect-session-memories! backend #:session-id "s1" #:project-root "." #:min-group-size 3))
  (check-equal? results '()))

(test-case "W5: reflect-session-memories! no backend returns empty"
  (define results (reflect-session-memories! #f #:session-id "s1" #:project-root "."))
  (check-equal? results '()))

(test-case "W5: reflect-session-memories! supersedes lineage"
  (define backend (make-memory-hash-backend))
  (gen:store-memory! backend (make-test-item "m1" "shared content about x" '("tag1")))
  (gen:store-memory! backend (make-test-item "m2" "shared content about y" '("tag1")))
  (define results
    (reflect-session-memories! backend #:session-id "s1" #:project-root "." #:min-group-size 2))
  (check-equal? (length results) 1)
  (define refl (car results))
  ;; Reflection item should have supersedes pointing to source IDs
  (define sup (hash-ref (memory-item-validity refl) 'supersedes))
  (check-not-false (member "m1" sup))
  (check-not-false (member "m2" sup)))

(test-case "W5: reflect-session-memories! deterministic same-input same-output"
  (define backend1 (make-memory-hash-backend))
  (gen:store-memory! backend1 (make-test-item "m1" "alpha about memory" '("t")))
  (gen:store-memory! backend1 (make-test-item "m2" "beta about memory" '("t")))
  (define r1 (reflect-session-memories! backend1 #:session-id "s1" #:project-root "."))

  (define backend2 (make-memory-hash-backend))
  (gen:store-memory! backend2 (make-test-item "m1" "alpha about memory" '("t")))
  (gen:store-memory! backend2 (make-test-item "m2" "beta about memory" '("t")))
  (define r2 (reflect-session-memories! backend2 #:session-id "s1" #:project-root "."))

  (check-equal? (memory-item-content (car r1)) (memory-item-content (car r2)))
  (check-equal? (memory-item-id (car r1)) (memory-item-id (car r2))))

;; ---------------------------------------------------------------------------
;; v0.95.18 W0: Audit regression tests (expected red before W5)
;; ---------------------------------------------------------------------------

(test-case "W0 F7: default reflection min group size is conservative (3)"
  (check-equal? (current-reflection-min-group-size) 3))

(test-case "W0 F7: one shared tag is not sufficient for reflection relatedness"
  (define a (make-test-item "a" "unrelated alpha" '("shared" "left")))
  (define b (make-test-item "b" "unrelated beta" '("shared" "right")))
  (check-false (items-related? a b 0.3)))

(test-case "W0 F7: reflection ID is deterministic and not wall-clock based"
  (define backend (make-memory-hash-backend))
  (gen:store-memory! backend (make-test-item "id-a" "alpha about reflection" '("t1" "t2")))
  (gen:store-memory! backend (make-test-item "id-b" "beta about reflection" '("t1" "t2")))
  (define results
    (reflect-session-memories! backend #:session-id "s1" #:project-root "." #:min-group-size 2))
  (check-equal? (length results) 1)
  (check-false (regexp-match? #px"^refl_[0-9]+_" (memory-item-id (car results)))))
