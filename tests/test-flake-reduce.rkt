#lang racket/base

;; @speed fast
;; @suite unit-fast
;; @boundary unit
;;
;; tests/test-flake-reduce.rkt — the campaign's W3 bounded flake reduction tests.
;;
;; The reducer is driven entirely through injected run predicates (no real
;; processes, no sleeps, no wall-clock dependence — the clock is an injected
;; thunk), so every scenario here is deterministic and fast:
;;   - minimal A->failing reduction on an order-dependent fixture (failure
;;     reproduces only when B runs after A);
;;   - single-predecessor reduction (A->failing pair);
;;   - hard budget enforcement stops with 'unresolved;
;;   - standalone-green / never-reproducing cases reduce to an empty
;;     predecessor delta and are never reported as "non-flaky";
;;   - B->A->failing expansion is recorded;
;;   - cold/warm + worker-isolation flags;
;;   - no sleeps anywhere in the two W3 modules.

(require rackunit
         rackunit/text-ui
         racket/file
         racket/list
         racket/runtime-path
         racket/string
         (only-in "../scripts/run-tests/flake-reduce.rkt"
                  reduction-budget
                  reduction-budget?
                  make-reduction-budget
                  reduction-budget-max-reruns
                  reduction-budget-wall-clock-ms
                  reduction-budget-created-at
                  budget-tracker
                  budget-tracker?
                  make-budget-tracker
                  budget-exhausted?
                  budget-spend!
                  budget-reruns-used
                  step-exact-sequence
                  step-standalone
                  step-delta-debug
                  step-minimal-pair
                  step-expansion
                  cold-vs-warm-flag
                  worker-isolation-flag
                  reduce-incident))

(define-runtime-path here ".")

;; ------------------------------------------------------------
;; Synthetic fixtures (pure predicates over predecessor lists)
;; ------------------------------------------------------------

;; Order-dependent: the failing test fails ONLY when B runs after A (both
;; present, A first). Standalone or single-predecessor contexts are green.
(define (order-fails? seq)
  (define ia (index-of seq 'A))
  (define ib (index-of seq 'B))
  (and ia ib (< ia ib)))

;; Single-predecessor: the failing test fails ONLY when A ran.
(define (presence-fails? seq)
  (and (member 'A seq) #t))

;; A fixed injected clock: deterministic, no wall-clock dependence, no sleeps.
(define (frozen-now)
  1000.0)

;; An advancing injected clock: every read jumps one second forward, so a
;; 1 ms wall-clock budget is exhausted at the very first check.
(define (make-advancing-now)
  (define t (box 1000.0))
  (lambda ()
    (set-box! t (+ 1000.0 (unbox t)))
    (unbox t)))

(define (reduced-verdict result)
  (hash-ref result 'verdict))

;; ------------------------------------------------------------
;; Suite
;; ------------------------------------------------------------

(define suite
  (test-suite "flake-reduce bounded reduction"

    (test-case "budget struct carries max-reruns, wall-clock-ms, created-at"
      (define b (make-reduction-budget 7 1234))
      (check-true (reduction-budget? b))
      (check-equal? (reduction-budget-max-reruns b) 7)
      (check-equal? (reduction-budget-wall-clock-ms b) 1234)
      (check-true (real? (reduction-budget-created-at b)))
      (check-true (budget-tracker? (make-budget-tracker b #:now frozen-now))))

    (test-case "reducer finds the minimal context on an order-dependent fixture"
      ;; failure needs A before B; noise predecessors must be reduced away
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "minimal-multi-predecessor")
      (check-equal? (hash-ref result 'minimal-predecessors)
                    '(A B)
                    "reduction must keep exactly the reproducing A->B context")
      (check-equal? (hash-ref result 'predecessor-delta) '(n1 n2))
      (check-equal? (hash-ref result 'standalone-green) #t)
      (check-equal? (hash-ref result 'exact-sequence-reproduced) #t)
      (check-false (hash-ref result 'budget-exhausted)))

    (test-case "reducer finds the minimal single-predecessor A->failing pair"
      (define result
        (reduce-incident "tests/test-single.rkt"
                         '(n1 n2 A n3)
                         presence-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "minimal-single-predecessor")
      (check-equal? (hash-ref result 'minimal-predecessors)
                    '(A)
                    "the minimal A->failing pair keeps exactly predecessor A")
      (check-equal? (hash-ref result 'predecessor-delta) '(n1 n2 n3)))

    (test-case "hard rerun budget stops the reduction with unresolved"
      ;; three reruns total: exact + standalone consume two, delta-debug gets
      ;; one attempt and then the budget is exhausted
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 3 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "unresolved")
      (check-true (hash-ref result 'budget-exhausted))
      (check-equal? (hash-ref result 'reruns-used) 3)
      ;; evidence gathered so far is retained, never discarded
      (check-true (pair? (hash-ref result 'evidence))))

    (test-case "hard wall-clock budget stops immediately with zero reruns"
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 1)
                         #:now (make-advancing-now)))
      (check-equal? (hash-ref result 'verdict) "unresolved")
      (check-equal? (hash-ref result 'reruns-used) 0)
      (check-true (hash-ref result 'budget-exhausted)))

    (test-case "never-reproducing (standalone-green) case reduces to an empty delta"
      (define result
        (reduce-incident "tests/test-flaky.rkt"
                         '(n1 A n2 B)
                         (lambda (seq) #f)
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "not-reproduced")
      (check-equal? (hash-ref result 'minimal-predecessors)
                    '()
                    "no predecessor is implicated when nothing reproduces")
      (check-equal? (hash-ref result 'predecessor-delta) '() "empty predecessor delta")
      (check-equal? (hash-ref result 'standalone-green) #t)
      ;; the reducer NEVER reports non-flaky: not-reproduced is recorded data
      (check-not-equal? (reduced-verdict result) "non-flaky"))

    (test-case "standalone-reproducing case reports an empty predecessor context"
      (define result
        (reduce-incident "tests/test-always.rkt"
                         '(n1 A n2 B)
                         (lambda (seq) #t)
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "standalone-reproduces")
      (check-equal? (hash-ref result 'minimal-predecessors) '())
      (check-equal? (hash-ref result 'standalone-green) #f))

    (test-case "B->A->failing expansion is recorded with tested/reproduced"
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (define expansion (hash-ref result 'expansion))
      (check-equal? (hash-ref expansion 'tested) '(n1 n2))
      (check-equal? (hash-ref expansion 'reproduced)
                    '(n1 n2)
                    "inserting noise before the minimal context keeps A before B")
      (check-true (andmap (lambda (c) (if (member c (hash-ref expansion 'tested)) #t #f))
                          (hash-ref expansion 'reproduced))))

    (test-case "irreducible sequences are reported as data"
      ;; the predicate needs the FULL order, nothing can be removed
      (define full-order-fails? (lambda (seq) (and (equal? seq '(A B)) #t)))
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(A B)
                         full-order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref result 'verdict) "irreducible-full-sequence")
      (check-equal? (hash-ref result 'minimal-predecessors) '(A B)))

    (test-case "reduction is deterministic: identical inputs give identical results"
      (define (run)
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (run) (run) "repeated reduction must be byte-identical"))

    (test-case "verdict vocabulary never contains a non-flaky claim"
      (define allowed
        '("minimal-single-predecessor" "minimal-multi-predecessor"
                                       "irreducible-full-sequence"
                                       "standalone-reproduces"
                                       "not-reproduced"
                                       "unresolved"))
      (define results
        (list (reduce-incident "f"
                               '(n1 A n2 B)
                               order-fails?
                               #:budget (make-reduction-budget 64 60000)
                               #:now frozen-now)
              (reduce-incident "f"
                               '(n1 n2 A n3)
                               presence-fails?
                               #:budget (make-reduction-budget 64 60000)
                               #:now frozen-now)
              (reduce-incident "f"
                               '(n1 A n2 B)
                               (lambda (s) #f)
                               #:budget (make-reduction-budget 64 60000)
                               #:now frozen-now)
              (reduce-incident "f"
                               '(n1 A n2 B)
                               (lambda (s) #t)
                               #:budget (make-reduction-budget 64 60000)
                               #:now frozen-now)
              (reduce-incident "f"
                               '(n1 A n2 B)
                               order-fails?
                               #:budget (make-reduction-budget 3 60000)
                               #:now frozen-now)))
      (for ([r (in-list results)])
        (check-true (if (member (reduced-verdict r) allowed) #t #f)
                    (format "verdict ~a outside the vocabulary" (reduced-verdict r)))))

    (test-case "cold/warm and worker-isolation flags classify over predicates"
      (check-equal? (cold-vs-warm-flag #:cold (lambda () #t) #:warm (lambda () #f)) 'cold-only)
      (check-equal? (cold-vs-warm-flag #:cold (lambda () #t) #:warm (lambda () #t)) 'both)
      (check-equal? (cold-vs-warm-flag #:cold (lambda () #f) #:warm (lambda () #t)) 'warm-only)
      (check-equal? (cold-vs-warm-flag #:cold (lambda () #f) #:warm (lambda () #f)) 'neither)
      (check-equal? (worker-isolation-flag #:isolated (lambda () #f) #:shared (lambda () #t))
                    'shared-only))

    (test-case "environment flags default to not-probed and accept injections"
      (define result
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now
                         #:cold-run (lambda () #t)
                         #:warm-run (lambda () #f)
                         #:isolated-run (lambda () #t)
                         #:shared-run (lambda () #t)))
      (check-equal? (hash-ref result 'cold-warm-flag) "cold-only")
      (check-equal? (hash-ref result 'worker-isolation-flag) "both")
      (define plain
        (reduce-incident "tests/test-order.rkt"
                         '(n1 A n2 B)
                         order-fails?
                         #:budget (make-reduction-budget 64 60000)
                         #:now frozen-now))
      (check-equal? (hash-ref plain 'cold-warm-flag) "not-probed")
      (check-equal? (hash-ref plain 'worker-isolation-flag) "not-probed"))

    (test-case "protocol steps are individually pure and budget-aware"
      (define b (make-reduction-budget 64 60000))
      (define tracker (make-budget-tracker b #:now frozen-now))
      (check-equal? (step-exact-sequence order-fails? '(n1 A n2 B) tracker #:now frozen-now) #t)
      (check-equal? (step-standalone order-fails? tracker #:now frozen-now) #f)
      (check-equal? (budget-reruns-used tracker) 2)
      (check-equal? (step-delta-debug order-fails? '(n1 A n2 B) tracker #:now frozen-now) '(A B))
      (check-equal? (step-minimal-pair '(A)) 'A)
      (check-equal? (step-minimal-pair '(A B)) #f)
      (check-equal? (hash-ref (step-expansion order-fails? '(A B) '(n1 n2) tracker #:now frozen-now)
                              'reproduced)
                    '(n1 n2))
      (check-true (budget-exhausted? tracker #:now (lambda () 999999.0))
                  "past the wall-clock deadline the tracker must report exhaustion"))

    (test-case "budget-spend! accounting and exhaustion boundary"
      (define b (make-reduction-budget 2 60000))
      (define tracker (make-budget-tracker b #:now frozen-now))
      (check-false (budget-exhausted? tracker #:now frozen-now))
      (budget-spend! tracker)
      (check-false (budget-exhausted? tracker #:now frozen-now))
      (budget-spend! tracker)
      (check-true (budget-exhausted? tracker #:now frozen-now))
      (check-equal? (budget-reruns-used tracker) 2))

    (test-case "no sleeps anywhere in the W3 flake modules"
      (for ([src (in-list (list "flake-reduce.rkt" "flake-forensics.rkt"))])
        (define text (file->string (build-path here ".." "scripts" "run-tests" src)))
        (check-false (regexp-match? #px"\\(sleep" text) (format "~a must not call sleep" src))))))

;; ------------------------------------------------------------
;; Run
;; ------------------------------------------------------------

(define failures (run-tests suite))

(module+ main
  (when (positive? failures)
    (exit 1)))
