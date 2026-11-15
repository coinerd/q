#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: unit

;; Duration-aware shard planning — planner unit tests.
;; Determinism guard: identical inputs (inventory + duration snapshot) must
;; produce identical plans; the planner regenerates the plan from a
;; checked-in fixture snapshot and compares.
;;
;; SCOPE (§11.3): the `report`/`measure` experiment surfaces (plan JSON
;; rendering, starvation/tail checks, cohort-anchored regeneration) are
;; deleted with the modes that produced them; what remains is the
;; deterministic plan the CI lanes consume via `--shard-plan active`.

(require rackunit
         rackunit/text-ui)

(require racket/runtime-path
         json
         (only-in "../scripts/run-tests/classify.rkt" shard-files)
         (only-in "../scripts/run-tests/shard-plan.rkt"
                  build-shard-plan
                  build-shard-plan/safe
                  plan-shard-files
                  plan-predicted-max
                  load-duration-snapshot
                  inventory-preserved?
                  shard-plan-mode
                  shard-plan-reason
                  shard-plan-substituted
                  shard-plan-known-count
                  shard-plan-default-seconds
                  shard-plan-total-files
                  shard-plan-predicted
                  shard-plan-shards))

(define-runtime-path fixture-path (build-path "fixtures" "shard-plan-durations.json"))

(define fixture-files
  '("tests/test-aa-slow.rkt" "tests/test-bb-slow.rkt"
                             "tests/test-cc-slow.rkt"
                             "tests/test-dd-medium.rkt"
                             "tests/test-ee-medium.rkt"
                             "tests/test-ff-medium.rkt"
                             "tests/test-gg-fast.rkt"
                             "tests/test-hh-fast.rkt"
                             "tests/test-ii-fast.rkt"))

;; One file deliberately absent from the snapshot → substitution must be recorded.
(define inventory-with-substitution (append fixture-files '("tests/test-jj-unmeasured.rkt")))

;; The round-robin reference prediction, computed from classify.rkt's own
;; `shard-files` semantics plus the same duration weights (the deleted
;; planner-comparison helper did exactly this).
(define (round-robin-predicted-max files durations shard-total)
  (define sorted (sort (remove-duplicates files) string<?))
  (define weights
    (for/hash ([f (in-list sorted)])
      (values f (hash-ref durations f 0.0))))
  (apply max
         (cons 0.0
               (for/list ([i (in-range shard-total)])
                 (for/sum ([f (in-list (shard-files sorted i shard-total))])
                          (hash-ref weights f 0.0))))))

(define shard-plan-suite
  (test-suite "Duration-aware shard plan tests"

    (test-case "fixture snapshot loads with status ok"
      (define-values (dur status) (load-duration-snapshot (path->string fixture-path)))
      (check-eq? status 'ok)
      (check-equal? (hash-count dur) 9)
      (check-equal? (hash-ref dur "tests/test-aa-slow.rkt" #f) 320.0))

    (test-case "missing snapshot source → status missing, empty durations"
      (define-values (dur status)
        (load-duration-snapshot "tests/fixtures/does-not-exist-durations.json"))
      (check-eq? status 'missing)
      (check-equal? (hash-count dur) 0))

    (test-case "no snapshot source → status disabled"
      (define-values (_dur status) (load-duration-snapshot #f))
      (check-eq? status 'disabled))

    (test-case "planner balances better than round-robin on skew fixture"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan fixture-files 3 #:durations dur))
      (check-eq? (shard-plan-mode plan) 'duration-aware)
      (check-true (inventory-preserved? plan))
      (check-equal? (shard-plan-total-files plan) 9)
      ;; duration-aware max must be <= the round-robin max, and strictly better
      ;; on this skew fixture (three heavy files cannot share a shard in RR).
      (check-true (<= (plan-predicted-max plan) (round-robin-predicted-max fixture-files dur 3)))
      (check-true (< (plan-predicted-max plan) (round-robin-predicted-max fixture-files dur 3))))

    (test-case "determinism guard: identical inputs → identical plans (fixture)"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan-a (build-shard-plan inventory-with-substitution 3 #:durations dur))
      (define plan-b (build-shard-plan inventory-with-substitution 3 #:durations dur))
      (check-equal? (shard-plan-shards plan-a) (shard-plan-shards plan-b))
      (check-equal? (shard-plan-predicted plan-a) (shard-plan-predicted plan-b))
      (check-equal? (plan-shard-files plan-a 0) (plan-shard-files plan-b 0)))

    (test-case "determinism guard: regeneration from fixture is stable (repeat x3)"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plans
        (for/list ([_ (in-range 3)])
          (build-shard-plan inventory-with-substitution 3 #:durations dur)))
      (for ([p (in-list (rest plans))])
        (check-equal? (shard-plan-shards p) (shard-plan-shards (first plans)))
        (check-equal? (shard-plan-predicted p) (shard-plan-predicted (first plans)))))

    (test-case "missing durations use conservative default and are recorded"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan inventory-with-substitution 3 #:durations dur))
      ;; p95 of the known durations (sorted: 7.25 9 12.5 100.75 110.25 120.5 280 300 320)
      ;; idx = floor(0.95 * 8) = 7 → 300.0
      (check-equal? (shard-plan-substituted plan) '("tests/test-jj-unmeasured.rkt"))
      (check-equal? (shard-plan-known-count plan) 9)
      (check-equal? (shard-plan-default-seconds plan) 300.0))

    (test-case "inventory is preserved: every file appears exactly once"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan inventory-with-substitution 3 #:durations dur))
      (check-equal? (shard-plan-total-files plan) 10)
      (check-true (inventory-preserved? plan))
      (define union (sort (apply append (shard-plan-shards plan)) string<?))
      (check-equal? union (sort inventory-with-substitution string<?)))

    (test-case "co-location group stays in one shard"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan
        (build-shard-plan fixture-files
                          3
                          #:durations dur
                          #:co-locate '(("tests/test-gg-fast.rkt" "tests/test-aa-slow.rkt"))))
      (check-true (inventory-preserved? plan))
      (for ([shard (in-list (shard-plan-shards plan))])
        (define gg? (member "tests/test-gg-fast.rkt" shard))
        (define aa? (member "tests/test-aa-slow.rkt" shard))
        (check-equal? (and gg? #t) (and aa? #t))))

    (test-case "anti-co-location: separated files land in different shards"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan
        (build-shard-plan fixture-files
                          3
                          #:durations dur
                          #:separate '(("tests/test-aa-slow.rkt" "tests/test-bb-slow.rkt"))))
      (define aa-shard
        (for/first ([shard (in-list (shard-plan-shards plan))]
                    [i (in-naturals)]
                    #:when (member "tests/test-aa-slow.rkt" shard))
          i))
      (define bb-shard
        (for/first ([shard (in-list (shard-plan-shards plan))]
                    [i (in-naturals)]
                    #:when (member "tests/test-bb-slow.rkt" shard))
          i))
      (check-false (equal? aa-shard bb-shard)))

    (test-case "fallback: planner error degrades to round-robin with reason"
      (define plan (build-shard-plan/safe fixture-files 0))
      (check-eq? (shard-plan-mode plan) 'round-robin-fallback)
      (check-true (string? (shard-plan-reason plan)))
      ;; fallback still preserves the inventory
      (check-true (inventory-preserved? plan)))))

(module+ test
  (exit (run-tests (test-suite "all shard-plan suites"
                     shard-plan-suite))))
