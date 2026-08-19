#lang racket

;; @speed fast
;; @suite default
;; @boundary unit

;; BOUNDARY: unit

;; Duration-aware shard planning (W7) — planner unit tests.
;; Determinism guard: identical inputs (inventory + duration snapshot) must
;; produce identical plans; the planner regenerates the plan from a
;; checked-in fixture snapshot and compares.

(require rackunit
         rackunit/text-ui)

(require racket/runtime-path
         json
         (only-in "../scripts/run-tests/shard-plan.rkt"
                  build-shard-plan
                  build-shard-plan/safe
                  plan->jsexpr
                  plan->assignments
                  plan-shard-files
                  plan-predicted-max
                  round-robin-predicted-max
                  print-shard-plan-report
                  write-plan-json!
                  load-duration-snapshot
                  inventory-preserved?
                  activation-recommendation
                  shard-plan-mode
                  shard-plan-substituted
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
      ;; round-robin max = aa+ad+ag = 320+12.5 ... check the invariant only:
      ;; duration-aware max must be <= round-robin max, and strictly better
      ;; on this skew fixture (three heavy files cannot share a shard in RR).
      (check-true (<= (plan-predicted-max plan) (round-robin-predicted-max plan)))
      (check-true (< (plan-predicted-max plan) (round-robin-predicted-max plan))))

    (test-case "determinism guard: identical inputs → identical plans (fixture)"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan-a (build-shard-plan inventory-with-substitution 3 #:durations dur))
      (define plan-b (build-shard-plan inventory-with-substitution 3 #:durations dur))
      (check-equal? (plan->assignments plan-a) (plan->assignments plan-b))
      (check-equal? (shard-plan-predicted plan-a) (shard-plan-predicted plan-b))
      (check-equal? (plan->jsexpr plan-a) (plan->jsexpr plan-b))
      (check-equal? (plan-shard-files plan-a 0) (plan-shard-files plan-b 0)))

    (test-case "determinism guard: regeneration from fixture is stable (repeat x3)"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plans
        (for/list ([_ (in-range 3)])
          (plan->jsexpr (build-shard-plan inventory-with-substitution 3 #:durations dur))))
      (for ([p (in-list (rest plans))])
        (check-equal? p (first plans))))

    (test-case "missing durations use conservative default and are recorded"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan inventory-with-substitution 3 #:durations dur))
      ;; p95 of the known durations (sorted: 7.25 9 12.5 100.75 110.25 120.5 280 300 320)
      ;; idx = floor(0.95 * 8) = 7 → 300.0
      (check-equal? (shard-plan-substituted plan) '("tests/test-jj-unmeasured.rkt"))
      (define j (plan->jsexpr plan))
      (check-equal? (hash-ref (hash-ref j 'durations) 'substituted_count) 1)
      (check-equal? (hash-ref (hash-ref j 'durations) 'known) 9)
      (check-equal? (hash-ref (hash-ref j 'durations) 'default_seconds) 300.0))

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
      (check-true (string? (hash-ref (plan->jsexpr plan) 'reason)))
      ;; fallback still preserves the inventory
      (check-true (inventory-preserved? plan)))

    (test-case "activation recommendation: hold when no predicted improvement"
      ;; durations disabled → all weights equal → planner cannot beat RR max
      (define plan (build-shard-plan fixture-files 3))
      (define rec (activation-recommendation plan))
      (check-equal? (car rec) "hold"))

    (test-case "activation recommendation: activate when prediction improves"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan fixture-files 3 #:durations dur))
      (define rec (activation-recommendation plan))
      (check-equal? (car rec) "activate"))

    (test-case "report output contains per-shard assignments and predictions"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan fixture-files 3 #:durations dur))
      (define out (open-output-string))
      (print-shard-plan-report plan out)
      (define text (get-output-string out))
      (check-true (regexp-match? #rx"shard 0/3" text))
      (check-true (regexp-match? #rx"predicted max shard" text))
      (check-true (regexp-match? #rx"tests/test-aa-slow.rkt" text)))

    (test-case "plan json round-trips through write-plan-json!"
      (define-values (dur _status) (load-duration-snapshot (path->string fixture-path)))
      (define plan (build-shard-plan fixture-files 3 #:durations dur))
      (define tmp (make-temporary-file "shard-plan-~a.json"))
      (dynamic-wind (lambda () (void))
                    (lambda ()
                      (write-plan-json! plan tmp)
                      (define re-read (with-input-from-file tmp read-json))
                      (check-equal? (hash-ref re-read 'schema) "shard-plan/1")
                      (check-equal? (hash-ref re-read 'shard_total) 3)
                      (check-equal? (hash-ref re-read 'file_count) 9))
                    (lambda ()
                      (with-handlers ([exn:fail? (lambda (_) (void))])
                        (delete-file tmp)))))))

(module+ test
  (exit (run-tests shard-plan-suite)))
