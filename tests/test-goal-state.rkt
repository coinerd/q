#lang racket/base

;; q/tests/test-goal-state.rkt — Tests for runtime/goal-state.rkt
;;
;; Covers: construction, contracts, serialization round-trip, defaults.

(require racket/port
         racket/string
         rackunit
         "../runtime/goal-state.rkt"
         "../util/jsonl.rkt")

;; ============================================================
;; Helper
;; ============================================================

(define (round-trip-goal-state gs)
  (hash->goal-state (goal-state->hash gs)))

(define (round-trip-goal-check gc)
  (hash->goal-check (goal-check->hash gc)))

(define (round-trip-evaluation-result er)
  (hash->evaluation-result (evaluation-result->hash er)))

(define (round-trip-check-result cr)
  (hash->check-result (check-result->hash cr)))

;; ============================================================
;; goal-check
;; ============================================================

(check-equal? (goal-check-command (make-goal-check #:command "raco test" #:label "tests"))
              "raco test"
              "goal-check command stored")

(check-equal? (goal-check-expected-exit (make-goal-check #:command "raco test" #:label "tests"))
              0
              "goal-check expected-exit defaults to 0")

(check-true (goal-check? (round-trip-goal-check (make-goal-check #:command "raco test"
                                                                 #:label "tests")))
            "goal-check round-trip")

;; ============================================================
;; check-result
;; ============================================================

(check-equal?
 (check-result-exit-code (make-check-result #:label "tests" #:exit-code 0 #:stdout "all pass"))
 0
 "check-result stores exit code")

(check-true
 (check-result? (round-trip-check-result
                 (make-check-result #:label "t" #:exit-code 1 #:stdout "err" #:stderr "fail")))
 "check-result round-trip")

(check-true (let* ([long-str (make-string 600 #\x)]
                   [cr (make-check-result #:label "t" #:exit-code 0 #:stdout long-str)])
              (< (string-length (check-result-stdout cr)) 600))
            "check-result truncates long stdout")

;; ============================================================
;; evaluation-result
;; ============================================================

(check-true (evaluation-result-achieved? (make-evaluation-result #:achieved? #t
                                                                 #:reason "tests pass"))
            "evaluation-result achieved stores")

(check-false (evaluation-result-achieved? (make-evaluation-result #:achieved? #f
                                                                  #:reason "still failing"))
             "evaluation-result not-achieved stores")

(check-equal? (evaluation-result-reason (make-evaluation-result #:achieved? #f
                                                                #:reason "missing tests"))
              "missing tests"
              "evaluation-result reason stores")

(check-true
 (evaluation-result?
  (round-trip-evaluation-result
   (make-evaluation-result #:achieved? #t #:reason "ok" #:model-used "gpt-4o-mini" #:token-cost 150)))
 "evaluation-result round-trip")

;; ============================================================
;; goal-state construction
;; ============================================================

(check-true (goal-state? (make-goal-state #:goal-text "make tests pass"))
            "goal-state construction with defaults")

(check-equal? (goal-state-status (make-goal-state #:goal-text "test"))
              'active
              "goal-state defaults to active")

(check-equal? (goal-state-max-turns (make-goal-state #:goal-text "test"))
              8
              "goal-state defaults to 8 max-turns")

(check-equal? (goal-state-goal-text (make-goal-state #:goal-text "make tests pass"))
              "make tests pass"
              "goal-state stores goal-text")

(check-true (string? (goal-state-id (make-goal-state #:goal-text "test")))
            "goal-state auto-generates UUID id")

(check-true (exact-nonnegative-integer? (goal-state-started-at (make-goal-state #:goal-text "test")))
            "goal-state auto-generates started-at timestamp")

;; ============================================================
;; goal-state full construction
;; ============================================================

(let* ([chk (make-goal-check #:command "raco test" #:label "tests")]
       [gs (make-goal-state #:goal-text "tests pass"
                            #:max-turns 4
                            #:evaluator-model "gpt-4o-mini"
                            #:evaluator-mode 'agent
                            #:checks (list chk))])
  (check-equal? (goal-state-max-turns gs) 4)
  (check-equal? (goal-state-evaluator-model gs) "gpt-4o-mini")
  (check-equal? (goal-state-evaluator-mode gs) 'agent)
  (check-equal? (length (goal-state-checks gs)) 1)
  (check-equal? (goal-state-turns-used gs) 0))

;; ============================================================
;; goal-state contract violations
;; ============================================================

(check-exn exn:fail:contract?
           (lambda () (make-goal-state #:goal-text 42))
           "goal-state rejects non-string goal-text")

(check-exn exn:fail:contract?
           (lambda () (make-goal-state #:goal-text "test" #:status 'invalid))
           "goal-state rejects invalid status")

(check-exn exn:fail:contract?
           (lambda () (make-goal-state #:goal-text "test" #:max-turns -1))
           "goal-state rejects negative max-turns")

;; ============================================================
;; goal-state serialization round-trip
;; ============================================================

(let* ([chk (make-goal-check #:command "raco test" #:label "tests")]
       [ev (make-evaluation-result #:achieved? #f
                                   #:reason "still failing"
                                   #:check-results (list (make-check-result #:label "tests"
                                                                            #:exit-code 1
                                                                            #:stderr "1 failure"))
                                   #:model-used "gpt-4o-mini"
                                   #:token-cost 50)]
       [gs (make-goal-state #:goal-text "tests pass"
                            #:status 'active
                            #:max-turns 4
                            #:evaluator-model "gpt-4o-mini"
                            #:evaluator-mode 'agent
                            #:checks (list chk)
                            #:last-evaluation ev)]
       [rt (round-trip-goal-state gs)])
  (check-equal? (goal-state-goal-text rt) "tests pass")
  (check-equal? (goal-state-status rt) 'active)
  (check-equal? (goal-state-max-turns rt) 4)
  (check-equal? (goal-state-evaluator-mode rt) 'agent)
  (check-equal? (length (goal-state-checks rt)) 1)
  (check-false (evaluation-result-achieved? (goal-state-last-evaluation rt)))
  (check-equal? (evaluation-result-token-cost (goal-state-last-evaluation rt)) 50))

;; ============================================================
;; hash->goal-state backward compat (missing optional fields)
;; ============================================================

(let ([gs (hash->goal-state (hasheq 'goal-text "simple"))])
  (check-equal? (goal-state-goal-text gs) "simple")
  (check-equal? (goal-state-status gs) 'active)
  (check-equal? (goal-state-checks gs) '())
  (check-false (goal-state-last-evaluation gs))
  (check-false (goal-state-meta gs)))

;; ============================================================
;; goal-state->hash produces valid jsexpr
;; ============================================================

(let* ([gs (make-goal-state #:goal-text "test")]
       [h (goal-state->hash gs)])
  (check-true (hash? h) "goal-state->hash returns hash")
  (check-true (string? (hash-ref h 'id)) "hash has id")
  (check-equal? (hash-ref h 'goal-text) "test"))

(displayln "All goal-state tests passed.")
