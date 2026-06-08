#lang racket/base

;; q/tests/test-goal-state.rkt — Tests for runtime/goal-state.rkt
;;
;; Covers: construction, contracts, serialization round-trip, defaults.

(require racket/port
         racket/string
         rackunit
         "../runtime/goal/goal-state.rkt"
         "../util/json/jsonl.rkt")

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

(test-case "goal-state: block 1"
  (let ([gs (hash->goal-state (hasheq 'goal-text "simple"))])
    (check-equal? (goal-state-goal-text gs) "simple")
    (check-equal? (goal-state-status gs) 'active)
    (check-equal? (goal-state-checks gs) '())
    (check-false (goal-state-last-evaluation gs))
    (check-false (goal-state-meta gs))))

;; ============================================================
;; goal-state->hash produces valid jsexpr
;; ============================================================

(let* ([gs (make-goal-state #:goal-text "test")]
       [h (goal-state->hash gs)])
  (check-true (hash? h) "goal-state->hash returns hash")
  (check-true (string? (hash-ref h 'id)) "hash has id")
  (check-equal? (hash-ref h 'goal-text) "test"))

;; ============================================================
;; W1: Typed events
;; ============================================================

(require "../agent/event-structs/base.rkt"
         "../agent/event-structs/session-events.rkt")

;; goal-start-event (positional: type ts sid tid goal-text max-turns checks)
(test-case "goal-state: block 2"
  (let ([e (goal-start-event "goal.started" 0 "s1" "t1" "tests pass" 8 '())])
    (check-equal? (goal-start-event-goal-text e) "tests pass")
    (check-equal? (goal-start-event-max-turns e) 8)
    (check-true (string? (typed-event-type e)) "goal-start-event has string type")))

;; goal-turn-start-event
(test-case "goal-state: block 3"
  (let ([e (goal-turn-start-event "goal.turn.started" 0 "s1" "t1" 1 "tests pass")])
    (check-equal? (goal-turn-start-event-turn-number e) 1)
    (check-equal? (goal-turn-start-event-goal-text e) "tests pass")))

;; goal-evaluated-event (positional: ... achieved? reason turn-number token-cost)
(test-case "goal-state: block 4"
  (let ([e (goal-evaluated-event "goal.evaluated" 0 "s1" "t1" #f "still failing" 2 50)])
    (check-false (goal-evaluated-event-achieved? e))
    (check-equal? (goal-evaluated-event-reason e) "still failing")
    (check-equal? (goal-evaluated-event-token-cost e) 50)))

;; goal-evaluated-event without token-cost
(test-case "goal-state: block 5"
  (let ([e (goal-evaluated-event "goal.evaluated" 0 "s1" "t1" #t "done" 3 0)])
    (check-equal? (goal-evaluated-event-token-cost e) 0 "token-cost explicit 0")))

;; goal-check-event (label exit-code [timed-out? #f] [stdout ""] [stderr ""])
(test-case "goal-state: block 6"
  (let ([e (goal-check-event "goal.check.completed" 0 "s1" "t1" "tests" 0 #f "" "")])
    (check-equal? (goal-check-event-label e) "tests")
    (check-equal? (goal-check-event-exit-code e) 0)
    (check-false (goal-check-event-timed-out? e))))

;; goal-achieved-event (goal-text turns-used [total-token-cost 0])
(test-case "goal-state: block 7"
  (let ([e (goal-achieved-event "goal.achieved" 0 "s1" "t1" "tests pass" 5 300)])
    (check-equal? (goal-achieved-event-goal-text e) "tests pass")
    (check-equal? (goal-achieved-event-turns-used e) 5)))

;; goal-failed-event
(test-case "goal-state: block 8"
  (let ([e (goal-failed-event "goal.failed" 0 "s1" "t1" "tests pass" "max-turns" 8)])
    (check-equal? (goal-failed-event-reason e) "max-turns")
    (check-equal? (goal-failed-event-turns-used e) 8)))

;; Event type is a string
(test-case "goal-state: block 9"
  (let ([e (goal-start-event "goal.started" 0 "s1" "t1" "test" 4 '())])
    (check-true (string? (typed-event-type e)) "goal-start-event has string type")))

;; ============================================================
;; W1: Persistence
;; ============================================================

(require racket/file
         "../runtime/session/session-store.rkt")

(define test-goal-dir (make-temporary-file "goal-test-~a" 'directory))

(test-case "goal-state: block 10"
  (let ()
    (define log-path (build-path test-goal-dir "session.jsonl"))
    ;; Write session version header
    (write-session-version-header! log-path)
    ;; Append goal state
    (define gs (make-goal-state #:goal-text "tests pass" #:max-turns 4))
    (append-goal-state! log-path gs)
    ;; Load latest
    (define loaded (load-latest-goal-state log-path))
    (check-true (goal-state? loaded) "load-latest-goal-state returns struct")
    (check-equal? (goal-state-goal-text loaded) "tests pass")
    (check-equal? (goal-state-max-turns loaded) 4)
    (check-equal? (goal-state-status loaded) 'active)))

;; Load returns #f when no goals
(test-case "goal-state: block 11"
  (let ()
    (define log-path (build-path test-goal-dir "empty-session.jsonl"))
    (write-session-version-header! log-path)
    (check-false (load-latest-goal-state log-path) "load-latest returns #f when empty")))

;; Multiple goals → latest wins
(test-case "goal-state: block 12"
  (let ()
    (define log-path (build-path test-goal-dir "multi-session.jsonl"))
    (write-session-version-header! log-path)
    (append-goal-state! log-path (make-goal-state #:goal-text "first" #:max-turns 2))
    (append-goal-state! log-path
                        (make-goal-state #:goal-text "second" #:max-turns 4 #:status 'achieved))
    (define loaded (load-latest-goal-state log-path))
    (check-equal? (goal-state-goal-text loaded) "second")
    (check-equal? (goal-state-status loaded) 'achieved)))

;; Cleanup
(delete-directory/files test-goal-dir)

;; ============================================================
;; W1: Context filtering — goal-state excluded from context
;; ============================================================

(require "../runtime/context-assembly/session-walk.rkt"
         "../util/message/message.rkt")

(test-case "goal-state: block 13"
  (let ()
    (define gs-msg
      (make-message "g1"
                    #f
                    'system
                    'goal-state
                    (list (hasheq 'goal-text "test"))
                    (inexact->exact (round (current-inexact-milliseconds)))
                    #f))
    (check-false (entry->context-message gs-msg) "goal-state kind excluded from context")))

(test-case "goal-state: block 14"
  (let ()
    (define user-msg
      (make-message "m1"
                    #f
                    'user
                    'message
                    (list "hello")
                    (inexact->exact (round (current-inexact-milliseconds)))
                    #f))
    (check-true (message? (entry->context-message user-msg))
                "regular message still included in context")))

;; ============================================================
;; W0: evaluations field tests (v0.71.7)
;; ============================================================

;; Default evaluations is empty
(test-case "goal-state: block 15"
  (let ()
    (define gs (make-goal-state #:goal-text "test"))
    (check-equal? (goal-state-evaluations gs) '() "evaluations defaults to empty")))

;; Explicit evaluations
(test-case "goal-state: block 16"
  (let ()
    (define er1 (make-evaluation-result #:achieved? #f #:reason "not yet"))
    (define er2 (make-evaluation-result #:achieved? #t #:reason "done"))
    (define gs (make-goal-state #:goal-text "test" #:evaluations (list er1 er2)))
    (check-equal? (length (goal-state-evaluations gs)) 2 "evaluations field populated")))

;; Round-trip with evaluations
(test-case "goal-state: block 17"
  (let ()
    (define er (make-evaluation-result #:achieved? #t #:reason "ok" #:token-cost 42))
    (define gs (make-goal-state #:goal-text "round-trip" #:evaluations (list er)))
    (define h (goal-state->hash gs))
    (define gs2 (hash->goal-state h))
    (check-equal? (length (goal-state-evaluations gs2)) 1)
    (check-equal? (evaluation-result-token-cost (car (goal-state-evaluations gs2))) 42)))

;; Backward compat: missing evaluations key defaults to empty
(test-case "goal-state: block 18"
  (let ()
    (define h (hasheq 'goal-text "old"))
    (define gs (hash->goal-state h))
    (check-equal? (goal-state-evaluations gs) '() "missing evaluations key → empty")))
