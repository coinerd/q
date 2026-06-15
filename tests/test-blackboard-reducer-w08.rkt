#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-blackboard-reducer-w08.rkt — W0 (v0.99.8) Reducer Audit Fix Tests
;;
;; Tests for R1 (capped-append) and A1 (mas.blackboard.sync handler).

(require rackunit
         rackunit/text-ui
         (only-in "../agent/blackboard.rkt"
                  blackboard-state?
                  blackboard-state-test-results
                  blackboard-state-artifact-refs
                  blackboard-state-verifier-decisions
                  blackboard-state-open-hypotheses
                  blackboard-state-last-updated
                  blackboard-state-wave-status
                  empty-blackboard)
         (only-in "../agent/blackboard-reducer.rkt"
                  apply-event
                  apply-events
                  capped-append
                  MAX-BLACKBOARD-ENTRIES))

(define suite
  (test-suite "Blackboard Reducer W0 Audit Fixes (v0.99.8)"

    ;; ── R1: capped-append ──

    (test-case "capped-append appends normally below cap"
      (check-equal? (capped-append '(a b) 'c) '(a b c)))

    (test-case "capped-append drops oldest at cap"
      (define full-list (build-list MAX-BLACKBOARD-ENTRIES values))
      (define result (capped-append full-list 'new))
      (check-equal? (length result) MAX-BLACKBOARD-ENTRIES)
      (check-equal? (last result) 'new)
      (check-false (member 0 result)))

    (test-case "MAX-BLACKBOARD-ENTRIES is 50"
      (check-equal? MAX-BLACKBOARD-ENTRIES 50))

    (test-case "R1: test-results capped at 50"
      (define events
        (for/list ([i (in-range 60)])
          (hash 'event
                'mas.test.result
                'data
                (hash 'file (format "test-~a.rkt" i) 'result 'pass)
                'timestamp
                (+ 1000 i))))
      (define state (apply-events events))
      (check-equal? (length (blackboard-state-test-results state)) 50))

    (test-case "R1: artifact-refs capped at 50"
      (define events
        (for/list ([i (in-range 60)])
          (hash 'event
                'mas.artifact.produced
                'data
                (hash 'name (format "artifact-~a" i))
                'timestamp
                (+ 1000 i))))
      (define state (apply-events events))
      (check-equal? (length (blackboard-state-artifact-refs state)) 50))

    (test-case "R1: verifier-decisions capped at 50"
      (define events
        (for/list ([i (in-range 60)])
          (hash 'event
                'gsd.verification.completed
                'data
                (hash 'verdict 'approve)
                'timestamp
                (+ 1000 i))))
      (define state (apply-events events))
      (check-equal? (length (blackboard-state-verifier-decisions state)) 50))

    (test-case "R1: open-hypotheses capped at 50"
      (define events
        (for/list ([i (in-range 60)])
          (hash 'event
                'mas.hypothesis.opened
                'data
                (hash 'id (format "H~a" i))
                'timestamp
                (+ 1000 i))))
      (define state (apply-events events))
      (check-equal? (length (blackboard-state-open-hypotheses state)) 50))

    ;; ── A1: mas.blackboard.sync handler ──

    (test-case "A1: mas.blackboard.sync updates last-updated"
      (define evt (hash 'event 'mas.blackboard.sync 'data (hash) 'timestamp 9999))
      (define state (apply-event empty-blackboard evt))
      (check-equal? (blackboard-state-last-updated state) 9999))

    (test-case "A1: mas.blackboard.sync preserves other fields"
      (define events
        (list (hash 'event 'gsd.wave.started 'data (hash 'wave "W0") 'timestamp 1000)
              (hash 'event 'mas.blackboard.sync 'data (hash) 'timestamp 2000)))
      (define state (apply-events events))
      (check-true (hash-has-key? (blackboard-state-wave-status state) "W0"))
      (check-equal? (blackboard-state-last-updated state) 2000))))

(run-tests suite)
