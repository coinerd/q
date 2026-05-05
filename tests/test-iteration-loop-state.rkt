#lang racket/base

;; tests/test-iteration-loop-state.rkt — Loop-state struct tests (v0.29.16 W0)

(require rackunit
         rackunit/text-ui
         "../runtime/iteration/loop-state.rkt"
         racket/set)

(define suite
  (test-suite "loop-state structs"

    (test-case "make-initial-counters produces correct defaults"
      (define c (make-initial-counters))
      (check-equal? (loop-counters-iteration c) 0)
      (check-equal? (loop-counters-consecutive-tool-count c) 0)
      (check-equal? (loop-counters-seen-paths c) (set))
      (check-equal? (loop-counters-intent-retry-count c) 0)
      (check-equal? (loop-counters-consecutive-error-count c) 0)
      (check-equal? (loop-counters-recent-tool-names c) '())
      (check-equal? (loop-counters-explore-count c) 0)
      (check-equal? (loop-counters-implement-count c) 0)
      (check-equal? (loop-counters-stall-retry-count c) 0))

    (test-case "loop-counters accessor round-trip"
      (define c (loop-counters 5 3 (set "a" "b") 2 1 '("read" "write") 4 7 6))
      (check-equal? (loop-counters-iteration c) 5)
      (check-equal? (loop-counters-consecutive-tool-count c) 3)
      (check-equal? (loop-counters-seen-paths c) (set "a" "b"))
      (check-equal? (loop-counters-intent-retry-count c) 2)
      (check-equal? (loop-counters-consecutive-error-count c) 1)
      (check-equal? (loop-counters-recent-tool-names c) '("read" "write"))
      (check-equal? (loop-counters-explore-count c) 4)
      (check-equal? (loop-counters-implement-count c) 7)
      (check-equal? (loop-counters-stall-retry-count c) 6))

    (test-case "loop-infra accessor round-trip"
      (define infra (loop-infra 'ctx 'ext-reg 'reg 'bus "sid-1" "/tmp/log" 'token))
      (check-equal? (loop-infra-ctx infra) 'ctx)
      (check-equal? (loop-infra-ext-reg infra) 'ext-reg)
      (check-equal? (loop-infra-reg infra) 'reg)
      (check-equal? (loop-infra-bus infra) 'bus)
      (check-equal? (loop-infra-session-id infra) "sid-1")
      (check-equal? (loop-infra-log-path infra) "/tmp/log")
      (check-equal? (loop-infra-token infra) 'token))

    (test-case "loop-counters is transparent"
      (define c (loop-counters 1 2 (set) 0 0 '() 0 0 0))
      (check-true (loop-counters? c))
      (check-false (loop-counters? 42)))

    (test-case "loop-infra is transparent"
      (define infra (loop-infra 'a 'b 'c 'd 'e 'f 'g))
      (check-true (loop-infra? infra))
      (check-false (loop-infra? 'not-infra)))

    (test-case "struct-copy loop-counters"
      (define c (make-initial-counters))
      (define c2 (struct-copy loop-counters c [iteration 10]))
      (check-equal? (loop-counters-iteration c2) 10)
      (check-equal? (loop-counters-consecutive-tool-count c2) 0))

    (test-case "struct-copy loop-infra"
      (define infra (loop-infra 'ctx 'ext-reg 'reg 'bus "sid" "/log" 'tok))
      (define infra2 (struct-copy loop-infra infra [session-id "sid-2"]))
      (check-equal? (loop-infra-session-id infra2) "sid-2")
      (check-equal? (loop-infra-ctx infra2) 'ctx))))

(module+ test
  (run-tests suite))

(module+ main
  (run-tests suite))
