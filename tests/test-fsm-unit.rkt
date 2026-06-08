#lang racket/base

;; tests/test-fsm-unit.rkt — T-7 tests for util/fsm.rkt

(require rackunit
         rackunit/text-ui
         "../util/fsm/fsm.rkt")

;; Simple traffic-light FSM: red -> green -> yellow -> red
(define traffic-machine
  (make-fsm '(red green yellow)
            '(go slow stop)
            (list (cons (cons 'red 'go) 'green)
                  (cons (cons 'green 'slow) 'yellow)
                  (cons (cons 'yellow 'stop) 'red))))

;; Disconnected FSM for path tests
(define disconnected
  (make-fsm '(a b c)
            '(x y)
            (list (cons (cons 'a 'x) 'b))))

(define fsm-suite
  (test-suite
   "FSM unit tests"
   (test-case "fsm-unit: make-fsm creates fsm struct"
     (check-pred fsm? traffic-machine)
     (check-equal? (fsm-states traffic-machine) '(red green yellow))
     (check-equal? (fsm-events traffic-machine) '(go slow stop))
     (check-equal? (length (fsm-transitions traffic-machine)) 3))

   (test-case "fsm-lookup returns correct next state"
     (check-equal? (fsm-lookup traffic-machine 'red 'go) 'green)
     (check-equal? (fsm-lookup traffic-machine 'green 'slow) 'yellow)
     (check-equal? (fsm-lookup traffic-machine 'yellow 'stop) 'red))

   (test-case "fsm-lookup returns #f for invalid transition"
     (check-false (fsm-lookup traffic-machine 'red 'slow))
     (check-false (fsm-lookup traffic-machine 'green 'go))
     (check-false (fsm-lookup traffic-machine 'yellow 'go)))

   (test-case "fsm-unit: fsm-valid-transition? returns boolean"
     (check-true (fsm-valid-transition? traffic-machine 'red 'go))
     (check-false (fsm-valid-transition? traffic-machine 'red 'slow)))

   (test-case "fsm-unit: fsm-valid-targets lists reachable states"
     (check-equal? (fsm-valid-targets traffic-machine 'red) '(green))
     (check-equal? (fsm-valid-targets traffic-machine 'green) '(yellow))
     (check-equal? (fsm-valid-targets traffic-machine 'yellow) '(red))
     (check-equal? (fsm-valid-targets traffic-machine 'nonexistent) '()))

   (test-case "fsm-find-path finds path between states"
     (check-equal? (fsm-find-path traffic-machine 'red 'green) '(go))
     (check-equal? (fsm-find-path traffic-machine 'red 'yellow) '(go slow))
     (check-equal? (fsm-find-path traffic-machine 'red 'red) '()))

   (test-case "fsm-unit: fsm-find-path returns #f for unreachable"
     (check-false (fsm-find-path disconnected 'a 'c)))

   (test-case "fsm-state and fsm-event structs"
     (define s (fsm-state 'red))
     (define e (fsm-event 'go))
     (check-pred fsm-state? s)
     (check-pred fsm-event? e)
     (check-equal? (fsm-state-name s) 'red)
     (check-equal? (fsm-event-name e) 'go)
     (check-false (fsm-state? e))
     (check-false (fsm-event? s)))
   ))

(run-tests fsm-suite)
