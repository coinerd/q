#lang racket

;; BOUNDARY: unit

;; tests/test-turn-model.rkt — Turn-model struct tests (R1-P1)

(require rackunit
         rackunit/text-ui
         "../agent/turn-model.rkt")

(define turn-model-suite
  (test-suite "turn-model structs"

    (test-case "turn-context construction"
      (define tc (turn-context "sid" 0 '(msg) '() (hasheq) (hasheq) #f))
      (check-equal? (turn-context-session-id tc) "sid")
      (check-equal? (turn-context-iteration tc) 0))

    (test-case "turn-command discriminated union"
      (define cmd-start (make-turn-start (hasheq 'user "hi")))
      (check-true (turn-start? cmd-start))
      (check-false (turn-cancel? cmd-start))
      (define cmd-cancel (make-turn-cancel (hasheq 'reason "abort")))
      (check-true (turn-cancel? cmd-cancel)))

    (test-case "turn-decision blocked/complete"
      (define blocked (make-decision-blocked "hook"))
      (check-true (decision-blocked? blocked))
      (check-false (decision-complete? blocked))
      (define complete (make-decision-complete (hasheq 'result "ok")))
      (check-true (decision-complete? complete))
      (check-false (decision-blocked? complete)))

    (test-case "fsm-transition construction"
      (define tx (fsm-transition 'idle 'processing 'next))
      (check-equal? (fsm-transition-from-state tx) 'idle)
      (check-equal? (fsm-transition-to-state tx) 'processing)
      (check-equal? (fsm-transition-action tx) 'next))))

(run-tests turn-model-suite 'verbose)
