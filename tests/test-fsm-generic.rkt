#lang racket/base
;; BOUNDARY: pure

;; tests/test-fsm-generic.rkt — Generic FSM library tests (F16)

(require rackunit
         rackunit/text-ui
         (only-in "../util/fsm/fsm.rkt"
                  make-fsm
                  fsm?
                  fsm-states
                  fsm-events
                  fsm-transitions
                  fsm-lookup
                  fsm-valid-transition?
                  fsm-valid-targets
                  fsm-find-path))

(define simple-fsm
  (make-fsm '(idle running done)
            '(start finish reset)
            '(((idle . start) . running) ((running . finish) . done)
                                         ((running . reset) . idle)
                                         ((done . reset) . idle))))

(define fsm-tests
  (test-suite "generic-fsm"

    (test-case "fsm-generic: make-fsm creates fsm struct"
      (check-true (fsm? simple-fsm))
      (check-equal? (fsm-states simple-fsm) '(idle running done))
      (check-equal? (fsm-events simple-fsm) '(start finish reset))
      (check-equal? (length (fsm-transitions simple-fsm)) 4))

    (test-case "fsm-lookup finds valid transitions"
      (check-equal? (fsm-lookup simple-fsm 'idle 'start) 'running)
      (check-equal? (fsm-lookup simple-fsm 'running 'finish) 'done)
      (check-equal? (fsm-lookup simple-fsm 'running 'reset) 'idle)
      (check-equal? (fsm-lookup simple-fsm 'done 'reset) 'idle))

    (test-case "fsm-lookup returns #f for invalid transitions"
      (check-false (fsm-lookup simple-fsm 'idle 'finish))
      (check-false (fsm-lookup simple-fsm 'done 'start))
      (check-false (fsm-lookup simple-fsm 'idle 'reset)))

    (test-case "fsm-generic: fsm-valid-transition? returns boolean"
      (check-true (fsm-valid-transition? simple-fsm 'idle 'start))
      (check-false (fsm-valid-transition? simple-fsm 'idle 'finish)))

    (test-case "fsm-generic: fsm-valid-targets lists reachable states"
      (check-equal? (fsm-valid-targets simple-fsm 'idle) '(running))
      (check-equal? (sort (fsm-valid-targets simple-fsm 'running) symbol<?) '(done idle))
      (check-equal? (fsm-valid-targets simple-fsm 'done) '(idle)))

    (test-case "fsm-find-path finds shortest path"
      (check-equal? (fsm-find-path simple-fsm 'idle 'running) '(start))
      (check-equal? (fsm-find-path simple-fsm 'idle 'done) '(start finish))
      (check-equal? (fsm-find-path simple-fsm 'running 'idle) '(reset)))

    (test-case "fsm-generic: fsm-find-path returns #f for unreachable"
      ;; All states reachable in simple-fsm, so create one with unreachable
      (define partial-fsm (make-fsm '(a b c) '(go) '(((a . go) . b))))
      (check-false (fsm-find-path partial-fsm 'a 'c)))

    (test-case "fsm-find-path same state returns empty path"
      (check-equal? (fsm-find-path simple-fsm 'idle 'idle) '()))))

(run-tests fsm-tests)
