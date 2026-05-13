#lang racket

;; BOUNDARY: unit

;; tests/test-turn-reducer.rkt — Turn-reducer pure tests (R1-P2)

(require rackunit
         rackunit/text-ui
         "../agent/turn-reducer.rkt"
         "../agent/turn-model.rkt"
         "../util/hook-types.rkt")

(define reducer-suite
  (test-suite "turn-reducer pure decisions"

    (test-case "decide-after-start returns build-context"
      (define ctx (turn-context "sid" 0 '() '() (hasheq) (hasheq) #f))
      (define d (decide-after-start ctx))
      (check-true (decision-build-context? d)))

    (test-case "decide-after-context returns check-pre-hook"
      (define ctx (turn-context "sid" 0 '() '() (hasheq) (hasheq) #f))
      (define d (decide-after-context ctx))
      (check-true (decision-check-pre-hook? d)))

    (test-case "pre-hook block -> blocked decision"
      (define hr (hook-block "quota"))
      (define d (decide-after-pre-hook hr))
      (check-true (decision-blocked? d)))

    (test-case "pre-hook pass -> check-msg-hook decision"
      (define d (decide-after-pre-hook 'pass))
      (check-true (decision-check-msg-hook? d)))

    (test-case "msg-hook block -> blocked decision"
      (define hr (hook-block "policy"))
      (define d (decide-after-msg-hook hr))
      (check-true (decision-blocked? d)))

    (test-case "msg-hook pass -> begin-stream decision"
      (define d (decide-after-msg-hook 'pass))
      (check-true (decision-begin-stream? d)))

    (test-case "stream cancelled -> cancelled decision"
      (define sd (hasheq 'cancelled? #t 'cancel-reason "timeout"))
      (define d (decide-after-stream sd))
      (check-true (decision-cancelled? d)))

    (test-case "stream complete -> complete decision"
      (define sd (hasheq 'cancelled? #f 'text "hello"))
      (define d (decide-after-stream sd))
      (check-true (decision-complete? d)))

    (test-case "top-level reducer: start command"
      (define cmd (make-turn-start (turn-context "sid" 0 '() '() (hasheq) (hasheq) #f)))
      (define d (decide-turn-step cmd))
      (check-true (decision-build-context? d)))

    (test-case "top-level reducer: cancel command"
      (define cmd (make-turn-cancel (hasheq 'reason "abort")))
      (define d (decide-turn-step cmd))
      (check-true (decision-cancelled? d)))

    (test-case "top-level reducer: stream-complete command"
      (define cmd (make-turn-stream-complete (hasheq 'cancelled? #f 'result "ok")))
      (define d (decide-turn-step cmd))
      (check-true (decision-complete? d)))))

(run-tests reducer-suite 'verbose)
