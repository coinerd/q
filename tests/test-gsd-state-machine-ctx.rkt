#lang racket

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-state-machine-ctx.rkt
;; TDD test for W6: ctx-aware state-machine API

(require rackunit
         rackunit/text-ui
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  gsd-ctx-mode
                  gsd-ctx-state
                  gsd-ctx-history
                  gsd-ctx-set-state!
                  gsd-default-ctx)
         "../extensions/gsd/runtime-state-types.rkt"
         (only-in "../extensions/gsd/state-machine.rkt"
                  gsm-current
                  gsm-reset!
                  gsm-ctx-current
                  gsm-ctx-transition!
                  gsm-ctx-reset!
                  gsm-ctx-history
                  gsm-ctx-valid-next-states
                  ok?
                  ok-to
                  err?))

(define state-machine-ctx-suite
  (test-suite "gsd-state-machine-ctx"

    (test-case "gsm-ctx-current reads from independent ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Both start idle
      (check-equal? (gsm-ctx-current ctx-a) 'idle)
      (check-equal? (gsm-ctx-current ctx-b) 'idle)
      ;; Manually set ctx-a to exploring
      (gsd-ctx-set-state! ctx-a
                          (struct-copy gsd-runtime-state (gsd-ctx-state ctx-a) [mode 'exploring]))
      (check-equal? (gsm-ctx-current ctx-a) 'exploring "ctx-a changed")
      (check-equal? (gsm-ctx-current ctx-b) 'idle "ctx-b unchanged"))

    (test-case "gsm-ctx-transition! transitions independently"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      (define res (gsm-ctx-transition! ctx-a 'exploring))
      (check-true (ok? res) "transition succeeds")
      (check-equal? (ok-to res) 'exploring)
      (check-equal? (gsm-ctx-current ctx-a) 'exploring "ctx-a is exploring")
      (check-equal? (gsm-ctx-current ctx-b) 'idle "ctx-b still idle"))

    (test-case "gsm-ctx-reset! resets only target ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Transition ctx-a to exploring
      (gsm-ctx-transition! ctx-a 'exploring)
      ;; Reset only ctx-a
      (define res (gsm-ctx-reset! ctx-a))
      (check-true (ok? res) "reset succeeds")
      (check-equal? (gsm-ctx-current ctx-a) 'idle "ctx-a reset")
      (check-equal? (gsm-ctx-current ctx-b) 'idle "ctx-b still idle"))

    (test-case "gsm-ctx-history is isolated per ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      (gsm-ctx-transition! ctx-a 'exploring)
      (define hist-a (gsm-ctx-history ctx-a))
      (define hist-b (gsm-ctx-history ctx-b))
      (check-not-false hist-a "ctx-a has history")
      (check-equal? hist-b '() "ctx-b has empty history"))

    (test-case "gsm-ctx-valid-next-states returns correct states"
      (define ctx (make-gsd-context))
      (check-equal? (gsm-ctx-valid-next-states ctx) '(exploring))
      (gsm-ctx-transition! ctx 'exploring)
      (check-not-false (member 'plan-written (gsm-ctx-valid-next-states ctx))))

    (test-case "full lifecycle on independent ctx"
      (define ctx (make-gsd-context))
      (check-equal? (gsm-ctx-current ctx) 'idle)
      (gsm-ctx-transition! ctx 'exploring)
      (check-equal? (gsm-ctx-current ctx) 'exploring)
      (gsm-ctx-transition! ctx 'plan-written)
      (check-equal? (gsm-ctx-current ctx) 'plan-written)
      (gsm-ctx-transition! ctx 'executing)
      (check-equal? (gsm-ctx-current ctx) 'executing)
      (gsm-ctx-transition! ctx 'verifying)
      (check-equal? (gsm-ctx-current ctx) 'verifying)
      (gsm-ctx-transition! ctx 'idle)
      (check-equal? (gsm-ctx-current ctx) 'idle))

    (test-case "invalid transition returns err-result"
      (define ctx (make-gsd-context))
      (define res (gsm-ctx-transition! ctx 'executing))
      (check-true (err? res) "idle→executing is invalid"))))

(run-tests state-machine-ctx-suite)
