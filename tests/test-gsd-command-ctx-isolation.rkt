#lang racket

;; tests/test-gsd-command-ctx-isolation.rkt
;; TDD for W7: command handlers isolate per ctx via current-gsd-ctx parameter

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/runtime-state-types.rkt"
         (only-in "../extensions/gsd/session-state.rkt"
                  make-gsd-context
                  current-gsd-ctx
                  gsd-default-ctx
                  gsd-ctx-state
                  gsd-ctx-mode)
         (only-in "../extensions/gsd/state-machine.rkt"
                  gsm-ctx-current
                  gsm-ctx-transition!
                  gsm-ctx-transition-to!
                  gsm-ctx-reset!
                  gsm-ctx-history
                  gsm-ctx-mark-wave-complete!
                  gsm-ctx-total-waves
                  gsm-ctx-set-total-waves!
                  gsm-ctx-completed-waves
                  ok?
                  ok-to
                  err?))

(define command-ctx-isolation-suite
  (test-suite "gsd-command-ctx-isolation"

    (test-case "/plan equivalent isolates via ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Transition ctx-a to exploring (simulates /plan)
      (parameterize ([current-gsd-ctx ctx-a])
        (define res (gsm-ctx-transition-to! (current-gsd-ctx) 'exploring))
        (check-true (ok? res) "plan transition succeeds"))
      (check-equal? (gsm-ctx-current ctx-a) 'exploring "ctx-a is exploring")
      (check-equal? (gsm-ctx-current ctx-b) 'idle "ctx-b unaffected"))

    (test-case "/reset equivalent isolates via ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Set ctx-a to executing
      (gsm-ctx-transition! ctx-a 'exploring)
      (gsm-ctx-transition! ctx-a 'plan-written)
      (gsm-ctx-transition! ctx-a 'executing)
      ;; Reset only ctx-a
      (parameterize ([current-gsd-ctx ctx-a])
        (define res (gsm-ctx-reset! (current-gsd-ctx)))
        (check-true (ok? res) "reset succeeds"))
      (check-equal? (gsm-ctx-current ctx-a) 'idle "ctx-a reset")
      (check-equal? (gsm-ctx-current ctx-b) 'idle "ctx-b still idle"))

    (test-case "/wave-done equivalent isolates via ctx"
      (define ctx-a (make-gsd-context))
      (define ctx-b (make-gsd-context))
      ;; Setup ctx-a in executing state with 3 waves
      (gsm-ctx-transition! ctx-a 'exploring)
      (gsm-ctx-transition! ctx-a 'plan-written)
      (gsm-ctx-transition! ctx-a 'executing)
      (gsm-ctx-set-total-waves! ctx-a 3)
      ;; Mark wave 0 complete on ctx-a
      (parameterize ([current-gsd-ctx ctx-a])
        (gsm-ctx-mark-wave-complete! (current-gsd-ctx) 0))
      (check-equal? (set-count (gsm-ctx-completed-waves ctx-a)) 1 "ctx-a has 1 completed")
      (check-equal? (set-count (gsm-ctx-completed-waves ctx-b)) 0 "ctx-b has 0 completed"))

    (test-case "full lifecycle isolates via ctx"
      (define ctx (make-gsd-context))
      (parameterize ([current-gsd-ctx ctx])
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'idle)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'exploring)
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'exploring)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'plan-written)
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'plan-written)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'executing)
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'executing)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'verifying)
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'verifying)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'idle)
        (check-equal? (gsm-ctx-current (current-gsd-ctx)) 'idle)))

    (test-case "default ctx unaffected by parameterize"
      (define ctx (make-gsd-context))
      ;; Remember default state
      (define default-before (gsm-ctx-current gsd-default-ctx))
      ;; Operate on isolated ctx
      (parameterize ([current-gsd-ctx ctx])
        (gsm-ctx-transition-to! (current-gsd-ctx) 'exploring)
        (gsm-ctx-transition-to! (current-gsd-ctx) 'executing))
      ;; Default should be unchanged
      (check-equal? (gsm-ctx-current gsd-default-ctx) default-before "default unchanged"))))

(run-tests command-ctx-isolation-suite)
