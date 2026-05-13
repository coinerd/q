#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-command-dispatch.rkt -- A6-03 + A7-03: dispatch-gsd-command routing tests

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/command-parser.rkt"
         "../extensions/gsd/command-handlers.rkt"
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/state-machine.rkt"
         "../extensions/gsd/command-types.rkt")

;; Helper: reset GSD state, optionally walk to target state, run thunk, then cleanup
(define (with-clean-gsd-state target-state thunk)
  (reset-all-gsd-state!)
  (when target-state
    (case target-state
      [(exploring) (gsm-transition-to! 'exploring)]
      [(plan-written) (gsm-transition-to! 'exploring)
                      (gsm-transition-to! 'plan-written)]
      [(executing) (gsm-transition-to! 'exploring)
                   (gsm-transition-to! 'plan-written)
                   (gsm-transition-to! 'executing)]))
  (begin0
   (thunk)
   (reset-all-gsd-state!)))

(define dispatch-suite
  (test-suite "dispatch-gsd-command routing"

    ;; ── Stateless tests (A6-03) ──

    (test-case "/go routes to 'go action"
      (define parsed (parse-gsd-command "/go" "/go"))
      (define-values (action result) (dispatch-gsd-command parsed "/go" (current-directory)))
      (check-equal? action 'go)
      (check-equal? result (list (current-directory) "/go")))

    (test-case "/status routes to 'status action"
      (define parsed (parse-gsd-command "/gsd" "/gsd"))
      (define-values (action result) (dispatch-gsd-command parsed "/gsd" (current-directory)))
      (check-equal? action 'status)
      (check-false result))

    (test-case "/skip N routes to 'skip action with args"
      (define parsed (parse-gsd-command "/skip" "/skip 3"))
      (define-values (action result) (dispatch-gsd-command parsed "/skip 3" (current-directory)))
      (check-equal? action 'skip)
      (check-pred pair? result))

    (test-case "/plan <text> routes to 'plan-submit action"
      (define parsed (parse-gsd-command "/plan" "/plan implement foo"))
      (define-values (action result) (dispatch-gsd-command parsed "/plan implement foo" (current-directory)))
      (check-equal? action 'plan-submit)
      (check-true (string? result)))

    (test-case "unknown command routes to 'artifact action"
      (define parsed (parse-gsd-command "/artifact" "/artifact"))
      (define-values (action result) (dispatch-gsd-command parsed "/artifact" (current-directory)))
      (check-equal? action 'artifact))

    ;; ── Stateful tests (A7-03) ──

    (test-case "/reset dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/reset" "/reset"))
          (define-values (action result) (dispatch-gsd-command parsed "/reset" (current-directory)))
          (check-equal? action 'reset)
          (check-pred gsd-command-result? result)
          (check-true (gsd-success? result)))))

    (test-case "/replan dispatches from plan-written state"
      (with-clean-gsd-state 'plan-written
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-pred gsd-command-result? result)
          (check-true (gsd-success? result)))))

    (test-case "/replan dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-pred gsd-command-result? result)
          (check-true (gsd-success? result)))))

    (test-case "/replan fails from idle state"
      (with-clean-gsd-state #f
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-pred gsd-command-result? result)
          (check-false (gsd-success? result)))))

    (test-case "/skip dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/skip" "/skip 1"))
          (define-values (action result) (dispatch-gsd-command parsed "/skip 1" (current-directory)))
          (check-equal? action 'skip)
          (check-pred pair? result)
          (check-true (gsd-success? (car result))))))

    (test-case "/wave-done dispatches but fails without filesystem setup"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/wave-done" "/wave-done 0"))
          (define-values (action result) (dispatch-gsd-command parsed "/wave-done 0" (current-directory)))
          (check-equal? action 'wave-done)
          (check-pred gsd-command-result? result))))

    (test-case "/done dispatches but fails without filesystem setup"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/done" "/done"))
          (define-values (action result) (dispatch-gsd-command parsed "/done" (current-directory)))
          (check-equal? action 'done)
          (check-pred gsd-command-result? result))))))

(run-tests dispatch-suite 'verbose)
