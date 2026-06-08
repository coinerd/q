#lang racket

;; @speed fast  ;; @suite extensions

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
  (dynamic-wind void thunk (lambda () (reset-all-gsd-state!))))

(define dispatch-suite
  (test-suite "dispatch-gsd-command routing"

    ;; -- Stateless tests (A6-03) --

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

    (test-case "/skip N routes to 'skip action with parsed struct"
      (define parsed (parse-gsd-command "/skip" "/skip 3"))
      (define-values (action result) (dispatch-gsd-command parsed "/skip 3" (current-directory)))
      (check-equal? action 'skip)
      (check-true (gsd-cmd-skip? result)))

    (test-case "/plan <text> routes to 'plan-submit action"
      (define parsed (parse-gsd-command "/plan" "/plan implement foo"))
      (define-values (action result) (dispatch-gsd-command parsed "/plan implement foo" (current-directory)))
      (check-equal? action 'plan-submit)
      (check-true (string? result)))

    (test-case "unknown command routes to 'artifact action"
      (define parsed (parse-gsd-command "/artifact" "/artifact"))
      (define-values (action result) (dispatch-gsd-command parsed "/artifact" (current-directory)))
      (check-equal? action 'artifact))

    ;; -- Stateful tests: pure dispatch returns parsed structs (A7-03) --

    (test-case "/reset dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/reset" "/reset"))
          (define-values (action result) (dispatch-gsd-command parsed "/reset" (current-directory)))
          (check-equal? action 'reset)
          (check-true (gsd-cmd-reset? result)))))

    (test-case "/replan dispatches from plan-written state"
      (with-clean-gsd-state 'plan-written
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-true (gsd-cmd-replan? result)))))

    (test-case "/replan dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-true (gsd-cmd-replan? result)))))

    (test-case "/replan routes from idle state (execute will fail)"
      (with-clean-gsd-state #f
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-true (gsd-cmd-replan? result)))))

    (test-case "/skip dispatches from executing state"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/skip" "/skip 1"))
          (define-values (action result) (dispatch-gsd-command parsed "/skip 1" (current-directory)))
          (check-equal? action 'skip)
          (check-true (gsd-cmd-skip? result)))))

    (test-case "/wave-done dispatches to parsed struct"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/wave-done" "/wave-done 0"))
          (define-values (action result) (dispatch-gsd-command parsed "/wave-done 0" (current-directory)))
          (check-equal? action 'wave-done)
          (check-true (gsd-cmd-wave-done? result)))))

    (test-case "/done dispatches to parsed struct"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/done" "/done"))
          (define-values (action result) (dispatch-gsd-command parsed "/done" (current-directory)))
          (check-equal? action 'done)
          (check-true (gsd-cmd-done? result)))))

    ;; -- Edge-case tests (CF-01) --

    (test-case "/replan from exploring state routes correctly"
      (with-clean-gsd-state 'exploring
        (lambda ()
          (define parsed (parse-gsd-command "/replan" "/replan"))
          (define-values (action result) (dispatch-gsd-command parsed "/replan" (current-directory)))
          (check-equal? action 'replan)
          (check-true (gsd-cmd-replan? result)))))

    (test-case "/skip from idle state routes correctly"
      (with-clean-gsd-state #f
        (lambda ()
          (define parsed (parse-gsd-command "/skip" "/skip"))
          (define-values (action result) (dispatch-gsd-command parsed "/skip" (current-directory)))
          (check-equal? action 'skip)
          (check-true (gsd-cmd-skip? result))
          (check-equal? (gsd-cmd-skip-skip-arg result) ""))))

    (test-case "/skip with non-numeric arg routes correctly"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/skip" "/skip abc"))
          (define-values (action result) (dispatch-gsd-command parsed "/skip abc" (current-directory)))
          (check-equal? action 'skip)
          (check-equal? (gsd-cmd-skip-skip-arg result) "abc"))))

    (test-case "/wave-done with empty args routes correctly"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/wave-done" "/wave-done"))
          (define-values (action result) (dispatch-gsd-command parsed "/wave-done" (current-directory)))
          (check-equal? action 'wave-done)
          (check-equal? (gsd-cmd-wave-done-wave-arg result) ""))))

    (test-case "/done --force flag parsed correctly"
      (with-clean-gsd-state 'executing
        (lambda ()
          (define parsed (parse-gsd-command "/done" "/done --force"))
          (define-values (action result) (dispatch-gsd-command parsed "/done --force" (current-directory)))
          (check-equal? action 'done)
          (check-true (gsd-cmd-done-force? result)))))

    (test-case "/plan with no text routes to artifact"
      (define parsed (parse-gsd-command "/plan" "/plan"))
      (define-values (action result) (dispatch-gsd-command parsed "/plan" (current-directory)))
      (check-equal? action 'artifact)
      (check-true (gsd-cmd-plan? result)))))

(run-tests dispatch-suite 'verbose)
