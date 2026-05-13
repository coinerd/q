#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-command-dispatch.rkt -- A6-03: dispatch-gsd-command routing tests

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/command-parser.rkt"
         "../extensions/gsd/command-handlers.rkt")

(define dispatch-suite
  (test-suite "dispatch-gsd-command routing"

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
      (check-equal? action 'artifact))))

(run-tests dispatch-suite 'verbose)
