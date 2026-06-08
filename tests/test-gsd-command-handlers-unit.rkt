#lang racket/base

;; @speed fast  ;; @suite extensions

;; tests/test-gsd-command-handlers-unit.rkt — Unit tests for GSD command parsing & dispatch (T-6)
;; Tests the pure parse-gsd-command and dispatch-gsd-command functions.

(require racket/base
         rackunit
         rackunit/text-ui
         "../extensions/gsd/command-parser.rkt"
         "../extensions/gsd/command-handlers.rkt")

(define suite
  (test-suite
   "GSD command parsing + dispatch unit tests"

   ;; === parse-gsd-command ===

   (test-case "parse /go → gsd-cmd-go"
     (define cmd (parse-gsd-command "/go" "/go"))
     (check-not-false cmd)
     (check-true (gsd-cmd-go? cmd))
     (check-equal? (parsed-gsd-command-canonical-name cmd) "/go"))

   (test-case "parse /go alias /i → gsd-cmd-go"
     (define cmd (parse-gsd-command "/i" "/i"))
     (check-not-false cmd)
     (check-true (gsd-cmd-go? cmd)))

   (test-case "parse /go alias /implement → gsd-cmd-go"
     (define cmd (parse-gsd-command "/implement" "/implement"))
     (check-not-false cmd)
     (check-true (gsd-cmd-go? cmd)))

   (test-case "gsd-command-handlers-unit: parse /go with wave arg"
     (define cmd (parse-gsd-command "/go" "/go 3"))
     (check-not-false cmd)
     (check-true (gsd-cmd-go? cmd))
     (check-equal? (gsd-cmd-go-wave-arg cmd) "3"))

   (test-case "parse /done → gsd-cmd-done"
     (define cmd (parse-gsd-command "/done" "/done"))
     (check-not-false cmd)
     (check-true (gsd-cmd-done? cmd))
     (check-false (gsd-cmd-done-force? cmd)))

   (test-case "parse /done --force → gsd-cmd-done with force"
     (define cmd (parse-gsd-command "/done" "/done --force"))
     (check-not-false cmd)
     (check-true (gsd-cmd-done? cmd))
     (check-true (gsd-cmd-done-force? cmd)))

   (test-case "parse /gsd → gsd-cmd-status"
     (define cmd (parse-gsd-command "/gsd" "/gsd"))
     (check-not-false cmd)
     (check-true (gsd-cmd-status? cmd)))

   (test-case "parse /replan → gsd-cmd-replan"
     (define cmd (parse-gsd-command "/replan" "/replan"))
     (check-not-false cmd)
     (check-true (gsd-cmd-replan? cmd)))

   (test-case "parse /reset → gsd-cmd-reset"
     (define cmd (parse-gsd-command "/reset" "/reset"))
     (check-not-false cmd)
     (check-true (gsd-cmd-reset? cmd)))

   (test-case "parse /skip → gsd-cmd-skip"
     (define cmd (parse-gsd-command "/skip" "/skip 2"))
     (check-not-false cmd)
     (check-true (gsd-cmd-skip? cmd))
     (check-equal? (gsd-cmd-skip-skip-arg cmd) "2"))

   (test-case "parse /wave-done → gsd-cmd-wave-done"
     (define cmd (parse-gsd-command "/wave-done" "/wave-done 1"))
     (check-not-false cmd)
     (check-true (gsd-cmd-wave-done? cmd)))

   (test-case "parse /wave-done alias /wd"
     (define cmd (parse-gsd-command "/wd" "/wd 1"))
     (check-not-false cmd)
     (check-true (gsd-cmd-wave-done? cmd)))

   (test-case "parse /plan without text → artifact display"
     (define cmd (parse-gsd-command "/plan" "/plan"))
     (check-not-false cmd)
     (check-true (gsd-cmd-plan? cmd))
     (check-false (gsd-cmd-plan-plan-text cmd)))

   (test-case "parse /plan with text → plan submit"
     (define cmd (parse-gsd-command "/plan" "/plan implement feature X"))
     (check-not-false cmd)
     (check-true (gsd-cmd-plan? cmd))
     (check-equal? (gsd-cmd-plan-plan-text cmd) "implement feature X"))

   (test-case "parse /state → artifact"
     (define cmd (parse-gsd-command "/state" "/state"))
     (check-not-false cmd)
     (check-true (gsd-cmd-artifact? cmd))
     (check-equal? (gsd-cmd-artifact-artifact-name cmd) "STATE"))

   (test-case "parse /handoff → artifact"
     (define cmd (parse-gsd-command "/handoff" "/handoff"))
     (check-not-false cmd)
     (check-true (gsd-cmd-artifact? cmd))
     (check-equal? (gsd-cmd-artifact-artifact-name cmd) "HANDOFF"))

   (test-case "parse /p alias → plan"
     (define cmd (parse-gsd-command "/p" "/p"))
     (check-not-false cmd)
     (check-true (gsd-cmd-plan? cmd)))

   (test-case "parse unknown command → #f"
     (define cmd (parse-gsd-command "/unknown" "/unknown"))
     (check-false cmd))

   ;; === dispatch-gsd-command ===

   (test-case "dispatch /go → 'go action"
     (define parsed (parse-gsd-command "/go" "/go"))
     (define-values (action data) (dispatch-gsd-command parsed "/go" "/tmp"))
     (check-equal? action 'go))

   (test-case "dispatch /gsd → 'status action"
     (define parsed (parse-gsd-command "/gsd" "/gsd"))
     (define-values (action data) (dispatch-gsd-command parsed "/gsd" "/tmp"))
     (check-equal? action 'status))

   (test-case "dispatch /replan → 'replan action"
     (define parsed (parse-gsd-command "/replan" "/replan"))
     (define-values (action data) (dispatch-gsd-command parsed "/replan" "/tmp"))
     (check-equal? action 'replan)
     (check-true (gsd-cmd-replan? data)))

   (test-case "dispatch /skip → 'skip action"
     (define parsed (parse-gsd-command "/skip" "/skip 2"))
     (define-values (action data) (dispatch-gsd-command parsed "/skip 2" "/tmp"))
     (check-equal? action 'skip))

   (test-case "dispatch /done → 'done action"
     (define parsed (parse-gsd-command "/done" "/done"))
     (define-values (action data) (dispatch-gsd-command parsed "/done" "/tmp"))
     (check-equal? action 'done))

   (test-case "dispatch /plan with text → 'plan-submit action"
     (define parsed (parse-gsd-command "/plan" "/plan do thing"))
     (define-values (action data) (dispatch-gsd-command parsed "/plan do thing" "/tmp"))
     (check-equal? action 'plan-submit)
     (check-equal? data "do thing"))

   (test-case "dispatch /plan without text → 'artifact action"
     (define parsed (parse-gsd-command "/plan" "/plan"))
     (define-values (action data) (dispatch-gsd-command parsed "/plan" "/tmp"))
     (check-equal? action 'artifact))

   ;; === gsd-command-specs ===

   (test-case "gsd-command-specs has expected entries"
     (define canonicals (map gsd-command-spec-canonical gsd-command-specs))
     (for ([expected '("/plan" "/state" "/handoff" "/go" "/wave-done" "/done" "/replan" "/skip" "/reset" "/gsd")])
       (check-not-false (member expected canonicals)
                        (format "Expected ~a in specs" expected))))

   (test-case "aliases-for returns list with canonical"
     (define aliases (aliases-for "/go"))
     (check-not-false (member "/go" aliases))
     (check-not-false (member "/i" aliases))
     (check-not-false (member "/implement" aliases)))))

(run-tests suite)
