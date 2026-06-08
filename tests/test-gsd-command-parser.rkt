#lang racket

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/test-gsd-command-parser.rkt — Pure parser tests (R-04/R-16)

(require rackunit
         rackunit/text-ui
         "../extensions/gsd/command-parser.rkt")

(define parser-suite
  (test-suite "GSD command parser tests"

    ;; ── Go command variants ──
    (test-case "parse /go"
      (define r (parse-gsd-command "/go" "/go"))
      (check-pred gsd-cmd-go? r)
      (check-equal? (parsed-gsd-command-canonical-name r) "/go"))

    (test-case "parse /implement alias"
      (define r (parse-gsd-command "/implement" "/implement"))
      (check-pred gsd-cmd-go? r)
      (check-equal? (parsed-gsd-command-canonical-name r) "/implement"))

    (test-case "parse /i alias"
      (define r (parse-gsd-command "/i" "/i"))
      (check-pred gsd-cmd-go? r))

    (test-case "gsd-command-parser: parse /go with wave arg"
      (define r (parse-gsd-command "/go" "/go 3"))
      (check-pred gsd-cmd-go? r)
      (check-equal? (gsd-cmd-go-wave-arg r) "3"))

    ;; ── Plan command ──
    (test-case "parse /plan with text → gsd-cmd-plan"
      (define r (parse-gsd-command "/plan" "/plan implement feature X"))
      (check-pred gsd-cmd-plan? r)
      (check-equal? (gsd-cmd-plan-plan-text r) "implement feature X"))

    (test-case "parse /p alias with text"
      (define r (parse-gsd-command "/p" "/p do something"))
      (check-pred gsd-cmd-plan? r)
      (check-equal? (gsd-cmd-plan-plan-text r) "do something"))

    (test-case "parse /plan without text → gsd-cmd-artifact"
      ;; /plan with no text falls through to artifact display
      (define r (parse-gsd-command "/plan" "/plan"))
      (check-pred gsd-cmd-plan? r)
      (check-false (gsd-cmd-plan-plan-text r)))

    ;; ── Artifact display ──
    (test-case "parse /state"
      (define r (parse-gsd-command "/state" "/state"))
      (check-pred gsd-cmd-artifact? r)
      (check-equal? (gsd-cmd-artifact-artifact-name r) "STATE"))

    (test-case "parse /s alias"
      (define r (parse-gsd-command "/s" "/s"))
      (check-pred gsd-cmd-artifact? r)
      (check-equal? (gsd-cmd-artifact-artifact-name r) "STATE"))

    (test-case "parse /handoff"
      (define r (parse-gsd-command "/handoff" "/handoff"))
      (check-pred gsd-cmd-artifact? r)
      (check-equal? (gsd-cmd-artifact-artifact-name r) "HANDOFF"))

    (test-case "parse /ho alias"
      (define r (parse-gsd-command "/ho" "/ho"))
      (check-pred gsd-cmd-artifact? r)
      (check-equal? (gsd-cmd-artifact-artifact-name r) "HANDOFF"))

    ;; ── Status ──
    (test-case "parse /gsd"
      (define r (parse-gsd-command "/gsd" "/gsd"))
      (check-pred gsd-cmd-status? r))

    ;; ── Replan ──
    (test-case "parse /replan"
      (define r (parse-gsd-command "/replan" "/replan"))
      (check-pred gsd-cmd-replan? r))

    ;; ── Skip ──
    (test-case "parse /skip"
      (define r (parse-gsd-command "/skip" "/skip 2"))
      (check-pred gsd-cmd-skip? r)
      (check-equal? (gsd-cmd-skip-skip-arg r) "2"))

    ;; ── Reset ──
    (test-case "parse /reset"
      (define r (parse-gsd-command "/reset" "/reset"))
      (check-pred gsd-cmd-reset? r))

    ;; ── Wave-done ──
    (test-case "parse /wave-done"
      (define r (parse-gsd-command "/wave-done" "/wave-done 1"))
      (check-pred gsd-cmd-wave-done? r)
      (check-equal? (gsd-cmd-wave-done-wave-arg r) "1"))

    (test-case "parse /wd alias"
      (define r (parse-gsd-command "/wd" "/wd 5"))
      (check-pred gsd-cmd-wave-done? r)
      (check-equal? (gsd-cmd-wave-done-wave-arg r) "5"))

    ;; ── Done ──
    (test-case "parse /done"
      (define r (parse-gsd-command "/done" "/done"))
      (check-pred gsd-cmd-done? r)
      (check-false (gsd-cmd-done-force? r)))

    (test-case "parse /done --force"
      (define r (parse-gsd-command "/done" "/done --force"))
      (check-pred gsd-cmd-done? r)
      (check-true (gsd-cmd-done-force? r)))

    ;; ── Unknown ──
    (test-case "gsd-command-parser: unknown command returns #f"
      (define r (parse-gsd-command "/unknown" "/unknown"))
      (check-false r))

    (test-case "empty string returns #f"
      (define r (parse-gsd-command "" ""))
      (check-false r))

    ;; ── Base struct fields ──
    (test-case "canonical-name preserved"
      (define r (parse-gsd-command "/go" "/go"))
      (check-equal? (parsed-gsd-command-canonical-name r) "/go")
      (check-equal? (parsed-gsd-command-args r) ""))

    ;; ── All parsed commands are also parsed-gsd-command? ──
    (test-case "subtypes satisfy parsed-gsd-command?"
      (for ([cmd
             (in-list
              '("/go" "/gsd" "/replan" "/skip" "/reset" "/done" "/wave-done" "/state" "/handoff"))])
        (define r (parse-gsd-command cmd cmd))
        (check-pred parsed-gsd-command? r (format "~a should be parsed-gsd-command?" cmd))))))

(run-tests parser-suite 'verbose)
