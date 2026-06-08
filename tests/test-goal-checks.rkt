#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-goal-checks.rkt — Deterministic check execution tests

(require rackunit
         racket/string
         (only-in "../runtime/goal/goal-state.rkt"
                  goal-check?
                  goal-check-command
                  goal-check-label
                  make-goal-check
                  check-result?
                  check-result-exit-code
                  check-result-stdout
                  check-result-label
                  check-result-timed-out?)
         "../runtime/goal/goal-checks.rkt")

;; ============================================================
;; parse-goal-checks — no checks
;; ============================================================

(test-case "goal-checks: block 1"
  (let ()
    (define-values (goal-text checks) (parse-goal-checks "tests pass"))
    (check-equal? goal-text "tests pass")
    (check-equal? checks '())))

;; ============================================================
;; parse-goal-checks — one check
;; ============================================================

(test-case "goal-checks: block 2"
  (let ()
    (define-values (goal-text checks) (parse-goal-checks "tests pass --check 'raco test'"))
    (check-equal? goal-text "tests pass")
    (check-equal? (length checks) 1)
    (check-equal? (goal-check-command (car checks)) "raco test")
    (check-equal? (goal-check-label (car checks)) "check-1")))

;; ============================================================
;; parse-goal-checks — two checks with mixed quotes
;; ============================================================

(test-case "goal-checks: block 3"
  (let ()
    (define-values (goal-text checks)
      (parse-goal-checks "module compiles --check \"raco make\" --check 'raco test'"))
    (check-equal? goal-text "module compiles")
    (check-equal? (length checks) 2)
    (check-equal? (goal-check-command (car checks)) "raco make")
    (check-equal? (goal-check-command (cadr checks)) "raco test")))

;; ============================================================
;; validate-check-safety — safe command
;; ============================================================

(test-case "goal-checks: block 4"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'raco test'"))
    (define reasons (validate-check-safety checks))
    (check-equal? reasons '())))

;; ============================================================
;; validate-check-safety — dangerous command blocked
;; ============================================================

(test-case "goal-checks: block 5"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'rm -rf /'"))
    (define reasons (validate-check-safety checks))
    (check-true (> (length reasons) 0) "critical command blocked")))

;; ============================================================
;; execute-goal-check — true command passes
;; ============================================================

(test-case "goal-checks: block 6"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'true'"))
    (define result (execute-goal-check (car checks) #:timeout 5))
    (check-equal? (check-result-exit-code result) 0)
    (check-equal? (check-result-label result) "check-1")))

;; ============================================================
;; execute-goal-check — false command fails
;; ============================================================

(test-case "goal-checks: block 7"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'false'"))
    (define result (execute-goal-check (car checks) #:timeout 5))
    (check-not-equal? (check-result-exit-code result) 0)))

;; ============================================================
;; execute-goal-check — output captured
;; ============================================================

(test-case "goal-checks: block 8"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'echo hello'"))
    (define result (execute-goal-check (car checks) #:timeout 5))
    (check-equal? (check-result-exit-code result) 0)
    (check-true (string-contains? (check-result-stdout result) "hello"))))

;; ============================================================
;; execute-all-checks — multiple checks
;; ============================================================

(test-case "goal-checks: block 9"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'true' --check 'echo ok'"))
    (define results (execute-all-checks checks #:timeout 5))
    (check-equal? (length results) 2)
    (check-equal? (check-result-exit-code (car results)) 0)
    (check-equal? (check-result-exit-code (cadr results)) 0)))

;; ============================================================
;; checks-summary — all pass
;; ============================================================

(test-case "goal-checks: block 10"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'true' --check 'true'"))
    (define results (execute-all-checks checks #:timeout 5))
    (define-values (all-passed? summary) (checks-summary results))
    (check-true all-passed?)
    (check-true (string-contains? summary "PASS"))))

;; ============================================================
;; checks-summary — mixed pass/fail
;; ============================================================

(test-case "goal-checks: block 11"
  (let ()
    (define-values (_ checks) (parse-goal-checks "x --check 'true' --check 'false'"))
    (define results (execute-all-checks checks #:timeout 5))
    (define-values (all-passed? summary) (checks-summary results))
    (check-false all-passed?)
    (check-true (string-contains? summary "FAIL"))))

;; ============================================================
;; W1: Shell safety — command substitution now high severity (v0.71.7)
;; ============================================================

;; Command substitution $(...) is rejected by validate-check-safety
(test-case "goal-checks: block 12"
  (let ()
    (define checks (list (make-goal-check #:command "echo $(whoami)" #:label "sub")))
    (define reasons (validate-check-safety checks))
    (check-true (pair? reasons) "command substitution rejected")))

;; Pipe commands are rejected
(test-case "goal-checks: block 13"
  (let ()
    (define checks (list (make-goal-check #:command "cat /etc/passwd | bash" #:label "pipe")))
    (define reasons (validate-check-safety checks))
    (check-true (pair? reasons) "pipe rejected")))
