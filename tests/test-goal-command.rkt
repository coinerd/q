#lang racket/base

;; tests/test-goal-command.rkt — Tests for /goal command parsing and handling

(require rackunit
         racket/string
         "../tui/command-parse.rkt")

;; ============================================================
;; Command table includes /goal and /g
;; ============================================================

(let ()
  (define table (make-command-table))
  (check-equal? (hash-ref table "/goal" #f) (cons 'goal 'optional))
  (check-equal? (hash-ref table "/g" #f) (cons 'goal 'optional)))

;; ============================================================
;; Parse /goal commands
;; ============================================================

(let ()
  (define result (parse-command-name "/goal"))
  (check-equal? (parsed-command-canonical-name result) 'goal)
  (check-equal? (parsed-command-args result) '()))

(let ()
  (define result (parse-command-name "/goal clear"))
  (check-equal? (parsed-command-canonical-name result) 'goal)
  (check-equal? (parsed-command-args result) '("clear")))

(let ()
  (define result (parse-command-name "/goal status"))
  (check-equal? (parsed-command-canonical-name result) 'goal)
  (check-equal? (parsed-command-args result) '("status")))

(let ()
  (define result (parse-command-name "/goal \"make all tests pass\""))
  (check-equal? (parsed-command-canonical-name result) 'goal)
  (check-not-false (parsed-command-args result)))

(let ()
  (define result (parse-command-name "/g"))
  (check-equal? (parsed-command-canonical-name result) 'goal))

(displayln "All goal-command tests passed.")
