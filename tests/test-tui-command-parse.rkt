#lang racket

;; @speed fast
;; @suite default

(require rackunit
         "../tui/command-parse.rkt")

(test-case "tokenize returns two values for commands"
  (define-values (cmd args) (tokenize "/switch branch-1"))
  (check-equal? cmd "/switch")
  (check-equal? args '("branch-1")))

(test-case "tokenize returns two values with #f command for non-commands"
  (define-values (empty-cmd empty-args) (tokenize ""))
  (check-false empty-cmd)
  (check-equal? empty-args '())
  (define-values (text-cmd text-args) (tokenize "hello world"))
  (check-false text-cmd)
  (check-equal? text-args '()))

(test-case "parse-command-name handles normalized tokenize arity"
  (check-false (parse-command-name "hello"))
  (define parsed (parse-command-name "/help"))
  (check-true (parsed-command? parsed))
  (check-equal? (parsed-command-canonical-name parsed) 'help))
