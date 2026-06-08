#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         "../tools/builtins/spawn-subagent.rkt")

;; subagent-config struct
(test-case "subagent-config construction"
  (define cfg (subagent-config "do stuff" "assistant" 5 #f #f))
  (check-equal? (subagent-config-task cfg) "do stuff")
  (check-equal? (subagent-config-role cfg) "assistant")
  (check-equal? (subagent-config-max-turns cfg) 5)
  (check-false (subagent-config-tools cfg))
  (check-false (subagent-config-model cfg)))

(test-case "subagent-config transparent"
  (define cfg (subagent-config "task" "role" 3 '("read") "gpt-4"))
  (check-equal? (subagent-config-tools cfg) '("read"))
  (check-equal? (subagent-config-model cfg) "gpt-4"))

;; parse-subagent-config
(test-case "parse-subagent-config with all fields"
  (define cfg
    (parse-subagent-config
     (hasheq 'task "hello" 'role "coder" 'max-turns 10 'tools '("read" "write") 'model "test-model")))
  (check-equal? (subagent-config-task cfg) "hello")
  (check-equal? (subagent-config-max-turns cfg) 10)
  (check-equal? (subagent-config-tools cfg) '("read" "write"))
  (check-equal? (subagent-config-model cfg) "test-model"))

(test-case "parse-subagent-config with defaults"
  (define cfg (parse-subagent-config (hasheq 'task "hello")))
  (check-equal? (subagent-config-task cfg) "hello")
  (check-equal? (subagent-config-max-turns cfg) 5)
  (check-false (subagent-config-tools cfg))
  (check-false (subagent-config-model cfg)))
