#lang racket/base
;; BOUNDARY: pure

;; tests/test-command-types.rkt — Shared command AST type tests (F14)

(require rackunit
         rackunit/text-ui
         "../util/command-types.rkt")

(define command-types-tests
  (test-suite "command-types"
    (test-case "shared-command construction"
      (define cmd (shared-command 'go "wave 0" 'required 'gsd))
      (check-equal? (shared-command-name cmd) 'go)
      (check-equal? (shared-command-args cmd) "wave 0")
      (check-equal? (shared-command-kind cmd) 'required)
      (check-equal? (shared-command-source cmd) 'gsd))

    (test-case "cmd-entry registry"
      (define entry (cmd-entry "/test" "Test command" 'general '() '("t")))
      (check-equal? (cmd-entry-name entry) "/test")
      (check-equal? (cmd-entry-summary entry) "Test command")
      (check-equal? (cmd-entry-aliases entry) '("t")))

    (test-case "register and lookup"
      (define reg (hash))
      (define entry (cmd-entry "/foo" "Foo" 'general '() '()))
      (define reg2 (register-command! reg entry))
      (check-equal? (lookup-command reg2 "/foo") entry)
      (check-false (lookup-command reg2 "/bar")))))

(run-tests command-types-tests)
