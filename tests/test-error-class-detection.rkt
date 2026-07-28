#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

;; test-error-class-detection.rkt — W11: Semantic error-class loop detection (R3/R5)

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/rollback-actions.rkt")

(define error-class-tests
  (test-suite "Error Class Detection (W11 R3/R5)"

    ;; ============================================================
    ;; R3: Error-class extraction from tool output
    ;; ============================================================

    (test-case "tool-error-class->string: git not found"
      (check-equal?
       (tool-error-class->string "fatal: not a git repository (or any of the parent directories)")
       "git-not-found")
      (check-equal? (tool-error-class->string "Not a git repository") "git-not-found")
      (check-equal? (tool-error-class->string "NOT A GIT REPOSITORY") "git-not-found"))

    (test-case "tool-error-class->string: file not found"
      (check-equal? (tool-error-class->string "No such file or directory: /etc/passwd")
                    "file-not-found")
      (check-equal? (tool-error-class->string "open: No such file or directory") "file-not-found"))

    (test-case "tool-error-class->string: command not found"
      (check-equal? (tool-error-class->string "/bin/sh: command not found: foobar")
                    "command-not-found")
      (check-equal? (tool-error-class->string "bash: foobar: command not found") "command-not-found"))

    (test-case "tool-error-class->string: non-zero exit"
      (check-equal? (tool-error-class->string "exit code 1") "non-zero-exit")
      (check-equal? (tool-error-class->string "subprocess exited with exit status 2")
                    "non-zero-exit"))

    (test-case "tool-error-class->string: generic fallback"
      (check-equal? (tool-error-class->string "Something went wrong but not sure what")
                    "generic-error")
      (check-equal? (tool-error-class->string "") "generic-error")
      (check-equal? (tool-error-class->string "something random") "generic-error"))

    (test-case "tool-error-class->string: permission denied"
      (check-equal? (tool-error-class->string "Permission denied: /root/.ssh") "permission-denied"))

    (test-case "tool-error-class->string: timeout"
      (check-equal? (tool-error-class->string "timeout: the monitored command dumped core") "timeout")
      ;; "timed out" is not the same as "timeout". This keeps the test accurate.
      (check-equal? (tool-error-class->string "Operation timed out") "generic-error"))

    ;; ============================================================
    ;; Error class => signal mapping
    ;; ============================================================

    (test-case "error-class->signal maps correctly"
      (check-equal? (error-class->signal "git-not-found") 'stuck-path)
      (check-equal? (error-class->signal "file-not-found") 'stuck-path)
      (check-equal? (error-class->signal "not-found") 'stuck-path)
      (check-equal? (error-class->signal "command-not-found") 'missing-tool)
      (check-equal? (error-class->signal "permission-denied") 'access-denied)
      (check-equal? (error-class->signal "non-zero-exit") 'command-failure)
      (check-equal? (error-class->signal "timeout") 'timeout)
      (check-equal? (error-class->signal "generic-error") 'generic-error))

    ;; ============================================================
    ;; R3: Repeated error class detection
    ;; ============================================================

    (test-case "detect-repeated-error-class: 3+ same errors"
      ;; History: most recent first -> git-not-found, git-not-found, git-not-found
      (define history
        '((git-not-found "git error 1") (git-not-found "git error 2") (git-not-found "git error 3")))
      (define result (detect-repeated-error-class history))
      (check-true (pair? result))
      (check-equal? (car result) 'git-not-found))

    (test-case "detect-repeated-error-class: less than threshold"
      (define history '((git-not-found "git error") (git-not-found "git error 2")))
      (define result (detect-repeated-error-class history))
      (check-false result))

    (test-case "detect-repeated-error-class: mixed errors"
      (define history
        '((git-not-found "git error") (not-found "file error") (git-not-found "git error 2")))
      (define result (detect-repeated-error-class history))
      (check-false result "mixed errors not repeated"))

    (test-case "detect-repeated-error-class: empty history"
      (define result (detect-repeated-error-class '()))
      (check-false result))

    ;; ============================================================
    ;; Error-class-history parameter
    ;; ============================================================

    (test-case "current-error-class-history parameter"
      (current-error-class-history '())
      (check-equal? (current-error-class-history) '())
      (current-error-class-history '((git-not-found "test1") (timeout "test2")))
      (check-equal? (length (current-error-class-history)) 2)
      (check-equal? (caar (current-error-class-history)) 'git-not-found))))

(module+ test
  (require rackunit/text-ui)
  (run-tests error-class-tests))

(module+ main
  (run-tests error-class-tests))
