#lang racket/base

;; @speed fast
;; @suite default

(require racket/file
         rackunit
         rackunit/text-ui
         (only-in "../tools/builtins/bash.rkt" tool-bash)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-details
                  tool-result-is-error?)
         (only-in "helpers/temp-fs.rkt" with-temp-dir))

(define (call-with-timeout seconds thunk)
  (define ch (make-channel))
  (thread (lambda () (channel-put ch (thunk))))
  (sync/timeout seconds ch))

(define bash-f1-tests
  (test-suite "bash F-1 tool-result hang regression"

    (test-case "heredoc payload with literal parentheses and ampersand returns a tool result"
      (with-temp-dir (tmp)
        (define output-path (build-path tmp "index.html"))
        (define command
          (format "cat > ~a <<'EOF'\nlinear-gradient(135deg, #111, #222) &copy;\nEOF"
                  (path->string output-path)))
        (define result (call-with-timeout 2 (lambda () (tool-bash (hasheq 'command command)))))
        (check-not-false result)
        (check-pred tool-result? result)
        (check-false (tool-result-is-error? result))
        (check-equal? (hash-ref (tool-result-details result) 'exit-code) 0)
        (check-true (file-exists? output-path))
        (check-regexp-match #rx"Command produced no output"
                            (hash-ref (car (tool-result-content result)) 'text))))))

(run-tests bash-f1-tests)
