#lang racket

;; @speed fast
;; @suite security

;; BOUNDARY: integration

;; test-destructive-warning.rkt — SEC-02 tests for destructive command warning
;; Updated: v0.97.12 — warnings now go to tool result output (not stderr) to fix TUI leak

(require rackunit
         racket/string
         racket/port
         (only-in "../tools/builtins/bash.rkt"
                  destructive-command?
                  tool-bash
                  make-bash-execution-config
                  current-bash-execution-config
                  bash-execution-config-warn-on-destructive?
                  bash-execution-config-block-destructive?)
         (only-in "../util/message/protocol-types.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-is-error?))

;; Helper: extract text content from tool result
(define (result-text result)
  (define content (tool-result-content result))
  (if (and (list? content) (pair? content) (hash? (car content)))
      (hash-ref (car content) 'text "")
      ""))

;; --------------------------------------------------
;; Test 1: Default config warns on destructive
;; --------------------------------------------------
(let ([cfg (make-bash-execution-config)])
  (check-true (bash-execution-config-warn-on-destructive? cfg)
              "default config should warn on destructive"))

;; --------------------------------------------------
;; Test 2: destructive-command? correctly identifies destructive commands
;; --------------------------------------------------
(test-case "test-destructive-warning: checks block 1"
  (check-true (destructive-command? "rm -rf /tmp/test") "rm -rf should be detected as destructive")
  (check-true (destructive-command? "shutdown -h now") "shutdown should be detected as destructive")
  (check-false (destructive-command? "ls -la /tmp") "ls should NOT be detected as destructive")
  (check-false (destructive-command? "echo hello") "echo should NOT be detected as destructive"))

;; --------------------------------------------------
;; Test 3: Destructive commands include warning in tool result output
;; (v0.97.12: warning moved from stderr to tool result to fix TUI leak)
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #t)])
                (tool-bash (hasheq 'command "rm -rf /tmp/nonexistent_test_path_for_sec02")))])
  (define text (result-text result))
  (check-not-false (and (string-contains? text "WARNING")
                        (string-contains? text "Destructive command detected"))
                   "rm -rf should include WARNING in tool result output"))

;; --------------------------------------------------
;; Test 4: Non-destructive commands do NOT include a warning
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #t)])
                (tool-bash (hasheq 'command "ls")))])
  (define text (result-text result))
  (check-false (string-contains? text "Destructive command detected")
               "ls should NOT include a destructive-command warning in output"))

;; --------------------------------------------------
;; Test 4b: Benign commands do NOT leak warning/diagnostic
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #t
                                                                                        #:block? #f)])
                (tool-bash (hasheq 'command "echo hello_world_for_benign_test")))])
  (define text (result-text result))
  (check-false (string-contains? text "Destructive command detected")
               "benign command should not produce destructive warning")
  (check-false (string-contains? text "[CLASSIFIER-DIAG]")
               "classifier disagreement diagnostics should not appear in output"))

;; --------------------------------------------------
;; Test 4c: Risky regex-only commands still warn
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #t
                                                                                        #:block? #f)])
                (tool-bash (hasheq 'command "source /tmp/untrusted-q-test-script")))])
  (define text (result-text result))
  (check-not-false (string-contains? text "Destructive command detected")
                   "risky regex-only commands should still warn in tool output"))

;; --------------------------------------------------
;; Test 5: Setting warn? #f suppresses warnings on destructive commands
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #f)])
                (tool-bash (hasheq 'command "rm -rf /tmp/nonexistent_test_path_for_sec02")))])
  (define text (result-text result))
  (check-false (string-contains? text "Destructive command detected")
               "rm -rf with warning disabled should NOT produce a warning"))

;; ==================================================
;; SEC-01: block-destructive via config struct
;; ==================================================

;; --------------------------------------------------
;; Test 6: block-destructive? #t blocks destructive commands
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:block? #t)])
                (tool-bash (hasheq 'command "rm -rf /tmp/blocked_test_path")))])
  (check-true (tool-result-is-error? result)
              "blocked destructive command should return error tool-result")
  (define content (tool-result-content result))
  (check-not-false (string-contains? (hash-ref (car content) 'text "") "Blocked destructive command")
                   "error message should mention blocked destructive command"))

;; --------------------------------------------------
;; Test 7: block-destructive? #f allows non-destructive commands
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:block? #f
                                                                                        #:warn? #f)])
                (tool-bash (hasheq 'command "echo blocked_test")))])
  (check-false (tool-result-is-error? result)
               "non-destructive command should succeed regardless of block setting"))

;; --------------------------------------------------
;; Test 8: blocking takes priority over warning
;; --------------------------------------------------
(let ([result (parameterize ([current-bash-execution-config (make-bash-execution-config #:warn? #t
                                                                                        #:block? #t)])
                (tool-bash (hasheq 'command "rm -rf /tmp/blocked_test_path")))])
  (check-true (tool-result-is-error? result)
              "blocked destructive command should return error even with warning on")
  (define text (result-text result))
  ;; When blocking, no subprocess runs so no warning should be in output
  (check-false (string-contains? text "WARNING")
               "blocking should prevent warning from being emitted"))
