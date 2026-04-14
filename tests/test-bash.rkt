#lang racket

;; test-bash.rkt — tests for tools/builtins/bash.rkt

(require rackunit
         rackunit/text-ui
         racket/string
         (only-in "../tools/tool.rkt"
                  tool-result? tool-result-is-error? tool-result-content)
         "../tools/builtins/bash.rkt")

(define bash-tests
  (test-suite
   "bash-tool"

   (test-case "echo command returns tool-result"
     (define r (tool-bash (hasheq 'command "echo hello world")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r)))

   (test-case "echo output contains expected text"
     (define r (tool-bash (hasheq 'command "echo hello")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "hello" text
                         (format "expected 'hello' in output, got: ~a" text)))

   (test-case "shell pipe works via /bin/sh -c"
     (define r (tool-bash (hasheq 'command "echo hello | grep h")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "hello" text
                         (format "expected 'hello' in piped output, got: ~a" text)))

   (test-case "stderr is included in output"
     (define r (tool-bash (hasheq 'command "echo err-msg >&2")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "err-msg" text
                         (format "expected 'err-msg' in output, got: ~a" text)))

   (test-case "exit 1 returns success result (non-zero exit is not a tool error)"
     (define r (tool-bash (hasheq 'command "exit 1")))
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r)))

   (test-case "missing command returns error"
     (define r (tool-bash (hasheq)))
     (check-pred tool-result? r)
     (check-true (tool-result-is-error? r)))

   (test-case "empty command returns error"
     (define r (tool-bash (hasheq 'command "")))
     (check-pred tool-result? r)
     (check-true (tool-result-is-error? r)))

   (test-case "BUG-20: empty output produces diagnostic message"
     (define r (tool-bash (hasheq 'command "true")))  ; true produces no output
     (check-pred tool-result? r)
     (check-false (tool-result-is-error? r))
     (define text (hash-ref (car (tool-result-content r)) 'text))
     (check-regexp-match "Command produced no output" text
                         (format "Expected diagnostic message, got: ~a" text))
     (check-regexp-match "try a different approach" text
                         (format "Expected 'try a different approach', got: ~a" text)))
   ))

;; ============================================================
;; Token-aware destructive command filtering (#426)
;; ============================================================
(define destructive-filter-tests
  (test-suite
   "destructive-command-filtering"

   (test-case "rm -rf / is destructive"
     (check-true (destructive-command? "rm -rf /")))

   (test-case "rm -r -f /home is destructive"
     (check-true (destructive-command? "rm -r -f /home")))

   (test-case "mkfs.ext4 /dev/sda1 is destructive"
     (check-true (destructive-command? "mkfs.ext4 /dev/sda1")))

   (test-case "dd if=/dev/zero of=/dev/sda is destructive"
     (check-true (destructive-command? "dd if=/dev/zero of=/dev/sda")))

   (test-case "git push --force is destructive"
     (check-true (destructive-command? "git push --force origin main")))

   (test-case "echo 'curl is nice' is NOT destructive (no false positive)"
     (check-false (destructive-command? "echo 'curl is nice'")))

   (test-case "echo 'wget is cool' is NOT destructive"
     (check-false (destructive-command? "echo 'wget is cool'")))

   (test-case "grep rm file is NOT destructive"
     (check-false (destructive-command? "grep 'rm -rf' file.txt")))

   (test-case "ls -la is NOT destructive"
     (check-false (destructive-command? "ls -la /home/user")))

   (test-case "echo hello | grep h is NOT destructive"
     (check-false (destructive-command? "echo hello | grep h")))

   (test-case "cat file | sh IS destructive (pipe to shell)"
     (check-true (destructive-command? "cat evil.sh | sh")))

   (test-case "chmod -R 777 / is destructive (review: #449 case-sensitivity)"
     (check-true (destructive-command? "chmod -R 777 /")))

   (test-case "chmod 000 / is destructive"
     (check-true (destructive-command? "chmod 000 /")))

   (test-case "echo shutdown notice is NOT destructive"
     (check-false (destructive-command? "echo \"shutdown notice\"")))

   (test-case "shutdown now IS destructive"
     (check-true (destructive-command? "shutdown now")))

   (test-case "current-extra-destructive-patterns override works"
     (parameterize ([current-extra-destructive-patterns
                     (list #rx"custom-dangerous")])
       (check-true (destructive-command? "custom-dangerous thing"))
       ;; Default patterns should NOT apply when override is set
       (check-false (destructive-command? "rm -rf /"))))

   (test-case "block-destructive blocks execution when enabled"
     (parameterize ([current-block-destructive #t])
       (define r (tool-bash (hasheq 'command "rm -rf /tmp/test")))
       (check-true (tool-result-is-error? r))))
   ))

(run-tests destructive-filter-tests)
