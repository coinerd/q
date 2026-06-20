#lang racket

;; @speed fast
;; @suite fast

;; W3 v0.99.35: Tests for subprocess-helpers.rkt
;; Pure result-boundary functions extracted from subprocess.rkt:
;; - Result struct + constructors (make-error-result, make-timeout-result, make-success-result)
;; - Exit code classifiers (exit-success?, exit-timeout?, exit-error?)
;; - Message formatters (format-timeout-message, format-truncation-message)
;; - Secret detection (check-secret-var?, secret-patterns, SECRET-IMPLICIT-ALLOWLIST)

(require rackunit
         rackunit/text-ui)

(require (only-in "../sandbox/subprocess-helpers.rkt"
                  subprocess-result
                  subprocess-result?
                  subprocess-result-exit-code
                  subprocess-result-stdout
                  subprocess-result-stderr
                  subprocess-result-timed-out?
                  subprocess-result-elapsed-ms
                  subprocess-result-truncated?
                  EXIT-CODE-TIMEOUT
                  EXIT-CODE-ERROR
                  make-error-result
                  make-timeout-result
                  make-success-result
                  exit-success?
                  exit-timeout?
                  exit-error?
                  format-timeout-message
                  format-truncation-message
                  secret-patterns
                  SECRET-IMPLICIT-ALLOWLIST
                  check-secret-var?))

(define-test-suite result-constructor-tests
                   (test-case "make-error-result constructs error result"
                     (define r (make-error-result "Failed to execute: boom" 500))
                     (check-equal? (subprocess-result-exit-code r) EXIT-CODE-ERROR)
                     (check-equal? (subprocess-result-stdout r) "")
                     (check-equal? (subprocess-result-stderr r) "Failed to execute: boom")
                     (check-false (subprocess-result-timed-out? r))
                     (check-equal? (subprocess-result-elapsed-ms r) 500)
                     (check-false (subprocess-result-truncated? r)))
                   (test-case "make-timeout-result constructs timeout result"
                     (define r (make-timeout-result "partial-out" "partial-err" 30 5000 #t))
                     (check-equal? (subprocess-result-exit-code r) EXIT-CODE-TIMEOUT)
                     (check-equal? (subprocess-result-stdout r) "partial-out")
                     (check-true (string-contains? (subprocess-result-stderr r) "timed out"))
                     (check-true (string-contains? (subprocess-result-stderr r) "partial-err"))
                     (check-true (subprocess-result-timed-out? r))
                     (check-equal? (subprocess-result-elapsed-ms r) 5000)
                     (check-true (subprocess-result-truncated? r)))
                   (test-case "make-success-result constructs success result"
                     (define r (make-success-result 0 "hello\n" "" 100 #f))
                     (check-equal? (subprocess-result-exit-code r) 0)
                     (check-equal? (subprocess-result-stdout r) "hello\n")
                     (check-equal? (subprocess-result-stderr r) "")
                     (check-false (subprocess-result-timed-out? r))
                     (check-equal? (subprocess-result-elapsed-ms r) 100)
                     (check-false (subprocess-result-truncated? r)))
                   (test-case "make-success-result with non-zero exit"
                     (define r (make-success-result 1 "out" "err" 50 #f))
                     (check-equal? (subprocess-result-exit-code r) 1)
                     (check-false (subprocess-result-timed-out? r))))

(define-test-suite exit-classifier-tests
                   (test-case "exit-success? for 0"
                     (check-true (exit-success? 0)))
                   (test-case "exit-success? false for 1"
                     (check-false (exit-success? 1)))
                   (test-case "exit-success? false for -1"
                     (check-false (exit-success? EXIT-CODE-ERROR)))
                   (test-case "exit-success? false for -9"
                     (check-false (exit-success? EXIT-CODE-TIMEOUT)))
                   (test-case "exit-timeout? for -9"
                     (check-true (exit-timeout? EXIT-CODE-TIMEOUT)))
                   (test-case "exit-timeout? false for 0"
                     (check-false (exit-timeout? 0)))
                   (test-case "exit-error? for -1"
                     (check-true (exit-error? EXIT-CODE-ERROR)))
                   (test-case "exit-error? false for 0"
                     (check-false (exit-error? 0)))
                   (test-case "exit-error? false for -9"
                     (check-false (exit-error? EXIT-CODE-TIMEOUT))))

(define-test-suite message-formatter-tests
                   (test-case "format-timeout-message contains timeout seconds"
                     (define msg (format-timeout-message 30))
                     (check-true (string-contains? msg "30") "contains timeout value")
                     (check-true (string-contains? msg "timed out") "mentions timeout")
                     (check-true (string-contains? msg "Partial output") "mentions partial output"))
                   (test-case "format-truncation-message contains byte count"
                     (define msg (format-truncation-message 4096))
                     (check-true (string-contains? msg "4096") "contains byte value")
                     (check-true (string-contains? msg "truncated") "mentions truncation")))

(define-test-suite
 secret-detection-tests
 (test-case "check-secret-var? detects API_KEY"
   (check-true (check-secret-var? "API_KEY" '() '())))
 (test-case "check-secret-var? detects SECRET"
   (check-true (check-secret-var? "MY_SECRET" '() '())))
 (test-case "check-secret-var? detects TOKEN"
   (check-true (check-secret-var? "GITHUB_TOKEN" '() '())))
 (test-case "check-secret-var? detects PASSWORD"
   (check-true (check-secret-var? "DB_PASSWORD" '() '())))
 (test-case "check-secret-var? allows PATH"
   (check-false (check-secret-var? "PATH" '() '())))
 (test-case "check-secret-var? allows HOME"
   (check-false (check-secret-var? "HOME" '() '())))
 (test-case "check-secret-var? allows TERM"
   (check-false (check-secret-var? "TERM" '() '())))
 (test-case "check-secret-var? implicit allowlist: AUTHOR"
   (check-false (check-secret-var? "AUTHOR" '() '())))
 (test-case "check-secret-var? implicit allowlist: XAUTHORITY"
   (check-false (check-secret-var? "XAUTHORITY" '() '())))
 (test-case "check-secret-var? implicit allowlist: SSH_AUTH_SOCK"
   (check-false (check-secret-var? "SSH_AUTH_SOCK" '() '())))
 (test-case "check-secret-var? detects bare AUTH"
   (check-true (check-secret-var? "AUTH" '() '())))
 (test-case "check-secret-var? detects AUTH_TOKEN"
   (check-true (check-secret-var? "MY_AUTH_TOKEN" '() '())))
 (test-case "extra denylist adds patterns"
   (check-true (check-secret-var? "MY_CUSTOM_VAR" (list #rx"(?i:CUSTOM)") '())))
 (test-case "extra allowlist overrides pattern"
   (check-false (check-secret-var? "MY_TOKEN_COUNT" '() (list #rx"(?i:TOKEN_COUNT)"))))
 (test-case "allowlist takes priority over denylist"
   (check-false (check-secret-var? "CUSTOM_FLAG" (list #rx"(?i:CUSTOM)") (list #rx"(?i:FLAG)"))))
 (test-case "secret-patterns is a non-empty list"
   (check-true (and (pair? secret-patterns) #t)))
 (test-case "SECRET-IMPLICIT-ALLOWLIST includes known vars"
   (check-true (and (member "SSH_AUTH_SOCK" SECRET-IMPLICIT-ALLOWLIST) #t) "contains SSH_AUTH_SOCK")))

(define-test-suite all-subprocess-helpers-tests
                   result-constructor-tests
                   exit-classifier-tests
                   message-formatter-tests
                   secret-detection-tests)

(module+ test
  (run-tests all-subprocess-helpers-tests))

(module+ main
  (run-tests all-subprocess-helpers-tests))
