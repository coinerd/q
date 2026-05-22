#lang racket
;; @suite security

;; BOUNDARY: integration

;; test-destructive-warning.rkt — SEC-02 tests for destructive command warning
;; Updated: uses bash-execution-config struct (v0.46.3 F10)

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
         (only-in "../util/protocol-types.rkt"
                  tool-result?
                  tool-result-content
                  tool-result-is-error?))

;; --------------------------------------------------
;; Test 1: Default config warns on destructive
;; --------------------------------------------------
(let ([cfg (make-bash-execution-config)])
  (check-true (bash-execution-config-warn-on-destructive? cfg)
              "default config should warn on destructive"))

;; --------------------------------------------------
;; Test 2: destructive-command? correctly identifies destructive commands
;; --------------------------------------------------
(check-true (destructive-command? "rm -rf /tmp/test") "rm -rf should be detected as destructive")
(check-true (destructive-command? "shutdown -h now") "shutdown should be detected as destructive")
(check-false (destructive-command? "ls -la /tmp") "ls should NOT be detected as destructive")
(check-false (destructive-command? "echo hello") "echo should NOT be detected as destructive")

;; --------------------------------------------------
;; Test 3: Destructive commands trigger a warning on stderr
;; --------------------------------------------------
(let ([captured-stderr (open-output-string)])
  (parameterize ([current-error-port captured-stderr]
                 [current-bash-execution-config (make-bash-execution-config #:warn? #t)])
    (tool-bash (hasheq 'command "rm -rf /tmp/nonexistent_test_path_for_sec02")))
  (define stderr-str (get-output-string captured-stderr))
  (check-not-false (and (string-contains? stderr-str "WARNING")
                        (string-contains? stderr-str "Destructive command detected"))
                   "rm -rf should produce a WARNING on stderr"))

;; --------------------------------------------------
;; Test 4: Non-destructive commands do NOT trigger a warning
;; --------------------------------------------------
(let ([captured-stderr (open-output-string)])
  (parameterize ([current-error-port captured-stderr]
                 [current-bash-execution-config (make-bash-execution-config #:warn? #t)])
    (tool-bash (hasheq 'command "ls")))
  (define stderr-str (get-output-string captured-stderr))
  (check-false (string-contains? stderr-str "Destructive command detected")
               "ls should NOT produce a destructive-command warning on stderr"))

;; --------------------------------------------------
;; Test 5: Setting warn? #f suppresses warnings on destructive commands
;; --------------------------------------------------
(let ([captured-stderr (open-output-string)])
  (parameterize ([current-error-port captured-stderr]
                 [current-bash-execution-config (make-bash-execution-config #:warn? #f)])
    (tool-bash (hasheq 'command "rm -rf /tmp/nonexistent_test_path_for_sec02")))
  (define stderr-str (get-output-string captured-stderr))
  (check-false (string-contains? stderr-str "Destructive command detected")
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
(let ([captured-stderr (open-output-string)])
  (parameterize ([current-error-port captured-stderr]
                 [current-bash-execution-config (make-bash-execution-config #:warn? #t #:block? #t)])
    (define result (tool-bash (hasheq 'command "rm -rf /tmp/blocked_test_path")))
    (check-true (tool-result-is-error? result)
                "blocked destructive command should return error even with warning on")
    (define stderr-str (get-output-string captured-stderr))
    ;; When blocking, no subprocess runs so no warning should be emitted
    (check-false (string-contains? stderr-str "WARNING")
                 "blocking should prevent warning from being emitted")))
