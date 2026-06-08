#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-cli.rkt — thin aggregator + remaining CLI tests not yet extracted.
;; Extracted domains live in:
;;   - test-cli-args.rkt
;;   - test-cli-builder.rkt
;;   - test-cli-format.rkt
;;   - test-cli-interactive.rkt
;;   - test-cli-markdown.rkt

(require rackunit
         rackunit/text-ui
         "../util/message/protocol-types.rkt"
         "../util/error/error-classify.rkt"
         "../interfaces/cli.rkt")

(define/provide-test-suite
 test-cli
 ;; ═══════════════════════════════════════════
 ;; parse-cli-args — pure function
 ;; ═══════════════════════════════════════════
 (test-suite "parse-cli-args"

   (test-case "no args → chat command, interactive mode"
     (define cfg (parse-cli-args #()))
     (check-equal? (cli-config-command cfg) 'chat)
     (check-equal? (cli-config-mode cfg) 'interactive)
     (check-false (cli-config-session-id cfg))
     (check-false (cli-config-prompt cfg)))

   (test-case "positional prompt → prompt command, single mode"
     (define cfg (parse-cli-args #("Explain recursion")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-mode cfg) 'single)
     (check-equal? (cli-config-prompt cfg) "Explain recursion"))

   (test-case "--session <id> → resume command"
     (define cfg (parse-cli-args #("--session" "abc123")))
     (check-equal? (cli-config-command cfg) 'resume)
     (check-equal? (cli-config-session-id cfg) "abc123")
     (check-equal? (cli-config-mode cfg) 'interactive))

   (test-case "--help → help command"
     (define cfg (parse-cli-args #("--help")))
     (check-equal? (cli-config-command cfg) 'help))

   (test-case "-h → help command"
     (define cfg (parse-cli-args #("-h")))
     (check-equal? (cli-config-command cfg) 'help))

   (test-case "--version → version command"
     (define cfg (parse-cli-args #("--version")))
     (check-equal? (cli-config-command cfg) 'version))

   (test-case "--json → json mode"
     (define cfg (parse-cli-args #("--json")))
     (check-equal? (cli-config-mode cfg) 'json))

   (test-case "--json with prompt → prompt command, json mode"
     (define cfg (parse-cli-args #("--json" "hello")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-mode cfg) 'json)
     (check-equal? (cli-config-prompt cfg) "hello"))

   (test-case "--rpc → rpc mode"
     (define cfg (parse-cli-args #("--rpc")))
     (check-equal? (cli-config-mode cfg) 'rpc))

   (test-case "--model <name>"
     (define cfg (parse-cli-args #("--model" "gpt-4")))
     (check-equal? (cli-config-model cfg) "gpt-4"))

   (test-case "--project-dir <path>"
     (define cfg (parse-cli-args #("--project-dir" "/tmp/myproject")))
     (check-equal? (cli-config-project-dir cfg) "/tmp/myproject"))

   (test-case "--config <path>"
     (define cfg (parse-cli-args #("--config" "/etc/q/config.yaml")))
     (check-equal? (cli-config-config-path cfg) "/etc/q/config.yaml"))

   (test-case "--verbose"
     (define cfg (parse-cli-args #("--verbose")))
     (check-true (cli-config-verbose? cfg)))

   (test-case "-v"
     (define cfg (parse-cli-args #("-v")))
     (check-true (cli-config-verbose? cfg)))

   (test-case "--max-turns 5"
     (define cfg (parse-cli-args #("--max-turns" "5")))
     (check-equal? (cli-config-max-turns cfg) 5))

   (test-case "--no-tools"
     (define cfg (parse-cli-args #("--no-tools")))
     (check-true (cli-config-no-tools? cfg)))

   (test-case "--tool (single)"
     (define cfg (parse-cli-args #("--tool" "bash")))
     (check-equal? (cli-config-tools cfg) '("bash")))

   (test-case "--tool (multiple)"
     (define cfg (parse-cli-args #("--tool" "bash" "--tool" "read" "--tool" "write")))
     (check-equal? (cli-config-tools cfg) '("bash" "read" "write")))

   (test-case "combined: --session + --model + --verbose"
     (define cfg (parse-cli-args #("--session" "s1" "--model" "claude-3" "--verbose")))
     (check-equal? (cli-config-command cfg) 'resume)
     (check-equal? (cli-config-session-id cfg) "s1")
     (check-equal? (cli-config-model cfg) "claude-3")
     (check-true (cli-config-verbose? cfg)))

   (test-case "defaults: max-turns=10, no-tools=#f, tools='()"
     (define cfg (parse-cli-args #()))
     (check-equal? (cli-config-max-turns cfg) 10)
     (check-false (cli-config-no-tools? cfg))
     (check-equal? (cli-config-tools cfg) '())
     (check-false (cli-config-model cfg))
     (check-false (cli-config-project-dir cfg))
     (check-false (cli-config-config-path cfg))
     (check-false (cli-config-verbose? cfg)))

   (test-case "flags before prompt"
     (define cfg (parse-cli-args #("--model" "gpt-4" "--max-turns" "3" "do stuff")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-model cfg) "gpt-4")
     (check-equal? (cli-config-max-turns cfg) 3)
     (check-equal? (cli-config-prompt cfg) "do stuff"))

   (test-case "unknown flag → help command with error"
     (define cfg (parse-cli-args #("--bogus")))
     (check-equal? (cli-config-command cfg) 'help))

   (test-case "--max-turns non-numeric → help command"
     (define cfg (parse-cli-args #("--max-turns" "abc")))
     (check-equal? (cli-config-command cfg) 'help)))
 ;; ═══════════════════════════════════════════
 ;; cli-config->runtime-config — pure function
 ;; ═══════════════════════════════════════════
 (test-suite "cli-config->runtime-config"

   (test-case "basic conversion"
     (define cfg
       (cli-config 'prompt
                   #f
                   "hello"
                   "gpt-4"
                   'single
                   "/tmp/proj"
                   "/tmp/config.yaml"
                   #f
                   5
                   #t
                   '()
                   #f
                   #f
                   '()
                   #f
                   #f
                   #f
                   #f))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'model) "gpt-4")
     (check-equal? (hash-ref rt 'max-iterations) 5)
     (check-equal? (hash-ref rt 'project-dir) "/tmp/proj")
     (check-equal? (hash-ref rt 'config-path) "/tmp/config.yaml")
     (check-true (hash-ref rt 'no-tools?))
     (check-equal? (hash-ref rt 'tools) '()))

   (test-case "defaults filled in"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'max-iterations) 10)
     (check-false (hash-ref rt 'no-tools?))
     (check-equal? (hash-ref rt 'tools) '()))

   (test-case "session-id present for resume"
     (define cfg
       (cli-config 'resume "sess-123" #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'session-id) "sess-123")))
 ;; ═══════════════════════════════════════════
 ;; --session-dir flag
 ;; ═══════════════════════════════════════════
 (test-suite "--session-dir flag"

   (test-case "--session-dir sets session-dir field"
     (define cfg (parse-cli-args #("--session-dir" "/tmp/my-sessions")))
     (check-equal? (cli-config-session-dir cfg) "/tmp/my-sessions"))

   (test-case "--session-dir propagated to runtime config"
     (define cfg (parse-cli-args #("--session-dir" "/tmp/s")))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'session-dir) "/tmp/s"))

   (test-case "session-dir defaults to #f"
     (define cfg (parse-cli-args #()))
     (check-false (cli-config-session-dir cfg)))

   (test-case "--session-dir without value shows help"
     (define cfg (parse-cli-args #("--session-dir")))
     (check-equal? (cli-config-command cfg) 'help)))
 ;; ═══════════════════════════════════════════
 ;; print-usage — I/O test
 ;; ═══════════════════════════════════════════
 (test-suite "print-usage"

   (test-case "output contains key strings"
     (define port (open-output-string))
     (print-usage port)
     (define output (get-output-string port))
     (check-not-false (regexp-match? #rx"Usage" output))
     (check-not-false (regexp-match? #rx"--model" output))
     (check-not-false (regexp-match? #rx"--session" output))
     (check-not-false (regexp-match? #rx"--json" output))
     (check-not-false (regexp-match? #rx"--rpc" output))
     (check-not-false (regexp-match? #rx"--verbose" output))
     (check-not-false (regexp-match? #rx"--help" output))))
 ;; ═══════════════════════════════════════════
 ;; print-version — I/O test
 ;; ═══════════════════════════════════════════
 (test-suite "print-version"

   (test-case "output contains 'q' and version-like text"
     (define port (open-output-string))
     (print-version port)
     (define output (get-output-string port))
     (check-not-false (regexp-match? #rx"q" output))))
 ;; ═══════════════════════════════════════════
 ;; run-cli-single — discards session-fn return values
 ;; ═══════════════════════════════════════════
 (test-suite "run-cli-single"

   (test-case "returns void regardless of session-fn return value"
     (define cfg (cli-config 'prompt #f "test" #f 'single #f #f #f 10 #f '() #f #f '() #f #f #f #f))
     (define result
       (run-cli-single cfg
                       #:session-fn (lambda (prompt) (values 'session-struct 'loop-result-struct))
                       #:out (open-output-string)))
     (check-equal? result (void) "run-cli-single returns void when prompt is provided")))
 ;; ═══════════════════════════════════════════
 ;; Issue #143: 'init' subcommand parsing
 ;; ═══════════════════════════════════════════
 (test-suite "Issue #143: 'init' subcommand"

   (test-case "init subcommand → init command"
     (define cfg (parse-cli-args #("init")))
     (check-equal? (cli-config-command cfg) 'init))

   (test-case "init appears in usage"
     (define port (open-output-string))
     (print-usage port)
     (define output (get-output-string port))
     (check-true (string-contains? output "init") "usage should mention init command")))
 ;; ═══════════════════════════════════════════
 ;; Issue #143: run-init-wizard
 ;; ═══════════════════════════════════════════
 (test-suite "Issue #143: run-init-wizard"

   (test-case "wizard creates config with valid provider"
     (define tmp (make-temporary-file "q-cli-wizard-~a" 'directory))
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (check-not-exn (lambda ()
                         (run-init-wizard #:in (open-input-string "openai\nsk-real-key-not-test\n\n")
                                          #:out (open-output-string)
                                          #:config-dir tmp))))
      (lambda () (delete-directory/files tmp #:must-exist? #f))))

   (test-case "wizard rejects invalid provider"
     (define tmp (make-temporary-file "q-cli-wizard-~a" 'directory))
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (define out (open-output-string))
        (run-init-wizard #:in (open-input-string "y\ninvalid\n") #:out out #:config-dir tmp)
        (define output (get-output-string out))
        (check-true (string-contains? output "Invalid provider") "should reject invalid provider"))
      (lambda () (delete-directory/files tmp #:must-exist? #f))))))
;; ═══════════════════════════════════════════
;; Issue #160: 'sessions' subcommand parsing
;; ═══════════════════════════════════════════
(define suite-160
  (test-suite "Issue #160: 'sessions' subcommand"
    (test-case "sessions list subcommand"
      (define cfg (parse-cli-args #("sessions" "list")))
      (check-equal? (cli-config-command cfg) 'sessions)
      (check-equal? (cli-config-sessions-subcommand cfg) 'list))
    (test-case "sessions info subcommand"
      (define cfg (parse-cli-args #("sessions" "info" "abc123")))
      (check-equal? (cli-config-command cfg) 'sessions)
      (check-equal? (cli-config-sessions-subcommand cfg) 'info)
      (check-equal? (cli-config-sessions-args cfg) '("abc123")))
    (test-case "sessions delete subcommand"
      (define cfg (parse-cli-args #("sessions" "delete" "def456")))
      (check-equal? (cli-config-command cfg) 'sessions)
      (check-equal? (cli-config-sessions-subcommand cfg) 'delete)
      (check-equal? (cli-config-sessions-args cfg) '("def456")))
    (test-case "sessions without subcommand"
      (define cfg (parse-cli-args #("sessions")))
      (check-equal? (cli-config-command cfg) 'help))
    (test-case "sessions with invalid subcommand"
      (define cfg (parse-cli-args #("sessions" "bogus")))
      (check-equal? (cli-config-command cfg) 'help))
    (test-case "sessions appears in usage"
      (define port (open-output-string))
      (print-usage port)
      (define output (get-output-string port))
      (check-true (string-contains? output "sessions") "usage should mention sessions"))))
;; ═══════════════════════════════════════════
;; Issue #162: /sessions slash command
;; ═══════════════════════════════════════════
(define suite-162
  (test-suite "Issue #162: /sessions slash command"
    (test-case "/sessions"
      (check-equal? (parse-slash-command "/sessions") '(sessions)))
    (test-case "/sessions list"
      (check-equal? (parse-slash-command "/sessions list") '(sessions list)))
    (test-case "/sessions info abc123"
      (check-equal? (parse-slash-command "/sessions info abc123") '(sessions info "abc123")))
    (test-case "/sessions delete abc123"
      (check-equal? (parse-slash-command "/sessions delete abc123") '(sessions delete "abc123")))
    (test-case "/sessions info without id"
      (check-equal? (parse-slash-command "/sessions info") '(sessions info)))
    (test-case "/sessions delete without id"
      (check-equal? (parse-slash-command "/sessions delete") '(sessions delete)))
    (test-case "/sessions with unknown subcommand"
      (check-equal? (parse-slash-command "/sessions foo") '(sessions)))))
;; ═══════════════════════════════════════════
;; Issue #165: classify-error
;; ═══════════════════════════════════════════
(define suite-165
  (test-suite "Issue #165: classify-error"
    (test-case "hash-ref"
      (define e (exn:fail "hash-ref: contract violation" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "contract")))
    (test-case "read-json"
      (define e (exn:fail "read-json: expected" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "JSON")))
    (test-case "connection refused"
      (define e (exn:fail "connection refused" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "connect")))
    (test-case "SSL"
      (define e
        (exn:fail "SSL handshake failed: certificate verify failed" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "SSL")))
    (test-case "file not found"
      (define e (exn:fail "file not found: /tmp/missing.rkt" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "file")))
    (test-case "permission denied"
      (define e (exn:fail "permission denied: /root/secret" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "Permission")))
    (test-case "unauthorized/401"
      (define e (exn:fail "HTTP 401 unauthorized" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "authentication")))
    (test-case "API key"
      (define e (exn:fail "API key is invalid" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "authentication")))
    (test-case "403"
      (define e (exn:fail "HTTP 403 forbidden" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "authentication")))
    (test-case "rate limit"
      (define e (exn:fail "rate.limit exceeded" (current-continuation-marks)))
      (define result (classify-error e))
      (check-not-false result)
      (check-true (string-contains? (cadr result) "rate limit")))
    (test-case "unknown error"
      (define e (exn:fail "something completely unexpected" (current-continuation-marks)))
      (check-false (classify-error e)))
    (test-case "classified result has suggestions"
      (define e (exn:fail "hash-ref: contract violation" (current-continuation-marks)))
      (define result (classify-error e))
      (check-true (and (pair? result) (list? (cdr result)) (>= (length (cdr result)) 1))))))
;; ═══════════════════════════════════════════
;; Issue #149: format-classified-error
;; ═══════════════════════════════════════════
(define suite-149
  (test-suite "Issue #149: format-classified-error"
    (test-case "classified error includes message and suggestions"
      (define e (exn:fail "hash-ref: contract violation" (current-continuation-marks)))
      (define output (format-classified-error e #f))
      (check-true (string-contains? output "Error: hash-ref:"))
      (check-true (string-contains? output "missing"))
      (check-true (string-contains? output "\u2192")))
    (test-case "unclassified error shows raw message only"
      (define e (exn:fail "something weird happened" (current-continuation-marks)))
      (define output (format-classified-error e #f))
      (check-true (string-contains? output "Error: something weird happened"))
      (check-false (string-contains? output "\u2192")))
    (test-case "verbose mode includes stack trace"
      (define e (exn:fail "test error" (current-continuation-marks)))
      (define output (format-classified-error e #t))
      (check-true (string-contains? output "Stack trace:")))
    (test-case "non-verbose mode omits stack trace"
      (define e (exn:fail "test error" (current-continuation-marks)))
      (define output (format-classified-error e #f))
      (check-false (string-contains? output "Stack trace:")))))
;; ═══════════════════════════════════════════
;; Issue #166: --verbose flag
;; ═══════════════════════════════════════════
(define suite-166
  (test-suite "Issue #166: --verbose flag"
    (test-case "verbose error in interactive mode shows classified error"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #t 10 #f '() #f #f '() #f #f #f #f))
      (define in (open-input-string "bad\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p)
                                          (when (equal? p "bad")
                                            (error "hash-ref: contract violation")))
                           #:in in
                           #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "Error:"))
      (check-true (string-contains? output "missing"))
      (check-true (string-contains? output "Stack trace:")))
    (test-case "non-verbose error in interactive mode omits stack trace"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f #f #f))
      (define in (open-input-string "bad\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p)
                                          (when (equal? p "bad")
                                            (error "hash-ref: contract violation")))
                           #:in in
                           #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "Error:"))
      (check-true (string-contains? output "missing"))
      (check-false (string-contains? output "Stack trace:")))
    (test-case "verbose error in single-shot mode outputs to stderr"
      (define cfg (cli-config 'prompt #f "test" #f 'single #f #f #t 10 #f '() #f #f '() #f #f #f #f))
      (define err (open-output-string))
      (parameterize ([current-error-port err])
        (run-cli-single cfg #:session-fn (lambda (prompt) (error "hash-ref: missing key"))))
      (define output (get-output-string err))
      (check-true (string-contains? output "Error:"))
      (check-true (string-contains? output "Stack trace:")))))

;; Run all suites
(run-tests test-cli)
(run-tests suite-160)
(run-tests suite-162)
(run-tests suite-165)
(run-tests suite-149)
(run-tests suite-166)
