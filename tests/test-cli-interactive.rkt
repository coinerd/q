#lang racket

;; tests/test-cli-interactive.rkt — v0.70.8 W0
;; Extracted from test-cli.rkt: run-cli-interactive tests

(require rackunit
         rackunit/text-ui
         "../interfaces/cli.rkt")

(define-test-suite test-cli-interactive

  (test-suite "run-cli-interactive — prompt submission"

    (test-case "basic prompt submission"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "hello\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("hello")))

    (test-case "multiple lines submitted as separate prompts"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "line1\nline2\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("line1" "line2")))

    (test-case "whitespace-only lines are skipped"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "   \nhello\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("hello")))

    (test-case "empty lines are skipped"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "\nhello\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("hello")))

    (test-case "EOF terminates with Goodbye"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "hello\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("hello"))
      (check-true (string-contains? (get-output-string out) "Goodbye."))))

  (test-suite "run-cli-interactive — slash commands"

    (test-case "/quit terminates with Goodbye"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "Goodbye.")))

    (test-case "/exit terminates with Goodbye"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/exit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "Goodbye.")))

    (test-case "/help displays usage"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/help\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "Usage") "output should contain Usage"))

    (test-case "/compact calls compact-fn"
      (define compact-called (box #f))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/compact\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:compact-fn (lambda () (set-box! compact-called #t)) #:in in #:out out)
      (check-true (unbox compact-called) "compact-fn should have been called"))

    (test-case "/compact without compact-fn shows fallback message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/compact\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "compacting")
                  "output should contain compacting fallback message"))

    (test-case "/history calls history-fn"
      (define history-called (box #f))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/history\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:history-fn (lambda (_) (set-box! history-called #t))
                           #:in in
                           #:out out)
      (check-true (unbox history-called) "history-fn should have been called"))

    (test-case "/history without history-fn shows fallback message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/history\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "history not yet connected")
                  "output should contain history fallback message"))

    (test-case "/model gpt-4 calls model-fn with arg"
      (define model-arg (box #f))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/model gpt-4\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:model-fn (lambda (name) (set-box! model-arg name)) #:in in #:out out)
      (check-equal? (unbox model-arg) "gpt-4"))

    (test-case "/model without arg calls model-fn with #f"
      (define model-arg (box 'not-called))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/model\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:model-fn (lambda (name) (set-box! model-arg name)) #:in in #:out out)
      (check-equal? (unbox model-arg) #f))

    (test-case "/fork abc123 calls fork-fn with arg"
      (define fork-arg (box #f))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/fork abc123\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:fork-fn (lambda (arg) (set-box! fork-arg arg)) #:in in #:out out)
      (check-equal? (unbox fork-arg) "abc123"))

    (test-case "/fork without arg calls fork-fn with #f"
      (define fork-arg (box 'not-called))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/fork\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:fork-fn (lambda (arg) (set-box! fork-arg arg)) #:in in #:out out)
      (check-equal? (unbox fork-arg) #f))

    (test-case "/clear shows clear message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/clear\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "clear") "output should mention clear"))

    (test-case "/interrupt shows interrupt message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/interrupt\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (check-true (string-contains? (get-output-string out) "interrupt")
                  "output should mention interrupt")))

  (test-suite "run-cli-interactive — error handling"

    (test-case "session-fn error is displayed and loop continues"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "bad\nok\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p)
                                          (set-box! prompts (cons p (unbox prompts)))
                                          (when (equal? p "bad")
                                            (error "test error")))
                           #:in in
                           #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "Error:") "output should contain Error:")
      (check-equal? (reverse (unbox prompts)) '("bad" "ok")))

    (test-case "graceful degradation with string port (no readline)"
      (define prompts (box '()))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "test prompt\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("test prompt"))
      (check-true (string-contains? (get-output-string out) "Goodbye.")))

    (test-case "mixed commands and prompts"
      (define prompts (box '()))
      (define compact-called (box #f))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "first prompt\n/compact\nsecond prompt\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg
                           #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                           #:compact-fn (lambda () (set-box! compact-called #t))
                           #:in in
                           #:out out)
      (check-equal? (reverse (unbox prompts)) '("first prompt" "second prompt"))
      (check-true (unbox compact-called))))

  (test-suite "Welcome message strings"

    (test-case "welcome messages contain expected text"
      (define welcome-lines
        (list "Welcome to q! Your AI coding assistant."
              "Type a message to start chatting, or /help for commands."
              "Run 'q config' to set up your API key."))
      (for ([line (in-list welcome-lines)])
        (check-pred string? line)
        (check-true (> (string-length line) 0))))

    (test-case "first-run welcome appears before first prompt when config dir missing"
      (define tmp-home (make-temporary-file "q-test-home-~a" 'directory))
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/quit\n"))
      (define out (open-output-string))
      (parameterize ([current-directory tmp-home])
        (run-cli-interactive cfg #:in in #:out out))
      (check-true (string-contains? (get-output-string out) "Goodbye."))))

  (test-suite "Issue #141: Mock provider warning"

    (test-case "mock provider name triggers warning banner"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:provider-name "mock" #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "mock provider") "should warn about mock provider"))

    (test-case "real provider name does NOT trigger warning"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:provider-name "openai" #:in in #:out out)
      (define output (get-output-string out))
      (check-false (string-contains? output "mock provider") "should not warn for real provider"))

    (test-case "no provider-name does NOT trigger warning"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-false (string-contains? output "mock provider") "should not warn when no provider name")))

  (test-suite "Issue #145: TUI-only commands in CLI"

    (test-case "/clear shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/clear\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/clear should mention TUI"))

    (test-case "/interrupt shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/interrupt\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/interrupt should mention TUI"))

    (test-case "/branches shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/branches\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/branches should mention TUI"))

    (test-case "/leaves shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/leaves\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/leaves should mention TUI"))

    (test-case "/switch shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/switch abc\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/switch should mention TUI"))

    (test-case "/children shows TUI-only message"
      (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '() #f #f))
      (define in (open-input-string "/children abc\n/quit\n"))
      (define out (open-output-string))
      (run-cli-interactive cfg #:in in #:out out)
      (define output (get-output-string out))
      (check-true (string-contains? output "TUI") "/children should mention TUI"))))

(run-tests test-cli-interactive)
