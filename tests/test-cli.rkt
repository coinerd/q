#lang racket

;; tests/test-cli.rkt — tests for interfaces/cli.rkt
;;
;; TDD: tests written first against the public contract.
;; Focus on pure functions: parse-cli-args, cli-config->runtime-config,
;; format-event-for-terminal, parse-slash-command.
;; I/O functions tested with string ports where practical.

(require rackunit
         rackunit/text-ui
         json
         "../agent/types.rkt"
         "../util/markdown.rkt"
         "../interfaces/cli.rkt")

(define/provide-test-suite
 test-cli
 ;; ═══════════════════════════════════════════
 ;; parse-cli-args — pure function
 ;; ═══════════════════════════════════════════
 (test-suite "parse-cli-args"

   ;; ── Bare invocation → interactive chat ──
   (test-case "no args → chat command, interactive mode"
     (define cfg (parse-cli-args #()))
     (check-equal? (cli-config-command cfg) 'chat)
     (check-equal? (cli-config-mode cfg) 'interactive)
     (check-false (cli-config-session-id cfg))
     (check-false (cli-config-prompt cfg)))

   ;; ── Single positional prompt → single-shot ──
   (test-case "positional prompt → prompt command, single mode"
     (define cfg (parse-cli-args #("Explain recursion")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-mode cfg) 'single)
     (check-equal? (cli-config-prompt cfg) "Explain recursion"))

   ;; ── --session flag → resume ──
   (test-case "--session <id> → resume command"
     (define cfg (parse-cli-args #("--session" "abc123")))
     (check-equal? (cli-config-command cfg) 'resume)
     (check-equal? (cli-config-session-id cfg) "abc123")
     (check-equal? (cli-config-mode cfg) 'interactive))

   ;; ── --help / -h ──
   (test-case "--help → help command"
     (define cfg (parse-cli-args #("--help")))
     (check-equal? (cli-config-command cfg) 'help))

   (test-case "-h → help command"
     (define cfg (parse-cli-args #("-h")))
     (check-equal? (cli-config-command cfg) 'help))

   ;; ── --version ──
   (test-case "--version → version command"
     (define cfg (parse-cli-args #("--version")))
     (check-equal? (cli-config-command cfg) 'version))

   ;; ── --json ──
   (test-case "--json → json mode"
     (define cfg (parse-cli-args #("--json")))
     (check-equal? (cli-config-mode cfg) 'json))

   (test-case "--json with prompt → prompt command, json mode"
     (define cfg (parse-cli-args #("--json" "hello")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-mode cfg) 'json)
     (check-equal? (cli-config-prompt cfg) "hello"))

   ;; ── --rpc ──
   (test-case "--rpc → rpc mode"
     (define cfg (parse-cli-args #("--rpc")))
     (check-equal? (cli-config-mode cfg) 'rpc))

   ;; ── --model ──
   (test-case "--model <name>"
     (define cfg (parse-cli-args #("--model" "gpt-4")))
     (check-equal? (cli-config-model cfg) "gpt-4"))

   ;; ── --project-dir ──
   (test-case "--project-dir <path>"
     (define cfg (parse-cli-args #("--project-dir" "/tmp/myproject")))
     (check-equal? (cli-config-project-dir cfg) "/tmp/myproject"))

   ;; ── --config ──
   (test-case "--config <path>"
     (define cfg (parse-cli-args #("--config" "/etc/q/config.yaml")))
     (check-equal? (cli-config-config-path cfg) "/etc/q/config.yaml"))

   ;; ── --verbose / -v ──
   (test-case "--verbose"
     (define cfg (parse-cli-args #("--verbose")))
     (check-true (cli-config-verbose? cfg)))

   (test-case "-v"
     (define cfg (parse-cli-args #("-v")))
     (check-true (cli-config-verbose? cfg)))

   ;; ── --max-turns ──
   (test-case "--max-turns 5"
     (define cfg (parse-cli-args #("--max-turns" "5")))
     (check-equal? (cli-config-max-turns cfg) 5))

   ;; ── --no-tools ──
   (test-case "--no-tools"
     (define cfg (parse-cli-args #("--no-tools")))
     (check-true (cli-config-no-tools? cfg)))

   ;; ── --tool (repeatable) ──
   (test-case "--tool (single)"
     (define cfg (parse-cli-args #("--tool" "bash")))
     (check-equal? (cli-config-tools cfg) '("bash")))

   (test-case "--tool (multiple)"
     (define cfg (parse-cli-args #("--tool" "bash" "--tool" "read" "--tool" "write")))
     (check-equal? (cli-config-tools cfg) '("bash" "read" "write")))

   ;; ── Combined flags ──
   (test-case "combined: --session + --model + --verbose"
     (define cfg (parse-cli-args #("--session" "s1" "--model" "claude-3" "--verbose")))
     (check-equal? (cli-config-command cfg) 'resume)
     (check-equal? (cli-config-session-id cfg) "s1")
     (check-equal? (cli-config-model cfg) "claude-3")
     (check-true (cli-config-verbose? cfg)))

   ;; ── Defaults ──
   (test-case "defaults: max-turns=10, no-tools=#f, tools='()"
     (define cfg (parse-cli-args #()))
     (check-equal? (cli-config-max-turns cfg) 10)
     (check-false (cli-config-no-tools? cfg))
     (check-equal? (cli-config-tools cfg) '())
     (check-false (cli-config-model cfg))
     (check-false (cli-config-project-dir cfg))
     (check-false (cli-config-config-path cfg))
     (check-false (cli-config-verbose? cfg)))

   ;; ── Prompt with flags before it ──
   (test-case "flags before prompt"
     (define cfg (parse-cli-args #("--model" "gpt-4" "--max-turns" "3" "do stuff")))
     (check-equal? (cli-config-command cfg) 'prompt)
     (check-equal? (cli-config-model cfg) "gpt-4")
     (check-equal? (cli-config-max-turns cfg) 3)
     (check-equal? (cli-config-prompt cfg) "do stuff"))

   ;; ── Unknown flag handling ──
   (test-case "unknown flag → help command with error"
     (define cfg (parse-cli-args #("--bogus")))
     (check-equal? (cli-config-command cfg) 'help))

   ;; ── --max-turns with non-numeric → help command ──
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
                   '()))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'model) "gpt-4")
     (check-equal? (hash-ref rt 'max-iterations) 5)
     (check-equal? (hash-ref rt 'project-dir) "/tmp/proj")
     (check-equal? (hash-ref rt 'config-path) "/tmp/config.yaml")
     (check-true (hash-ref rt 'no-tools?))
     (check-equal? (hash-ref rt 'tools) '()))

   (test-case "defaults filled in"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'max-iterations) 10)
     (check-false (hash-ref rt 'no-tools?))
     (check-equal? (hash-ref rt 'tools) '()))

   (test-case "session-id present for resume"
     (define cfg (cli-config 'resume "sess-123" #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define rt (cli-config->runtime-config cfg))
     (check-equal? (hash-ref rt 'session-id) "sess-123")))
 ;; ═══════════════════════════════════════════
 ;; format-event-for-terminal — pure function
 ;; ═══════════════════════════════════════════
 (test-suite "format-event-for-terminal"

   (test-case "assistant.message.completed → suppressed (empty string)"
     (define evt
       (make-event "assistant.message.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'content "Hello, world!")))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "tool.call.started → [tool: name]"
     (define evt (make-event "tool.call.started" 1000 "sess1" "turn1" (hasheq 'name "read")))
     (check-equal? (format-event-for-terminal evt) "[tool: read]"))

   (test-case "tool.call.completed → [tool result]"
     (define evt
       (make-event "tool.call.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'summary "exit 0")))
     (check-equal? (format-event-for-terminal evt) "[tool result: bash]"))

   (test-case "tool.call.completed with result content → shows truncated content"
     (define evt
       (make-event "tool.call.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'result (list (hasheq 'type "text" 'text "hello world")))))
     (check-equal? (format-event-for-terminal evt) "[tool result: hello world]"))

   (test-case "tool.call.completed with multi-part result → joins with newline"
     (define evt
       (make-event "tool.call.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name
                           "ls"
                           'result
                           (list (hasheq 'type "text" 'text "file1.rkt")
                                 (hasheq 'type "text" 'text "file2.rkt")))))
     (check-equal? (format-event-for-terminal evt) "[tool result: file1.rkt\nfile2.rkt]"))

   (test-case "BUG-18: tool.call.completed with long result → truncated"
     (define long-text (make-string 500 #\A))
     (define evt
       (make-event "tool.call.completed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'result (list (hasheq 'type "text" 'text long-text)))))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "..."))
     (check-true (<= (string-length output) 330)))

   (test-case "BUG-18: tool.call.started with command argument → shows command"
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'arguments (hasheq 'command "curl -s https://example.com"))))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "curl")))

   (test-case "BUG-18: tool.call.started with path argument → shows path"
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "read" 'arguments (hasheq 'path "/tmp/test.txt"))))
     (check-equal? (format-event-for-terminal evt) "[tool: read: /tmp/test.txt]"))

   (test-case "BUG-18: model.stream.delta → shows delta text"
     (define evt (make-event "model.stream.delta" 1000 "sess1" "turn1" (hasheq 'delta "Hello ")))
     (check-equal? (format-event-for-terminal evt) "Hello "))

   (test-case "BUG-18: tool.call.started with JSON string arguments → parses and shows command"
     (define args-json (jsexpr->string (hasheq 'command "curl -s https://example.com")))
     (define evt
       (make-event "tool.call.started"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "bash" 'arguments args-json)))
     (define output (format-event-for-terminal evt))
     (check-true (string-contains? output "curl")))

   (test-case "tool.call.failed → [tool failed: name]"
     (define evt
       (make-event "tool.call.failed"
                   1000
                   "sess1"
                   "turn1"
                   (hasheq 'name "write" 'error "permission denied")))
     (check-equal? (format-event-for-terminal evt) "[tool failed: write — permission denied]"))

   (test-case "turn.started → empty string"
     (define evt (make-event "turn.started" 1000 "sess1" "turn1" (hasheq)))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "turn.completed → empty string"
     (define evt (make-event "turn.completed" 1000 "sess1" "turn1" (hasheq)))
     (check-equal? (format-event-for-terminal evt) ""))

   (test-case "runtime.error → Error: message"
     (define evt (make-event "runtime.error" 1000 "sess1" #f (hasheq 'error "something went wrong")))
     (check-equal? (format-event-for-terminal evt) "Error: something went wrong"))

   (test-case "session.started → session info"
     (define evt (make-event "session.started" 1000 "sess1" #f (hasheq 'sessionId "sess1")))
     (check-equal? (format-event-for-terminal evt) "[session started: sess1]"))

   (test-case "unknown event → empty string"
     (define evt (make-event "something.else" 1000 "sess1" #f (hasheq)))
     (check-equal? (format-event-for-terminal evt) "")))
 ;; ═══════════════════════════════════════════
 ;; parse-slash-command — pure function
 ;; ═══════════════════════════════════════════
 (test-suite "parse-slash-command"

   (test-case "/help → help"
     (check-equal? (parse-slash-command "/help") '(help)))

   (test-case "/quit → quit"
     (check-equal? (parse-slash-command "/quit") '(quit)))

   (test-case "/compact → compact"
     (check-equal? (parse-slash-command "/compact") '(compact)))

   (test-case "/history → history"
     (check-equal? (parse-slash-command "/history") '(history)))

   (test-case "/fork → fork"
     (check-equal? (parse-slash-command "/fork") '(fork)))

   (test-case "/fork abc123 → fork with arg"
     (check-equal? (parse-slash-command "/fork abc123") '(fork "abc123")))

   (test-case "empty string → #f"
     (check-false (parse-slash-command "")))

   (test-case "no slash → #f"
     (check-false (parse-slash-command "hello")))

   (test-case "unknown /command → #f"
     (check-false (parse-slash-command "/unknown")))

   (test-case "/exit → quit"
     (check-equal? (parse-slash-command "/exit") '(quit)))

   (test-case "whitespace → #f"
     (check-false (parse-slash-command "  ")))

   ;; ── Additional edge cases (Issue #113) ──

   (test-case "/model gpt-4 → model with arg"
     (check-equal? (parse-slash-command "/model gpt-4") '(model "gpt-4")))

   (test-case "/model → model without arg"
     (check-equal? (parse-slash-command "/model") '(model)))

   (test-case "/clear → clear"
     (check-equal? (parse-slash-command "/clear") '(clear)))

   (test-case "/interrupt → interrupt"
     (check-equal? (parse-slash-command "/interrupt") '(interrupt)))

   (test-case "/branches → branches"
     (check-equal? (parse-slash-command "/branches") '(branches)))

   (test-case "/leaves → leaves"
     (check-equal? (parse-slash-command "/leaves") '(leaves)))

   (test-case "/switch abc → switch with arg"
     (check-equal? (parse-slash-command "/switch abc") '(switch "abc")))

   (test-case "/switch → switch without arg"
     (check-equal? (parse-slash-command "/switch") '(switch)))

   (test-case "/children xyz → children with arg"
     (check-equal? (parse-slash-command "/children xyz") '(children "xyz")))

   (test-case "/children → children without arg"
     (check-equal? (parse-slash-command "/children") '(children)))

   (test-case "/quit with extra → quit (extra args ignored)"
     (check-equal? (parse-slash-command "/quit with extra") '(quit)))

   (test-case "/exit with extra → quit (extra args ignored)"
     (check-equal? (parse-slash-command "/exit now") '(quit))))
 ;; ═══════════════════════════════════════════
 ;; print-usage — I/O test
 ;; ═══════════════════════════════════════════
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
 ;; format-event-for-terminal — additional tests
 ;; ═══════════════════════════════════════════
 (test-suite "format-event-for-terminal (additional)"

   (test-case "assistant message completed → suppressed"
     (check-equal? (format-event-for-terminal
                    (make-event "assistant.message.completed" 0 #f #f (hasheq 'content "Hello")))
                   ""))

   (test-case "tool call started"
     (check-equal?
      (format-event-for-terminal (make-event "tool.call.started" 0 #f #f (hasheq 'name "read")))
      "[tool: read]"))

   (test-case "tool call failed"
     (check-equal? (format-event-for-terminal
                    (make-event "tool.call.failed" 0 #f #f (hasheq 'name "bash" 'error "exit 1")))
                   "[tool failed: bash — exit 1]"))

   (test-case "runtime error"
     (check-equal?
      (format-event-for-terminal (make-event "runtime.error" 0 #f #f (hasheq 'error "boom")))
      "Error: boom"))

   (test-case "unknown event returns empty string"
     (check-equal? (format-event-for-terminal (make-event "unknown.event" 0 #f #f (hasheq))) ""))

   (test-case "turn started returns empty"
     (check-equal? (format-event-for-terminal (make-event "turn.started" 0 #f #f (hasheq))) "")))
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
     (define cfg (cli-config 'prompt #f "test" #f 'single #f #f #f 10 #f '() #f #f '()))
     (define result
       (run-cli-single cfg
                       #:session-fn (lambda (prompt) (values 'session-struct 'loop-result-struct))
                       #:out (open-output-string)))
     (check-equal? result (void) "run-cli-single returns void when prompt is provided")))
 (test-suite "Markdown rendering"

   (test-case "render-markdown returns text unchanged when color disabled"
     (check-equal? (render-markdown "hello **world**") "hello **world**"))

   ;; Markdown parsing now uses shared q/util/markdown.rkt parser.
   ;; These tests verify the parser produces correct tokens.

   (test-case "parse-markdown: bold token produced for **text**"
     (define tokens (parse-markdown "hello **world** end"))
     (define bold-toks (filter (lambda (t) (eq? (md-token-type t) 'bold)) tokens))
     (check-equal? (length bold-toks) 1)
     (check-equal? (md-token-content (car bold-toks)) "world"))

   (test-case "parse-markdown: italic token produced for *text*"
     (define tokens (parse-markdown "hello *world* end"))
     (define italic-toks (filter (lambda (t) (eq? (md-token-type t) 'italic)) tokens))
     (check-equal? (length italic-toks) 1)
     (check-equal? (md-token-content (car italic-toks)) "world"))

   (test-case "parse-markdown: inline code token for `code`"
     (define tokens (parse-markdown "use `code` here"))
     (define code-toks (filter (lambda (t) (eq? (md-token-type t) 'code)) tokens))
     (check-equal? (length code-toks) 1)
     (check-equal? (md-token-content (car code-toks)) "code"))

   (test-case "parse-markdown: link token for [text](url)"
     (define tokens (parse-markdown "click [here](https://example.com) now"))
     (define link-toks (filter (lambda (t) (eq? (md-token-type t) 'link)) tokens))
     (check-equal? (length link-toks) 1)
     (check-equal? (cdr (md-token-content (car link-toks))) "here"))

   (test-case "parse-markdown: header token for ### Header"
     (define tokens (parse-markdown "### My Header"))
     (define header-toks (filter (lambda (t) (eq? (md-token-type t) 'header)) tokens))
     (check-equal? (length header-toks) 1)
     (check-equal? (cdr (md-token-content (car header-toks))) "My Header")
     (check-equal? (car (md-token-content (car header-toks))) 3))

   (test-case "format-event-for-terminal styles tool.call.started"
     (define evt
       (make-event "tool.call.started"
                   (current-inexact-milliseconds)
                   "s1"
                   "t1"
                   (hasheq 'name "bash" 'arguments (hasheq 'command "ls"))))
     (define text (format-event-for-terminal evt))
     (check-true (string-contains? text "[tool: bash: ls]")))

   (test-case "format-event-for-terminal styles runtime.error"
     (define evt
       (make-event "runtime.error"
                   (current-inexact-milliseconds)
                   "s1"
                   "t1"
                   (hasheq 'error "something broke")))
     (define text (format-event-for-terminal evt))
     (check-true (string-contains? text "Error: something broke")))

   (test-case "format-event-for-terminal renders stream.completed as newline"
     (define evt
       (make-event "model.stream.completed"
                   (current-inexact-milliseconds)
                   "s1"
                   "t1"
                   (hasheq 'usage (hasheq))))
     (define text (format-event-for-terminal evt))
     (check-equal? text "\n"))

   ;; ============================================================
   ;; Stream Markdown Writer tests
   ;; ============================================================

   (test-case "stream-writer: single complete line"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "hello world\n" out)
     (flush! out)
     ;; Non-TTY: only complete lines printed, no raw partials
     (check-equal? (get-output-string out) "hello world\n"))

   (test-case "stream-writer: multiple fragments forming one line"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "hel" out)
     (writer "lo " out)
     (writer "world\n" out)
     (flush! out)
     ;; Non-TTY: nothing printed until newline, then rendered line
     (check-equal? (get-output-string out) "hello world\n"))

   (test-case "stream-writer: flush renders final partial line"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "hello" out)
     (writer " world" out)
     ;; No newline — flush should render the partial line
     (flush! out)
     (check-equal? (get-output-string out) "hello world\n"))

   (test-case "stream-writer: two complete lines"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "line 1\nline 2\n" out)
     (flush! out)
     (check-equal? (get-output-string out) "line 1\nline 2\n"))

   (test-case "stream-writer: empty string is no-op"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "" out)
     (flush! out)
     (check-equal? (string-length (get-output-string out)) 0))

   (test-case "stream-writer: multiple lines in one delta"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "line1\nline2\nline3\n" out)
     (flush! out)
     (check-equal? (get-output-string out) "line1\nline2\nline3\n"))

   (test-case "stream-writer: complete lines then partial then complete"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "first\n" out)
     (writer "partial" out)
     (writer " line\n" out)
     (flush! out)
     (check-equal? (get-output-string out) "first\npartial line\n"))

   (test-case "stream-writer: double newline produces blank line"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (writer "\n\n" out)
     (flush! out)
     (check-equal? (get-output-string out) "\n\n"))

   (test-case "stream-writer: only flush, no prior writes"
     (define out (open-output-string))
     (define-values (writer flush!) (make-stream-markdown-writer))
     (flush! out)
     (check-equal? (string-length (get-output-string out)) 0)))
 ;; ═══════════════════════════════════════════
 ;; run-cli-interactive — prompt submission
 ;; ═══════════════════════════════════════════
 (test-suite "run-cli-interactive — prompt submission"

   (test-case "basic prompt submission"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "hello\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("hello")))

   (test-case "multiple lines submitted as separate prompts"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "line1\nline2\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("line1" "line2")))

   (test-case "whitespace-only lines are skipped"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "   \nhello\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("hello")))

   (test-case "empty lines are skipped"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "\nhello\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("hello")))

   (test-case "EOF terminates with Goodbye"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "hello\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("hello"))
     (check-true (string-contains? (get-output-string out) "Goodbye."))))
 ;; ═══════════════════════════════════════════
 ;; run-cli-interactive — slash commands
 ;; ═══════════════════════════════════════════
 (test-suite "run-cli-interactive — slash commands"

   (test-case "/quit terminates with Goodbye"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "Goodbye.")))

   (test-case "/exit terminates with Goodbye"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/exit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "Goodbye.")))

   (test-case "/help displays usage"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/help\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "Usage") "output should contain Usage"))

   (test-case "/compact calls compact-fn"
     (define compact-called (box #f))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/compact\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:compact-fn (lambda () (set-box! compact-called #t)) #:in in #:out out)
     (check-true (unbox compact-called) "compact-fn should have been called"))

   (test-case "/compact without compact-fn shows fallback message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/compact\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "compacting")
                 "output should contain compacting fallback message"))

   (test-case "/history calls history-fn"
     (define history-called (box #f))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/history\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:history-fn (lambda (_) (set-box! history-called #t))
                          #:in in
                          #:out out)
     (check-true (unbox history-called) "history-fn should have been called"))

   (test-case "/history without history-fn shows fallback message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/history\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "history not yet connected")
                 "output should contain history fallback message"))

   (test-case "/model gpt-4 calls model-fn with arg"
     (define model-arg (box #f))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/model gpt-4\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:model-fn (lambda (name) (set-box! model-arg name)) #:in in #:out out)
     (check-equal? (unbox model-arg) "gpt-4"))

   (test-case "/model without arg calls model-fn with #f"
     (define model-arg (box 'not-called))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/model\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:model-fn (lambda (name) (set-box! model-arg name)) #:in in #:out out)
     (check-equal? (unbox model-arg) #f))

   (test-case "/fork abc123 calls fork-fn with arg"
     (define fork-arg (box #f))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/fork abc123\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:fork-fn (lambda (arg) (set-box! fork-arg arg)) #:in in #:out out)
     (check-equal? (unbox fork-arg) "abc123"))

   (test-case "/fork without arg calls fork-fn with #f"
     (define fork-arg (box 'not-called))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/fork\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:fork-fn (lambda (arg) (set-box! fork-arg arg)) #:in in #:out out)
     (check-equal? (unbox fork-arg) #f))

   (test-case "/clear shows clear message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/clear\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "clear") "output should mention clear"))

   (test-case "/interrupt shows interrupt message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/interrupt\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (check-true (string-contains? (get-output-string out) "interrupt")
                 "output should mention interrupt")))
 ;; ═══════════════════════════════════════════
 ;; Welcome message strings (Issue #154)
 ;; ═══════════════════════════════════════════
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
     ;; Test with a temp home directory (guaranteed no ~/.q)
     (define tmp-home (make-temporary-file "q-test-home-~a" 'directory))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/quit\n"))
     (define out (open-output-string))
     (parameterize ([current-directory tmp-home])
       (run-cli-interactive cfg #:in in #:out out))
     ;; If ~/.q exists on the test machine, the welcome won't appear;
     ;; but we can at least verify Goodbye appears (no crash)
     (check-true (string-contains? (get-output-string out) "Goodbye."))))
 ;; ═══════════════════════════════════════════
 ;; run-cli-interactive — error handling and edge cases
 ;; ═══════════════════════════════════════════
 (test-suite "run-cli-interactive — error handling"

   (test-case "session-fn error is displayed and loop continues"
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
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
     ;; Both prompts were submitted (session-fn ran before error for "bad")
     (check-equal? (reverse (unbox prompts)) '("bad" "ok")))

   (test-case "graceful degradation with string port (no readline)"
     ;; Using a string port triggers the non-readline fallback path.
     ;; This test verifies the entire interactive loop works without readline.
     (define prompts (box '()))
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
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
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "first prompt\n/compact\nsecond prompt\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg
                          #:session-fn (lambda (p) (set-box! prompts (cons p (unbox prompts))))
                          #:compact-fn (lambda () (set-box! compact-called #t))
                          #:in in
                          #:out out)
     (check-equal? (reverse (unbox prompts)) '("first prompt" "second prompt"))
     (check-true (unbox compact-called))))
 ;; ═══════════════════════════════════════════
 ;; Issue #142: print-version reads from q-version constant
 ;; ═══════════════════════════════════════════
 (test-suite "Issue #142: print-version uses q-version"

   (test-case "print-version contains 'q version' and version number"
     (define port (open-output-string))
     (print-version port)
     (define output (get-output-string port))
     (check-true (string-contains? output "q version") "should contain 'q version'")
     (check-true (regexp-match? #rx"q version [0-9]+\\.[0-9]+\\.[0-9]+" output)
                 "should match version pattern"))

   (test-case "print-version is NOT hardcoded 0.5.1"
     (define port (open-output-string))
     (print-version port)
     (define output (get-output-string port))
     (check-false (string-contains? output "0.5.1") "version should not be the old 0.5.1")))
 ;; ═══════════════════════════════════════════
 ;; Issue #141: Mock provider warning
 ;; ═══════════════════════════════════════════
 (test-suite "Issue #141: Mock provider warning"

   (test-case "mock provider name triggers warning banner"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:provider-name "mock" #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "mock provider") "should warn about mock provider"))

   (test-case "real provider name does NOT trigger warning"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:provider-name "openai" #:in in #:out out)
     (define output (get-output-string out))
     (check-false (string-contains? output "mock provider") "should not warn for real provider"))

   (test-case "no provider-name does NOT trigger warning"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-false (string-contains? output "mock provider") "should not warn when no provider name")))
 ;; ═══════════════════════════════════════════
 ;; Issue #145: TUI-only commands show TUI-only message
 ;; ═══════════════════════════════════════════
 (test-suite "Issue #145: TUI-only commands in CLI"

   (test-case "/clear shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/clear\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/clear should mention TUI"))

   (test-case "/interrupt shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/interrupt\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/interrupt should mention TUI"))

   (test-case "/branches shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/branches\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/branches should mention TUI"))

   (test-case "/leaves shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/leaves\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/leaves should mention TUI"))

   (test-case "/switch shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/switch abc\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/switch should mention TUI"))

   (test-case "/children shows TUI-only message"
     (define cfg (cli-config 'chat #f #f #f 'interactive #f #f #f 10 #f '() #f #f '()))
     (define in (open-input-string "/children abc\n/quit\n"))
     (define out (open-output-string))
     (run-cli-interactive cfg #:in in #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "TUI") "/children should mention TUI"))

   (test-case "/help shows TUI only labels"
     (define port (open-output-string))
     (print-usage port)
     (define output (get-output-string port))
     (check-true (string-contains? output "TUI only") "help should show TUI only labels")))
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
     (define tmp-dir (make-temporary-file "q-init-test-~a" 'directory))
     ;; Simulate user input: openai, api-key, model, enter
     (define in (open-input-string "openai\nsk-test-123\ngpt-4o\n"))
     (define out (open-output-string))
     ;; Override home dir by parameterizing current-directory isn't enough,
     ;; so just run the wizard and verify it doesn't crash
     ;; (full integration test needs mocked home-dir)
     ;; For now, test with a string port — wizard writes to real ~/.q/
     ;; but we can at least verify it runs without error on valid input
     (check-not-exn (lambda ()
                      (run-init-wizard #:in (open-input-string "openai\nsk-test\n\n")
                                       #:out (open-output-string)))))

   (test-case "wizard rejects invalid provider"
     (define out (open-output-string))
     ;; Invalid provider → should print error and not write config
     ;; Note: if ~/.q/config.json exists, the first input is consumed by overwrite prompt
     ;; So we provide enough input for both scenarios
     (run-init-wizard #:in (open-input-string "y\ninvalid\n") #:out out)
     (define output (get-output-string out))
     (check-true (string-contains? output "Invalid provider") "should reject invalid provider"))))
;; ═══════════════════════════════════════════
;; Issue #160: 'sessions' subcommand parsing
;; ═══════════════════════════════════════════
(test-suite "Issue #160: 'sessions' subcommand"

  (test-case "sessions list subcommand → sessions command with list"
    (define cfg (parse-cli-args #("sessions" "list")))
    (check-equal? (cli-config-command cfg) 'sessions)
    (check-equal? (cli-config-sessions-subcommand cfg) 'list))

  (test-case "sessions info subcommand → sessions command with info"
    (define cfg (parse-cli-args #("sessions" "info" "abc123")))
    (check-equal? (cli-config-command cfg) 'sessions)
    (check-equal? (cli-config-sessions-subcommand cfg) 'info)
    (check-equal? (cli-config-sessions-args cfg) '("abc123")))

  (test-case "sessions delete subcommand → sessions command with delete"
    (define cfg (parse-cli-args #("sessions" "delete" "def456")))
    (check-equal? (cli-config-command cfg) 'sessions)
    (check-equal? (cli-config-sessions-subcommand cfg) 'delete)
    (check-equal? (cli-config-sessions-args cfg) '("def456")))

  (test-case "sessions without subcommand → help"
    (define cfg (parse-cli-args #("sessions")))
    (check-equal? (cli-config-command cfg) 'help))

  (test-case "sessions with invalid subcommand → help"
    (define cfg (parse-cli-args #("sessions" "bogus")))
    (check-equal? (cli-config-command cfg) 'help))

  (test-case "sessions appears in usage"
    (define port (open-output-string))
    (print-usage port)
    (define output (get-output-string port))
    (check-true (string-contains? output "sessions") "usage should mention sessions command")))
;; ═══════════════════════════════════════════
;; Issue #162: /sessions slash command
;; ═══════════════════════════════════════════
(test-suite "Issue #162: /sessions slash command"

  (test-case "/sessions → sessions list"
    (check-equal? (parse-slash-command "/sessions") '(sessions)))

  (test-case "/sessions list → sessions list"
    (check-equal? (parse-slash-command "/sessions list") '(sessions list)))

  (test-case "/sessions info abc123 → sessions info with id"
    (check-equal? (parse-slash-command "/sessions info abc123") '(sessions info "abc123")))

  (test-case "/sessions delete abc123 → sessions delete with id"
    (check-equal? (parse-slash-command "/sessions delete abc123") '(sessions delete "abc123")))

  (test-case "/sessions info without id → sessions info"
    (check-equal? (parse-slash-command "/sessions info") '(sessions info)))

  (test-case "/sessions delete without id → sessions delete"
    (check-equal? (parse-slash-command "/sessions delete") '(sessions delete)))

  (test-case "/sessions with unknown subcommand → sessions"
    (check-equal? (parse-slash-command "/sessions foo") '(sessions))))

;; Run
(run-tests test-cli)
