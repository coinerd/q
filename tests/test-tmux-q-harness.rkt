#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-tmux-q-harness.rkt — Unit tests for tmux q harness
;;
;; Tests pure functions (normalization, command construction, env var
;; handling) without requiring tmux to be available.

(require json
         rackunit
         racket/file
         racket/list
         racket/string
         "helpers/tmux-q-harness.rkt")

;; ============================================================
;; ANSI/VT normalization tests
;; ============================================================

(check-equal? (normalize-pane-output "hello world") "hello world" "plain text unchanged")

(check-equal? (normalize-pane-output "hello\x1b[31m world\x1b[0m")
              "hello world"
              "CSI color codes stripped")

(check-equal? (normalize-pane-output "\x1b[2J\x1b[Hhello")
              "hello"
              "clear-screen + cursor-home stripped")

(check-equal? (normalize-pane-output "line1\r\nline2") "line1\nline2" "CRLF normalized to LF")

(check-equal? (normalize-pane-output "text\x1b[?2004htext")
              "texttext"
              "bracketed paste markers stripped")

(check-equal? (normalize-pane-output "\x1b]0;title\x07text") "text" "OSC sequence stripped")

(check-equal? (normalize-pane-output "a\x00b") "ab" "null bytes removed")

(check-equal? (normalize-pane-output "  hello  \n  world  \n")
              "  hello\n  world"
              "trailing whitespace trimmed, leading preserved")

(check-equal? (normalize-pane-output "\x1bP>\x1b\\text") "text" "DCS/APC sequences stripped")

(check-equal? (normalize-pane-output "") "" "empty string unchanged")

(check-equal? (normalize-pane-output "hello\x1b[1;1fworld")
              "helloworld"
              "cursor-position absolute stripped")

(check-equal? (normalize-pane-output "ab\x08c") "ac" "backspace processed")

;; ============================================================
;; Command construction tests
;; ============================================================

(check-equal? (build-tmux-new-session-command #:session-name "test-sess"
                                              #:cols 100
                                              #:rows 30
                                              #:cwd "/tmp/proj"
                                              #:shell-command "racket main.rkt --tui")
              '("tmux" "-f"
                       "/dev/null"
                       "new-session"
                       "-d"
                       "-s"
                       "test-sess"
                       "-x"
                       "100"
                       "-y"
                       "30"
                       "-c"
                       "/tmp/proj"
                       "racket main.rkt --tui")
              "new-session command correct")

(check-equal? (build-tmux-send-keys-command "test-sess" "hello world")
              '("tmux" "send-keys" "-t" "test-sess" "hello world" "Enter")
              "send-keys with Enter")

(check-equal? (build-tmux-send-keys-command "test-sess" "hello world" #:press-enter? #f)
              '("tmux" "send-keys" "-t" "test-sess" "hello world")
              "send-keys without Enter")

(check-equal? (build-tmux-send-key-command "test-sess" "C-c")
              '("tmux" "send-keys" "-t" "test-sess" "C-c")
              "send-key (raw key)")

(check-equal? (build-tmux-capture-pane-command "test-sess" #:lines 50)
              '("tmux" "capture-pane" "-t" "test-sess" "-p" "-S" "-50")
              "capture-pane with lines")

(check-equal? (build-tmux-capture-pane-command "test-sess" #:raw? #t)
              '("tmux" "capture-pane" "-t" "test-sess" "-p" "-S" "-50" "-e")
              "capture-pane raw mode with default lines")

(check-equal? (build-tmux-resize-command "test-sess" 60 20)
              '("tmux" "resize-window" "-t" "test-sess" "-x" "60" "-y" "20")
              "resize-window command")

(check-equal? (build-tmux-kill-session-command "test-sess")
              '("tmux" "kill-session" "-t" "test-sess")
              "kill-session command")

(check-equal? (build-tmux-list-sessions-command) '("tmux" "list-sessions") "list-sessions command")

;; ============================================================
;; Env var handling tests
;; ============================================================

(check-equal? (tmux-test-mode->symbol "1") 'run "Q_TMUX_TUI_TESTS=1 -> run")

(check-equal? (tmux-test-mode->symbol "require") 'require "Q_TMUX_TUI_TESTS=require -> require")

(check-equal? (tmux-test-mode->symbol "0") 'skip "Q_TMUX_TUI_TESTS=0 -> skip")

(check-equal? (tmux-test-mode->symbol #f) 'skip "Q_TMUX_TUI_TESTS unset -> skip")

(check-equal? (tmux-test-mode->symbol "") 'skip "Q_TMUX_TUI_TESTS='' -> skip")

(check-equal? (tmux-test-mode->symbol "anything-else") 'skip "Q_TMUX_TUI_TESTS=invalid -> skip")

;; ============================================================
;; Redaction tests
;; ============================================================

(check-equal? (redact-sensitive "HOME=/tmp/q-home") "HOME=<REDACTED>" "HOME path redacted")

(check-equal? (redact-sensitive "API_KEY=secret123") "API_KEY=<REDACTED>" "API_KEY redacted")

(check-equal? (redact-sensitive "TOKEN=abc123") "TOKEN=<REDACTED>" "TOKEN redacted")

(check-equal? (redact-sensitive "normal text") "normal text" "non-sensitive text unchanged")

;; ============================================================
;; Wait-for-text predicate tests
;; ============================================================

(check-true (text-found-in? "hello" "hello world") "exact substring found")

(check-false (text-found-in? "goodbye" "hello world") "substring not found")

(check-true (text-found-in? "Mock response" "some\nMock response from q.\nmore")
            "multiline search works")

(check-false (text-found-in? "" "") "empty in empty is false")

;; ============================================================
;; Session name generation
;; ============================================================

(check-true (and (string? (make-session-name)) (> (string-length (make-session-name)) 0))
            "session name is non-empty string")

(check-true (string-prefix? (make-session-name) "q-test-") "session name has q-test- prefix")

(check-false (string-contains? (make-session-name) ".") "session name has no dot (tmux restriction)")

(check-false (string-contains? (make-session-name) ":")
             "session name has no colon (tmux restriction)")

;; ============================================================
;; Structured trace/event completion helpers
;; ============================================================

(define (with-temp-dir proc)
  (define dir (make-temporary-file "q-tmux-harness-test-~a" 'directory))
  (dynamic-wind void
                (lambda () (proc dir))
                (lambda ()
                  (with-handlers ([exn:fail? (lambda (_e) (void))])
                    (delete-directory/files dir)))))

(test-case "trace completion helper finds stream.turn.completed in session trace"
  (with-temp-dir (lambda (dir)
                   (define sid-dir (build-path dir "01TESTSESSION"))
                   (make-directory* sid-dir)
                   (define trace-path (build-path sid-dir "trace.jsonl"))
                   (call-with-output-file
                    trace-path
                    (lambda (out)
                      (write-json (hasheq 'phase "turn.started" 'turnId "t-1" 'seq 1) out)
                      (newline out)
                      (write-json (hasheq 'phase "model.stream.delta" 'turnId "t-1" 'seq 2) out)
                      (newline out)
                      (write-json (hasheq 'phase "stream.turn.completed" 'turnId "t-1" 'seq 3) out)
                      (newline out))
                    #:exists 'replace)
                   (check-equal? (find-trace-jsonl-paths (path->string dir))
                                 (list (path->string trace-path)))
                   (define events (read-trace-events (path->string trace-path)))
                   (check-equal? (length events) 3)
                   (check-equal? (trace-entry-phase (last events)) "stream.turn.completed")
                   (check-true (turn-completion-trace-entry? (last events) #:turn-id "t-1"))
                   (check-false (turn-completion-trace-entry? (last events) #:turn-id "other"))
                   (define latest (latest-turn-completion-event (path->string dir) #:turn-id "t-1"))
                   (check-equal? (trace-entry-phase latest) "stream.turn.completed")
                   (check-equal? (trace-entry-turn-id latest) "t-1"))))

(test-case "trace completion helper ignores malformed JSONL and accepts canonical turn.completed"
  (with-temp-dir
   (lambda (dir)
     (define sid-dir (build-path dir "01TESTSESSION"))
     (make-directory* sid-dir)
     (define trace-path (build-path sid-dir "trace.jsonl"))
     (call-with-output-file trace-path
                            (lambda (out)
                              (display "not-json" out)
                              (newline out)
                              (write-json (hasheq 'phase "turn.completed" 'turnId "t-2" 'seq 4) out)
                              (newline out))
                            #:exists 'replace)
     (define events (read-trace-events (path->string trace-path)))
     (check-equal? (length events) 1)
     (check-equal? (trace-entry-phase (car events)) "turn.completed")
     (check-equal? (trace-entry-turn-id (car events)) "t-2")
     (check-true (turn-completion-trace-entry? (car events))))))

;; ============================================================
;; Real-provider safe harness helpers
;; ============================================================

(test-case "real-provider-authorized? returns #f without env gates"
  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))])
    (putenv "Q_TMUX_TUI_TESTS" "0")
    (putenv "Q_TMUX_TUI_REAL_PROVIDER" "0")
    (check-false (real-provider-authorized?))
    (putenv "Q_TMUX_TUI_TESTS" "1")
    (check-false (real-provider-authorized?))
    (putenv "Q_TMUX_TUI_REAL_PROVIDER" "1")
    (check-false (real-provider-authorized?))
    (putenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM" "wrong")
    (check-false (real-provider-authorized?))
    (putenv "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM" "I_UNDERSTAND_COSTS")
    (check-true (real-provider-authorized?))))

(test-case "make-real-provider-tmux-env raises without gates"
  (parameterize ([current-environment-variables
                  (environment-variables-copy (current-environment-variables))])
    (putenv "Q_TMUX_TUI_TESTS" "0")
    (putenv "Q_TMUX_TUI_REAL_PROVIDER" "0")
    (check-exn #rx"real-provider mode requires" (lambda () (make-real-provider-tmux-env)))))

(test-case "copy-q-config-to-temp-home! copies config files without printing"
  (with-temp-dir (lambda (dir)
                   ;; Create mock source HOME with .q/config.json and credentials.json
                   (define source-home (build-path dir "source-home"))
                   (define source-q (build-path source-home ".q"))
                   (make-directory* source-q)
                   (call-with-output-file (build-path source-q "config.json")
                                          (lambda (out) (write-json (hasheq 'model "test-model") out))
                                          #:exists 'replace)
                   (call-with-output-file (build-path source-q "credentials.json")
                                          (lambda (out) (display "{\"key\":\"sk-FAKE123\"}" out))
                                          #:exists 'replace)
                   ;; Create env
                   (define env (make-tmux-test-env #:base-dir dir))
                   ;; Copy config
                   (define copied
                     (copy-q-config-to-temp-home! env #:source-home (path->string source-home)))
                   (check-equal? (sort copied string<?) '("config.json" "credentials.json"))
                   ;; Verify files exist in temp HOME
                   (define dest-q (build-path (tmux-env-home env) ".q"))
                   (check-true (file-exists? (build-path dest-q "config.json")))
                   (check-true (file-exists? (build-path dest-q "credentials.json")))
                   ;; Verify config.rkt was not copied (doesn't exist in source)
                   (check-false (file-exists? (build-path dest-q "config.rkt"))))))

(test-case "detect-queued-prompts detects queued indicator"
  (check-true (detect-queued-prompts "some output\n[Queued — will run after current task]\nmore"))
  (check-false (detect-queued-prompts "normal pane output without queued"))
  (check-false (detect-queued-prompts "")))

(test-case "redact-sensitive redacts bearer tokens"
  (define result (redact-sensitive "Authorization: Bearer abc123secret"))
  (check-false (string-contains? result "abc123secret"))
  (check-true (string-contains? result "<REDACTED>")))

(test-case "redact-sensitive redacts API key patterns"
  (define result (redact-sensitive "api_key=sk-proj123abc456"))
  (check-false (string-contains? result "sk-proj123abc456"))
  (check-true (string-contains? result "<REDACTED>")))

(test-case "redact-sensitive redacts HOME paths"
  (define home-path "/tmp/q-tmux-home-1234567890")
  (define result
    (redact-sensitive (format "Config file: ~a/.q/config.json\nsome text" home-path)
                      #:home-path home-path))
  (check-false (string-contains? result home-path))
  (check-true (string-contains? result "<HOME>")))

(test-case "redact-sensitive preserves non-sensitive text"
  (define result (redact-sensitive "just normal text without secrets"))
  (check-equal? result "just normal text without secrets"))

(test-case "prompt-result struct has expected fields"
  (define pr (prompt-result "hello" (hasheq 'phase "turn.completed") "output" 'completed))
  (check-equal? (prompt-result-prompt pr) "hello")
  (check-equal? (prompt-result-status pr) 'completed)
  (check-equal? (prompt-result-capture pr) "output"))

;; ============================================================
;; Approval-prompt detection tests
;; ============================================================

(test-case "detect-approval-prompt detects subagent approval overlay"
  (define pane-text
    "\u26a1 Subagent Approval Required\n  Capabilities: read, ls, grep\n  Task: list files\n\n  [y] Approve   [n] Deny   [Esc] Cancel")
  (check-true (detect-approval-prompt pane-text)))

(test-case "detect-approval-prompt detects generic approval"
  (check-true (detect-approval-prompt "Some text\nApproval Required\n[y] Approve   [n] Deny")))

(test-case "detect-approval-prompt returns #f for normal pane"
  (check-false (detect-approval-prompt "q> hello\nThis is a normal response"))
  (check-false (detect-approval-prompt "")))

(test-case "parse-approval-prompt extracts subagent type and capabilities"
  (define pane-text
    "\u26a1 Subagent Approval Required\n  Capabilities: read, ls, grep\n  Task: explore the codebase\n\n  [y] Approve   [n] Deny   [Esc] Cancel")
  (define info (parse-approval-prompt pane-text))
  (check-true (approval-info? info))
  (check-equal? (approval-info-type info) 'subagent)
  (check-equal? (approval-info-capabilities info) '("read" "ls" "grep"))
  (check-equal? (approval-info-task-preview info) "explore the codebase"))

(test-case "parse-approval-prompt returns #f when no approval present"
  (check-false (parse-approval-prompt "normal pane output without approval")))

(test-case "classify-approval-safety returns 'safe for read-only capabilities"
  (define info (approval-info 'subagent '("read" "ls" "grep") "list files" "raw"))
  (check-equal? (classify-approval-safety info) 'safe))

(test-case "classify-approval-safety returns 'dangerous for bash capability"
  (define info (approval-info 'subagent '("read" "bash") "run a command" "raw"))
  (check-equal? (classify-approval-safety info) 'dangerous))

(test-case "classify-approval-safety returns 'dangerous for tmux kill-server in task"
  (define info (approval-info 'subagent '("read") "run tmux kill-server" "raw"))
  (check-equal? (classify-approval-safety info) 'dangerous))

(test-case "classify-approval-safety returns 'dangerous for rm -rf in task"
  (define info (approval-info 'subagent '("read") "rm -rf /tmp/data" "raw"))
  (check-equal? (classify-approval-safety info) 'dangerous))

(test-case "classify-approval-safety returns 'dangerous for write capability"
  (define info (approval-info 'subagent '("read" "write") "edit a file" "raw"))
  (check-equal? (classify-approval-safety info) 'dangerous))

(test-case "classify-approval-safety returns 'caution for unknown capabilities"
  (define info (approval-info 'subagent '("read" "unknown-cap") "do something" "raw"))
  (check-equal? (classify-approval-safety info) 'caution))

(test-case "classify-approval-safety returns 'caution for empty capabilities"
  (define info (approval-info 'unknown '() "no caps" "raw"))
  (check-equal? (classify-approval-safety info) 'caution))

(test-case "dangerous-command-patterns includes broad tmux cleanup"
  (check-true (if (member "tmux kill-server" dangerous-command-patterns) #t #f))
  (check-true (if (member "tmux kill-session" dangerous-command-patterns) #t #f))
  (check-true (if (member "rm -rf" dangerous-command-patterns) #t #f)))

(test-case "safe-capabilities includes read-only tools"
  (check-true (if (member 'read safe-capabilities) #t #f))
  (check-true (if (member 'ls safe-capabilities) #t #f))
  (check-true (if (member 'grep safe-capabilities) #t #f)))

(test-case "dangerous-capabilities includes destructive tools"
  (check-true (if (member 'bash dangerous-capabilities) #t #f))
  (check-true (if (member 'write dangerous-capabilities) #t #f))
  (check-true (if (member 'edit dangerous-capabilities) #t #f))
  (check-true (if (member 'kill dangerous-capabilities) #t #f)))

;; ============================================================
;; Tool-execution trace verification tests (W5)
;; ============================================================

(test-case "tool-execution-phases includes key lifecycle events"
  (check-true (if (member "tool.call.started" tool-execution-phases) #t #f))
  (check-true (if (member "tool.execution.started" tool-execution-phases) #t #f))
  (check-true (if (member "tool.execution.completed" tool-execution-phases) #t #f)))

(test-case "trace-entry-data extracts data payload"
  (define entry (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")))
  (define data (trace-entry-data entry))
  (check-true (hash? data))
  (check-equal? (hash-ref data 'toolName) "read"))

(test-case "trace-entry-data returns #f for entries without data"
  (define entry (hasheq 'phase "turn.completed"))
  (check-false (trace-entry-data entry)))

(test-case "trace-entry-tool-name extracts toolName from data"
  (define entry (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")))
  (check-equal? (trace-entry-tool-name entry) "read"))

(test-case "trace-entry-tool-name handles kebab-case key"
  (define entry (hasheq 'phase "tool.execution.started" 'data (hasheq 'tool-name "bash")))
  (check-equal? (trace-entry-tool-name entry) "bash"))

(test-case "trace-entry-tool-name handles 'name key from stream events"
  (define entry (hasheq 'phase "tool.call.started" 'data (hasheq 'name "grep")))
  (check-equal? (trace-entry-tool-name entry) "grep"))

(test-case "trace-entry-tool-name returns #f when no data"
  (define entry (hasheq 'phase "turn.completed"))
  (check-false (trace-entry-tool-name entry)))

(test-case "tool-execution-trace-entry? recognizes tool events"
  (define entry (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")))
  (check-true (tool-execution-trace-entry? entry)))

(test-case "tool-execution-trace-entry? rejects non-tool events"
  (define entry (hasheq 'phase "turn.completed"))
  (check-false (tool-execution-trace-entry? entry)))

(test-case "tool-execution-trace-entry? filters by tool-name"
  (define entry (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")))
  (check-true (tool-execution-trace-entry? entry #:tool-name "read"))
  (check-false (tool-execution-trace-entry? entry #:tool-name "bash")))

(test-case "parse-tool-events converts trace entries to tool-info"
  ;; Create a temp dir with a fake trace.jsonl
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-123"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json
         (hasheq 'phase "tool.execution.started" 'turnId "t1" 'data (hasheq 'toolName "read"))
         out)
        (newline out)
        (write-json (hasheq 'phase
                            "tool.execution.completed"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'toolName "read" 'duration-ms 100))
                    out)
        (newline out)
        (write-json (hasheq 'phase "turn.completed" 'turnId "t1") out)
        (newline out))
      #:exists 'replace)
     (define events (parse-tool-events (path->string dir)))
     (check-equal? (length events) 2)
     (check-equal? (tool-info-phase (car events)) "tool.execution.started")
     (check-equal? (tool-info-tool-name (car events)) "read")
     (check-equal? (tool-info-phase (cadr events)) "tool.execution.completed"))))

(test-case "compute-file-fingerprint is stable for unchanged files"
  (with-temp-dir
   (lambda (dir)
     (define f (build-path dir "test.txt"))
     (call-with-output-file f (lambda (out) (display "hello world" out)) #:exists 'replace)
     (define fp1 (compute-file-fingerprint f))
     (define fp2 (compute-file-fingerprint f))
     (check-equal? fp1 fp2))))

(test-case "compute-file-fingerprint changes when file is modified"
  (with-temp-dir
   (lambda (dir)
     (define f (build-path dir "test.txt"))
     (call-with-output-file f (lambda (out) (display "hello world" out)) #:exists 'replace)
     (define fp1 (compute-file-fingerprint f))
     (call-with-output-file f (lambda (out) (display "MODIFIED CONTENT" out)) #:exists 'replace)
     (define fp2 (compute-file-fingerprint f))
     (check-false (equal? fp1 fp2)))))

(test-case "verify-file-unchanged! passes for identical file"
  (with-temp-dir (lambda (dir)
                   (define f (build-path dir "test.txt"))
                   (call-with-output-file f (lambda (out) (display "content" out)) #:exists 'replace)
                   (define fp (compute-file-fingerprint f))
                   (verify-file-unchanged! f fp))))

(test-case "verify-file-unchanged! fails for modified file"
  (with-temp-dir (lambda (dir)
                   (define f (build-path dir "test.txt"))
                   (call-with-output-file f (lambda (out) (display "original" out)) #:exists 'replace)
                   (define fp (compute-file-fingerprint f))
                   (call-with-output-file f (lambda (out) (display "changed" out)) #:exists 'replace)
                   (check-exn #rx"file was modified" (lambda () (verify-file-unchanged! f fp))))))

(test-case "detect-sensitive-leak detects bearer tokens"
  (check-true (detect-sensitive-leak "Authorization: Bearer abc123secretkey")))

(test-case "detect-sensitive-leak detects API keys"
  (check-true (detect-sensitive-leak "key=sk-proj123abc456def789")))

(test-case "detect-sensitive-leak ignores already-redacted content"
  (check-false (detect-sensitive-leak "Authorization: Bearer <REDACTED>")))

(test-case "detect-sensitive-leak returns #f for clean text"
  (check-false (detect-sensitive-leak "just normal text without secrets")))

(test-case "detect-sensitive-leak returns #f for empty string"
  (check-false (detect-sensitive-leak "")))

(test-case "verify-artifact-redacted! passes for clean artifact"
  (with-temp-dir
   (lambda (dir)
     (define f (build-path dir "artifact.txt"))
     (call-with-output-file f (lambda (out) (display "clean content" out)) #:exists 'replace)
     (verify-artifact-redacted! f))))

(test-case "verify-artifact-redacted! fails for leaked bearer token"
  (with-temp-dir
   (lambda (dir)
     (define f (build-path dir "artifact.txt"))
     (call-with-output-file f
                            (lambda (out) (display "Authorization: Bearer secretkey123" out))
                            #:exists 'replace)
     (check-exn #rx"un-redacted sensitive content" (lambda () (verify-artifact-redacted! f))))))

;; ============================================================
;; MAS/subagent lifecycle evidence tests (W6)
;; ============================================================

(test-case "mas-lifecycle-phases includes spawn-approval and coordination events"
  (check-true (if (member "mas.spawn-approval-requested" mas-lifecycle-phases) #t #f))
  (check-true (if (member "mas.artifact.produced" mas-lifecycle-phases) #t #f))
  (check-true (if (member "mas.blackboard.sync" mas-lifecycle-phases) #t #f))
  (check-true (if (member "mas.agent.activated" mas-lifecycle-phases) #t #f)))

(test-case "mas-trace-entry? recognizes MAS events"
  (define entry (hasheq 'phase "mas.spawn-approval-requested" 'data (hasheq)))
  (check-true (mas-trace-entry? entry)))

(test-case "mas-trace-entry? rejects non-MAS events"
  (define entry (hasheq 'phase "turn.completed"))
  (check-false (mas-trace-entry? entry)))

(test-case "parse-spawn-approval-event extracts capabilities and task"
  (define entry
    (hasheq 'phase
            "mas.spawn-approval-requested"
            'turnId
            "t1"
            'data
            (hasheq 'capabilities '(read ls grep) 'task-preview "Review the code")))
  (define parsed (parse-spawn-approval-event entry))
  (check-equal? (mas-spawn-event-capabilities parsed) '(read ls grep))
  (check-equal? (mas-spawn-event-task-preview parsed) "Review the code")
  (check-equal? (mas-spawn-event-turn-id parsed) "t1"))

(test-case "parse-spawn-approval-event handles string capabilities"
  (define entry
    (hasheq 'phase
            "mas.spawn-approval-requested"
            'data
            (hasheq 'capabilities '("read" "write") 'task-preview "Test")))
  (define parsed (parse-spawn-approval-event entry))
  (check-equal? (mas-spawn-event-capabilities parsed) '(read write)))

(test-case "parse-spawn-approval-event handles missing data"
  (define entry (hasheq 'phase "mas.spawn-approval-requested"))
  (define parsed (parse-spawn-approval-event entry))
  (check-equal? (mas-spawn-event-capabilities parsed) '())
  (check-equal? (mas-spawn-event-task-preview parsed) ""))

(test-case "find-mas-events finds MAS events in session trace"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-abc"))
                   (make-directory* session-sub)
                   (call-with-output-file
                    (build-path session-sub "trace.jsonl")
                    (lambda (out)
                      (write-json (hasheq 'phase
                                          "mas.spawn-approval-requested"
                                          'turnId
                                          "t1"
                                          'data
                                          (hasheq 'capabilities '("read") 'task-preview "Do thing"))
                                  out)
                      (newline out)
                      (write-json (hasheq 'phase "turn.completed" 'turnId "t1") out)
                      (newline out)
                      (write-json (hasheq 'phase
                                          "mas.artifact.produced"
                                          'data
                                          (hasheq 'name "result.md" 'path "/tmp/result.md"))
                                  out)
                      (newline out))
                    #:exists 'replace)
                   (define events (find-mas-events (path->string dir)))
                   (check-equal? (length events) 2)
                   (check-equal? (trace-entry-phase (car events)) "mas.spawn-approval-requested")
                   (check-equal? (trace-entry-phase (cadr events)) "mas.artifact.produced"))))

(test-case "parse-mas-lifecycle extracts full lifecycle from trace"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-xyz"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase
                            "mas.spawn-approval-requested"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'capabilities '("read" "ls") 'task-preview "Review code"))
                    out)
        (newline out)
        (write-json (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")) out)
        (newline out)
        (write-json
         (hasheq 'phase "tool.execution.completed" 'data (hasheq 'toolName "read" 'duration-ms 50))
         out)
        (newline out)
        (write-json (hasheq 'phase
                            "mas.artifact.produced"
                            'data
                            (hasheq 'name "report.md" 'path "/tmp/report.md"))
                    out)
        (newline out)
        (write-json
         (hasheq 'phase "mas.hypothesis.opened" 'data (hasheq 'id "h1" 'question "Is it correct?"))
         out)
        (newline out))
      #:exists 'replace)
     (define info (parse-mas-lifecycle (path->string dir)))
     (check-equal? (length (mas-lifecycle-info-spawn-approvals info)) 1)
     (check-equal? (length (mas-lifecycle-info-artifacts info)) 1)
     (check-equal? (length (mas-lifecycle-info-coordination-events info)) 1)
     (check-true (>= (mas-lifecycle-info-total-mas-events info) 3)))))

(test-case "verify-subagent-spawn-lifecycle passes for complete cycle"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-lc"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase
                            "mas.spawn-approval-requested"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'capabilities '("read") 'task-preview "Task"))
                    out)
        (newline out)
        (write-json (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "spawn-subagent"))
                    out)
        (newline out)
        (write-json (hasheq 'phase
                            "tool.execution.completed"
                            'data
                            (hasheq 'toolName "spawn-subagent" 'duration-ms 100))
                    out)
        (newline out))
      #:exists 'replace)
     (define info (parse-mas-lifecycle (path->string dir)))
     (check-true (verify-subagent-spawn-lifecycle info)))))

(test-case "verify-subagent-spawn-lifecycle fails without tool execution"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-noexec"))
                   (make-directory* session-sub)
                   (call-with-output-file
                    (build-path session-sub "trace.jsonl")
                    (lambda (out)
                      (write-json (hasheq 'phase
                                          "mas.spawn-approval-requested"
                                          'turnId
                                          "t1"
                                          'data
                                          (hasheq 'capabilities '("read") 'task-preview "Task"))
                                  out)
                      (newline out)
                      (write-json (hasheq 'phase "turn.completed" 'turnId "t1") out)
                      (newline out))
                    #:exists 'replace)
                   (define info (parse-mas-lifecycle (path->string dir)))
                   (check-false (verify-subagent-spawn-lifecycle info)))))

(test-case "verify-subagent-spawn-lifecycle fails without spawn-approval"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-nospawn"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase "tool.execution.started" 'data (hasheq 'toolName "read")) out)
        (newline out))
      #:exists 'replace)
     (define info (parse-mas-lifecycle (path->string dir)))
     (check-false (verify-subagent-spawn-lifecycle info)))))

(test-case "detect-mas-coordination-events returns unique phases"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-coord"))
                   (make-directory* session-sub)
                   (call-with-output-file
                    (build-path session-sub "trace.jsonl")
                    (lambda (out)
                      (write-json (hasheq 'phase "mas.hypothesis.opened" 'data (hasheq)) out)
                      (newline out)
                      (write-json (hasheq 'phase "mas.hypothesis.resolved" 'data (hasheq)) out)
                      (newline out)
                      (write-json (hasheq 'phase "mas.hypothesis.opened" 'data (hasheq)) out)
                      (newline out))
                    #:exists 'replace)
                   (define info (parse-mas-lifecycle (path->string dir)))
                   (define phases (detect-mas-coordination-events info))
                   (check-equal? (length phases) 2)
                   (check-true (if (member "mas.hypothesis.opened" phases) #t #f))
                   (check-true (if (member "mas.hypothesis.resolved" phases) #t #f)))))

;; ============================================================

;; ============================================================
;; Durable memory restart round-trip tests (W7)
;; ============================================================

(test-case "memory-event-phases includes store and retrieval"
  (check-true (if (member "memory.item.stored" memory-event-phases) #t #f))
  (check-true (if (member "memory.retrieval.performed" memory-event-phases) #t #f))
  (check-true (if (member "memory.item.updated" memory-event-phases) #t #f)))

(test-case "session-lifecycle-phases includes started and shutdown"
  (check-true (if (member "session.started" session-lifecycle-phases) #t #f))
  (check-true (if (member "session.shutdown" session-lifecycle-phases) #t #f)))

(test-case "memory-trace-entry? recognizes memory events"
  (check-true (memory-trace-entry? (hasheq 'phase "memory.item.stored")))
  (check-true (memory-trace-entry? (hasheq 'phase "memory.retrieval.performed")))
  (check-false (memory-trace-entry? (hasheq 'phase "turn.completed"))))

(test-case "session-lifecycle-trace-entry? recognizes session events"
  (check-true (session-lifecycle-trace-entry? (hasheq 'phase "session.started")))
  (check-true (session-lifecycle-trace-entry? (hasheq 'phase "session.shutdown")))
  (check-false (session-lifecycle-trace-entry? (hasheq 'phase "memory.item.stored"))))

(test-case "parse-memory-store-event extracts fields"
  (define entry
    (hasheq 'phase
            "memory.item.stored"
            'turnId
            "t1"
            'data
            (hasheq 'memory-id
                    "mem-001"
                    'mem-type
                    "preference"
                    'scope
                    "user"
                    'source
                    "tool"
                    'redacted-snippet
                    "prefers dark mode")))
  (define rec (parse-memory-store-event entry))
  (check-equal? (memory-store-record-memory-id rec) "mem-001")
  (check-equal? (memory-store-record-mem-type rec) "preference")
  (check-equal? (memory-store-record-scope rec) "user")
  (check-equal? (memory-store-record-redacted-snippet rec) "prefers dark mode")
  (check-equal? (memory-store-record-turn-id rec) "t1"))

(test-case "parse-memory-store-event handles missing data"
  (define entry (hasheq 'phase "memory.item.stored"))
  (define rec (parse-memory-store-event entry))
  (check-equal? (memory-store-record-memory-id rec) "")
  (check-equal? (memory-store-record-source rec) 'tool))

(test-case "parse-memory-retrieval-event extracts fields"
  (define entry
    (hasheq
     'phase
     "memory.retrieval.performed"
     'turnId
     "t2"
     'data
     (hasheq 'query-snippet "dark mode" 'result-count 3 'query-limit 5 'scope "user" 'latency-ms 42)))
  (define rec (parse-memory-retrieval-event entry))
  (check-equal? (memory-retrieval-record-query-snippet rec) "dark mode")
  (check-equal? (memory-retrieval-record-result-count rec) 3)
  (check-equal? (memory-retrieval-record-latency-ms rec) 42))

(test-case "parse-session-start-event extracts resume reason"
  (define entry
    (hasheq 'phase
            "session.started"
            'turnId
            #f
            'data
            (hasheq 'reason 'resume 'previous-session-id "sess-001" 'session-dir "/tmp/sess")))
  (define rec (parse-session-start-event entry))
  (check-equal? (session-lifecycle-record-reason rec) 'resume)
  (check-equal? (session-lifecycle-record-previous-session-id rec) "sess-001"))

(test-case "parse-session-start-event normalizes string reason to symbol"
  (define entry (hasheq 'phase "session.started" 'data (hasheq 'reason "resume")))
  (define rec (parse-session-start-event entry))
  (check-equal? (session-lifecycle-record-reason rec) 'resume))

(test-case "parse-session-start-event handles non-hash data"
  (define entry (hasheq 'phase "session.started" 'data "model-name"))
  (define rec (parse-session-start-event entry))
  (check-false (session-lifecycle-record-reason rec)))

(test-case "find-memory-events finds memory events in trace"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-dm"))
                   (make-directory* session-sub)
                   (call-with-output-file (build-path session-sub "trace.jsonl")
                                          (lambda (out)
                                            (write-json (hasheq 'phase
                                                                "memory.item.stored"
                                                                'turnId
                                                                "t1"
                                                                'data
                                                                (hasheq 'memory-id
                                                                        "mem-001"
                                                                        'mem-type
                                                                        "preference"
                                                                        'scope
                                                                        "user"
                                                                        'source
                                                                        "tool"
                                                                        'redacted-snippet
                                                                        "dark mode"))
                                                        out)
                                            (newline out)
                                            (write-json (hasheq 'phase "turn.completed" 'turnId "t1")
                                                        out)
                                            (newline out))
                                          #:exists 'replace)
                   (define events (find-memory-events (path->string dir)))
                   (check-equal? (length events) 1)
                   (check-equal? (trace-entry-phase (car events)) "memory.item.stored"))))

(test-case "parse-durable-memory-roundtrip finds complete round-trip"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-rt"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase "session.started" 'data (hasheq 'reason "new" 'session-id "s1"))
                    out)
        (newline out)
        (write-json (hasheq 'phase
                            "memory.item.stored"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'memory-id
                                    "mem-001"
                                    'mem-type
                                    "preference"
                                    'scope
                                    "user"
                                    'source
                                    "tool"
                                    'redacted-snippet
                                    "dark mode"))
                    out)
        (newline out)
        (write-json (hasheq 'phase "session.shutdown" 'data (hasheq 'reason "explicit")) out)
        (newline out)
        (write-json (hasheq 'phase
                            "session.started"
                            'data
                            (hasheq 'reason "resume" 'session-id "s2" 'previous-session-id "s1"))
                    out)
        (newline out)
        (write-json (hasheq 'phase
                            "memory.retrieval.performed"
                            'turnId
                            "t2"
                            'data
                            (hasheq 'query-snippet "dark" 'result-count 1 'latency-ms 10))
                    out)
        (newline out))
      #:exists 'replace)
     (define rt (parse-durable-memory-roundtrip (path->string dir)))
     (check-true (durable-memory-roundtrip-has-store? rt))
     (check-true (durable-memory-roundtrip-has-retrieval? rt))
     (check-true (durable-memory-roundtrip-has-restart? rt))
     (check-true (durable-memory-roundtrip-roundtrip-complete? rt)))))

(test-case "parse-durable-memory-roundtrip incomplete without retrieval"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-no-ret"))
                   (make-directory* session-sub)
                   (call-with-output-file
                    (build-path session-sub "trace.jsonl")
                    (lambda (out)
                      (write-json (hasheq 'phase "session.started" 'data (hasheq 'reason "new")) out)
                      (newline out)
                      (write-json (hasheq 'phase
                                          "memory.item.stored"
                                          'turnId
                                          "t1"
                                          'data
                                          (hasheq 'memory-id "mem-001" 'mem-type "p" 'scope "u"))
                                  out)
                      (newline out)
                      (write-json (hasheq 'phase "session.started" 'data (hasheq 'reason "resume"))
                                  out)
                      (newline out))
                    #:exists 'replace)
                   (define rt (parse-durable-memory-roundtrip (path->string dir)))
                   (check-true (durable-memory-roundtrip-has-store? rt))
                   (check-false (durable-memory-roundtrip-has-retrieval? rt))
                   (check-true (durable-memory-roundtrip-has-restart? rt))
                   (check-false (durable-memory-roundtrip-roundtrip-complete? rt)))))

(test-case "parse-durable-memory-roundtrip incomplete without restart"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-no-restart"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase
                            "memory.item.stored"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'memory-id "mem-001" 'mem-type "p" 'scope "u"))
                    out)
        (newline out)
        (write-json
         (hasheq 'phase "memory.retrieval.performed" 'turnId "t2" 'data (hasheq 'result-count 1))
         out)
        (newline out))
      #:exists 'replace)
     (define rt (parse-durable-memory-roundtrip (path->string dir)))
     (check-true (durable-memory-roundtrip-has-store? rt))
     (check-true (durable-memory-roundtrip-has-retrieval? rt))
     (check-false (durable-memory-roundtrip-has-restart? rt))
     (check-false (durable-memory-roundtrip-roundtrip-complete? rt)))))

(test-case "verify-durable-memory-roundtrip delegates to roundtrip-complete?"
  (with-temp-dir
   (lambda (dir)
     (define session-sub (build-path dir "session-vrf"))
     (make-directory* session-sub)
     (call-with-output-file
      (build-path session-sub "trace.jsonl")
      (lambda (out)
        (write-json (hasheq 'phase "session.started" 'data (hasheq 'reason "new")) out)
        (newline out)
        (write-json (hasheq 'phase
                            "memory.item.stored"
                            'turnId
                            "t1"
                            'data
                            (hasheq 'memory-id "mem-001" 'mem-type "p" 'scope "u"))
                    out)
        (newline out)
        (write-json (hasheq 'phase "session.started" 'data (hasheq 'reason "resume")) out)
        (newline out)
        (write-json
         (hasheq 'phase "memory.retrieval.performed" 'turnId "t2" 'data (hasheq 'result-count 1))
         out)
        (newline out))
      #:exists 'replace)
     (define rt (parse-durable-memory-roundtrip (path->string dir)))
     (check-true (verify-durable-memory-roundtrip rt)))))

(test-case "parse-durable-memory-roundtrip empty trace produces no store/retrieval"
  (with-temp-dir (lambda (dir)
                   (define session-sub (build-path dir "session-empty"))
                   (make-directory* session-sub)
                   (call-with-output-file (build-path session-sub "trace.jsonl")
                                          (lambda (out)
                                            (write-json (hasheq 'phase "turn.completed") out)
                                            (newline out))
                                          #:exists 'replace)
                   (define rt (parse-durable-memory-roundtrip (path->string dir)))
                   (check-false (durable-memory-roundtrip-has-store? rt))
                   (check-false (durable-memory-roundtrip-has-retrieval? rt))
                   (check-false (durable-memory-roundtrip-has-restart? rt))
                   (check-false (durable-memory-roundtrip-roundtrip-complete? rt)))))
