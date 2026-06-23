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
