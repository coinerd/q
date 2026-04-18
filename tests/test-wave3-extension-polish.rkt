#lang racket

;; tests/test-wave3-extension-polish.rkt — Wave 3 extension & session polish tests
;;
;; Tests for v0.11.0 Wave 3 sub-issues:
;;   #1185: RPC Extension UI Sub-Protocol
;;   #1186: Bash Execution as First-Class Message Type
;;   #1187: Prompt Templates with Argument Expansion

(require rackunit
         racket/port
         "../util/protocol-types.rkt"
         "../skills/template.rkt"
         "../extensions/ui-channel.rkt"
         "../wiring/rpc-ui-adapter.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-msg id role kind content [meta (hasheq)])
  (make-message id #f role kind content (current-seconds) meta))

;; ============================================================
;; #1185: RPC Extension UI Sub-Protocol
;; ============================================================
;; The RPC UI bridge wiring is tested in test-rpc-ui-adapter.rkt.
;; Here we verify the wiring is importable and the handler map is complete.

(test-case "#1185: rpc-ui-adapter provides expected functions"
  (check-true (procedure? start-rpc-ui-bridge!))
  (check-true (procedure? make-rpc-ui-response-handler)))

;; ============================================================
;; #1186: Bash Execution Message Type
;; ============================================================

(test-case "#1186: bash-execution-entry? recognizes bash-execution kind"
  (define msg (make-msg "b1" 'tool 'bash-execution '()))
  (check-true (bash-execution-entry? msg))
  (check-false (bash-execution-entry? (make-msg "b2" 'tool 'tool-result '())))
  (check-false (bash-execution-entry? (make-msg "b3" 'user 'message '()))))

(test-case "#1186: bash-execution preserves metadata in meta field"
  (define msg (make-msg "b1" 'tool 'bash-execution
                        '()
                        (hasheq 'command "ls -la"
                                'exit-code 0
                                'duration-ms 150
                                'timed-out? #f)))
  (check-true (bash-execution-entry? msg))
  (check-equal? (hash-ref (message-meta msg) 'command) "ls -la")
  (check-equal? (hash-ref (message-meta msg) 'exit-code) 0)
  (check-equal? (hash-ref (message-meta msg) 'duration-ms) 150)
  (check-equal? (hash-ref (message-meta msg) 'timed-out?) #f))

(test-case "#1186: any-tool-result-entry? matches both kinds"
  (define tool-msg (make-msg "t1" 'tool 'tool-result '()))
  (define bash-msg (make-msg "b1" 'tool 'bash-execution '()))
  (define user-msg (make-msg "u1" 'user 'message '()))
  (check-not-false (any-tool-result-entry? tool-msg) "tool-result should match")
  (check-not-false (any-tool-result-entry? bash-msg) "bash-execution should match")
  (check-false (any-tool-result-entry? user-msg) "user message should not match"))

(test-case "#1186: tool-result-entry? still works for backward compat"
  (define msg (make-msg "t1" 'tool 'tool-result '()))
  (check-true (tool-result-entry? msg))
  ;; bash-execution is NOT tool-result — they're distinct
  (define bash-msg (make-msg "b1" 'tool 'bash-execution '()))
  (check-false (tool-result-entry? bash-msg)))

(test-case "#1186: bash-execution message round-trips through serialization"
  (define msg (make-msg "b1" 'tool 'bash-execution
                        '()
                        (hasheq 'command "make test" 'exit-code 1)))
  (define jsexpr (message->jsexpr msg))
  (define restored (jsexpr->message jsexpr))
  (check-true (bash-execution-entry? restored))
  (check-equal? (message-kind restored) 'bash-execution)
  (check-equal? (hash-ref (message-meta restored) 'command) "make test")
  (check-equal? (hash-ref (message-meta restored) 'exit-code) 1))

;; ============================================================
;; #1187: Prompt Templates with Argument Expansion
;; ============================================================

(test-case "#1187: $1 $2 positional expansion"
  (define tpl "Hello $1, you are $2!")
  (check-equal? (render-template-with-positional-args tpl '("Alice" "awesome"))
                "Hello Alice, you are awesome!"))

(test-case "#1187: $@ expands to all args"
  (define tpl "Files: $@")
  (check-equal? (render-template-with-positional-args tpl '("a.rkt" "b.rkt" "c.rkt"))
                "Files: a.rkt b.rkt c.rkt"))

(test-case "#1187: ${@:N:L} slice expansion"
  (define tpl "Selected: ${@:2:2}")
  (check-equal? (render-template-with-positional-args tpl '("a" "b" "c" "d"))
                "Selected: b c"))

(test-case "#1187: missing $N expands to empty"
  (define tpl "Only $1 and $3 here")
  (check-equal? (render-template-with-positional-args tpl '("first"))
                "Only first and  here"))

(test-case "#1187: empty args"
  (define tpl "No args: $@ and $1")
  (check-equal? (render-template-with-positional-args tpl '())
                "No args:  and "))

(test-case "#1187: combined $@ and $1"
  (define tpl "All: $@ | First: $1")
  (check-equal? (render-template-with-positional-args tpl '("x" "y" "z"))
                "All: x y z | First: x"))

(test-case "#1187: ${@:N:L} at end of range"
  (define tpl "Last two: ${@:3:2}")
  (check-equal? (render-template-with-positional-args tpl '("a" "b" "c" "d"))
                "Last two: c d"))

(test-case "#1187: ${@:N:L} exceeds available args"
  (define tpl "Overflow: ${@:2:5}")
  (check-equal? (render-template-with-positional-args tpl '("a" "b" "c"))
                "Overflow: b c"))

(test-case "#1187: render-template {{var}} still works"
  (define tpl "Hello {{name}}, welcome to {{place}}!")
  (check-equal? (render-template tpl (hasheq 'name "Alice" 'place "Racket"))
                "Hello Alice, welcome to Racket!"))

(test-case "#1185: bridge can start and accept requests"
  (define ui-ch (make-channel))
  (define buf (open-output-string))
  ;; Start the bridge (it reads from ui-ch in a background thread)
  (start-rpc-ui-bridge! ui-ch buf)
  ;; Send a confirm request through the channel
  (define resp-ch (make-channel))
  (define req (ui-request 'confirm resp-ch "Do you want to proceed?"))
  (thread (lambda () (channel-put ui-ch req)))
  ;; Give the bridge time to process
  (sleep 0.1)
  ;; Check that a notification was written to buf
  (define output (get-output-string buf))
  (check-true (string-contains? output "ui.confirm")
              (format "expected ui.confirm in output, got: ~a" output)))
