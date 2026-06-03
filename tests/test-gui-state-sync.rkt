#lang racket/base

(require rackunit
         rackunit/text-ui
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../util/event/event.rkt")

;; Helper: create a minimal event
(define (mk-event tag payload)
  (make-event tag (current-inexact-milliseconds) #f #f payload))

;; Helper: fresh state-box
(define (fresh-box)
  (box (make-gui-state)))

(define test-user-input
  (test-suite "user.input"
    (test-case "adds user message to empty state"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "user.input" (hash 'text "hello")))
      (define state (unbox sb))
      (define msgs (gui-state-messages state))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "user")
      (check-equal? (gui-message-text (car msgs)) "hello"))

    (test-case "appends to existing messages"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "user.input" (hash 'text "first")))
      (sub (mk-event "user.input" (hash 'text "second")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-text (cadr msgs)) "second"))))

(define test-stream-delta
  (test-suite "model.stream.delta"
    (test-case "creates assistant message on first delta"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.delta" (hash 'delta "Hello")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "assistant")
      (check-equal? (gui-message-text (car msgs)) "Hello"))

    (test-case "updates existing assistant message"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.delta" (hash 'delta "Hello")))
      (sub (mk-event "model.stream.delta" (hash 'delta " world")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-text (car msgs)) "Hello world"))

    (test-case "ignores empty delta"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.delta" (hash 'delta "")))
      (check-equal? (length (gui-state-messages (unbox sb))) 0))))

(define test-stream-thinking
  (test-suite "model.stream.thinking"
    (test-case "sets processing status when no assistant msg yet"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta "Thinking...")))
      (check-equal? (gui-state-status (unbox sb)) 'processing))

    (test-case "does not change status when assistant msg exists"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.delta" (hash 'delta "x")))
      (sub (mk-event "model.stream.thinking" (hash 'delta "y")))
      ;; status should remain idle (not overwritten to processing)
      (check-equal? (gui-state-status (unbox sb)) 'idle))))

(define test-stream-completed
  (test-suite "model.stream.completed"
    (test-case "resets status to idle"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "model.stream.completed" (hash)))
      (check-equal? (gui-state-status (unbox sb)) 'idle))

    (test-case "status reset happens exactly once"
      (define count 0)
      (define sb (fresh-box))
      (define notify-box (box (lambda () (set! count (+ count 1)))))
      (define sub (make-gui-event-subscriber sb notify-box))
      (sub (mk-event "model.stream.completed" (hash)))
      (check-equal? count 1))))

(define test-turn-started
  (test-suite "turn.started"
    (test-case "sets processing status"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "turn.started" (hash)))
      (check-equal? (gui-state-status (unbox sb)) 'processing))))

(define test-turn-completed
  (test-suite "turn.completed"
    (test-case "sets idle status"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "turn.completed" (hash)))
      (check-equal? (gui-state-status (unbox sb)) 'idle))))

(define test-tool-call-started
  (test-suite "tool.call.started"
    (test-case "adds tool message with name"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "bash")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (check-equal? (gui-message-role (car msgs)) "tool")
      (check-equal? (gui-message-text (car msgs)) "[bash]"))

    (test-case "uses 'unknown' when no name given"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash)))
      (check-equal? (gui-message-text (car (gui-state-messages (unbox sb)))) "[unknown]"))))

(define test-error-events
  (test-suite "error events"
    (test-case "sets error status for 'error' tag"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "provider.error" (hash 'msg "fail")))
      (check-equal? (gui-state-status (unbox sb)) 'error))

    (test-case "sets error status for 'model.error' tag"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.error" (hash)))
      (check-equal? (gui-state-status (unbox sb)) 'error))))

(define test-unknown-event
  (test-suite "unknown events"
    (test-case "does not crash on unknown event tag"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "something.weird" (hash)))
      ;; state unchanged
      (check-equal? (gui-state-messages (unbox sb)) '()))

    (test-case "does not call notify on unknown event"
      (define count 0)
      (define sb (fresh-box))
      (define notify-box (box (lambda () (set! count (+ count 1)))))
      (define sub (make-gui-event-subscriber sb notify-box))
      (sub (mk-event "unknown" (hash)))
      (check-equal? count 0))))

(define test-notify-callback
  (test-suite "notify callback"
    (test-case "calls notify callback on state change"
      (define count 0)
      (define sb (fresh-box))
      (define notify-box (box (lambda () (set! count (+ count 1)))))
      (define sub (make-gui-event-subscriber sb notify-box))
      (sub (mk-event "user.input" (hash 'text "hi")))
      (check-equal? count 1))

    (test-case "calls notify callback on each event"
      (define count 0)
      (define sb (fresh-box))
      (define notify-box (box (lambda () (set! count (+ count 1)))))
      (define sub (make-gui-event-subscriber sb notify-box))
      (sub (mk-event "user.input" (hash 'text "a")))
      (sub (mk-event "user.input" (hash 'text "b")))
      (check-equal? count 2))

    (test-case "works when notify-callback-box is #f"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb #f))
      ;; should not crash
      (sub (mk-event "user.input" (hash 'text "x")))
      (check-equal? (length (gui-state-messages (unbox sb))) 1))))

(define test-tool-call-with-args
  (test-suite "tool.call.started with args"
    (test-case "shows arg summary for tool with arguments"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "bash" 'arguments (hash 'command "ls -la"))))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (define msg-text (gui-message-text (car msgs)))
      (check-not-false (regexp-match? #rx"bash" msg-text))
      (check-not-false (regexp-match? #rx"command" msg-text)))))

(define test-tool-execution-completed
  (test-suite "tool.execution.completed"
    (test-case "updates last tool message with OK result"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      ;; First: tool call started
      (sub (mk-event "tool.call.started" (hash 'name "bash" 'arguments (hash 'command "ls -la"))))
      ;; Then: tool execution completed with correct camelCase keys
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1 "should update existing tool message, not add new")
      (check-equal? (gui-message-role (car msgs)) "tool")
      (check-not-false (regexp-match? #rx"\u2192 OK" (gui-message-text (car msgs)))))

    (test-case "shows FAIL for error result"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "read")))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'error)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (check-not-false (regexp-match? #rx"\u2192 FAIL" (gui-message-text (car msgs)))))

    (test-case "defaults to OK when no resultSummary"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "bash")))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1)
      (check-not-false (regexp-match? #rx"\u2192 OK" (gui-message-text (car msgs)))))))

(define test-parallel-tool-completions
  (test-suite "parallel tool completion correlation"
    (test-case "3 parallel reads: each completion updates correct tool"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      ;; 3 tool calls in rapid succession
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "a.txt"))))
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "b.txt"))))
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "c.txt"))))
      ;; 3 completions
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 3 "three tool messages remain")
      ;; Each should have exactly one \u2192 OK
      (for ([m (in-list msgs)])
        (check-not-false (regexp-match? #rx"\u2192 OK" (gui-message-text m))
                         "each has exactly one result")
        (check-false (regexp-match? #rx"\u2192 OK.*\u2192 OK" (gui-message-text m))
                     "no double results")))

    (test-case "mixed tools: read OK, bash FAIL, read OK"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "read")))
      (sub (mk-event "tool.call.started" (hash 'name "bash")))
      (sub (mk-event "tool.call.started" (hash 'name "read")))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'error)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 3)
      (check-not-false (regexp-match? #rx"\u2192 OK" (gui-message-text (car msgs))) "first read OK")
      (check-not-false (regexp-match? #rx"\u2192 FAIL" (gui-message-text (cadr msgs))) "bash FAIL")
      (check-not-false (regexp-match? #rx"\u2192 OK" (gui-message-text (caddr msgs)))
                       "second read OK"))

    (test-case "completion with no matching started event"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      ;; No tool.call.started, just a completion
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 0 "no message added for orphan completion"))))

(run-tests (test-suite "gui-state-sync"
             test-user-input
             test-stream-delta
             test-stream-thinking
             test-stream-completed
             test-turn-started
             test-turn-completed
             test-tool-call-started
             test-error-events
             test-unknown-event
             test-notify-callback
             test-tool-call-with-args
             test-tool-execution-completed
             test-parallel-tool-completions))
