#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

(require rackunit
         rackunit/text-ui
         racket/class
         racket/list
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../ui-core/conversation-artifact.rkt"
         "../ui-core/disclosure-state.rkt"
         "../ui-core/theme-protocol.rkt"
         "../util/event/event.rkt")

;; Helper: create a minimal event
(define (mk-event tag payload [session-id "gui-session"] [turn-id "gui-turn"])
  (make-event tag (current-inexact-milliseconds) session-id turn-id payload))

;; Helper: fresh state-box
(define (fresh-box)
  (box (make-gui-state)))

(define click-text%
  (class object%
    (super-new)
    (define content "")
    (define clickback #f)
    (define/public (insert str [pos #f])
      (define at (or pos (string-length content)))
      (set! content (string-append (substring content 0 at) str (substring content at))))
    (define/public (delete start end)
      (set! content (string-append (substring content 0 start) (substring content end))))
    (define/public (last-position) (string-length content))
    (define/public (lock value) (void))
    (define/public (change-style delta [start 0] [end 0]) (void))
    (define/public (get-text start end) (substring content start end))
    (define/public (set-clickback start end callback [delta #f] [call-on-down? #f])
      (set! clickback callback))
    (define/public (click!) (clickback this 0 0))))

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

    (test-case "gui-state-sync: appends to existing messages"
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
    (test-case "active identity rejects same-turn cross-session stream contamination"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta "A") "session-a" "same-turn"))
      (sub (mk-event "model.stream.thinking" (hash 'delta "B") "session-b" "same-turn"))
      (sub (mk-event "model.stream.completed" (hash) "session-a" "same-turn"))
      (sub (mk-event "model.stream.completed" (hash) "session-b" "same-turn"))
      (define thinking
        (filter (lambda (m) (eq? (gui-message-kind m) 'thinking)) (gui-state-messages (unbox sb))))
      (check-equal? (length thinking) 1)
      (check-equal? (conversation-artifact-body (hash-ref (gui-message-meta (car thinking))
                                                          'artifact))
                    "A"))

    (test-case "sets processing status when no assistant msg yet"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.thinking" (hash 'delta "Thinking...")))
      (check-equal? (gui-state-status (unbox sb)) 'processing))

    (test-case "sets processing status for thinking delta"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "model.stream.delta" (hash 'delta "x")))
      (sub (mk-event "model.stream.thinking" (hash 'delta "y")))
      ;; status should be processing
      (check-equal? (gui-state-status (unbox sb)) 'processing))))

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
      (check-equal? count 1))

    (test-case "notify callback runs after the subscriber releases the GUI lock"
      (define callback-owned-lock? #f)
      (define sb (fresh-box))
      (define notify-box
        (box (lambda ()
               (set! callback-owned-lock? (semaphore-try-wait? gui-state-lock))
               (when callback-owned-lock?
                 (semaphore-post gui-state-lock)))))
      ((make-gui-event-subscriber sb notify-box) (mk-event "model.stream.completed" (hash)))
      (check-true callback-owned-lock?))

    (test-case "stale terminal cannot idle an unrelated active turn"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "turn.started" (hash) "session-a" "active-turn"))
      (sub (mk-event "model.stream.thinking" (hash 'delta "working") "session-a" "active-turn"))
      (sub (mk-event "model.stream.completed" (hash) "session-a" "stale-turn"))
      (sub (mk-event "assistant.message.completed" (hash 'content "old") "session-a" "stale-turn"))
      (check-equal? (gui-state-status (unbox sb)) 'processing)
      (check-equal? (gui-state-active-session-id (unbox sb)) "session-a")
      (check-equal? (gui-state-active-turn-id (unbox sb)) "active-turn"))))

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
      (sub (mk-event "turn.started" (hash)))
      (sub (mk-event "provider.error" (hash 'msg "fail")))
      (check-equal? (gui-state-status (unbox sb)) 'error))

    (test-case "sets error status for 'model.error' tag"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "turn.started" (hash)))
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
      (check-equal? (length (gui-state-messages (unbox sb))) 1))

    (test-case "notification pending state is invocation-local under concurrency"
      (define count-lock (make-semaphore 1))
      (define count 0)
      (define sb (fresh-box))
      (define notify-box
        (box (lambda () (call-with-semaphore count-lock (lambda () (set! count (add1 count)))))))
      (define sub (make-gui-event-subscriber sb notify-box))
      (define workers
        (for/list ([i (in-range 100)])
          (thread (lambda ()
                    (sub (mk-event (if (even? i) "user.input" "unknown")
                                   (hash 'text (number->string i))))))))
      (for-each thread-wait workers)
      (check-equal? count 50))

    (test-case "disclosure clicks and events mutate GUI state atomically"
      (define artifact
        (make-conversation-artifact #:id "click-artifact"
                                    #:session-id "gui-session"
                                    #:turn-id "gui-turn"
                                    #:kind 'thinking
                                    #:body "reason"
                                    #:lifecycle 'completed))
      (define sb (box (gui-state-upsert-artifact (make-gui-state) artifact)))
      (define text (new click-text%))
      (define queue-enabled? #t)
      (define notify
        (make-notify-gui-callback sb
                                  (box '())
                                  (box "")
                                  text
                                  (default-theme)
                                  unbox
                                  set-box!
                                  (lambda (callback)
                                    (when queue-enabled?
                                      (callback)))))
      (notify)
      (set! queue-enabled? #f)
      (define sub (make-gui-event-subscriber sb))
      (define workers
        (append (for/list ([i (in-range 100)])
                  (thread (lambda () (send text click!))))
                (for/list ([i (in-range 100)])
                  (thread (lambda ()
                            (sub (mk-event "user.input" (hash 'text (number->string i)))))))))
      (for-each thread-wait workers)
      (check-equal? (length (gui-state-messages (unbox sb))) 101)
      (check-false (disclosure-expanded? (gui-state-disclosure (unbox sb)) "click-artifact")))))

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
    (test-case "adds new tool-end entry with OK result"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "bash" 'arguments (hash 'command "ls -la"))))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 2 "should add new entry for result")
      (check-equal? (gui-message-kind (car msgs)) 'tool-start)
      (check-equal? (gui-message-kind (cadr msgs)) 'tool-end)
      (check-not-false (regexp-match? #rx"bash" (gui-message-text (cadr msgs))))
      (check-not-false (regexp-match? #rx"completed" (gui-message-text (cadr msgs)))))

    (test-case "shows tool-fail for error result"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "read")))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'error)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-kind (cadr msgs)) 'tool-fail)
      (check-not-false (regexp-match? #rx"error" (gui-message-text (cadr msgs)))))

    (test-case "defaults to completed when no resultSummary"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "bash")))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash")))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 2)
      (check-equal? (gui-message-kind (cadr msgs)) 'tool-end))))

(define test-parallel-tool-completions
  (test-suite "parallel tool completion correlation"
    (test-case "3 parallel reads: 3 starts + 3 ends = 6 entries"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "a.txt"))))
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "b.txt"))))
      (sub (mk-event "tool.call.started" (hash 'name "read" 'arguments (hash 'path "c.txt"))))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "read" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 6 "3 tool-start + 3 tool-end")
      (for ([m (in-list (take msgs 3))])
        (check-equal? (gui-message-kind m) 'tool-start))
      (for ([m (in-list (drop msgs 3))])
        (check-equal? (gui-message-kind m) 'tool-end)))

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
      (check-equal? (length msgs) 6 "3 starts + 3 results")
      (check-equal? (gui-message-kind (list-ref msgs 3)) 'tool-end)
      (check-equal? (gui-message-kind (list-ref msgs 4)) 'tool-fail)
      (check-equal? (gui-message-kind (list-ref msgs 5)) 'tool-end))

    (test-case "completion with no matching started event adds standalone entry"
      (define sb (fresh-box))
      (define sub (make-gui-event-subscriber sb))
      (sub (mk-event "tool.execution.completed" (hash 'toolName "bash" 'resultSummary 'completed)))
      (define msgs (gui-state-messages (unbox sb)))
      (check-equal? (length msgs) 1 "orphan completion adds a tool-end entry"))))

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
