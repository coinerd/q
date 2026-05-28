#lang racket/base

(require rackunit
         rackunit/text-ui
         "../gui/state-sync.rkt"
         "../util/event.rkt")

;; Helper: create a minimal event
(define (mk-event tag payload)
  (make-event tag (current-inexact-milliseconds) #f #f payload))

;; Helper: fresh state-box
(define (fresh-box)
  (box (hash 'messages '() 'status 'idle)))

(define test-user-input
  (test-suite
   "user.input"
   (test-case "adds user message to empty state"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "user.input" (hash 'text "hello")))
     (define state (unbox sb))
     (define msgs (hash-ref state 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "user")
     (check-equal? (hash-ref (car msgs) 'text) "hello"))

   (test-case "appends to existing messages"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "user.input" (hash 'text "first")))
     (sub (mk-event "user.input" (hash 'text "second")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 2)
     (check-equal? (hash-ref (cadr msgs) 'text) "second"))))

(define test-stream-delta
  (test-suite
   "model.stream.delta"
   (test-case "creates assistant message on first delta"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.stream.delta" (hash 'delta "Hello")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "assistant")
     (check-equal? (hash-ref (car msgs) 'text) "Hello"))

   (test-case "updates existing assistant message"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.stream.delta" (hash 'delta "Hello")))
     (sub (mk-event "model.stream.delta" (hash 'delta " world")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'text) "Hello world"))

   (test-case "ignores empty delta"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.stream.delta" (hash 'delta "")))
     (check-equal? (length (hash-ref (unbox sb) 'messages)) 0))))

(define test-stream-thinking
  (test-suite
   "model.stream.thinking"
   (test-case "sets processing status when no assistant msg yet"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.stream.thinking" (hash 'delta "Thinking...")))
     (check-equal? (hash-ref (unbox sb) 'status) 'processing))

   (test-case "does not change status when assistant msg exists"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.stream.delta" (hash 'delta "x")))
     (sub (mk-event "model.stream.thinking" (hash 'delta "y")))
     ;; status should remain idle (not overwritten to processing)
     (check-equal? (hash-ref (unbox sb) 'status) 'idle))))

(define test-stream-completed
  (test-suite
   "model.stream.completed"
   (test-case "resets status to idle"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "turn.started" (hash)))
     (sub (mk-event "model.stream.completed" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'idle))

   (test-case "status reset happens exactly once"
     (define count 0)
     (define sb (fresh-box))
     (define notify-box (box (lambda () (set! count (+ count 1)))))
     (define sub (make-gui-event-subscriber sb notify-box))
     (sub (mk-event "model.stream.completed" (hash)))
     (check-equal? count 1))))

(define test-turn-started
  (test-suite
   "turn.started"
   (test-case "sets processing status"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "turn.started" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'processing))))

(define test-turn-completed
  (test-suite
   "turn.completed"
   (test-case "sets idle status"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "turn.started" (hash)))
     (sub (mk-event "turn.completed" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'idle))))

(define test-tool-call-started
  (test-suite
   "tool.call.started"
   (test-case "adds tool message with name"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "tool.call.started" (hash 'name "bash")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'role) "tool")
     (check-equal? (hash-ref (car msgs) 'text) "[bash]"))

   (test-case "uses 'unknown' when no name given"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "tool.call.started" (hash)))
     (check-equal? (hash-ref (car (hash-ref (unbox sb) 'messages)) 'text) "[unknown]"))))

(define test-error-events
  (test-suite
   "error events"
   (test-case "sets error status for 'error' tag"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "provider.error" (hash 'msg "fail")))
     (check-equal? (hash-ref (unbox sb) 'status) 'error))

   (test-case "sets error status for 'model.error' tag"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "model.error" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'error))))

(define test-unknown-event
  (test-suite
   "unknown events"
   (test-case "does not crash on unknown event tag"
     (define sb (fresh-box))
     (define sub (make-gui-event-subscriber sb))
     (sub (mk-event "something.weird" (hash)))
     ;; state unchanged
     (check-equal? (hash-ref (unbox sb) 'messages) '()))

   (test-case "does not call notify on unknown event"
     (define count 0)
     (define sb (fresh-box))
     (define notify-box (box (lambda () (set! count (+ count 1)))))
     (define sub (make-gui-event-subscriber sb notify-box))
     (sub (mk-event "unknown" (hash)))
     (check-equal? count 0))))

(define test-notify-callback
  (test-suite
   "notify callback"
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
     (check-equal? (length (hash-ref (unbox sb) 'messages)) 1))))

(run-tests
 (test-suite "gui-state-sync"
   test-user-input
   test-stream-delta
   test-stream-thinking
   test-stream-completed
   test-turn-started
   test-turn-completed
   test-tool-call-started
   test-error-events
   test-unknown-event
   test-notify-callback))
