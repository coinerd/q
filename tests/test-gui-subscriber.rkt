#lang racket/base

(require rackunit
         rackunit/text-ui
         "../gui/state-sync.rkt"
         "../agent/event-bus.rkt"
         "../util/event/event.rkt")

(define (mk-event tag payload)
  (make-event tag (current-inexact-milliseconds) #f #f payload))

(define test-integration-pipeline
  (test-suite
   "integration-pipeline"
   (test-case "full conversation flow via event bus"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (define notify-count 0)
     (define notify-box (box (lambda () (set! notify-count (+ notify-count 1)))))
     (subscribe! bus (make-gui-event-subscriber sb notify-box))

     ;; User sends message
     (publish! bus (mk-event "user.input" (hash 'text "Hello")))
     (check-equal? (length (hash-ref (unbox sb) 'messages)) 1)
     (check-equal? notify-count 1)

     ;; Turn starts
     (publish! bus (mk-event "turn.started" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'processing)
     (check-equal? notify-count 2)

     ;; Stream deltas
     (publish! bus (mk-event "model.stream.delta" (hash 'delta "Hi")))
     (publish! bus (mk-event "model.stream.delta" (hash 'delta " there")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 2) ;; user + assistant
     (check-equal? (hash-ref (cadr msgs) 'text) "Hi there")
     (check-equal? notify-count 4)

     ;; Stream completes
     (publish! bus (mk-event "model.stream.completed" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'idle)
     (check-equal? notify-count 5)

     ;; Tool call
     (publish! bus (mk-event "tool.call.started" (hash 'name "read")))
     (check-equal? (length (hash-ref (unbox sb) 'messages)) 3)
     (check-equal? notify-count 6))

   (test-case "error event via bus sets error status"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb))
     (publish! bus (mk-event "model.error" (hash 'reason "timeout")))
     (check-equal? (hash-ref (unbox sb) 'status) 'error))

   (test-case "multiple subscribers on same bus"
     (define bus (make-event-bus))
     (define sb1 (box (hash 'messages '() 'status 'idle)))
     (define sb2 (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb1))
     (subscribe! bus (make-gui-event-subscriber sb2))
     (publish! bus (mk-event "user.input" (hash 'text "multi")))
     (check-equal? (length (hash-ref (unbox sb1) 'messages)) 1)
     (check-equal? (length (hash-ref (unbox sb2) 'messages)) 1))

   (test-case "subscriber survives unknown event on bus"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb))
     (publish! bus (mk-event "weird.event" (hash)))
     (publish! bus (mk-event "user.input" (hash 'text "after")))
     (check-equal? (length (hash-ref (unbox sb) 'messages)) 1))

   (test-case "notify callback not invoked when no state change"
     ;; Unknown event does not change state and should not notify
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (define count 0)
     (define notify-box (box (lambda () (set! count (+ count 1)))))
     (subscribe! bus (make-gui-event-subscriber sb notify-box))
     (publish! bus (mk-event "unknown.tag" (hash)))
     (check-equal? count 0))
   (test-case "thinking then delta sequence"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb))
     (publish! bus (mk-event "model.stream.thinking" (hash 'delta "Analyzing...")))
     (check-equal? (hash-ref (unbox sb) 'status) 'processing)
     (publish! bus (mk-event "model.stream.delta" (hash 'delta "Result")))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (hash-ref (car msgs) 'role) "assistant")
     (check-equal? (hash-ref (car msgs) 'text) "Result"))

   (test-case "turn.completed resets after turn.started"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb))
     (publish! bus (mk-event "turn.started" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'processing)
     (publish! bus (mk-event "turn.completed" (hash)))
     (check-equal? (hash-ref (unbox sb) 'status) 'idle))

   (test-case "multiple stream deltas create single assistant msg"
     (define bus (make-event-bus))
     (define sb (box (hash 'messages '() 'status 'idle)))
     (subscribe! bus (make-gui-event-subscriber sb))
     (for ([i 5])
       (publish! bus (mk-event "model.stream.delta" (hash 'delta (format "~a" i)))))
     (define msgs (hash-ref (unbox sb) 'messages))
     (check-equal? (length msgs) 1)
     (check-equal? (hash-ref (car msgs) 'text) "01234"))
))

(run-tests test-integration-pipeline)
