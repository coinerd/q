#lang racket

;; q/tests/test-gui-event-subscriber-characterization.rkt — Characterization tests
;; for gui/state-sync.rkt event subscriber behavior.
;;
;; W0.2: Lock existing behavior before refactor.
;; Tests verify the GUI event subscriber's response to various event types.

(require rackunit
         rackunit/text-ui
         "../gui/state-sync.rkt"
         "../gui/gui-types.rkt"
         "../util/event/event.rkt")

(define (make-test-event type payload)
  (make-event type (current-inexact-milliseconds) #f #f payload))

(define (make-fresh-state)
  (make-gui-state #:model "test-model"))

(define (collect-messages state)
  (map gui-message-text (gui-state-messages state)))

(define (collect-roles state)
  (map gui-message-role (gui-state-messages state)))

(define-test-suite
 test-gui-event-subscriber-characterization
 (test-case "user.input adds user message to transcript"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "user.input" (hash 'text "hello")))
   (define msgs (gui-state-messages (unbox state-box)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-role (car msgs)) "user")
   (check-equal? (gui-message-text (car msgs)) "hello"))
 (test-case "model.stream.delta accumulates assistant text"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "model.stream.delta" (hash 'delta "Hello ")))
   (subscriber (make-test-event "model.stream.delta" (hash 'delta "world")))
   (define msgs (gui-state-messages (unbox state-box)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-role (car msgs)) "assistant")
   (check-equal? (gui-message-text (car msgs)) "Hello world"))
 (test-case "model.stream.completed resets accumulator and sets idle"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "model.stream.delta" (hash 'delta "partial")))
   (subscriber (make-test-event "model.stream.completed" (hash)))
   (check-equal? (gui-state-status (unbox state-box)) 'idle)
   ;; After completion, the "partial" message still exists; next delta updates it
   (subscriber (make-test-event "model.stream.delta" (hash 'delta "new")))
   (define msgs (gui-state-messages (unbox state-box)))
   ;; CHARACTERIZATION: Only 1 message — the accumulator reset but the subscriber
   ;; updates the existing assistant message (last one) with the new accumulated text
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-text (car msgs)) "new"))
 (test-case "turn.started sets processing status"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "turn.started" (hash)))
   (check-equal? (gui-state-status (unbox state-box)) 'processing))
 (test-case "turn.completed sets idle status"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "turn.started" (hash)))
   (subscriber (make-test-event "turn.completed" (hash)))
   (check-equal? (gui-state-status (unbox state-box)) 'idle))
 (test-case "tool.call.started adds tool message"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "tool.call.started"
                                (hash 'name "bash" 'arguments (hash 'command "ls"))))
   (define msgs (gui-state-messages (unbox state-box)))
   (check-equal? (length msgs) 1)
   (check-equal? (gui-message-role (car msgs)) "tool")
   (check-true (string-contains? (gui-message-text (car msgs)) "bash")))
 (test-case "tool.execution.completed updates matching tool message"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "tool.call.started" (hash 'name "bash" 'arguments #f)))
   (subscriber (make-test-event "tool.execution.completed"
                                (hash 'toolName "bash" 'resultSummary 'completed)))
   (define msgs (gui-state-messages (unbox state-box)))
   (check-equal? (length msgs) 1)
   (check-true (string-contains? (gui-message-text (car msgs)) "OK")))
 (test-case "error events set error status"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "provider.error" (hash 'message "timeout")))
   (check-equal? (gui-state-status (unbox state-box)) 'error))
 (test-case "unknown events are ignored"
   (define state-box (box (make-fresh-state)))
   (define subscriber (make-gui-event-subscriber state-box))
   (subscriber (make-test-event "something.random" (hash 'data "irrelevant")))
   (check-equal? (length (gui-state-messages (unbox state-box))) 0)
   (check-equal? (gui-state-status (unbox state-box)) 'idle))
 (test-case "notify callback is invoked when provided"
   (define state-box (box (make-fresh-state)))
   (define notify-box (box #f))
   (define notify-called (box 0))
   (set-box! notify-box (lambda () (set-box! notify-called (add1 (unbox notify-called)))))
   (define subscriber (make-gui-event-subscriber state-box notify-box))
   (subscriber (make-test-event "user.input" (hash 'text "hi")))
   (check-equal? (unbox notify-called) 1)))

(run-tests test-gui-event-subscriber-characterization)
