#lang racket

;; tests/test-loop-events.rkt — Agent loop typed event emission (#1323)
;;
;; Tests that the agent loop emits typed event structs on the event bus.

(require rackunit
         rackunit/text-ui
         "../agent/event-bus.rkt"
         "../agent/event-types.rkt"
         "../util/protocol-types.rkt")

(define loop-events-suite
  (test-suite "loop-events: typed event emission"

    (test-case "typed-event structs are constructable"
      (define tse (turn-start-event "turn-start" 1000.0 "s1" "t1" "model-a" "provider-b"))
      (check-equal? (typed-event-type tse) "turn-start")
      (check-equal? (typed-event-session-id tse) "s1")
      (check-equal? (turn-start-event-model tse) "model-a")
      (check-equal? (turn-start-event-provider tse) "provider-b"))

    (test-case "message events have content fields"
      (define mse (message-start-event "message-start" 1000.0 "s1" "t1" 'assistant "model-a"))
      (check-equal? (message-start-event-role mse) 'assistant)
      (define mue (message-update-event "message-update" 1001.0 "s1" "t1" 'assistant "hello"))
      (check-equal? (message-update-event-delta mue) "hello")
      (define mee (message-end-event "message-end" 1002.0 "s1" "t1" 'assistant "stop"))
      (check-equal? (message-end-event-role mee) 'assistant))

    (test-case "tool execution events have tool-name fields"
      (define tese (tool-execution-start-event "tool-start" 1000.0 "s1" "t1" "bash" "tc1"))
      (check-equal? (tool-execution-start-event-tool-name tese) "bash")
      (define teue (tool-execution-update-event "tool-update" 1001.0 "s1" "t1" "bash" "running..."))
      (check-equal? (tool-execution-update-event-progress teue) "running...")
      (define teee (tool-execution-end-event "tool-end" 1002.0 "s1" "t1" "bash" 150 'done))
      (check-equal? (tool-execution-end-event-result-summary teee) 'done))

    (test-case "typed events can be published on event bus"
      (define bus (make-event-bus))
      (define received '())
      (subscribe! bus (lambda (evt) (set! received (cons evt received))))
      ;; Publish a raw event with typed-event as payload
      (define tse (turn-start-event "turn-start" 1000.0 "s1" "t1" "model-a" "prov"))
      (publish! bus (make-event "turn.start" 1000.0 "s1" "t1" tse))
      (check-equal? (length received) 1)
      (define payload (event-payload (car received)))
      (check-true (turn-start-event? payload))
      (check-equal? (turn-start-event-model payload) "model-a"))

    (test-case "per-tool typed events are constructable"
      (define b (bash-tool-call-event "tc" 1.0 "s" "t" "bash" (hasheq) "tc1" "bash cmd" 30 "/tmp"))
      (check-true (bash-tool-call-event? b))
      (check-true (tool-call-event? b))
      (check-equal? (bash-tool-call-event-command b) "bash cmd")

      (define e (edit-tool-call-event "tc2" 1.0 "s" "t" "edit" (hasheq) "tc2" "/foo.rkt" '()))
      (check-true (edit-tool-call-event? e))
      (check-equal? (edit-tool-call-event-path e) "/foo.rkt")

      (define w (write-tool-call-event "tc3" 1.0 "s" "t" "write" (hasheq) "tc3" "/bar.rkt" "content"))
      (check-true (write-tool-call-event? w))

      (define r (read-tool-call-event "tc4" 1.0 "s" "t" "read" (hasheq) "tc4" "/baz.rkt" 0 100))
      (check-true (read-tool-call-event? r))

      (define g
        (grep-tool-call-event "tc5" 1.0 "s" "t" "grep" (hasheq) "tc5" "pattern" "/src" "*.rkt"))
      (check-true (grep-tool-call-event? g))

      (define f (find-tool-call-event "tc6" 1.0 "s" "t" "find" (hasheq) "tc6" "*.rkt" "/src"))
      (check-true (find-tool-call-event? f))

      (define c (custom-tool-call-event "tc7" 1.0 "s" "t" "my-tool" (hasheq) "tc7"))
      (check-true (custom-tool-call-event? c))
      (check-equal? (tool-call-event-tool-name c) "my-tool"))))

(run-tests loop-events-suite)
