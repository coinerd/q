#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; BOUNDARY: serialization

;; tests/test-event-serializer-fast-path.rkt — R-13: Verify serializer registry fast path

(require rackunit
         rackunit/text-ui
         "../agent/event-emitter.rkt"
         "../agent/event-structs/base.rkt"
         (only-in "../agent/event-structs/tool-events.rkt"
                  make-tool-execution-start-event
                  tool-execution-start-event?)
         "../util/event/event-macro.rkt")

(define fast-path-suite
  (test-suite "Event serializer fast path tests (R-13)"

    (test-case "event-struct->hasheq uses serializer registry for typed events"
      (define evt
        (make-tool-execution-start-event #:session-id "test-session"
                                         #:turn-id "test-turn"
                                         #:tool-name "bash"
                                         #:tool-call-id "tc-1"))
      (define result (event-struct->hasheq evt))
      ;; Base fields present
      (check-equal? (hash-ref result 'type) "tool.execution.started")
      (check-equal? (hash-ref result 'session-id) "test-session")
      (check-equal? (hash-ref result 'turn-id) "test-turn")
      ;; Subclass fields present
      (check-equal? (hash-ref result 'toolName) "bash")
      (check-equal? (hash-ref result 'toolCallId) "tc-1"))

    (test-case "serializer lookup succeeds for registered types"
      (define ser (lookup-event-serializer "tool.execution.started"))
      (check-not-false ser "serializer should be registered for tool.execution.started"))

    (test-case "serializer lookup returns #f for unregistered types"
      (define ser (lookup-event-serializer "nonexistent.event.type"))
      (check-false ser))

    (test-case "event-struct->hasheq fallback works for unregistered events"
      ;; Create a minimal typed-event that has no serializer
      (define evt (typed-event "test.unregistered" (current-inexact-milliseconds) "s1" "t1"))
      (define result (event-struct->hasheq evt))
      ;; Base fields still present
      (check-equal? (hash-ref result 'type) "test.unregistered")
      (check-equal? (hash-ref result 'session-id) "s1"))))

(run-tests fast-path-suite 'verbose)
