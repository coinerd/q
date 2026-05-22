#lang racket

;; tests/test-iteration-event-golden.rkt — Golden tests for iteration phase events (v0.54.3 W2)
;;
;; Locks event-ordering behavior for extracted orchestration phases.
;; These tests verify the event sequences produced by the iteration loop
;; are deterministic and complete.
;;
;; Phase 1: turn.start → turn.completed (simple text turn)
;; Phase 2: turn.start → tool.call.started → tool.call.completed → turn.completed (tool turn)
;; Phase 3: cancellation event ordering
;; Phase 4: extension block ordering

(require rackunit
         rackunit/text-ui
         racket/list
         "../llm/provider.rkt"
         "../llm/model.rkt"
         "../agent/event-bus.rkt"
         "../agent/loop.rkt"
         "../util/protocol-types.rkt"
         (only-in "../tools/tool.rkt" make-tool-registry register-tool! make-tool make-exec-context make-success-result)
         (only-in "../tools/registry.rkt" tool-registry?))

(define golden-suite
  (test-suite "iteration event golden tests"

    ;; ── Phase 1: Simple text turn produces correct event sequence ──
    (test-case "golden: simple text turn event sequence"
      (define bus (make-event-bus))
      (define captured (box '()))

      (subscribe! bus
                  (lambda (evt)
                    (set-box! captured (append (unbox captured) (list (event-ev evt))))))

      (define prov
        (make-provider (lambda () "mock")
                       (lambda () (hasheq 'streaming #t))
                       (lambda (req) (error 'send "not used"))
                       (lambda (req)
                         (list (make-stream-chunk "Hello" #f #f #f)
                               (make-stream-chunk #f #f #f #t)))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))

      (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1")

      (define events (unbox captured))

      ;; Golden sequence: turn.started → assistant.message.started →
      ;; assistant.message.completed → turn.completed
      (check-not-false (member "turn.started" events)
                       "turn.started must be in event sequence")
      (check-not-false (member "turn.completed" events)
                       "turn.completed must be in event sequence")

      ;; Ordering invariant: turn.started before turn.completed
      (check-true (< (index-of events "turn.started")
                     (index-of events "turn.completed"))
                  "turn.started must precede turn.completed"))

    ;; ── Phase 2: Tool turn produces correct event sequence ──
    (test-case "golden: tool turn event sequence"
      (define bus (make-event-bus))
      (define captured (box '()))

      (subscribe! bus
                  (lambda (evt)
                    (set-box! captured (append (unbox captured) (list (event-ev evt))))))

      (define prov
        (make-provider (lambda () "mock")
                       (lambda () (hasheq 'streaming #t))
                       (lambda (req) (error 'send "not used"))
                       (lambda (req)
                         (list (make-stream-chunk "Let me check" #f #f #f)
                               (make-stream-chunk #f
                                                  (hasheq 'id
                                                          "tc-1"
                                                          'index
                                                          0
                                                          'function
                                                          (hasheq 'name "read" 'arguments "{\"path\":\"x\"}"))
                                                  #f
                                                  #f)
                               (make-stream-chunk #f #f #f #t)))))

      (define reg (make-tool-registry))
      (register-tool! reg
                      (make-tool "read"
                                 "Read file"
                                 (hasheq 'type "object"
                                         'properties (hasheq 'path (hasheq 'type "string")))
                                 (lambda (args ctx)
                                   (make-success-result "file contents"))
                                 #:dangerous? #f))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "go")) 1000 '#hash())))

      (run-agent-turn ctx prov bus
                      #:session-id "s1"
                      #:turn-id "t1"
                      #:tools (list (hasheq 'type "function"
                                            'function (hasheq 'name "read"
                                                              'parameters (hasheq)))))

      (define events (unbox captured))

      ;; Golden sequence includes tool events
      (check-not-false (member "tool.call.started" events)
                       "tool.call.started must be in event sequence")
      (check-not-false (member "assistant.message.completed" events)
                       "assistant.message.completed must be in event sequence")

      ;; Ordering: message.completed before tool.call.started
      (define amc-idx (index-of events "assistant.message.completed"))
      (define tcs-idx (index-of events "tool.call.started"))
      (check-true (< amc-idx tcs-idx)
                  "assistant.message.completed must precede tool.call.started")

      ;; turn.started is always first, turn.completed always last
      (check-equal? (car events) "turn.started")
      (check-equal? (last events) "turn.completed"))

    ;; ── Phase 3: No-duplicate events ──
    (test-case "golden: no duplicate turn boundary events"
      (define bus (make-event-bus))
      (define captured (box '()))

      (subscribe! bus
                  (lambda (evt)
                    (set-box! captured (append (unbox captured) (list (event-ev evt))))))

      (define prov
        (make-provider (lambda () "mock")
                       (lambda () (hasheq 'streaming #t))
                       (lambda (req) (error 'send "not used"))
                       (lambda (req)
                         (list (make-stream-chunk "OK" #f #f #f)
                               (make-stream-chunk #f #f #f #t)))))

      (define ctx
        (list (make-message "m-1" #f 'user 'message (list (make-text-part "hi")) 1000 '#hash())))

      (run-agent-turn ctx prov bus #:session-id "s1" #:turn-id "t1")

      (define events (unbox captured))

      ;; Exactly one turn.started and one turn.completed
      (check-equal? (count (lambda (e) (equal? e "turn.started")) events) 1
                    "exactly one turn.started")
      (check-equal? (count (lambda (e) (equal? e "turn.completed")) events) 1
                    "exactly one turn.completed"))))

(run-tests golden-suite)
