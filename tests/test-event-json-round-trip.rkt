#lang racket/base

;; tests/test-event-json-round-trip.rkt -- Round-trip safety for typed-event JSON codec
;; v0.35.0 W3 (W-15, W-21)

(require rackunit
         "../agent/event-json.rkt"
         "../agent/event-structs/turn-events.rkt"
         "../agent/event-structs/message-events.rkt"
         "../agent/event-structs/iteration-events.rkt"
         "../agent/event-structs/provider-events.rkt"
         "../agent/event-structs/session-events.rkt"
         "../agent/event-structs/tool-events.rkt"
         "../agent/event-structs/hook-events.rkt"
         "../agent/event-structs/base.rkt")

(define (round-trip evt)
  (jsexpr->typed-event (typed-event->jsexpr evt)))

(define (check-base-round-trip evt)
  (define rt (round-trip evt))
  (check-equal? (typed-event-type rt) (typed-event-type evt))
  (check-equal? (typed-event-session-id rt) (typed-event-session-id evt))
  (check-equal? (typed-event-turn-id rt) (typed-event-turn-id evt)))

;; Turn events
(test-case "round-trip: turn-start-event"
  (define evt
    (make-turn-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (check-base-round-trip evt)
  (check-equal? (turn-start-event-model (round-trip evt)) "gpt-4"))

(test-case "round-trip: turn-end-event"
  (define evt
    (make-turn-end-event #:session-id "s1" #:turn-id "t1" #:reason "completed" #:duration-ms 1500))
  (check-base-round-trip evt)
  (define rt (round-trip evt))
  (check-equal? (turn-end-event-reason rt) "completed")
  (check-equal? (turn-end-event-duration-ms rt) 1500))

;; Message events
(test-case "round-trip: message-start-event"
  (define evt
    (make-message-start-event #:session-id "s1" #:turn-id "t1" #:role "assistant" #:model "gpt-4"))
  (check-base-round-trip evt)
  (check-equal? (message-start-event-role (round-trip evt)) "assistant"))

(test-case "round-trip: message-update-event"
  (define evt
    (make-message-update-event #:session-id "s1" #:turn-id "t1" #:content "hello" #:delta "hel"))
  (check-base-round-trip evt)
  (check-equal? (message-update-event-content (round-trip evt)) "hello"))

(test-case "round-trip: message-end-event"
  (define evt
    (make-message-end-event #:session-id "s1" #:turn-id "t1" #:role "assistant" #:content-length 42))
  (check-base-round-trip evt)
  (define rt (round-trip evt))
  (check-equal? (message-end-event-role rt) "assistant")
  (check-equal? (message-end-event-content-length rt) 42))

;; Tool execution lifecycle events (from define-typed-event)
(test-case "round-trip: tool-execution-start-event"
  (define evt
    (make-tool-execution-start-event #:session-id "s1"
                                     #:turn-id "t1"
                                     #:tool-name "bash"
                                     #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (tool-execution-start-event-tool-name (round-trip evt)) "bash"))

(test-case "round-trip: tool-execution-update-event"
  (define evt
    (make-tool-execution-update-event #:session-id "s1"
                                      #:turn-id "t1"
                                      #:tool-name "bash"
                                      #:progress "running"))
  (check-base-round-trip evt)
  (check-equal? (tool-execution-update-event-tool-name (round-trip evt)) "bash"))

(test-case "round-trip: tool-execution-end-event"
  (define evt
    (make-tool-execution-end-event #:session-id "s1"
                                   #:turn-id "t1"
                                   #:tool-name "bash"
                                   #:duration-ms 200
                                   #:result-summary "done"))
  (check-base-round-trip evt)
  (check-equal? (tool-execution-end-event-tool-name (round-trip evt)) "bash"))

(test-case "round-trip: tool-call-event"
  (define evt
    (make-tool-call-event #:session-id "s1"
                          #:turn-id "t1"
                          #:tool-name "bash"
                          #:arguments (hasheq 'cmd "ls")
                          #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (tool-call-event-tool-name (round-trip evt)) "bash"))

(test-case "round-trip: tool-result-event"
  (define evt
    (make-tool-result-event #:session-id "s1"
                            #:turn-id "t1"
                            #:tool-call-id "tc1"
                            #:content "output"
                            #:is-error? #f))
  (check-base-round-trip evt)
  (check-equal? (tool-result-event-tool-call-id (round-trip evt)) "tc1"))

;; Per-tool events (manual constructors -- require #:timestamp and #:tool-call-id)
(test-case "round-trip: bash-tool-call-event"
  (define evt
    (make-bash-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 100
                               #:command "ls -la"
                               #:timeout 30
                               #:cwd "/tmp"
                               #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (bash-tool-call-event-command (round-trip evt)) "ls -la"))

(test-case "round-trip: edit-tool-call-event"
  (define evt
    (make-edit-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 100
                               #:path "foo.rkt"
                               #:edits '(("old" "new"))
                               #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (edit-tool-call-event-path (round-trip evt)) "foo.rkt"))

(test-case "round-trip: write-tool-call-event"
  (define evt
    (make-write-tool-call-event #:session-id "s1"
                                #:turn-id "t1"
                                #:timestamp 100
                                #:path "out.rkt"
                                #:content "(module ...)"
                                #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (write-tool-call-event-path (round-trip evt)) "out.rkt"))

(test-case "round-trip: read-tool-call-event"
  (define evt
    (make-read-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 100
                               #:path "in.rkt"
                               #:offset 10
                               #:limit 50
                               #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (read-tool-call-event-path (round-trip evt)) "in.rkt"))

(test-case "round-trip: grep-tool-call-event"
  (define evt
    (make-grep-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 100
                               #:pattern "TODO"
                               #:path "src"
                               #:glob "*.rkt"
                               #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (grep-tool-call-event-pattern (round-trip evt)) "TODO"))

(test-case "round-trip: find-tool-call-event"
  (define evt
    (make-find-tool-call-event #:session-id "s1"
                               #:turn-id "t1"
                               #:timestamp 100
                               #:pattern "test"
                               #:path "."
                               #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (find-tool-call-event-pattern (round-trip evt)) "test"))

(test-case "round-trip: custom-tool-call-event"
  (define evt
    (make-custom-tool-call-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:timestamp 100
                                 #:tool-name "mytool"
                                 #:arguments (hasheq 'x 1)
                                 #:tool-call-id "tc1"))
  (check-base-round-trip evt)
  (check-equal? (tool-call-event-tool-name (round-trip evt)) "mytool"))

;; Provider events
(test-case "round-trip: provider-request-event"
  (define evt
    (make-provider-request-event #:session-id "s1"
                                 #:turn-id "t1"
                                 #:model "gpt-4"
                                 #:provider "openai"))
  (check-base-round-trip evt)
  (check-equal? (provider-request-event-model (round-trip evt)) "gpt-4"))

(test-case "round-trip: provider-response-event"
  (define evt
    (make-provider-response-event #:session-id "s1"
                                  #:turn-id "t1"
                                  #:model "gpt-4"
                                  #:provider "openai"
                                  #:latency-ms 250))
  (check-base-round-trip evt)
  (define rt (round-trip evt))
  (check-equal? (provider-response-event-model rt) "gpt-4")
  (check-equal? (provider-response-event-latency-ms rt) 250))

(test-case "round-trip: model-stream-delta-event"
  (define evt
    (make-model-stream-delta-event #:session-id "s1" #:turn-id "t1" #:delta "hello" #:model "gpt-4"))
  (check-base-round-trip evt)
  (check-equal? (model-stream-delta-event-delta (round-trip evt)) "hello"))

(test-case "round-trip: model-stream-thinking-event"
  (define evt
    (make-model-stream-thinking-event #:session-id "s1"
                                      #:turn-id "t1"
                                      #:thinking "hmm"
                                      #:model "gpt-4"))
  (check-base-round-trip evt)
  (check-equal? (model-stream-thinking-event-thinking (round-trip evt)) "hmm"))

(test-case "round-trip: model-stream-completed-event"
  (define evt
    (make-model-stream-completed-event #:session-id "s1"
                                       #:turn-id "t1"
                                       #:model "gpt-4"
                                       #:provider "openai"))
  (check-base-round-trip evt)
  (check-equal? (model-stream-completed-event-model (round-trip evt)) "gpt-4"))

;; Session events
(test-case "round-trip: session-start-event"
  (define evt (make-session-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4"))
  (check-base-round-trip evt)
  (check-equal? (session-start-event-model (round-trip evt)) "gpt-4"))

(test-case "round-trip: session-shutdown-event"
  (define evt (make-session-shutdown-event #:session-id "s1" #:turn-id "t1" #:reason "complete"))
  (check-base-round-trip evt)
  (check-equal? (session-shutdown-event-reason (round-trip evt)) "complete"))

(test-case "round-trip: input-event"
  (define evt
    (make-input-event #:session-id "s1" #:turn-id "t1" #:input-type "text" #:content "hello"))
  (check-base-round-trip evt)
  (check-equal? (input-event-input-type (round-trip evt)) "text"))

(test-case "round-trip: model-select-event"
  (define evt
    (make-model-select-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4" #:provider "openai"))
  (check-base-round-trip evt)
  (check-equal? (model-select-event-model (round-trip evt)) "gpt-4"))

(test-case "round-trip: agent-start-event"
  (define evt (make-agent-start-event #:session-id "s1" #:turn-id "t1" #:model "gpt-4"))
  (check-base-round-trip evt)
  (check-equal? (agent-start-event-model (round-trip evt)) "gpt-4"))

(test-case "round-trip: agent-end-event"
  (define evt
    (make-agent-end-event #:session-id "s1" #:turn-id "t1" #:reason "done" #:duration-ms 3000))
  (check-base-round-trip evt)
  (define rt (round-trip evt))
  (check-equal? (agent-end-event-reason rt) "done")
  (check-equal? (agent-end-event-duration-ms rt) 3000))

(test-case "round-trip: context-event"
  (define evt
    (make-context-event #:session-id "s1" #:turn-id "t1" #:token-count 5000 #:window-size 128000))
  (check-base-round-trip evt)
  (check-equal? (context-event-token-count (round-trip evt)) 5000))

;; Hook events
(test-case "round-trip: model-request-blocked-event"
  (define evt (make-model-request-blocked-event #:session-id "s1" #:turn-id "t1" #:reason "quota"))
  (check-base-round-trip evt)
  (check-equal? (model-request-blocked-event-reason (round-trip evt)) "quota"))

(test-case "round-trip: message-blocked-event"
  (define evt
    (make-message-blocked-event #:session-id "s1" #:turn-id "t1" #:hook "pre-send" #:reason "filter"))
  (check-base-round-trip evt)
  (check-equal? (message-blocked-event-hook (round-trip evt)) "pre-send"))

(test-case "round-trip: turn-cancelled-event (via hash)"
  ;; Pre-existing bug: make-turn-cancelled-event has arity mismatch
  ;; Test round-trip from raw hash instead (W-21 partial coverage)
  (define h
    (hasheq 'type "turn.cancelled" 'timestamp 100 'sessionId "s1" 'turnId "t1" 'reason "timeout"))
  (define rt (jsexpr->typed-event h))
  (check-equal? (typed-event-type rt) "turn.cancelled")
  (check-equal? (typed-event-session-id rt) "s1"))

(test-case "round-trip: assistant-message-completed-event"
  (define evt
    (make-assistant-message-completed-event #:session-id "s1" #:turn-id "t1" #:content-length 100))
  (check-base-round-trip evt)
  (check-equal? (assistant-message-completed-event-content-length (round-trip evt)) 100))

;; Iteration events (not in dispatch-deserialize -- round-trip preserves type but loses fields)
(test-case "round-trip: auto-retry-event"
  (define evt
    (make-auto-retry-event #:session-id "s1"
                           #:turn-id "t1"
                           #:attempt 2
                           #:max-attempts 3
                           #:error-type "timeout"))
  (check-base-round-trip evt)
  (check-equal? (typed-event-type (round-trip evt)) (typed-event-type evt)))

(test-case "round-trip: compaction-event"
  (define evt
    (make-compaction-event #:session-id "s1"
                           #:turn-id "t1"
                           #:reason "budget"
                           #:tokens-before 1000
                           #:tokens-after 500))
  (check-base-round-trip evt)
  (check-equal? (typed-event-type (round-trip evt)) (typed-event-type evt)))

(test-case "round-trip: injection-event"
  (define evt
    (make-injection-event #:session-id "s1"
                          #:turn-id "t1"
                          #:source "context"
                          #:content-type "text"
                          #:content-length 42))
  (check-base-round-trip evt)
  (check-equal? (typed-event-type (round-trip evt)) (typed-event-type evt)))

;; Registry completeness
(test-case "all-known-event-types returns non-empty list"
  (define types (all-known-event-types))
  (check-true (list? types))
  (check > (length types) 25))

(test-case "unknown type round-trips as base typed-event"
  (define h (hasheq 'type "bogus.event" 'timestamp 100 'sessionId "s1" 'turnId "t1"))
  (define rt (jsexpr->typed-event h))
  (check-equal? (typed-event-type rt) "bogus.event")
  (check-equal? (typed-event-session-id rt) "s1"))
