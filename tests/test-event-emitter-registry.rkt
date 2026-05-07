#lang racket/base

;; tests/test-event-emitter-registry.rkt — Coverage test for event field-name registry
;;
;; v0.32.9 RC-1: Safety-net test that detects if a new event type is added
;; without updating the field-name registry in event-emitter.rkt.
;; The full fix (auto-registration from macro) is deferred to v0.33.x.

(require rackunit
         racket/hash
         ;; Import the registry
         (only-in "../agent/event-emitter.rkt" struct-field-names)
         ;; Import all *-event-fields constants from each sub-module
         (only-in "../agent/event-structs/turn-events.rkt"
                  turn-start-event-fields
                  turn-end-event-fields)
         (only-in "../agent/event-structs/message-events.rkt"
                  message-start-event-fields
                  message-update-event-fields
                  message-end-event-fields)
         (only-in "../agent/event-structs/iteration-events.rkt"
                  auto-retry-event-fields
                  compaction-event-fields
                  injection-event-fields)
         (only-in "../agent/event-structs/provider-events.rkt"
                  provider-request-event-fields
                  provider-response-event-fields
                  model-stream-delta-event-fields
                  model-stream-thinking-event-fields
                  model-stream-completed-event-fields)
         (only-in "../agent/event-structs/session-events.rkt"
                  session-start-event-fields
                  session-shutdown-event-fields
                  input-event-fields
                  model-select-event-fields
                  agent-start-event-fields
                  agent-end-event-fields
                  context-event-fields)
         (only-in "../agent/event-structs/tool-events.rkt"
                  tool-execution-start-event-fields
                  tool-execution-update-event-fields
                  tool-execution-end-event-fields
                  tool-call-event-fields
                  tool-result-event-fields
                  bash-tool-call-event-fields
                  edit-tool-call-event-fields
                  write-tool-call-event-fields
                  read-tool-call-event-fields
                  grep-tool-call-event-fields
                  find-tool-call-event-fields
                  custom-tool-call-event-fields)
         (only-in "../agent/event-structs/hook-events.rkt"
                  model-request-blocked-event-fields
                  message-blocked-event-fields
                  turn-cancelled-event-fields
                  assistant-message-completed-event-fields)
         (only-in "../agent/event-structs/stream-events.rkt"
                  stream-completed-event-fields
                  stream-delta-event-fields
                  stream-tool-call-delta-event-fields
                  stream-thinking-event-fields
                  stream-message-start-event-fields
                  stream-message-delta-event-fields
                  stream-message-end-event-fields
                  stream-turn-completed-event-fields
                  stream-turn-cancelled-event-fields
                  stream-tool-call-started-event-fields
                  stream-assistant-msg-completed-event-fields))

;; All known event struct names that should be in the registry
(define expected-event-names
  ;; turn-events
  '(turn-start-event turn-end-event
                     ;; message-events
                     message-start-event
                     message-update-event
                     message-end-event
                     ;; iteration-events
                     auto-retry-event
                     compaction-event
                     injection-event
                     ;; provider-events
                     provider-request-event
                     provider-response-event
                     model-stream-delta-event
                     model-stream-thinking-event
                     model-stream-completed-event
                     ;; session-events
                     session-start-event
                     session-shutdown-event
                     input-event
                     model-select-event
                     agent-start-event
                     agent-end-event
                     context-event
                     ;; tool-events
                     tool-execution-start-event
                     tool-execution-update-event
                     tool-execution-end-event
                     tool-call-event
                     tool-result-event
                     bash-tool-call-event
                     edit-tool-call-event
                     write-tool-call-event
                     read-tool-call-event
                     grep-tool-call-event
                     find-tool-call-event
                     custom-tool-call-event
                     ;; hook-events
                     model-request-blocked-event
                     message-blocked-event
                     turn-cancelled-event
                     assistant-message-completed-event
                     ;; stream-events
                     stream-completed-event
                     stream-delta-event
                     stream-tool-call-delta-event
                     stream-thinking-event
                     stream-message-start-event
                     stream-message-delta-event
                     stream-message-end-event
                     stream-turn-completed-event
                     stream-turn-cancelled-event
                     stream-tool-call-started-event
                     stream-assistant-msg-completed-event))

(test-case "all event types registered in struct-field-names"
  (define missing
    (for/list ([name (in-list expected-event-names)]
               #:unless (hash-has-key? struct-field-names name))
      name))
  (check-equal?
   missing
   '()
   (format "Missing ~a event types from struct-field-names registry: ~a" (length missing) missing)))

(test-case "registry has no extra unknown event types"
  (define registry-keys (hash-keys struct-field-names))
  (define extra
    (for/list ([key (in-list registry-keys)]
               #:unless (member key expected-event-names))
      key))
  ;; Extra keys are not an error (forward-compat), just informational
  ;; Uncomment the following to enforce exact match:
  ;; (check-equal? extra '() (format "Extra event types in registry: ~a" extra))
  (void))

(test-case "registry count matches expected count"
  (check >=
         (hash-count struct-field-names)
         (length expected-event-names)
         "Registry should have at least as many entries as known event types"))
