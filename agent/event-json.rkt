#lang racket/base

;; agent/event-json.rkt — JSON serialization for typed events + registry
;;
;; Extracted from event-types.rkt. Contains:
;;   - typed-event->jsexpr / jsexpr->typed-event
;;   - event-extra-fields (serialization helpers)
;;   - dispatch-deserialize (type-tagged match dispatch)
;;   - all-known-event-types / event-name->tool-name (registry)

(require racket/match
         "../util/typed-event-predicates.rkt")

(provide typed-event->jsexpr
         jsexpr->typed-event
         all-known-event-types
         event-name->tool-name)

;; ============================================================
;; JSON Serialization
;; ============================================================

;; typed-event->jsexpr : typed-event? -> hash?
(define (typed-event->jsexpr evt)
  (define base
    `#hasheq((type . ,(typed-event-type evt))
             (timestamp . ,(typed-event-timestamp evt))
             (sessionId . ,(typed-event-session-id evt))
             (turnId . ,(typed-event-turn-id evt))))
  (define extra (event-extra-fields evt))
  (for/fold ([h base]) ([(k v) (in-hash extra)])
    (hash-set h k v)))

;; jsexpr->typed-event : hash? -> typed-event?
(define (jsexpr->typed-event h)
  (define type (hash-ref h 'type #f))
  (define ts (hash-ref h 'timestamp 0))
  (define sid (hash-ref h 'sessionId ""))
  (define tid (hash-ref h 'turnId #f))
  (dispatch-deserialize type ts sid tid h))

;; ============================================================
;; Serialization helpers
;; ============================================================

;; Collect extra fields for each concrete event type
(define (event-extra-fields evt)
  (match evt
    [(? turn-start-event?)
     (hasheq 'model (turn-start-event-model evt) 'provider (turn-start-event-provider evt))]
    [(? turn-end-event?)
     (hasheq 'reason (turn-end-event-reason evt) 'durationMs (turn-end-event-duration-ms evt))]
    [(? message-start-event?)
     (hasheq 'role (message-start-event-role evt) 'model (message-start-event-model evt))]
    [(? message-update-event?)
     (hasheq 'content (message-update-event-content evt) 'delta (message-update-event-delta evt))]
    [(? message-end-event?)
     (hasheq 'role
             (message-end-event-role evt)
             'contentLength
             (message-end-event-content-length evt))]
    [(? tool-execution-start-event?)
     (hasheq 'toolName
             (tool-execution-start-event-tool-name evt)
             'toolCallId
             (tool-execution-start-event-tool-call-id evt))]
    [(? tool-execution-update-event?)
     (hasheq 'toolName
             (tool-execution-update-event-tool-name evt)
             'progress
             (tool-execution-update-event-progress evt))]
    [(? tool-execution-end-event?)
     (hasheq 'toolName
             (tool-execution-end-event-tool-name evt)
             'durationMs
             (tool-execution-end-event-duration-ms evt)
             'resultSummary
             (tool-execution-end-event-result-summary evt))]
    [(? bash-tool-call-event?)
     (hasheq 'toolName
             "bash"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'command
             (bash-tool-call-event-command evt)
             'timeout
             (bash-tool-call-event-timeout evt)
             'cwd
             (bash-tool-call-event-cwd evt))]
    [(? edit-tool-call-event?)
     (hasheq 'toolName
             "edit"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (edit-tool-call-event-path evt)
             'edits
             (edit-tool-call-event-edits evt))]
    [(? write-tool-call-event?)
     (hasheq 'toolName
             "write"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (write-tool-call-event-path evt)
             'content
             (write-tool-call-event-content evt))]
    [(? read-tool-call-event?)
     (hasheq 'toolName
             "read"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (read-tool-call-event-path evt)
             'offset
             (read-tool-call-event-offset evt)
             'limit
             (read-tool-call-event-limit evt))]
    [(? grep-tool-call-event?)
     (hasheq 'toolName
             "grep"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'pattern
             (grep-tool-call-event-pattern evt)
             'path
             (grep-tool-call-event-path evt)
             'glob
             (grep-tool-call-event-glob evt))]
    [(? find-tool-call-event?)
     (hasheq 'toolName
             "find"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'pattern
             (find-tool-call-event-pattern evt)
             'path
             (find-tool-call-event-path evt))]
    [(? custom-tool-call-event?)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'arguments
             (tool-call-event-arguments evt))]
    [(? tool-call-event?)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'arguments
             (tool-call-event-arguments evt)
             'toolCallId
             (tool-call-event-tool-call-id evt))]
    [(? tool-result-event?)
     (hasheq 'toolCallId
             (tool-result-event-tool-call-id evt)
             'content
             (tool-result-event-content evt)
             'isError
             (tool-result-event-is-error? evt))]
    [(? provider-request-event?)
     (hasheq 'model
             (provider-request-event-model evt)
             'provider
             (provider-request-event-provider evt))]
    [(? provider-response-event?)
     (hasheq 'model
             (provider-response-event-model evt)
             'provider
             (provider-response-event-provider evt)
             'latencyMs
             (provider-response-event-latency-ms evt))]
    ;; Streaming events
    [(? model-stream-delta-event?)
     (hasheq 'delta (model-stream-delta-event-delta evt) 'model (model-stream-delta-event-model evt))]
    [(? model-stream-thinking-event?)
     (hasheq 'thinking
             (model-stream-thinking-event-thinking evt)
             'model
             (model-stream-thinking-event-model evt))]
    [(? model-stream-completed-event?)
     (hasheq 'model
             (model-stream-completed-event-model evt)
             'provider
             (model-stream-completed-event-provider evt))]
    ;; Blocked events
    [(? model-request-blocked-event?) (hasheq 'reason (model-request-blocked-event-reason evt))]
    [(? message-blocked-event?)
     (hasheq 'hook (message-blocked-event-hook evt) 'reason (message-blocked-event-reason evt))]
    [(? turn-cancelled-event?) (hasheq 'reason (turn-cancelled-event-reason evt))]
    [(? assistant-message-completed-event?)
     (hasheq 'contentLength (assistant-message-completed-event-content-length evt))]
    [(? session-start-event?) (hasheq 'model (session-start-event-model evt))]
    [(? session-shutdown-event?) (hasheq 'reason (session-shutdown-event-reason evt))]
    [(? input-event?)
     (hasheq 'inputType (input-event-input-type evt) 'content (input-event-content evt))]
    [(? model-select-event?)
     (hasheq 'model (model-select-event-model evt) 'provider (model-select-event-provider evt))]
    [(? agent-start-event?) (hasheq 'model (agent-start-event-model evt))]
    [(? agent-end-event?)
     (hasheq 'reason (agent-end-event-reason evt) 'durationMs (agent-end-event-duration-ms evt))]
    [(? context-event?)
     (hasheq 'tokenCount (context-event-token-count evt) 'windowSize (context-event-window-size evt))]
    [_ (hasheq)]))

;; Dispatch deserialization by type string
(define (dispatch-deserialize type ts sid tid h)
  (match type
    ["turn.started"
     (turn-start-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["turn.completed"
     (turn-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["message.started"
     (message-start-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'model ""))]
    ["message.updated"
     (message-update-event type ts sid tid (hash-ref h 'content "") (hash-ref h 'delta ""))]
    ["message.completed"
     (message-end-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'contentLength 0))]
    ["tool.execution.started"
     (tool-execution-start-event type
                                 ts
                                 sid
                                 tid
                                 (hash-ref h 'toolName "")
                                 (hash-ref h 'toolCallId ""))]
    ["tool.execution.updated"
     (tool-execution-update-event type
                                  ts
                                  sid
                                  tid
                                  (hash-ref h 'toolName "")
                                  (hash-ref h 'progress ""))]
    ["tool.execution.completed"
     (tool-execution-end-event type
                               ts
                               sid
                               tid
                               (hash-ref h 'toolName "")
                               (hash-ref h 'durationMs 0)
                               (hash-ref h 'resultSummary ""))]
    ["tool.bash.called"
     (bash-tool-call-event type
                           ts
                           sid
                           tid
                           "bash"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'command "")
                           (hash-ref h 'timeout 30)
                           (hash-ref h 'cwd ""))]
    ["tool.edit.called"
     (edit-tool-call-event type
                           ts
                           sid
                           tid
                           "edit"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'path "")
                           (hash-ref h 'edits '()))]
    ["tool.write.called"
     (write-tool-call-event type
                            ts
                            sid
                            tid
                            "write"
                            (hasheq)
                            (hash-ref h 'toolCallId "")
                            (hash-ref h 'path "")
                            (hash-ref h 'content ""))]
    ["tool.read.called"
     (read-tool-call-event type
                           ts
                           sid
                           tid
                           "read"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'path "")
                           (hash-ref h 'offset #f)
                           (hash-ref h 'limit #f))]
    ["tool.grep.called"
     (grep-tool-call-event type
                           ts
                           sid
                           tid
                           "grep"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'pattern "")
                           (hash-ref h 'path "")
                           (hash-ref h 'glob ""))]
    ["tool.find.called"
     (find-tool-call-event type
                           ts
                           sid
                           tid
                           "find"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'pattern "")
                           (hash-ref h 'path ""))]
    ["tool.custom.called"
     (custom-tool-call-event type
                             ts
                             sid
                             tid
                             (hash-ref h 'toolName "unknown")
                             (hash-ref h 'arguments (hasheq))
                             (hash-ref h 'toolCallId ""))]
    ["tool.called"
     (tool-call-event type
                      ts
                      sid
                      tid
                      (hash-ref h 'toolName "")
                      (hash-ref h 'arguments (hasheq))
                      (hash-ref h 'toolCallId ""))]
    ["tool.result"
     (tool-result-event type
                        ts
                        sid
                        tid
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'content "")
                        (hash-ref h 'isError #f))]
    ["model.request.started"
     (provider-request-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["model.request.completed"
     (provider-response-event type
                              ts
                              sid
                              tid
                              (hash-ref h 'model "")
                              (hash-ref h 'provider "")
                              (hash-ref h 'latencyMs 0))]
    ["session.started" (session-start-event type ts sid tid (hash-ref h 'model ""))]
    ["session.shutdown" (session-shutdown-event type ts sid tid (hash-ref h 'reason ""))]
    ["input" (input-event type ts sid tid (hash-ref h 'inputType "") (hash-ref h 'content ""))]
    ["model.selected"
     (model-select-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["agent.started" (agent-start-event type ts sid tid (hash-ref h 'model ""))]
    ["agent.completed"
     (agent-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["context.built"
     (context-event type ts sid tid (hash-ref h 'tokenCount 0) (hash-ref h 'windowSize 0))]
    ;; Streaming events
    ["model.stream.delta"
     (model-stream-delta-event type ts sid tid (hash-ref h 'delta "") (hash-ref h 'model ""))]
    ["model.stream.thinking"
     (model-stream-thinking-event type ts sid tid (hash-ref h 'thinking "") (hash-ref h 'model ""))]
    ["model.stream.completed"
     (model-stream-completed-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ;; Blocked events
    ["model.request.blocked" (model-request-blocked-event type ts sid tid (hash-ref h 'reason ""))]
    ["message.blocked"
     (message-blocked-event type ts sid tid (hash-ref h 'hook "") (hash-ref h 'reason ""))]
    ["turn.cancelled" (turn-cancelled-event type ts sid tid (hash-ref h 'reason ""))]
    ["assistant.message.completed"
     (assistant-message-completed-event type ts sid tid (hash-ref h 'contentLength 0))]
    [_ (typed-event type ts sid tid)]))

;; ============================================================
;; Registry
;; ============================================================

(define (all-known-event-types)
  '("turn.started" "turn.completed"
                   "message.started"
                   "message.updated"
                   "message.completed"
                   "tool.execution.started"
                   "tool.execution.updated"
                   "tool.execution.completed"
                   "tool.called"
                   "tool.result"
                   "tool.bash.called"
                   "tool.edit.called"
                   "tool.write.called"
                   "tool.read.called"
                   "tool.grep.called"
                   "tool.find.called"
                   "tool.custom.called"
                   "model.request.started"
                   "model.request.completed"
                   "session.started"
                   "session.shutdown"
                   "input"
                   "model.selected"
                   "agent.started"
                   "agent.completed"
                   "context.built"
                   "model.stream.delta"
                   "model.stream.thinking"
                   "model.stream.completed"
                   "model.request.blocked"
                   "message.blocked"
                   "turn.cancelled"
                   "assistant.message.completed"))

(define (event-name->tool-name type)
  (match type
    ["tool.bash.called" "bash"]
    ["tool.edit.called" "edit"]
    ["tool.write.called" "write"]
    ["tool.read.called" "read"]
    ["tool.grep.called" "grep"]
    ["tool.find.called" "find"]
    [_ #f]))
