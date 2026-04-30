#lang racket/base

;; agent/event-json.rkt — JSON serialization for typed events + registry
;;
;; Extracted from event-types.rkt. Contains:
;;   - typed-event->jsexpr / jsexpr->typed-event
;;   - event-extra-fields (serialization helpers)
;;   - dispatch-deserialize (type-tagged match dispatch)
;;   - all-known-event-types / event-name->tool-name (registry)

(require racket/match
         "event-structs.rkt")

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
  (cond
    [(turn-start-event? evt)
     (hasheq 'model (turn-start-event-model evt) 'provider (turn-start-event-provider evt))]
    [(turn-end-event? evt)
     (hasheq 'reason (turn-end-event-reason evt) 'durationMs (turn-end-event-duration-ms evt))]
    [(message-start-event? evt)
     (hasheq 'role (message-start-event-role evt) 'model (message-start-event-model evt))]
    [(message-update-event? evt)
     (hasheq 'content (message-update-event-content evt) 'delta (message-update-event-delta evt))]
    [(message-end-event? evt)
     (hasheq 'role
             (message-end-event-role evt)
             'contentLength
             (message-end-event-content-length evt))]
    [(tool-execution-start-event? evt)
     (hasheq 'toolName
             (tool-execution-start-event-tool-name evt)
             'toolCallId
             (tool-execution-start-event-tool-call-id evt))]
    [(tool-execution-update-event? evt)
     (hasheq 'toolName
             (tool-execution-update-event-tool-name evt)
             'progress
             (tool-execution-update-event-progress evt))]
    [(tool-execution-end-event? evt)
     (hasheq 'toolName
             (tool-execution-end-event-tool-name evt)
             'durationMs
             (tool-execution-end-event-duration-ms evt)
             'resultSummary
             (tool-execution-end-event-result-summary evt))]
    [(bash-tool-call-event? evt)
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
    [(edit-tool-call-event? evt)
     (hasheq 'toolName
             "edit"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (edit-tool-call-event-path evt)
             'edits
             (edit-tool-call-event-edits evt))]
    [(write-tool-call-event? evt)
     (hasheq 'toolName
             "write"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'path
             (write-tool-call-event-path evt)
             'content
             (write-tool-call-event-content evt))]
    [(read-tool-call-event? evt)
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
    [(grep-tool-call-event? evt)
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
    [(find-tool-call-event? evt)
     (hasheq 'toolName
             "find"
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'pattern
             (find-tool-call-event-pattern evt)
             'path
             (find-tool-call-event-path evt))]
    [(custom-tool-call-event? evt)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'toolCallId
             (tool-call-event-tool-call-id evt)
             'arguments
             (tool-call-event-arguments evt))]
    [(tool-call-event? evt)
     (hasheq 'toolName
             (tool-call-event-tool-name evt)
             'arguments
             (tool-call-event-arguments evt)
             'toolCallId
             (tool-call-event-tool-call-id evt))]
    [(tool-result-event? evt)
     (hasheq 'toolCallId
             (tool-result-event-tool-call-id evt)
             'content
             (tool-result-event-content evt)
             'isError
             (tool-result-event-is-error? evt))]
    [(provider-request-event? evt)
     (hasheq 'model
             (provider-request-event-model evt)
             'provider
             (provider-request-event-provider evt))]
    [(provider-response-event? evt)
     (hasheq 'model
             (provider-response-event-model evt)
             'provider
             (provider-response-event-provider evt)
             'latencyMs
             (provider-response-event-latency-ms evt))]
    [(session-start-event? evt) (hasheq 'model (session-start-event-model evt))]
    [(session-shutdown-event? evt) (hasheq 'reason (session-shutdown-event-reason evt))]
    [(input-event? evt)
     (hasheq 'inputType (input-event-input-type evt) 'content (input-event-content evt))]
    [(model-select-event? evt)
     (hasheq 'model (model-select-event-model evt) 'provider (model-select-event-provider evt))]
    [(agent-start-event? evt) (hasheq 'model (agent-start-event-model evt))]
    [(agent-end-event? evt)
     (hasheq 'reason (agent-end-event-reason evt) 'durationMs (agent-end-event-duration-ms evt))]
    [(context-event? evt)
     (hasheq 'tokenCount (context-event-token-count evt) 'windowSize (context-event-window-size evt))]
    [else (hasheq)]))

;; Dispatch deserialization by type string
(define (dispatch-deserialize type ts sid tid h)
  (match type
    ["turn-start" (turn-start-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["turn-end" (turn-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["message-start"
     (message-start-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'model ""))]
    ["message-update"
     (message-update-event type ts sid tid (hash-ref h 'content "") (hash-ref h 'delta ""))]
    ["message-end"
     (message-end-event type ts sid tid (hash-ref h 'role "") (hash-ref h 'contentLength 0))]
    ["tool-execution-start"
     (tool-execution-start-event type
                                 ts
                                 sid
                                 tid
                                 (hash-ref h 'toolName "")
                                 (hash-ref h 'toolCallId ""))]
    ["tool-execution-update"
     (tool-execution-update-event type
                                  ts
                                  sid
                                  tid
                                  (hash-ref h 'toolName "")
                                  (hash-ref h 'progress ""))]
    ["tool-execution-end"
     (tool-execution-end-event type
                               ts
                               sid
                               tid
                               (hash-ref h 'toolName "")
                               (hash-ref h 'durationMs 0)
                               (hash-ref h 'resultSummary ""))]
    ["bash-tool-call"
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
    ["edit-tool-call"
     (edit-tool-call-event type
                           ts
                           sid
                           tid
                           "edit"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'path "")
                           (hash-ref h 'edits '()))]
    ["write-tool-call"
     (write-tool-call-event type
                            ts
                            sid
                            tid
                            "write"
                            (hasheq)
                            (hash-ref h 'toolCallId "")
                            (hash-ref h 'path "")
                            (hash-ref h 'content ""))]
    ["read-tool-call"
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
    ["grep-tool-call"
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
    ["find-tool-call"
     (find-tool-call-event type
                           ts
                           sid
                           tid
                           "find"
                           (hasheq)
                           (hash-ref h 'toolCallId "")
                           (hash-ref h 'pattern "")
                           (hash-ref h 'path ""))]
    ["custom-tool-call"
     (custom-tool-call-event type
                             ts
                             sid
                             tid
                             (hash-ref h 'toolName "unknown")
                             (hash-ref h 'arguments (hasheq))
                             (hash-ref h 'toolCallId ""))]
    ["tool-call"
     (tool-call-event type
                      ts
                      sid
                      tid
                      (hash-ref h 'toolName "")
                      (hash-ref h 'arguments (hasheq))
                      (hash-ref h 'toolCallId ""))]
    ["tool-result"
     (tool-result-event type
                        ts
                        sid
                        tid
                        (hash-ref h 'toolCallId "")
                        (hash-ref h 'content "")
                        (hash-ref h 'isError #f))]
    ["provider-request"
     (provider-request-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["provider-response"
     (provider-response-event type
                              ts
                              sid
                              tid
                              (hash-ref h 'model "")
                              (hash-ref h 'provider "")
                              (hash-ref h 'latencyMs 0))]
    ["session-start" (session-start-event type ts sid tid (hash-ref h 'model ""))]
    ["session-shutdown" (session-shutdown-event type ts sid tid (hash-ref h 'reason ""))]
    ["input" (input-event type ts sid tid (hash-ref h 'inputType "") (hash-ref h 'content ""))]
    ["model-select"
     (model-select-event type ts sid tid (hash-ref h 'model "") (hash-ref h 'provider ""))]
    ["agent-start" (agent-start-event type ts sid tid (hash-ref h 'model ""))]
    ["agent-end" (agent-end-event type ts sid tid (hash-ref h 'reason "") (hash-ref h 'durationMs 0))]
    ["context" (context-event type ts sid tid (hash-ref h 'tokenCount 0) (hash-ref h 'windowSize 0))]
    [_ (typed-event type ts sid tid)]))

;; ============================================================
;; Registry
;; ============================================================

(define (all-known-event-types)
  '("turn-start" "turn-end"
                 "message-start"
                 "message-update"
                 "message-end"
                 "tool-execution-start"
                 "tool-execution-update"
                 "tool-execution-end"
                 "tool-call"
                 "tool-result"
                 "bash-tool-call"
                 "edit-tool-call"
                 "write-tool-call"
                 "read-tool-call"
                 "grep-tool-call"
                 "find-tool-call"
                 "custom-tool-call"
                 "provider-request"
                 "provider-response"
                 "session-start"
                 "session-shutdown"
                 "input"
                 "model-select"
                 "agent-start"
                 "agent-end"
                 "context"))

(define (event-name->tool-name type)
  (match type
    ["bash-tool-call" "bash"]
    ["edit-tool-call" "edit"]
    ["write-tool-call" "write"]
    ["read-tool-call" "read"]
    ["grep-tool-call" "grep"]
    ["find-tool-call" "find"]
    [_ #f]))
