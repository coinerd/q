#lang racket/base

;; agent/event-json.rkt -- JSON serialization for typed events via registry
;;
;; H-01: Replaced 474-line manual dual-match with registry dispatch.
;; Event types auto-register serializers/deserializers via define-typed-event.
;; Per-tool events (manual structs) register explicitly below.

(require racket/match
         "event-structs/typed-event-predicates.rkt"
         ;; Per-tool event imports for manual serializer registration
         (only-in "event-structs/tool-events.rkt"
                  bash-tool-call-event
                  bash-tool-call-event-command
                  bash-tool-call-event-timeout
                  bash-tool-call-event-cwd
                  bash-tool-call-event?
                  edit-tool-call-event
                  edit-tool-call-event-path
                  edit-tool-call-event-edits
                  edit-tool-call-event?
                  write-tool-call-event
                  write-tool-call-event-path
                  write-tool-call-event-content
                  write-tool-call-event?
                  read-tool-call-event
                  read-tool-call-event-path
                  read-tool-call-event-offset
                  read-tool-call-event-limit
                  read-tool-call-event?
                  grep-tool-call-event
                  grep-tool-call-event-pattern
                  grep-tool-call-event-path
                  grep-tool-call-event-glob
                  grep-tool-call-event?
                  find-tool-call-event
                  find-tool-call-event-pattern
                  find-tool-call-event-path
                  find-tool-call-event?
                  custom-tool-call-event
                  custom-tool-call-event?
                  tool-call-event-tool-call-id
                  tool-call-event-tool-name
                  tool-call-event-arguments)
         ;; Registry functions
         (only-in "../util/event-macro.rkt"
                  lookup-event-serializer
                  lookup-event-deserializer
                  register-event-serializer!
                  register-event-deserializer!
                  current-schema-version))

(provide typed-event->jsexpr
         jsexpr->typed-event
         all-known-event-types
         event-name->tool-name
         register-tool-event-serializer!)

;; register-tool-event-serializer! : string? (-> typed-event? hash?) -> void?
;; Wraps a base serializer with auto-injected schemaVersion.
(define (register-tool-event-serializer! event-type base-serializer)
  (register-event-serializer!
   event-type
   (lambda (evt)
     (hash-set (base-serializer evt) 'schemaVersion (current-schema-version)))))

;; ============================================================
;; Per-tool event serializer registration (manual structs)
;; ============================================================

;; bash-tool-call-event
(register-tool-event-serializer! "tool.bash.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "bash"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'command
                                      (bash-tool-call-event-command evt)
                                      'timeout
                                      (bash-tool-call-event-timeout evt)
                                      'cwd
                                      (bash-tool-call-event-cwd evt))))

(register-event-deserializer! "tool.bash.called"
                              (lambda (type ts sid tid h)
                                (bash-tool-call-event type
                                                      ts
                                                      sid
                                                      tid
                                                      "bash"
                                                      (hasheq)
                                                      (hash-ref h 'toolCallId "")
                                                      (hash-ref h 'command "")
                                                      (hash-ref h 'timeout 30)
                                                      (hash-ref h 'cwd ""))))

;; edit-tool-call-event
(register-tool-event-serializer! "tool.edit.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "edit"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'path
                                      (edit-tool-call-event-path evt)
                                      'edits
                                      (edit-tool-call-event-edits evt))))

(register-event-deserializer! "tool.edit.called"
                              (lambda (type ts sid tid h)
                                (edit-tool-call-event type
                                                      ts
                                                      sid
                                                      tid
                                                      "edit"
                                                      (hasheq)
                                                      (hash-ref h 'toolCallId "")
                                                      (hash-ref h 'path "")
                                                      (hash-ref h 'edits '()))))

;; write-tool-call-event
(register-tool-event-serializer! "tool.write.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "write"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'path
                                      (write-tool-call-event-path evt)
                                      'content
                                      (write-tool-call-event-content evt))))

(register-event-deserializer! "tool.write.called"
                              (lambda (type ts sid tid h)
                                (write-tool-call-event type
                                                       ts
                                                       sid
                                                       tid
                                                       "write"
                                                       (hasheq)
                                                       (hash-ref h 'toolCallId "")
                                                       (hash-ref h 'path "")
                                                       (hash-ref h 'content ""))))

;; read-tool-call-event
(register-tool-event-serializer! "tool.read.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "read"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'path
                                      (read-tool-call-event-path evt)
                                      'offset
                                      (read-tool-call-event-offset evt)
                                      'limit
                                      (read-tool-call-event-limit evt))))

(register-event-deserializer! "tool.read.called"
                              (lambda (type ts sid tid h)
                                (read-tool-call-event type
                                                      ts
                                                      sid
                                                      tid
                                                      "read"
                                                      (hasheq)
                                                      (hash-ref h 'toolCallId "")
                                                      (hash-ref h 'path "")
                                                      (hash-ref h 'offset #f)
                                                      (hash-ref h 'limit #f))))

;; grep-tool-call-event
(register-tool-event-serializer! "tool.grep.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "grep"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'pattern
                                      (grep-tool-call-event-pattern evt)
                                      'path
                                      (grep-tool-call-event-path evt)
                                      'glob
                                      (grep-tool-call-event-glob evt))))

(register-event-deserializer! "tool.grep.called"
                              (lambda (type ts sid tid h)
                                (grep-tool-call-event type
                                                      ts
                                                      sid
                                                      tid
                                                      "grep"
                                                      (hasheq)
                                                      (hash-ref h 'toolCallId "")
                                                      (hash-ref h 'pattern "")
                                                      (hash-ref h 'path "")
                                                      (hash-ref h 'glob ""))))

;; find-tool-call-event
(register-tool-event-serializer! "tool.find.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      "find"
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'pattern
                                      (find-tool-call-event-pattern evt)
                                      'path
                                      (find-tool-call-event-path evt))))

(register-event-deserializer! "tool.find.called"
                              (lambda (type ts sid tid h)
                                (find-tool-call-event type
                                                      ts
                                                      sid
                                                      tid
                                                      "find"
                                                      (hasheq)
                                                      (hash-ref h 'toolCallId "")
                                                      (hash-ref h 'pattern "")
                                                      (hash-ref h 'path ""))))

;; custom-tool-call-event
(register-tool-event-serializer! "tool.custom.called"
                            (lambda (evt)
                              (hasheq 'toolName
                                      (tool-call-event-tool-name evt)
                                      'toolCallId
                                      (tool-call-event-tool-call-id evt)
                                      'arguments
                                      (tool-call-event-arguments evt))))

(register-event-deserializer! "tool.custom.called"
                              (lambda (type ts sid tid h)
                                (custom-tool-call-event type
                                                        ts
                                                        sid
                                                        tid
                                                        (hash-ref h 'toolName "unknown")
                                                        (hash-ref h 'arguments (hasheq))
                                                        (hash-ref h 'toolCallId ""))))

;; ============================================================
;; JSON Serialization (H-01: registry dispatch)
;; ============================================================

;; typed-event->jsexpr : typed-event? -> hash?
(define (typed-event->jsexpr evt)
  (define base
    (hasheq 'type
            (typed-event-type evt)
            'timestamp
            (typed-event-timestamp evt)
            'sessionId
            (typed-event-session-id evt)
            'turnId
            (typed-event-turn-id evt)
            'schemaVersion
            (current-schema-version)))
  (define type-str (typed-event-type evt))
  (define serializer (lookup-event-serializer type-str))
  (when (not serializer)
    (log-warning "q/event-json: no serializer for typed event '~a', using base fields only" type-str))
  (if serializer
      (for/fold ([h base]) ([(k v) (in-hash (serializer evt))])
        (hash-set h k v))
      base))

;; jsexpr->typed-event : hash? -> typed-event?
(define (jsexpr->typed-event h)
  (define type (hash-ref h 'type #f))
  (define ts (hash-ref h 'timestamp 0))
  (define sid (hash-ref h 'sessionId ""))
  (define tid (hash-ref h 'turnId #f))
  (define schema-version (hash-ref h 'schemaVersion 1))
  (when (> schema-version (current-schema-version))
    (log-warning "q/event-json: event type '~a' has schemaVersion ~a > current (~a)"
                 type schema-version (current-schema-version)))
  (define deserializer (lookup-event-deserializer type))
  (when (and (not deserializer) (> (hash-count h) 4))
    (log-warning "q/event-json: no deserializer for event type '~a'" type))
  (if deserializer
      (deserializer type ts sid tid h)
      (typed-event type ts sid tid)))

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
                   "model.stream.delta.tool-call"
                   "model.stream.thinking"
                   "model.stream.completed"
                   "tool.call.started"
                   "provider.stream.delta"
                   "provider.stream.thinking"
                   "provider.stream.completed"
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
