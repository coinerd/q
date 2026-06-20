#lang racket/base

;; agent/event-json.rkt -- JSON serialization for typed events via registry
;;
;; H-01: Replaced 474-line manual dual-match with registry dispatch.
;; Event types auto-register serializers/deserializers via define-typed-event.
;; Per-tool events (manual structs) register explicitly below.
;;
;; W4 v0.99.35: Pure serialization logic extracted to event-json-helpers.rkt.
;; This module now focuses on registry registration + dispatch, delegating
;; pure data extraction/conversion to the helpers module.

(require racket/contract
         racket/match
         "event-structs/typed-event-predicates.rkt"
         (only-in "event-structs/base.rkt"
                  typed-event
                  typed-event-type
                  typed-event-timestamp
                  typed-event-session-id
                  typed-event-turn-id)
         ;; Browser event module registers browser serializers/deserializers.
         (prefix-in browser: "../browser/events.rkt")
         ;; W4 v0.99.35: Pure serialization helpers
         "event-json-helpers.rkt"
         ;; Registry functions
         (only-in "../util/event/event-macro.rkt"
                  lookup-event-serializer
                  lookup-event-deserializer
                  register-event-serializer!
                  register-event-deserializer!
                  current-schema-version
                  lookup-event-schema-version))

(provide (contract-out [typed-event->jsexpr (-> typed-event? hash?)]
                       [jsexpr->typed-event (-> hash? (or/c typed-event? #f))]
                       [all-known-event-types (-> (listof string?))]
                       [event-name->tool-name (-> string? (or/c string? #f))]
                       [register-tool-event-serializer! (-> string? (-> typed-event? hash?) void?)]))

;; register-tool-event-serializer! : string? (-> typed-event? hash?) -> void?
;; Wraps a base serializer with auto-injected schemaVersion.
(define (register-tool-event-serializer! event-type base-serializer)
  (register-event-serializer!
   event-type
   (lambda (evt)
     (hash-set (base-serializer evt) 'schemaVersion (lookup-event-schema-version event-type)))))

;; ============================================================
;; Per-tool event serializer/deserializer registration
;;
;; W4 v0.99.35: Pure serializer/deserializer logic now lives in
;; event-json-helpers.rkt. This section registers them with the global
;; registry at module-load time.
;; ============================================================

(register-tool-event-serializer! "tool.bash.called" serialize-bash-tool-call)
(register-event-deserializer! "tool.bash.called" deserialize-bash-tool-call)

(register-tool-event-serializer! "tool.edit.called" serialize-edit-tool-call)
(register-event-deserializer! "tool.edit.called" deserialize-edit-tool-call)

(register-tool-event-serializer! "tool.write.called" serialize-write-tool-call)
(register-event-deserializer! "tool.write.called" deserialize-write-tool-call)

(register-tool-event-serializer! "tool.read.called" serialize-read-tool-call)
(register-event-deserializer! "tool.read.called" deserialize-read-tool-call)

(register-tool-event-serializer! "tool.grep.called" serialize-grep-tool-call)
(register-event-deserializer! "tool.grep.called" deserialize-grep-tool-call)

(register-tool-event-serializer! "tool.find.called" serialize-find-tool-call)
(register-event-deserializer! "tool.find.called" deserialize-find-tool-call)

(register-tool-event-serializer! "tool.custom.called" serialize-custom-tool-call)
(register-event-deserializer! "tool.custom.called" deserialize-custom-tool-call)

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
            (lookup-event-schema-version (typed-event-type evt))))
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
                 type
                 schema-version
                 (current-schema-version)))
  (define deserializer (lookup-event-deserializer type))
  (when (and (not deserializer) (> (hash-count h) 4))
    (log-warning "q/event-json: no deserializer for event type '~a'" type))
  (if deserializer
      (deserializer type ts sid tid h)
      (typed-event type ts sid tid)))
