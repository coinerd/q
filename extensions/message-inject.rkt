#lang racket/base

;; extensions/message-inject.rkt — Message injection API for extensions (#680-#683)
;;
;; Provides convenience functions for extensions to inject messages
;; into the conversation at specific roles:
;;   - inject-system-message!    (#681)
;;   - inject-user-message!      (#682)
;;   - inject-assistant-message! (#683)
;;
;; These functions create proper message structs and publish them
;; as events on the event bus. The iteration loop listens for
;; "message.injected" events and appends them to the context.
;;
;; Extensions call these via their extension context:
;;   (inject-system-message! (ctx-event-bus ctx) "session-id" "System override")
;;   (inject-user-message! (ctx-event-bus ctx) "session-id" "User note")

(require racket/contract
         "../agent/event-bus.rkt"
         "../agent/types.rkt"
         "../util/ids.rkt"
         "../util/protocol-types.rkt")

(provide (contract-out [inject-system-message! (-> event-bus? string? string? any/c)]
                       [inject-user-message! (-> event-bus? string? string? any/c)]
                       [inject-assistant-message! (-> event-bus? string? string? any/c)])
         ;; Low-level: create injection message
         make-injection-message
         ;; Event topic constant
         injection-event-topic)

;; ============================================================
;; Constants
;; ============================================================

(define injection-event-topic "message.injected")

;; ============================================================
;; make-injection-message : symbol? string? -> message?
;; ============================================================

;; Creates a properly-formed message for injection into the conversation.
(define (make-injection-message role text)
  (make-message (generate-id)
                #f
                role
                'text
                (list (make-text-part text))
                (now-seconds)
                (hasheq 'source 'extension-inject)))

;; ============================================================
;; inject-system-message! : event-bus? string? string? -> event?
;; ============================================================

;; Inject a system message into the conversation.
;; bus: the shared event bus (from ctx-event-bus)
;; session-id: current session ID (from ctx-session-id)
;; text: message content
(define (inject-system-message! bus session-id text)
  (define msg (make-injection-message 'system text))
  (publish! bus
            (make-event injection-event-topic
                        (current-seconds)
                        session-id
                        #f
                        (hasheq 'role 'system 'message msg))))

;; ============================================================
;; inject-user-message! : event-bus? string? string? -> event?
;; ============================================================

;; Inject a user message into the conversation.
(define (inject-user-message! bus session-id text)
  (define msg (make-injection-message 'user text))
  (publish! bus
            (make-event injection-event-topic
                        (current-seconds)
                        session-id
                        #f
                        (hasheq 'role 'user 'message msg))))

;; ============================================================
;; inject-assistant-message! : event-bus? string? string? -> event?
;; ============================================================

;; Inject an assistant message into the conversation.
(define (inject-assistant-message! bus session-id text)
  (define msg (make-injection-message 'assistant text))
  (publish! bus
            (make-event injection-event-topic
                        (current-seconds)
                        session-id
                        #f
                        (hasheq 'role 'assistant 'message msg))))
