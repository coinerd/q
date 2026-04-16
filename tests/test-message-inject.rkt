#lang racket

;; tests/test-message-inject.rkt — tests for Message Injection (#680-#683)
;;
;; Covers:
;;   - inject-system-message! (#681)
;;   - inject-user-message! (#682)
;;   - inject-assistant-message! (#683)
;;   - make-injection-message helper
;;   - Event bus integration
;;   - Extension-to-extension message injection

(require rackunit
         "../agent/event-bus.rkt"
         "../agent/types.rkt"
         "../util/protocol-types.rkt"
         "../extensions/message-inject.rkt")

;; ============================================================
;; make-injection-message
;; ============================================================

(test-case "make-injection-message creates message with correct role"
  (define msg (make-injection-message 'system "test"))
  (check-equal? (message-role msg) 'system)
  (check-equal? (message-kind msg) 'text)
  (check-true (string? (message-id msg)))
  (check-false (message-parent-id msg)))

(test-case "make-injection-message wraps text in content part"
  (define msg (make-injection-message 'user "hello world"))
  (check-equal? (length (message-content msg)) 1)
  (define part (car (message-content msg)))
  (check-true (text-part? part))
  (check-equal? (text-part-text part) "hello world"))

(test-case "make-injection-message sets source metadata"
  (define msg (make-injection-message 'assistant "response"))
  (define meta (message-meta msg))
  (check-equal? (hash-ref meta 'source) 'extension-inject))

;; ============================================================
;; inject-system-message! (#681)
;; ============================================================

(test-case "inject-system-message! publishes event with system role"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (inject-system-message! bus "sess-1" "system override")
  (check-not-false (unbox received))
  (check-equal? (event-ev (unbox received)) "message.injected")
  (check-equal? (hash-ref (event-payload (unbox received)) 'role) 'system)
  (define msg (hash-ref (event-payload (unbox received)) 'message))
  (check-equal? (message-role msg) 'system)
  (check-equal? (text-part-text (car (message-content msg))) "system override"))

;; ============================================================
;; inject-user-message! (#682)
;; ============================================================

(test-case "inject-user-message! publishes event with user role"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (inject-user-message! bus "sess-2" "user note")
  (check-not-false (unbox received))
  (check-equal? (event-ev (unbox received)) "message.injected")
  (check-equal? (hash-ref (event-payload (unbox received)) 'role) 'user)
  (define msg (hash-ref (event-payload (unbox received)) 'message))
  (check-equal? (message-role msg) 'user)
  (check-equal? (text-part-text (car (message-content msg))) "user note"))

;; ============================================================
;; inject-assistant-message! (#683)
;; ============================================================

(test-case "inject-assistant-message! publishes event with assistant role"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (inject-assistant-message! bus "sess-3" "assistant response")
  (check-not-false (unbox received))
  (check-equal? (event-ev (unbox received)) "message.injected")
  (check-equal? (hash-ref (event-payload (unbox received)) 'role) 'assistant)
  (define msg (hash-ref (event-payload (unbox received)) 'message))
  (check-equal? (message-role msg) 'assistant)
  (check-equal? (text-part-text (car (message-content msg))) "assistant response"))

;; ============================================================
;; Session ID propagation
;; ============================================================

(test-case "injected events carry correct session-id"
  (define bus (make-event-bus))
  (define received (box #f))
  (subscribe! bus (lambda (evt) (set-box! received evt)))
  (inject-user-message! bus "my-session-42" "hello")
  (check-equal? (event-session-id (unbox received)) "my-session-42"))

;; ============================================================
;; Extension-to-extension message injection via event bus
;; ============================================================

(test-case "extension can listen for injected messages from another"
  (define bus (make-event-bus))
  (define ext-a-received (box #f))
  ;; Ext A subscribes to injection events
  (subscribe! bus
              (lambda (evt)
                (when (equal? (event-ev evt) "message.injected")
                  (set-box! ext-a-received evt)))
              #:filter (lambda (evt) (equal? (event-ev evt) "message.injected")))
  ;; Ext B injects a message
  (inject-system-message! bus "shared-session" "injected by B")
  (check-not-false (unbox ext-a-received))
  (define msg (hash-ref (event-payload (unbox ext-a-received)) 'message))
  (check-equal? (text-part-text (car (message-content msg))) "injected by B"))

;; ============================================================
;; Multiple injections
;; ============================================================

(test-case "multiple injections produce separate events"
  (define bus (make-event-bus))
  (define received '())
  (subscribe! bus (lambda (evt) (set! received (cons evt received))))
  (inject-system-message! bus "s1" "first")
  (inject-user-message! bus "s1" "second")
  (inject-assistant-message! bus "s1" "third")
  (check-equal? (length received) 3)
  ;; Most recent first (stack order)
  (check-equal? (hash-ref (event-payload (car received)) 'role) 'assistant)
  (check-equal? (hash-ref (event-payload (cadr received)) 'role) 'user)
  (check-equal? (hash-ref (event-payload (caddr received)) 'role) 'system))

;; ============================================================
;; Message struct validity
;; ============================================================

(test-case "injected messages are valid message structs"
  (for ([role '(system user assistant)]
        [text '("sys" "usr" "ast")])
    (define msg (make-injection-message role text))
    (check-true (message? msg))
    (check-equal? (message-role msg) role)
    (check-true (and (message-id msg) (string? (message-id msg))))
    (check-true (exact-integer? (message-timestamp msg)))))

;; ============================================================
;; FEAT-61: Iteration loop integration
;; ============================================================

(require "../runtime/iteration.rkt")

(test-case "make-injected-collector! creates a box that collects injected messages"
  (define bus (make-event-bus))
  (define box (make-injected-collector! bus))
  ;; Inject a message
  (inject-system-message! bus "sess-61a" "injected via collector")
  ;; Check the box collected it
  (define msgs (unbox box))
  (check-equal? (length msgs) 1)
  (check-equal? (message-role (car msgs)) 'system))

(test-case "drain-injected-messages! returns collected messages and clears box"
  (define bus (make-event-bus))
  (define box (make-injected-collector! bus))
  (inject-system-message! bus "sess-61b" "drain me")
  (inject-user-message! bus "sess-61b" "drain me too")
  (define drained (drain-injected-messages! bus box "sess-61b"))
  (check-equal? (length drained) 2)
  ;; Box should be empty now
  (check-equal? (unbox box) '()))

(test-case "drain-injected-messages! on empty box returns empty list"
  (define bus (make-event-bus))
  (define box (make-injected-collector! bus))
  (define drained (drain-injected-messages! bus box "sess-61c"))
  (check-equal? drained '()))

(test-case "injected messages carry source=extension-inject metadata"
  (define bus (make-event-bus))
  (define box (make-injected-collector! bus))
  (inject-system-message! bus "sess-61d" "check meta")
  (define msgs (unbox box))
  (check-equal? (hash-ref (message-meta (car msgs)) 'source) 'extension-inject))

(test-case "multiple injections batch correctly via collector"
  (define bus (make-event-bus))
  (define box (make-injected-collector! bus))
  (for ([i (in-range 5)])
    (inject-user-message! bus "sess-61e" (format "msg ~a" i)))
  (define drained (drain-injected-messages! bus box "sess-61e"))
  (check-equal? (length drained) 5)
  ;; After drain, injecting more works
  (inject-user-message! bus "sess-61e" "after drain")
  (define second-drain (drain-injected-messages! bus box "sess-61e"))
  (check-equal? (length second-drain) 1))
