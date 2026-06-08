#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-loop-messages.rkt — W2-D2: Test scaffold for agent/loop-messages.rkt
;; v0.29.13: Smoke tests for usage helpers and message utilities.

(require rackunit
         "../agent/loop-messages.rkt"
         "../util/message/protocol-types.rkt")

(test-case "usage-empty? returns #t for empty hash"
  (check-true (usage-empty? (hasheq))))

(test-case "usage-empty? returns #f for non-empty hash"
  (check-false (usage-empty? (hasheq 'prompt_tokens 10 'completion_tokens 5))))

(test-case "parts->text-string extracts text from text-parts"
  (define parts (list (make-text-part "Hello ") (make-text-part "World")))
  (check-equal? (parts->text-string parts) "Hello World"))

(test-case "parts->text-string returns empty for empty list"
  (check-equal? (parts->text-string '()) ""))

;; T2-3 (A2-U7): message-role->api-role helper
(test-case "message-role->api-role maps standard roles"
  (check-equal? (message-role->api-role 'user) "user")
  (check-equal? (message-role->api-role 'assistant) "assistant")
  (check-equal? (message-role->api-role 'system) "system")
  (check-equal? (message-role->api-role 'tool) "tool"))

(test-case "message-role->api-role falls back to symbol->string"
  (check-equal? (message-role->api-role 'custom) "custom"))
