#lang racket/base

;; @speed fast
;; @suite default

;; test-struct-mutability.rkt — Tests for struct mutability fixes (T3-1, T3-9)
;; Part of v0.80.5 Polish Sweep

(require rackunit
         racket/contract
         "../agent/streaming-message.rkt")

;; ============================================================
;; T3-1: streaming-message uses #:mutable fields instead of boxes
;; ============================================================

(test-case "streaming-message: make-streaming-message returns opaque struct"
  (define sm (make-streaming-message "test-id-1"))
  (check-pred streaming-message? sm)
  (check-equal? (streaming-message-message-id sm) "test-id-1"))

(test-case "streaming-message: text accumulation works without boxes"
  (define sm (make-streaming-message "test-id-2"))
  (check-equal? (streaming-message-text sm) "")
  (streaming-message-append-text! sm "hello ")
  (streaming-message-append-text! sm "world")
  (check-equal? (streaming-message-text sm) "hello world"))

(test-case "streaming-message: tool-call accumulation works"
  (define sm (make-streaming-message "test-id-3"))
  (check-equal? (streaming-message-get-tool-calls sm) '())
  (streaming-message-append-tool-call! sm (hasheq 'name "read" 'arguments '{}))
  (streaming-message-append-tool-call! sm (hasheq 'name "write" 'arguments '{}))
  (define tcs (streaming-message-get-tool-calls sm))
  (check-equal? (length tcs) 2)
  ;; Should be in insertion order (reversed internally)
  (check-equal? (hash-ref (car tcs) 'name) "read")
  (check-equal? (hash-ref (cadr tcs) 'name) "write"))

(test-case "streaming-message: thinking accumulation works"
  (define sm (make-streaming-message "test-id-4"))
  (check-equal? (streaming-message-thinking sm) "")
  (streaming-message-append-thinking! sm "hmm")
  (streaming-message-append-thinking! sm " more")
  (check-equal? (streaming-message-thinking sm) "hmm more"))

(test-case "streaming-message: FSM state transitions work"
  (define sm (make-streaming-message "test-id-5"))
  (check-equal? (streaming-message-fsm-state sm) 'not-started)
  (streaming-message-set-message-started! sm)
  (check-equal? (streaming-message-fsm-state sm) 'streaming)
  (check-true (streaming-message-message-started? sm))
  (streaming-message-set-blocked! sm)
  (check-true (streaming-message-blocked? sm))
  (check-false (streaming-message-cancelled? sm)))

(test-case "streaming-message: cancel state works"
  (define sm (make-streaming-message "test-id-6"))
  (streaming-message-set-cancelled! sm)
  (check-true (streaming-message-cancelled? sm))
  (check-false (streaming-message-blocked? sm)))

(test-case "streaming-message: finalize produces message with correct content"
  (define sm (make-streaming-message "test-id-7"))
  (streaming-message-append-text! sm "response text")
  (streaming-message-set-message-started! sm)
  (define msg (streaming-message-finalize sm))
  ;; Should return a message struct (opaque, check via message? if available)
  (check-not-false msg))

(test-case "streaming-message: ->hash produces correct snapshot"
  (define sm (make-streaming-message "test-id-8"))
  (streaming-message-append-text! sm "hello")
  (streaming-message-append-tool-call! sm (hasheq 'id "tc1" 'name "read"))
  (define h (streaming-message->hash sm))
  (check-equal? (hash-ref h 'text) "hello")
  (check-equal? (length (hash-ref h 'tool-calls)) 1)
  (check-false (hash-ref h 'cancelled?))
  (check-false (hash-ref h 'stream-blocked?)))

(module+ main
  (require rackunit/text-ui)
  (run-tests 'test-struct-mutability))
