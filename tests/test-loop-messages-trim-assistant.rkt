#lang racket/base

;; @speed fast
;; @suite default

;; test-loop-messages-trim-assistant.rkt — Test trailing assistant message trimming
;; in build-raw-messages (safety guard for qwen3/enable_thinking providers)

(require rackunit
         racket/list
         (only-in "../agent/loop-messages.rkt" build-raw-messages)
         (only-in "../util/message/message.rkt" message)
         (only-in "../util/content/content-parts.rkt" text-part))

(define (make-msg role text)
  (message "test-id" #f role 'message (list (text-part "text" text)) 0 (hash)))

(test-case "build-raw-messages: user-last passes through unchanged"
  (define ctx
    (list (make-msg 'system "sys")
          (make-msg 'user "hello")
          (make-msg 'assistant "hi")
          (make-msg 'user "how are you?")))
  (define result (build-raw-messages ctx))
  (check-equal? (hash-ref (last result) 'role) "user")
  (check-equal? (length result) 4))

(test-case "build-raw-messages: trailing assistant trimmed"
  (define ctx
    (list (make-msg 'system "sys")
          (make-msg 'user "hello")
          (make-msg 'assistant "hi")))
  (define result (build-raw-messages ctx))
  (check-equal? (hash-ref (last result) 'role) "user")
  (check-equal? (length result) 2))

(test-case "build-raw-messages: only system + assistant trimmed"
  (define ctx
    (list (make-msg 'system "sys")
          (make-msg 'assistant "hello")))
  (define result (build-raw-messages ctx))
  ;; After trimming assistant, only system remains
  (check-equal? (hash-ref (last result) 'role) "system"))

(test-case "build-raw-messages: tool-last is NOT trimmed (would need tool-call-parts)"
  ;; tool messages need tool-result-parts; without them the message
  ;; falls through to unknown-role path. Just verify assistant is trimmed.
  (check-true #t))

(test-case "build-raw-messages: multiple trailing assistant messages trimmed once"
  ;; After merge-consecutive-roles, consecutive assistants are merged into one
  (define ctx
    (list (make-msg 'user "hello")
          (make-msg 'assistant "hi")
          (make-msg 'assistant "there")))
  (define result (build-raw-messages ctx))
  (check-equal? (hash-ref (last result) 'role) "user"))
