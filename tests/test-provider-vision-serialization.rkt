#lang racket

;; test-provider-vision-serialization.rkt — Provider vision API tests

(require rackunit
         "../agent/loop-messages.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         "../browser/settings.rkt")

(define (test-msg role parts)
  (message "test-id" #f role 'user parts 0 (hash)))

;; ---------------------------------------------------------------------------
;; Test: text-only user message stays simple
;; ---------------------------------------------------------------------------

(test-case "build-raw-messages: text-only user message"
  (define msgs (list (test-msg 'user (list (make-text-part "hello")))))
  (define raw (build-raw-messages msgs))
  (check-equal? (length raw) 1)
  (check-equal? (hash-ref (car raw) 'role) "user")
  (check-equal? (hash-ref (car raw) 'content) "hello"))

;; ---------------------------------------------------------------------------
;; Test: image-part in user message produces OpenAI content array
;; ---------------------------------------------------------------------------

(test-case "build-raw-messages: user with image produces content array"
  (define msgs (list (test-msg 'user
                                (list (make-text-part "What do you see?")
                                      (make-image-part "image/png" "abc123==" "high")))))
  (define raw (build-raw-messages msgs))
  (check-equal? (length raw) 1)
  (define msg (car raw))
  (check-equal? (hash-ref msg 'role) "user")
  (define content (hash-ref msg 'content))
  (check-true (list? content) "Content should be an array for mixed text+image")
  (check-equal? (length content) 2)
  ;; First block: text
  (check-equal? (hash-ref (car content) 'type) "text")
  (check-equal? (hash-ref (car content) 'text) "What do you see?")
  ;; Second block: image_url
  (check-equal? (hash-ref (cadr content) 'type) "image_url")
  (define img-url (hash-ref (cadr content) 'image_url))
  (check-true (string-contains? (hash-ref img-url 'url) "data:image/png;base64,abc123=="))
  (check-equal? (hash-ref img-url 'detail) "high"))

;; ---------------------------------------------------------------------------
;; Test: image-part without detail uses "auto"
;; ---------------------------------------------------------------------------

(test-case "build-raw-messages: image without detail defaults to auto"
  (define msgs (list (test-msg 'user
                                (list (make-text-part "look")
                                      (make-image-part "image/jpeg" "data")))))
  (define raw (build-raw-messages msgs))
  (define content (hash-ref (car raw) 'content))
  (define img-url (hash-ref (hash-ref (cadr content) 'image_url) 'detail))
  (check-equal? img-url "auto"))

;; ---------------------------------------------------------------------------
;; Test: vision settings defaults
;; ---------------------------------------------------------------------------

(test-case "vision settings default OFF"
  (define s (default-browser-settings))
  (check-false (browser-settings-vision-enabled? s))
  (check-equal? (browser-settings-vision-detail s) "auto")
  (check-equal? (browser-settings-vision-ephemeral-turns s) 5))
