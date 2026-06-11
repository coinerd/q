#lang racket

;; test-ephemeral-vision.rkt — Ephemeral vision context management tests

(require rackunit
         "../runtime/context-assembly/session-walk.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt")

(define (test-msg role parts)
  (message "test-id" #f role 'user parts 0 (hash)))

;; ---------------------------------------------------------------------------
;; Test: message without images passes through unchanged
;; ---------------------------------------------------------------------------

(test-case "strip-image-parts: no images → unchanged"
  (define msg (test-msg 'tool (list (make-text-part "result text"))))
  (define result (strip-image-parts msg))
  (check-equal? result msg))

;; ---------------------------------------------------------------------------
;; Test: image-part replaced with text summary
;; ---------------------------------------------------------------------------

(test-case "strip-image-parts: image replaced with summary"
  (define msg
    (test-msg 'tool
              (list (make-text-part "screenshot result") (make-image-part "image/png" "abc123=="))))
  (define result (strip-image-parts msg))
  (define content (message-content result))
  (check-equal? (length content) 2)
  ;; First part unchanged
  (check-pred text-part? (car content))
  (check-equal? (text-part-text (car content)) "screenshot result")
  ;; Second part replaced with text summary
  (check-pred text-part? (cadr content))
  (check-true (string-contains? (text-part-text (cadr content)) "[screenshot:"))
  (check-true (string-contains? (text-part-text (cadr content)) "expired"))
  ;; Original image-part is gone
  (check-false (for/or ([p (in-list content)])
                 (image-part? p))))

;; ---------------------------------------------------------------------------
;; Test: image-part with detail preserved in summary
;; ---------------------------------------------------------------------------

(test-case "strip-image-parts: mime type preserved in summary"
  (define msg (test-msg 'tool (list (make-image-part "image/jpeg" "data123"))))
  (define result (strip-image-parts msg))
  (define summary (text-part-text (car (message-content result))))
  (check-true (string-contains? summary "image/jpeg")))

;; ---------------------------------------------------------------------------
;; Test: strip-image-parts exported
;; ---------------------------------------------------------------------------

(test-case "strip-image-parts is exported"
  (check-pred procedure? strip-image-parts))
