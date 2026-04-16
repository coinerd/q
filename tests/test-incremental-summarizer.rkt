#lang racket

;; tests/test-incremental-summarizer.rkt — FEAT-81: Incremental summarization
;;
;; Tests the real incremental-summarize from runtime/incremental-summarizer.rkt
;; using a proper mock provider that satisfies provider-send.

(require rackunit
         rackunit/text-ui
         "../runtime/incremental-summarizer.rkt"
         "../util/protocol-types.rkt"
         "../tests/helpers/mock-provider.rkt")

;; Helper: build a message with a single text content part.
(define (make-test-msg role text)
  (message "test-id" #f role 'message (list (text-part "text" text)) 0 (hasheq)))

(define incremental-tests
  (test-suite "incremental summarization"

    (test-case "empty previous-summary with messages returns fresh string"
      (define mock-prov (make-simple-mock-provider "Fresh summary"))
      (define msgs (list (make-test-msg 'user "hello world")))
      (define result (incremental-summarize msgs "" mock-prov))
      (check-true (string? result))
      (check-true (string-contains? result "Fresh summary")))

    (test-case "non-empty previous-summary with empty messages returns previous unchanged"
      (define mock-prov (make-simple-mock-provider "should not be used"))
      (define result (incremental-summarize '() "existing summary" mock-prov))
      (check-equal? result "existing summary"))

    (test-case "non-empty previous-summary with messages returns updated string"
      (define mock-prov (make-simple-mock-provider "Updated summary"))
      (define msgs (list (make-test-msg 'assistant "did some work")))
      (define result (incremental-summarize msgs "old summary" mock-prov))
      (check-true (string? result))
      (check-true (string-contains? result "Updated summary")))))

(run-tests incremental-tests)
