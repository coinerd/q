#lang racket

;; tests/test-branch-summarizer.rkt — FEAT-79: Branch summarization
;;
;; Tests the real summarize-branch from runtime/branch-summarizer.rkt
;; using a proper mock provider that satisfies provider-send.

(require rackunit
         rackunit/text-ui
         "../runtime/branch-summarizer.rkt"
         "../util/protocol-types.rkt"
         "../tests/helpers/mock-provider.rkt")

;; Helper: build a message with a single text content part.
(define (make-test-msg role text)
  (message "test-id" #f role 'message (list (text-part "text" text)) 0 (hasheq)))

(define branch-tests
  (test-suite "branch summarization"

    (test-case "empty messages returns #f"
      (define mock-prov (make-simple-mock-provider "unused"))
      (check-false (summarize-branch '() mock-prov)))

    (test-case "non-empty messages returns string summary"
      (define mock-prov (make-simple-mock-provider "Summary of branch"))
      (define msgs (list (make-test-msg 'user "try approach A")))
      (define result (summarize-branch msgs mock-prov))
      (check-not-false result)
      (check-true (string? result))
      (check-true (string-contains? result "Summary of branch")))

    (test-case "with model-name arg returns string summary"
      (define mock-prov (make-simple-mock-provider "Branch summary with model"))
      (define msgs (list (make-test-msg 'user "explore option B")))
      (define result (summarize-branch msgs mock-prov "test-model"))
      (check-not-false result)
      (check-true (string? result))
      (check-true (string-contains? result "Branch summary with model")))))

(run-tests branch-tests)
