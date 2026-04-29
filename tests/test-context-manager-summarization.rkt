#lang racket/base

;; tests/test-context-manager-summarization.rkt — LLM summarization integration tests
;; STABILITY: testing
;;
;; Issue #2411: W1 — Fix LLM Summarization

(require rackunit
         racket/string
         racket/list
         "../util/protocol-types.rkt"
         "../runtime/context-manager.rkt")

;; ============================================================
;; Tests
;; ============================================================

(test-case "generate-context-summary: empty entries returns #f"
  (define result (generate-context-summary '() #f #f))
  (check-false result))

(test-case "generate-context-summary: fallback without provider"
  (define msgs
    (list (make-message "m1"
                        #f
                        'user
                        'message
                        (list (make-text-part "Hello world"))
                        (current-seconds)
                        (hasheq))))
  (define result (generate-context-summary msgs #f #f))
  (check-true (context-summary? result))
  (check-equal? (context-summary-from-id result) "m1")
  (check-equal? (context-summary-to-id result) "m1")
  (check-true (string-contains? (context-summary-text result) "## Progress")))

(test-case "generate-context-summary: cache hit"
  (define msgs
    (list (make-message "m1"
                        #f
                        'user
                        'message
                        (list (make-text-part "Hello"))
                        (current-seconds)
                        (hasheq))))
  (define cache (make-summary-cache))
  ;; First call populates cache
  (define result1 (generate-context-summary msgs #f #f #:cache cache))
  (check-true (context-summary? result1))
  ;; Second call should be cache hit
  (define result2 (generate-context-summary msgs #f #f #:cache cache))
  (check-true (context-summary? result2))
  (check-equal? (context-summary-text result1) (context-summary-text result2)))

(test-case "summary-cache: store and lookup"
  (define cache (make-summary-cache))
  (summary-cache-store! cache "a" "b" "test summary")
  (check-equal? (summary-cache-lookup cache "a" "b") "test summary")
  (check-false (summary-cache-lookup cache "x" "y")))

(test-case "generate-context-summary: multiple entries"
  (define msgs
    (list
     (make-message "m1" #f 'user 'message (list (make-text-part "First")) (current-seconds) (hasheq))
     (make-message "m2"
                   #f
                   'assistant
                   'message
                   (list (make-text-part "Second"))
                   (current-seconds)
                   (hasheq))
     (make-message "m3"
                   #f
                   'user
                   'message
                   (list (make-text-part "Third"))
                   (current-seconds)
                   (hasheq))))
  (define result (generate-context-summary msgs #f #f))
  (check-true (context-summary? result))
  (check-equal? (context-summary-from-id result) "m1")
  (check-equal? (context-summary-to-id result) "m3")
  (check-equal? (context-summary-entry-count result) 3))

(test-case "context-summary-prompt: produces non-empty string"
  (define msgs
    (list (make-message "m1"
                        #f
                        'user
                        'message
                        (list (make-text-part "Hello"))
                        (current-seconds)
                        (hasheq))))
  (define prompt (context-summary-prompt msgs))
  (check-true (string? prompt))
  (check-true (> (string-length prompt) 0))
  (check-true (string-contains? prompt "SESSION MESSAGES:")))

(test-case "context-summary-prompt: with previous summary"
  (define msgs
    (list (make-message "m1"
                        #f
                        'user
                        'message
                        (list (make-text-part "New info"))
                        (current-seconds)
                        (hasheq))))
  (define prompt (context-summary-prompt msgs #:previous-summary "Old summary"))
  (check-true (string-contains? prompt "EXISTING SUMMARY:"))
  (check-true (string-contains? prompt "Old summary")))

(test-case "generate-context-summary: non-provider falls back to simple"
  ;; Passing a non-provider value should fall back to simple-summary-text
  (define msgs
    (list
     (make-message "m1" #f 'user 'message (list (make-text-part "Test")) (current-seconds) (hasheq))))
  ;; "not-a-provider" is not a provider?, so should use fallback
  (define result (generate-context-summary msgs "not-a-provider" "model"))
  (check-true (context-summary? result))
  ;; Should use simple-summary-text (fallback), not crash
  (check-true (string-contains? (context-summary-text result) "## Progress")))
