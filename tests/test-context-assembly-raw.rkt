#lang racket

;; @speed fast
;; @suite default
;; BOUNDARY: pure

;; BOUNDARY: integration

;; tests/test-context-assembly-raw.rkt -- Tests for build-assembled-context/raw
;;
;; T-01: Verify the pure context-assembly core with memo and trace support.

(require rackunit
         rackunit/text-ui
         racket/string
         "../runtime/context-assembly/selection.rkt"
         "../runtime/context-assembly/budgeting.rkt"
         "../runtime/context/context-summary.rkt"
         "../util/message/message.rkt"
         "../util/content/content-parts.rkt"
         (only-in "../util/content/content-parts.rkt" text-part? text-part-text))

(define (make-test-msg text [id "msg-1"] [role 'user])
  (make-message id #f role 'text (list (make-text-part text)) 0 #f))

(define raw-assembly-tests
  (test-suite "build-assembled-context/raw"

    (test-case "empty messages returns minimal context"
      (define config (make-context-assembly-config #:recent-tokens 4096))
      (define memo (make-hash))
      (define result (build-assembled-context/raw '() config #f #:memo memo))
      (check-not-false result)
      (check-equal? (length (context-result-messages result)) 0)
      (check-equal? (context-result-total-tokens result) 0))

    (test-case "single user message is included in result"
      (define config (make-context-assembly-config #:recent-tokens 4096))
      (define memo (make-hash))
      (define msgs (list (make-test-msg "hello world" "msg-1")))
      (define result (build-assembled-context/raw msgs config #f #:memo memo))
      (check-not-false result)
      (check-equal? (length (context-result-messages result)) 1))

    (test-case "memoization reuses cache on second call"
      (define config (make-context-assembly-config #:recent-tokens 4096))
      (define memo (make-hash))
      (define msgs (list (make-test-msg "hello" "msg-1")))
      ;; First call
      (define r1 (build-assembled-context/raw msgs config #f #:memo memo))
      (check-not-false r1)
      ;; Memo should now have an entry
      (check-true (> (hash-count memo) 0) "memo should have entries after first call")
      ;; Second call with same memo should work
      (define r2 (build-assembled-context/raw msgs config #f #:memo memo))
      (check-not-false r2)
      ;; Both should return same number of messages
      (check-equal? (length (context-result-messages r1)) (length (context-result-messages r2))))

    (test-case "over-budget message is excluded"
      (define config (make-context-assembly-config #:recent-tokens 1))
      (define memo (make-hash))
      ;; Multiple messages to guarantee some are excluded
      (define msgs
        (for/list ([i (in-range 10)])
          (make-test-msg (format "message number ~a with enough text" i) (format "msg-~a" i))))
      (define result (build-assembled-context/raw msgs config #f #:memo memo))
      ;; With budget of 1, most should be excluded
      (check-true (> (context-result-excluded-count result) 0)
                  "at least some messages should be excluded with tiny budget"))

    (test-case "trace callback receives events"
      (define trace-events '())
      (define (trace-fn phase data)
        (set! trace-events (cons (cons phase data) trace-events)))
      (define config (make-context-assembly-config #:recent-tokens 4096))
      (define memo (make-hash))
      (define msgs (list (make-test-msg "test" "msg-1")))
      (define result (build-assembled-context/raw msgs config #f #:memo memo #:trace trace-fn))
      (check-not-false result)
      (check-true (> (length trace-events) 0) "trace should have received events"))

    ;; CA-01: Summary injection test
    (test-case "summary-injected-into-result-messages-when-excluded"
      ;; Build messages that exceed a tight budget, forcing exclusion + summary
      (define messages
        (for/list ([i (in-range 20)])
          (make-test-msg
           (format "Message ~a with enough text to consume tokens: ~a" i (make-string 200 #\x))
           (format "msg-~a" i))))
      (define budget 500) ;; tight budget → most messages excluded
      (define cfg (make-context-assembly-config #:recent-tokens budget))
      (define result
        (build-assembled-context/raw
         messages
         cfg
         #f
         #:memo (make-hash)
         #:generate-summary-proc
         (λ (excluded provider model-name cache)
           (context-summary "from-0"
                            "to-19"
                            "## Summary of excluded messages\nKey fact: important info here"
                            (length excluded)))))
      ;; The summary must be generated
      (define summary-obj (context-result-summary result))
      (check-not-false summary-obj "summary should be generated when messages excluded")
      ;; KEY CHECK: summary text must appear in the assembled messages sent to LLM
      (when summary-obj
        (define result-msgs (context-result-messages result))
        (define all-text
          (string-join (for*/list ([m (in-list result-msgs)]
                                   [part (in-list (message-content m))]
                                   #:when (text-part? part))
                         (text-part-text part))))
        (check-true (string-contains? all-text (context-summary-text summary-obj))
                    "summary text must be present in assembled context")))))

(module+ main
  (run-tests raw-assembly-tests))
