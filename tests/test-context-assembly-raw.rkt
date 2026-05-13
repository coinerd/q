#lang racket

;; BOUNDARY: integration

;; tests/test-context-assembly-raw.rkt -- Tests for build-assembled-context/raw
;;
;; T-01: Verify the pure context-assembly core with memo and trace support.

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/selection.rkt"
         "../runtime/context-assembly/budgeting.rkt"
         "../util/message.rkt"
         "../util/content-parts.rkt")

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
      (check-true (> (length trace-events) 0) "trace should have received events"))))

(module+ main
  (run-tests raw-assembly-tests))
