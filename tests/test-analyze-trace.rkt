#lang racket

;; tests/test-analyze-trace.rkt — tests for scripts/analyze-trace.rkt
;;
;; Tests metrics extraction from session messages.

(require rackunit
         rackunit/text-ui
         "../scripts/analyze-trace.rkt"
         (only-in "../util/protocol-types.rkt" make-message make-text-part make-tool-call-part))

;; ── Helpers ──

(define (make-text-msg role text [ts 0])
  (make-message "id1" #f role 'message (list (make-text-part text)) ts (hasheq)))

(define (make-tool-msg role tool-name tool-id [ts 0])
  (make-message "id2"
                #f
                role
                'message
                (list (make-tool-call-part tool-id tool-name "{}"))
                ts
                (hasheq)))

(define (make-complex-msg role text tool-name tool-id [ts 0])
  (make-message "id3"
                #f
                role
                'message
                (list (make-text-part text)
                      (make-tool-call-part tool-id tool-name "{\"path\":\"test.rkt\"}"))
                ts
                (hasheq)))

;; ── Test Suite ──

(define test-analyze-trace
  (test-suite "Trace analysis metrics extraction"

    (test-case "empty message list"
      (define m (extract-metrics '()))
      (check-equal? (hash-ref m 'total-messages) 0)
      (check-equal? (hash-ref (hash-ref m 'tool-calls) 'total) 0))

    (test-case "user/assistant messages counted"
      (define msgs (list (make-text-msg 'user "Hello") (make-text-msg 'assistant "Hi there")))
      (define m (extract-metrics msgs))
      (check-equal? (hash-ref m 'total-messages) 2)
      (check-equal? (hash-ref (hash-ref m 'roles) 'user) 1)
      (check-equal? (hash-ref (hash-ref m 'roles) 'assistant) 1)
      (check-equal? (hash-ref (hash-ref m 'turns) 'estimated-turns) 1))

    (test-case "tool calls extracted and counted"
      (define msgs
        (list (make-text-msg 'user "Read a file")
              (make-tool-msg 'assistant "read" "tc-1")
              (make-text-msg 'tool "file content")
              (make-tool-msg 'assistant "write" "tc-2")
              (make-text-msg 'tool "ok")))
      (define m (extract-metrics msgs))
      (define tc (hash-ref m 'tool-calls))
      (check-equal? (hash-ref tc 'total) 2)
      (check-equal? (hash-ref (hash-ref tc 'by-tool) "read") 1)
      (check-equal? (hash-ref (hash-ref tc 'by-tool) "write") 1))

    (test-case "content types counted"
      (define msgs
        (list (make-text-msg 'user "Hi") (make-complex-msg 'assistant "Let me read" "read" "tc-1")))
      (define m (extract-metrics msgs))
      (define ct (hash-ref m 'content-types))
      (check-equal? (hash-ref ct 'text) 2)
      (check-equal? (hash-ref ct 'tool-call) 1))

    (test-case "timing extracted from timestamps"
      (define msgs
        (list (make-text-msg 'user "Hello" 1000)
              (make-text-msg 'assistant "Hi" 2500)
              (make-text-msg 'user "Thanks" 3000)))
      (define m (extract-metrics msgs))
      (define timing (hash-ref m 'timing))
      (check-equal? (hash-ref timing 'duration-ms) 2000)
      (check-equal? (hash-ref timing 'first-timestamp) 1000)
      (check-equal? (hash-ref timing 'last-timestamp) 3000))

    (test-case "format-report produces readable output"
      (define msgs (list (make-text-msg 'user "Hello" 1000) (make-text-msg 'assistant "Hi" 2000)))
      (define m (extract-metrics msgs))
      (define report (format-report m))
      (check-true (string-contains? report "TRACE ANALYSIS REPORT"))
      (check-true (string-contains? report "Total messages: 2")))

    (test-case "metrics->json produces valid output"
      (define msgs (list (make-text-msg 'user "Hello")))
      (define m (extract-metrics msgs))
      (define json (metrics->json m))
      (check-true (string-contains? json "total-messages")))

    (test-case "multiple tool calls of same type counted"
      (define msgs
        (list (make-text-msg 'user "Read files")
              (make-tool-msg 'assistant "read" "tc-1")
              (make-text-msg 'tool "f1")
              (make-tool-msg 'assistant "read" "tc-2")
              (make-text-msg 'tool "f2")))
      (define m (extract-metrics msgs))
      (check-equal? (hash-ref (hash-ref (hash-ref m 'tool-calls) 'by-tool) "read") 2))))

(run-tests test-analyze-trace)
