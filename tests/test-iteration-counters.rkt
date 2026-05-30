#lang racket

;; BOUNDARY: integration

;; tests/test-iteration-counters.rkt -- Tests for runtime/iteration/counters.rkt
;;
;; v0.34.7 T-01: Dedicated tests for iteration counter module.
;; Tests compute-next-counters (pure counter update) and check-cancellation.

(require rackunit
         rackunit/text-ui
         racket/match
         "../agent/iteration/loop-state.rkt"
         (only-in "../agent/iteration/counters.rkt" compute-next-counters)
         (only-in "../util/protocol-types.rkt"
                  make-message
                  make-text-part
                  make-tool-call-part
                  make-tool-result-part
                  message-role
                  message-content))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-test-counters #:iteration [iter 0]
                            #:consecutive-tool-count [tc 0]
                            #:seen-paths [sp '()]
                            #:explore-count [ec 0]
                            #:implement-count [ic 0]
                            #:consecutive-error-count [cec 0]
                            #:recent-tool-names [rtn '()]
                            #:intent-retry-count [irc 0]
                            #:stall-retry-count [src 0])
  (loop-counters iter tc sp irc cec rtn ec ic src))

(define (make-assistant-msg text)
  (make-message "test-id"
                #f
                'assistant
                'message
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

(define (make-tool-call-msg . tool-specs)
  (define parts
    (for/list ([spec (in-list tool-specs)])
      (match-define (list name args) spec)
      (make-tool-call-part (format "tc-~a" name) name args)))
  (make-message "test-id" #f 'assistant 'message parts (current-seconds) (hasheq)))

(define (make-tool-result-msg name is-error? text)
  (make-message "test-id"
                name
                'tool
                'message
                (list (make-tool-result-part name text is-error?))
                (current-seconds)
                (hasheq)))

;; ============================================================
;; compute-next-counters tests
;; ============================================================

(define counters-tests
  (test-suite "iteration/counters"

    (test-case "empty message list preserves counters"
      (define c (make-test-counters #:consecutive-tool-count 5 #:explore-count 3 #:implement-count 2))
      (define result (compute-next-counters c '()))
      (check-equal? (loop-counters-consecutive-tool-count result) 5)
      (check-equal? (loop-counters-explore-count result) 3)
      (check-equal? (loop-counters-implement-count result) 2))

    (test-case "assistant text message increments nothing"
      (define c (make-test-counters))
      (define msgs (list (make-assistant-msg "hello")))
      (define result (compute-next-counters c msgs))
      (check-equal? (loop-counters-consecutive-tool-count result) 0)
      (check-equal? (loop-counters-explore-count result) 0)
      (check-equal? (loop-counters-implement-count result) 0))

    (test-case "explore tools increment explore-count"
      (define c (make-test-counters))
      (define msgs
        (list
         (make-tool-call-msg (list "read" (hasheq)) (list "grep" (hasheq)) (list "find" (hasheq)))))
      (define result (compute-next-counters c msgs))
      ;; read, grep, find are all explore tools
      (check-equal? (loop-counters-explore-count result) 3))

    (test-case "implement tools increment implement-count"
      (define c (make-test-counters))
      (define msgs (list (make-tool-call-msg (list "edit" (hasheq)) (list "write" (hasheq)))))
      (define result (compute-next-counters c msgs))
      ;; edit + write = 2 implement tools
      (check-equal? (loop-counters-implement-count result) 2))

    (test-case "mixed tools increment both counts"
      (define c (make-test-counters))
      (define msgs
        (list
         (make-tool-call-msg (list "read" (hasheq)) (list "edit" (hasheq)) (list "grep" (hasheq)))))
      (define result (compute-next-counters c msgs))
      (check-equal? (loop-counters-explore-count result) 2)
      (check-equal? (loop-counters-implement-count result) 1))

    (test-case "tool errors increment consecutive-error-count"
      (define c (make-test-counters))
      (define msgs
        (list (make-tool-result-msg "read" #t "file not found")
              (make-tool-result-msg "edit" #t "permission denied")))
      (define result (compute-next-counters c msgs))
      (check-equal? (loop-counters-consecutive-error-count result) 2))

    (test-case "successful tool results do not increment error count"
      (define c (make-test-counters))
      (define msgs (list (make-tool-result-msg "read" #f "file contents here")))
      (define result (compute-next-counters c msgs))
      (check-equal? (loop-counters-consecutive-error-count result) 0))

    (test-case "new tools are appended to recent-tool-names"
      (define c (make-test-counters #:recent-tool-names '("old-tool")))
      (define msgs (list (make-tool-call-msg (list "new-tool" (hasheq)))))
      (define result (compute-next-counters c msgs))
      (define recent (loop-counters-recent-tool-names result))
      (check-not-false (member "new-tool" recent))
      (check-not-false (member "old-tool" recent)))))

;; ============================================================
;; Run
;; ============================================================

(module+ main
  (run-tests counters-tests))
(module+ test
  (run-tests counters-tests))
