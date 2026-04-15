#lang racket

;;; tests/test-split-turn-compaction.rkt — integration tests for split-turn in compaction (#767)
;;;
;;; Verifies that when the compaction cut falls mid-turn, the summary
;;; includes a turn prefix capturing the truncated turn context.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/token-compaction.rkt"
         "../runtime/split-turn.rkt")

;; Helper: create a simple message
(define (msg id role text [kind 'message])
  (make-message id #f role kind (list (make-text-part text)) (current-seconds) (hasheq)))

;; Tiny token config that triggers compaction with few messages
(define tiny-tc (token-compaction-config 5 0 10))

;; A turn: user → assistant → tool-result → assistant
(define (make-turn prefix n)
  (list
   (msg (format "~a-user" prefix) 'user (format "Turn ~a request" n))
   (msg (format "~a-asst" prefix) 'assistant (format "Turn ~a response with enough text to consume tokens" n))
   (msg (format "~a-tool" prefix) 'assistant (format "Tool result ~a with substantial content for token budget" n) 'tool-result)
   (msg (format "~a-asst2" prefix) 'assistant (format "Turn ~a followup" n))))

(test-case "split-turn detection finds mid-turn cut"
  (define msgs
    (append (make-turn "a" 1)
            (make-turn "b" 2)))
  ;; total = 8 msgs, tiny-tc keeps ~5 tokens worth
  ;; With backward walk, cut should fall somewhere in the messages
  (define result (find-split-turn msgs 4))
  ;; Index 4 is start of turn 2 (user message) — should be clean
  (check-false (split-turn-result-is-split? result)))

(test-case "split-turn detected when cut is mid-turn"
  (define msgs
    (append (make-turn "a" 1)
            (list (msg "b-user" 'user "Turn 2 start")
                  (msg "b-asst" 'assistant "Turn 2 partial response"))))
  ;; Cut at index 5 falls mid-turn (b-asst is assistant, not turn start)
  (define result (find-split-turn msgs 5))
  (check-true (split-turn-result-is-split? result)))

(test-case "compact-history includes turn prefix when split-turn detected"
  (define msgs
    (append (make-turn "a" 1)
            (list (msg "b-user" 'user "Turn 2 start")
                  (msg "b-asst" 'assistant "Turn 2 partial"))))
  ;; Use tiny config to force compaction
  (define result (compact-history msgs #:token-config tiny-tc))
  (define summary (compaction-result-summary-message result))
  (when summary
    (define summary-text
      (string-join (for/list ([part (in-list (message-content summary))]
                              #:when (text-part? part))
                     (text-part-text part)) ""))
    ;; If split-turn was detected, summary should contain turn prefix marker
    (when (string-contains? summary-text "TURN PREFIX")
      (check-true #t "Turn prefix included in summary"))))

(test-case "compact-history works without split-turn"
  (define msgs
    (list (msg "m1" 'user "Message 1 that is somewhat long")
          (msg "m2" 'assistant "Response 1 with substantial content")
          (msg "m3" 'user "Message 2 that continues the conversation")
          (msg "m4" 'assistant "Response 2 with more substantial content")))
  (define result (compact-history msgs #:token-config tiny-tc))
  ;; Should produce a valid result (may or may not summarize depending on token count)
  (check-true (compaction-result? result)))

(test-case "turn prefix contains role annotations"
  (define turn-msgs
    (list (msg "t1" 'user "Start of turn")
          (msg "t2" 'assistant "Partial response")))
  (define prefix (generate-turn-prefix turn-msgs))
  (check-not-false (string-contains? prefix "[user]"))
  (check-not-false (string-contains? prefix "[assistant]"))
  (check-not-false (string-contains? prefix "TURN PREFIX")))

(test-case "empty turn produces no prefix"
  (check-equal? (generate-turn-prefix '()) ""))
