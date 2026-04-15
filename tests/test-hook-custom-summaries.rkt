#lang racket

;;; tests/test-hook-custom-summaries.rkt — tests for hook-provided custom summaries (#769)
;;;
;;; Verifies that session-before-compact hook can provide custom summary
;;; to skip the LLM call.

(require rackunit
         rackunit/text-ui
         "../util/protocol-types.rkt"
         "../runtime/compactor.rkt"
         "../runtime/token-compaction.rkt"
         "../util/hook-types.rkt")

;; Helper: create a simple message
(define (msg id role text)
  (make-message id #f role 'message (list (make-text-part text)) (current-seconds) (hasheq)))

;; Tiny token config
(define tiny-tc (token-compaction-config 3 0 5))

(test-case "hook returning custom summary skips summarize-fn"
  (define call-count (box 0))
  (define counting-summarizer
    (lambda (messages)
      (set-box! call-count (add1 (unbox call-count)))
      "LLM summary"))
  (define custom-hook
    (lambda (hook-point payload)
      (hook-result 'amend (hasheq 'summary "Custom hook summary"))))
  (define msgs
    (list (msg "m1" 'user "Message one text")
          (msg "m2" 'assistant "Response two text")
          (msg "m3" 'user "Message three text")
          (msg "m4" 'assistant "Response four text")
          (msg "m5" 'user "Message five text")))
  (define result
    (compact-history msgs
                     #:summarize-fn counting-summarizer
                     #:hook-dispatcher custom-hook
                     #:token-config tiny-tc))
  ;; Custom summary should be used
  (define summary (compaction-result-summary-message result))
  (define summary-text
    (and summary
         (string-join (for/list ([part (in-list (message-content summary))]
                                 #:when (text-part? part))
                        (text-part-text part)) "")))
  ;; The summarize-fn should NOT have been called
  (check-equal? (unbox call-count) 0)
  ;; Summary text should contain the custom text
  (check-not-false (and summary-text (string-contains? summary-text "Custom hook summary"))))

(test-case "hook returning pass still uses summarize-fn"
  (define call-count (box 0))
  (define counting-summarizer
    (lambda (messages)
      (set-box! call-count (add1 (unbox call-count)))
      "Default summary"))
  (define pass-hook
    (lambda (hook-point payload)
      (hook-result 'pass (hasheq))))
  (define msgs
    (list (msg "m1" 'user "Message one text")
          (msg "m2" 'assistant "Response two text")
          (msg "m3" 'user "Message three text")
          (msg "m4" 'assistant "Response four text")
          (msg "m5" 'user "Message five text")))
  (define result
    (compact-history msgs
                     #:summarize-fn counting-summarizer
                     #:hook-dispatcher pass-hook
                     #:token-config tiny-tc))
  ;; summarize-fn SHOULD have been called
  (check-equal? (unbox call-count) 1))

(test-case "hook returning block prevents compaction"
  (define block-hook
    (lambda (hook-point payload)
      (hook-result 'block (hasheq))))
  (define msgs
    (list (msg "m1" 'user "Message one text")
          (msg "m2" 'assistant "Response two text")
          (msg "m3" 'user "Message three text")
          (msg "m4" 'assistant "Response four text")
          (msg "m5" 'user "Message five text")))
  (define result
    (compact-history msgs
                     #:hook-dispatcher block-hook
                     #:token-config tiny-tc))
  ;; No summary should be produced
  (check-false (compaction-result-summary-message result))
  ;; All messages kept
  (check-equal? (length (compaction-result-kept-messages result)) 5))
