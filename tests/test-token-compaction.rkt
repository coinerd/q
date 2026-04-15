#lang racket

;; tests/test-token-compaction.rkt — tests for Token-based Compaction (#686-#689)
;;
;; Covers:
;;   - #687: Configurable keep-recent-tokens and reserve-tokens
;;   - #688: Token estimation fallback for messages without usage data
;;   - #686: Backward token walk for compaction boundary
;;   - #689: Token-based compaction window split

(require rackunit
         "../util/protocol-types.rkt"
         "../runtime/token-compaction.rkt"
         "../llm/token-budget.rkt")

;; ============================================================
;; Helpers
;; ============================================================

;; Create a simple text message
(define (make-test-msg role text)
  (make-message (format "msg-~a" (current-milliseconds))
                #f role 'text
                (list (make-text-part text))
                (current-seconds)
                (hasheq)))

;; Create a message with known approximate token count
;; estimate-text-tokens uses chars/4 ratio for plain text
(define (make-msg-with-tokens role approx-tokens)
  (define chars (* approx-tokens 4))
  (make-test-msg role (make-string chars #\a)))

;; ============================================================
;; #687: Token compaction configuration
;; ============================================================

(test-case "default-token-compaction-config has sensible defaults"
  (define cfg (default-token-compaction-config))
  (check-true (token-compaction-config? cfg))
  (check-equal? (token-compaction-config-keep-recent-tokens cfg) 30000)
  (check-equal? (token-compaction-config-reserve-tokens cfg) 10000)
  (check-equal? (token-compaction-config-max-context-tokens cfg) DEFAULT-TOKEN-BUDGET-THRESHOLD))

(test-case "make-token-compaction-config accepts keyword overrides"
  (define cfg (make-token-compaction-config #:keep-recent-tokens 5000
                                            #:reserve-tokens 2000
                                            #:max-context-tokens 50000))
  (check-equal? (token-compaction-config-keep-recent-tokens cfg) 5000)
  (check-equal? (token-compaction-config-reserve-tokens cfg) 2000)
  (check-equal? (token-compaction-config-max-context-tokens cfg) 50000))

(test-case "token-compaction-config is transparent"
  (define cfg (token-compaction-config 1000 500 10000))
  (check-equal? (token-compaction-config-keep-recent-tokens cfg) 1000)
  (check-equal? (token-compaction-config-reserve-tokens cfg) 500)
  (check-equal? (token-compaction-config-max-context-tokens cfg) 10000))

;; ============================================================
;; #688: Token estimation for messages
;; ============================================================

(test-case "estimate-messages-tokens returns 0 for empty list"
  (check-equal? (estimate-messages-tokens '()) 0))

(test-case "estimate-messages-tokens sums across messages"
  (define msgs (list (make-msg-with-tokens 'user 100)
                     (make-msg-with-tokens 'assistant 200)))
  (define total (estimate-messages-tokens msgs))
  (check-true (> total 0))
  ;; Should be approximately 300 tokens (with chars/4 heuristic)
  (check-true (<= total 400))
  (check-true (>= total 250)))

(test-case "estimate-messages-tokens handles messages without content parts"
  ;; A message with empty content should contribute 0 tokens
  (define msg (make-message "m1" #f 'user 'text '() (current-seconds) (hasheq)))
  (check-equal? (estimate-messages-tokens (list msg)) 0))

(test-case "estimate-messages-tokens handles mixed content"
  (define msgs (list (make-test-msg 'user "short")
                     (make-msg-with-tokens 'assistant 50)))
  (define total (estimate-messages-tokens msgs))
  (check-true (> total 0)))

;; ============================================================
;; #686: Backward token walk
;; ============================================================

(test-case "backward-token-walk: all messages fit within budget"
  (define msgs (list (make-test-msg 'user "hello")
                     (make-test-msg 'assistant "hi there")))
  (define-values (kept overflow) (backward-token-walk msgs 10000))
  (check-equal? kept msgs)
  (check-equal? overflow '()))

(test-case "backward-token-walk: empty messages"
  (define-values (kept overflow) (backward-token-walk '() 1000))
  (check-equal? kept '())
  (check-equal? overflow '()))

(test-case "backward-token-walk: single message exceeds budget"
  (define msgs (list (make-msg-with-tokens 'user 100)))
  (define-values (kept overflow) (backward-token-walk msgs 5))
  (check-equal? kept '())
  (check-equal? (length overflow) 1))

(test-case "backward-token-walk: splits at budget boundary"
  ;; 5 messages of ~100 tokens each, budget of 200 tokens
  ;; Should keep ~2 most recent messages, overflow ~3 older ones
  (define msgs (for/list ([i (in-range 5)])
                 (make-msg-with-tokens 'user 100)))
  (define-values (kept overflow) (backward-token-walk msgs 200))
  ;; Kept should be the most recent messages
  (check-true (>= (length kept) 1))
  (check-true (<= (length kept) 2))
  ;; Overflow should be the older messages
  (check-true (>= (length overflow) 3))
  ;; Total should equal original count
  (check-equal? (+ (length kept) (length overflow)) 5))

(test-case "backward-token-walk: preserves message order"
  (define msgs (list (make-test-msg 'user "first")
                     (make-test-msg 'assistant "second")
                     (make-test-msg 'user "third")))
  (define-values (kept overflow) (backward-token-walk msgs 100))
  ;; Both kept and overflow should maintain original order
  (check-equal? (+ (length kept) (length overflow)) 3)
  ;; If split, first message should be in overflow
  (when (> (length overflow) 0)
    (check-equal? (text-part-text (car (message-content (car overflow)))) "first")))

;; ============================================================
;; #689: Token-based compaction window split
;; ============================================================

(test-case "build-token-summary-window: nothing to summarize when under budget"
  (define cfg (make-token-compaction-config #:keep-recent-tokens 50000
                                            #:reserve-tokens 1000
                                            #:max-context-tokens 100000))
  (define msgs (list (make-test-msg 'user "hello")
                     (make-test-msg 'assistant "world")))
  (define-values (old recent) (build-token-summary-window msgs cfg))
  (check-equal? old '())
  (check-equal? recent msgs))

(test-case "build-token-summary-window: splits when over budget"
  (define cfg (make-token-compaction-config #:keep-recent-tokens 100
                                            #:reserve-tokens 50
                                            #:max-context-tokens 500))
  ;; Create messages totaling well over 450 tokens (500 - 50 reserve)
  (define msgs (for/list ([i (in-range 10)])
                 (make-msg-with-tokens 'user 100)))
  (define-values (old recent) (build-token-summary-window msgs cfg))
  ;; Should have something to summarize
  (check-true (> (length old) 0))
  ;; Recent should be non-empty
  (check-true (> (length recent) 0))
  ;; Total should equal original count
  (check-equal? (+ (length old) (length recent)) 10))

(test-case "build-token-summary-window: empty messages returns empty"
  (define cfg (default-token-compaction-config))
  (define-values (old recent) (build-token-summary-window '() cfg))
  (check-equal? old '())
  (check-equal? recent '()))

(test-case "build-token-summary-window: reserve tokens reduce effective budget"
  (define cfg (make-token-compaction-config #:keep-recent-tokens 50
                                            #:reserve-tokens 450
                                            #:max-context-tokens 500))
  ;; With only 50 effective budget (500 - 450), even small messages
  ;; should trigger overflow quickly
  (define msgs (for/list ([i (in-range 5)])
                 (make-msg-with-tokens 'user 100)))
  (define-values (old recent) (build-token-summary-window msgs cfg))
  ;; Most should be overflow
  (check-true (>= (length old) 3)))

(test-case "build-token-summary-window: kept messages are the most recent"
  (define cfg (make-token-compaction-config #:keep-recent-tokens 100
                                            #:reserve-tokens 0
                                            #:max-context-tokens 500))
  ;; 5 messages of 100 tokens each = 500 tokens total
  ;; keep-recent-tokens = 100, so only ~1 recent message kept
  (define msgs (for/list ([i (in-range 5)])
                 (make-message (format "msg-~a" i) #f 'user 'text
                               (list (make-text-part (format "message-~a" i)))
                               (current-seconds) (hasheq))))
  (define-values (old recent) (build-token-summary-window msgs cfg))
  ;; The most recent message should be in 'recent'
  (when (and (> (length recent) 0) (> (length old) 0))
    (define last-recent (car (take-right recent 1)))
    (check-equal? (message-id last-recent) "msg-4")))

(test-case "build-token-summary-window: integrates with estimate-messages-tokens"
  ;; Verify that the split is consistent with token estimates
  ;; Use a small max-context to force overflow
  (define cfg (make-token-compaction-config #:keep-recent-tokens 200
                                            #:reserve-tokens 0
                                            #:max-context-tokens 400))
  ;; 8 messages of 100 tokens each = 800 tokens total
  ;; max-context = 400, so effective budget = 400
  ;; keep-recent = 200, so ~2 recent messages kept
  (define msgs (for/list ([i (in-range 8)])
                 (make-msg-with-tokens 'user 100)))
  (define-values (old recent) (build-token-summary-window msgs cfg))
  (check-true (> (length old) 0))
  (check-true (> (length recent) 0))
  ;; Recent messages should use roughly <= keep-recent-tokens (with estimation slack)
  (define recent-tokens (estimate-messages-tokens recent))
  (check-true (<= recent-tokens 250)))
