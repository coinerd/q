#lang racket

;; tests/test-split-turn.rkt — tests for Split-turn Detection (#690-#692)
;;
;; Covers:
;;   - #690: detect split-turn during compaction window calculation
;;   - #691: generate turn-prefix summary
;;   - #692: parent feature integration

(require rackunit
         "../util/protocol-types.rkt"
         "../runtime/split-turn.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define msg-counter 0)
(define (next-id!)
  (set! msg-counter (add1 msg-counter))
  (format "msg-~a" msg-counter))

(define (make-user-msg text)
  (make-message (next-id!) #f 'user 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-assistant-msg text)
  (make-message (next-id!) #f 'assistant 'text
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-tool-result-msg text)
  (make-message (next-id!) #f 'tool 'tool-result
                (list (make-text-part text))
                (current-seconds) (hasheq)))

(define (make-system-msg text)
  (make-message (next-id!) #f 'system 'system-instruction
                (list (make-text-part text))
                (current-seconds) (hasheq)))

;; A typical multi-message turn: user → assistant → tool_result → assistant
(define (make-full-turn user-text assistant-text tool-text final-text)
  (list (make-user-msg user-text)
        (make-assistant-msg assistant-text)
        (make-tool-result-msg tool-text)
        (make-assistant-msg final-text)))

;; ============================================================
;; #690: Split-turn detection
;; ============================================================

(test-case "find-split-turn: empty messages returns no split"
  (define result (find-split-turn '() 0))
  (check-false (split-turn-result-is-split? result)))

(test-case "find-split-turn: split at index 0 is not a split"
  (define msgs (list (make-user-msg "hello")))
  (define result (find-split-turn msgs 0))
  (check-false (split-turn-result-is-split? result))
  (check-equal? (split-turn-result-split-index result) 0))

(test-case "find-split-turn: split at user boundary is not a split"
  ;; Two complete turns, split between them
  (define msgs (append (make-full-turn "q1" "a1" "t1" "f1")
                       (make-full-turn "q2" "a2" "t2" "f2")))
  ;; Split at index 4 (start of second turn = user message)
  (define result (find-split-turn msgs 4))
  (check-false (split-turn-result-is-split? result)))

(test-case "find-split-turn: split mid-turn detects split"
  ;; One full turn + start of second turn, split in middle of second turn
  (define msgs (append (make-full-turn "q1" "a1" "t1" "f1")
                       (list (make-user-msg "q2")
                             (make-assistant-msg "a2")
                             (make-tool-result-msg "t2"))))
  ;; Split at index 6 (after tool result but before turn completes)
  ;; The turn starts at index 4 (user "q2"), split at 6 is mid-turn
  (define result (find-split-turn msgs 6))
  (check-true (split-turn-result-is-split? result))
  (check-equal? (split-turn-result-turn-start-index result) 4)
  (check-equal? (length (split-turn-result-turn-messages result)) 2))

(test-case "find-split-turn: split after assistant in turn"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-assistant-msg "continuing")))
  ;; Split at index 2 (after second assistant msg, mid-turn)
  (define result (find-split-turn msgs 2))
  (check-true (split-turn-result-is-split? result))
  (check-equal? (split-turn-result-turn-start-index result) 0))

(test-case "find-split-turn: turn-messages contains the partial turn"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-tool-result-msg "t1")
                     (make-assistant-msg "f1")
                     (make-user-msg "q2")
                     (make-assistant-msg "a2")))
  ;; Split at index 5 (mid second turn, after assistant)
  (define result (find-split-turn msgs 5))
  (check-true (split-turn-result-is-split? result))
  (check-equal? (split-turn-result-turn-start-index result) 4)
  ;; Turn messages = index 4 only (user "q2") — split is at 5, so messages[4..5) = just user
  (check-equal? (length (split-turn-result-turn-messages result)) 1))

(test-case "turn-start-index: finds user message"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")))
  (check-equal? (turn-start-index msgs 1) 0))

(test-case "turn-start-index: user at index 0"
  (define msgs (list (make-user-msg "q1")))
  (check-equal? (turn-start-index msgs 0) 0))

(test-case "messages-at-turn-boundary: adjusts mid-turn to turn start"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-tool-result-msg "t1")))
  (define adjusted (messages-at-turn-boundary msgs 2))
  (check-equal? adjusted 0))

(test-case "messages-at-turn-boundary: keeps turn-boundary split"
  (define msgs (append (make-full-turn "q1" "a1" "t1" "f1")
                       (make-full-turn "q2" "a2" "t2" "f2")))
  (define adjusted (messages-at-turn-boundary msgs 4))
  (check-equal? adjusted 4))

(test-case "find-turn-start: finds nearest user message backward"
  (define msgs (list (make-user-msg "q1")
                     (make-assistant-msg "a1")
                     (make-tool-result-msg "t1")
                     (make-user-msg "q2")
                     (make-assistant-msg "a2")))
  (check-equal? (find-turn-start msgs 4) 3)
  (check-equal? (find-turn-start msgs 2) 0))

;; ============================================================
;; #691: Turn-prefix generation
;; ============================================================

(test-case "generate-turn-prefix: empty messages returns empty string"
  (check-equal? (generate-turn-prefix '()) ""))

(test-case "generate-turn-prefix: includes role annotations"
  (define msgs (list (make-user-msg "what is 2+2?")
                     (make-assistant-msg "let me calculate")))
  (define prefix (generate-turn-prefix msgs))
  (check-true (string-contains? prefix "[user]"))
  (check-true (string-contains? prefix "[assistant]"))
  (check-true (string-contains? prefix "TURN PREFIX")))

(test-case "generate-turn-prefix: includes message text"
  (define msgs (list (make-user-msg "what is 2+2?")))
  (define prefix (generate-turn-prefix msgs))
  (check-true (string-contains? prefix "what is 2+2?")))

(test-case "generate-turn-prefix: truncates very long messages"
  (define long-text (make-string 1000 #\x))
  (define msgs (list (make-user-msg long-text)))
  (define prefix (generate-turn-prefix msgs))
  ;; Should be truncated to ~500 chars + "..."
  (check-true (< (string-length prefix) 700))
  (check-true (string-contains? prefix "...")))

(test-case "generate-turn-prefix: handles tool-result messages"
  (define msgs (list (make-user-msg "run bash")
                     (make-assistant-msg "executing")
                     (make-tool-result-msg "output: 42")))
  (define prefix (generate-turn-prefix msgs))
  (check-true (string-contains? prefix "[tool]"))
  (check-true (string-contains? prefix "output: 42")))

;; ============================================================
;; #692: Integration — split-turn with token compaction
;; ============================================================

(test-case "integration: token-based split with turn detection"
  ;; Create messages where a token-based split would fall mid-turn
  (define msgs (append
                (make-full-turn "question one" "answer one" "tool one" "final one")
                (list (make-user-msg "question two")
                      (make-assistant-msg "answer two"))))
  ;; Simulate split at index 5 (mid second turn)
  (define split-idx 5)
  (define result (find-split-turn msgs split-idx))
  (check-true (split-turn-result-is-split? result))
  ;; Generate prefix for the partial turn
  (define prefix (generate-turn-prefix (split-turn-result-turn-messages result)))
  (check-true (string-contains? prefix "question two"))
  (check-true (> (string-length prefix) 0)))

(test-case "integration: no prefix needed for clean turn-boundary split"
  (define msgs (append (make-full-turn "q1" "a1" "t1" "f1")
                       (make-full-turn "q2" "a2" "t2" "f2")))
  (define result (find-split-turn msgs 4))
  (check-false (split-turn-result-is-split? result))
  (define prefix (generate-turn-prefix (split-turn-result-turn-messages result)))
  (check-equal? prefix ""))

(test-case "integration: system-instruction counts as turn-start"
  (define msgs (list (make-system-msg "you are helpful")
                     (make-user-msg "hello")
                     (make-assistant-msg "hi")))
  ;; turn-start at index 0 (system) and index 1 (user)
  (check-equal? (turn-start-index msgs 2) 1))
