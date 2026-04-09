#lang racket

(require rackunit
         "../llm/token-budget.rkt")

;; ============================================================
;; Test suite: llm/token-budget.rkt — token budget estimation
;; ============================================================

;; ------------------------------------------------------------
;; 1. estimate-context-tokens
;; ------------------------------------------------------------

;; Empty list → 0 tokens
(check-equal? (estimate-context-tokens '()) 0
              "empty messages → 0 tokens")

;; Single short message
(define msgs-1
  (list (hash 'role "user" 'content "Hello, how are you?")))
(define tokens-1 (estimate-context-tokens msgs-1))
(check-true (> tokens-1 0)
            "non-empty messages → positive token estimate")
(check-true (exact-integer? tokens-1)
            "token estimate is an exact integer")

;; More text → more tokens
(define msgs-2
  (list (hash 'role "user" 'content (make-string 1000 #\x))))
(define tokens-2 (estimate-context-tokens msgs-2))
(check-true (> tokens-2 tokens-1)
            "longer message → more tokens")

;; Multiple messages are summed
(define msgs-3
  (list (hash 'role "user" 'content "abc")
        (hash 'role "assistant" 'content "def")
        (hash 'role "user" 'content "ghi")))
(define tokens-3 (estimate-context-tokens msgs-3))
(check-true (> tokens-3 0)
            "multiple messages → positive tokens")

;; Content as a list of content parts (hash with 'text)
(define msgs-4
  (list (hash 'role "user"
              'content (list (hash 'type "text" 'text "Hello from content parts")))))
(define tokens-4 (estimate-context-tokens msgs-4))
(check-true (> tokens-4 0)
            "content parts → positive tokens")

;; ------------------------------------------------------------
;; 2. should-compact?
;; ------------------------------------------------------------

(check-false (should-compact? 0 1000)
             "0 tokens → no compaction")
(check-false (should-compact? 500 1000)
             "500 < 1000 → no compaction")
(check-true (should-compact? 800 1000)
            "800 >= 80% of 1000 → compaction needed")
(check-true (should-compact? 1000 1000)
            "1000 >= 80% of 1000 → compaction needed")
(check-true (should-compact? 1500 1000)
            "1500 >= 80% of 1000 → compaction needed")

;; threshold of 0 → always compact (edge case)
(check-true (should-compact? 0 0)
            "0 tokens with 0 threshold → compact (degenerate)")

;; ------------------------------------------------------------
;; 3. remaining-budget
;; ------------------------------------------------------------

(check-equal? (remaining-budget 0 1000) 1000
              "no tokens used → full budget remaining")
(check-equal? (remaining-budget 500 1000) 500
              "half used → half remaining")
(check-equal? (remaining-budget 1000 1000) 0
              "all used → 0 remaining")
(check-equal? (remaining-budget 1500 1000) -500
              "over budget → negative remaining")

;; ------------------------------------------------------------
;; 4. estimate-context-tokens heuristic is reasonable
;; ------------------------------------------------------------

;; Verify the heuristic is roughly chars/4
(define test-text "Hello world")  ; 11 chars → ~3 tokens
(define msgs-heuristic
  (list (hash 'role "user" 'content test-text)))
(define estimated (estimate-context-tokens msgs-heuristic))
(check-true (>= estimated 2)
            "11 chars should estimate at least 2 tokens")
(check-true (<= estimated 10)
            "11 chars should estimate at most 10 tokens (generous)")

(println "All token-budget tests passed!")
