#lang racket

(require rackunit
         "../llm/token-budget.rkt")

;; ============================================================
;; Test suite: llm/token-budget.rkt — token budget estimation
;; ============================================================

;; ------------------------------------------------------------
;; 1. estimate-context-tokens
;; ------------------------------------------------------------

(test-case
 "empty messages → 0 tokens"
 (check-equal? (estimate-context-tokens '()) 0))

(test-case
 "single short message → positive integer token estimate"
 (define msgs-1
   (list (hash 'role "user" 'content "Hello, how are you?")))
 (define tokens-1 (estimate-context-tokens msgs-1))
 (check-true (> tokens-1 0))
 (check-true (exact-integer? tokens-1)))

(test-case
 "longer message → more tokens"
 (define msgs-1
   (list (hash 'role "user" 'content "Hello, how are you?")))
 (define tokens-1 (estimate-context-tokens msgs-1))
 (define msgs-2
   (list (hash 'role "user" 'content (make-string 1000 #\x))))
 (define tokens-2 (estimate-context-tokens msgs-2))
 (check-true (> tokens-2 tokens-1)))

(test-case
 "multiple messages are summed → positive tokens"
 (define msgs-3
   (list (hash 'role "user" 'content "abc")
         (hash 'role "assistant" 'content "def")
         (hash 'role "user" 'content "ghi")))
 (define tokens-3 (estimate-context-tokens msgs-3))
 (check-true (> tokens-3 0)))

(test-case
 "content parts (list of hashes with 'text) → positive tokens"
 (define msgs-4
   (list (hash 'role "user"
               'content (list (hash 'type "text" 'text "Hello from content parts")))))
 (define tokens-4 (estimate-context-tokens msgs-4))
 (check-true (> tokens-4 0)))

;; ------------------------------------------------------------
;; 2. should-compact?
;; ------------------------------------------------------------

(test-case
 "should-compact? returns #f when tokens well below threshold"
 (check-false (should-compact? 0 1000))
 (check-false (should-compact? 500 1000)))

(test-case
 "should-compact? returns #t when tokens at or above effective 80% threshold"
 ;; With 10% safety margin: effective = 900, threshold = 900*0.8 = 720
 (check-true (should-compact? 720 1000))
 (check-true (should-compact? 900 1000))
 (check-true (should-compact? 1500 1000)))

(test-case
 "should-compact? degenerate: 0 tokens with 0 threshold → compact"
 (check-true (should-compact? 0 0)))

;; ------------------------------------------------------------
;; 3. remaining-budget
;; ------------------------------------------------------------

(test-case
 "remaining-budget applies safety margin (#450)"
 ;; With 10% safety margin: effective-budget = 1000 * 0.9 = 900
 (check-equal? (remaining-budget 0 1000) 900.0)
 (check-equal? (remaining-budget 500 1000) 400.0)
 (check-equal? (remaining-budget 900 1000) 0.0)
 (check-equal? (remaining-budget 1500 1000) -600.0))

;; ------------------------------------------------------------
;; 4. estimate-context-tokens heuristic is reasonable
;; ------------------------------------------------------------

(test-case
 "estimate-context-tokens heuristic is roughly chars/4"
 (define test-text "Hello world")  ; 11 chars → ~3 tokens
 (define msgs-heuristic
   (list (hash 'role "user" 'content test-text)))
 (define estimated (estimate-context-tokens msgs-heuristic))
 (check-true (>= estimated 2))
 (check-true (<= estimated 10)))
