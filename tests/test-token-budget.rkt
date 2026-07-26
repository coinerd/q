#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

(require rackunit
         "../llm/token-budget.rkt")

;; ============================================================
;; Test suite: llm/token-budget.rkt — token budget estimation
;; ============================================================

;; ------------------------------------------------------------
;; 1. estimate-context-tokens
;; ------------------------------------------------------------

(test-case "empty messages → 0 tokens"
  (check-equal? (estimate-context-tokens '()) 0))

(test-case "single short message → positive integer token estimate"
  (define msgs-1 (list (hash 'role "user" 'content "Hello, how are you?")))
  (define tokens-1 (estimate-context-tokens msgs-1))
  (check-true (> tokens-1 0))
  (check-true (exact-integer? tokens-1)))

(test-case "longer message → more tokens"
  (define msgs-1 (list (hash 'role "user" 'content "Hello, how are you?")))
  (define tokens-1 (estimate-context-tokens msgs-1))
  (define msgs-2 (list (hash 'role "user" 'content (make-string 1000 #\x))))
  (define tokens-2 (estimate-context-tokens msgs-2))
  (check-true (> tokens-2 tokens-1)))

(test-case "multiple messages are summed → positive tokens"
  (define msgs-3
    (list (hash 'role "user" 'content "abc")
          (hash 'role "assistant" 'content "def")
          (hash 'role "user" 'content "ghi")))
  (define tokens-3 (estimate-context-tokens msgs-3))
  (check-true (> tokens-3 0)))

(test-case "content parts (list of hashes with 'text) → positive tokens"
  (define msgs-4
    (list (hash 'role "user" 'content (list (hash 'type "text" 'text "Hello from content parts")))))
  (define tokens-4 (estimate-context-tokens msgs-4))
  (check-true (> tokens-4 0)))

;; ------------------------------------------------------------
;; 2. should-compact?
;; ------------------------------------------------------------

(test-case "should-compact? returns #f when tokens well below threshold"
  (check-false (should-compact? 0 1000))
  (check-false (should-compact? 500 1000)))

(test-case "should-compact? returns #t when tokens at or above effective 80% threshold"
  ;; With 10% safety margin: effective = 900, threshold = 900*0.8 = 720
  (check-true (should-compact? 720 1000))
  (check-true (should-compact? 900 1000))
  (check-true (should-compact? 1500 1000)))

(test-case "should-compact? degenerate: 0 tokens with 0 threshold → compact"
  (check-true (should-compact? 0 0)))

;; ------------------------------------------------------------
;; 3. remaining-budget
;; ------------------------------------------------------------

(test-case "remaining-budget applies safety margin (#450)"
  ;; With 10% safety margin: effective-budget = 1000 * 0.9 = 900
  (check-equal? (remaining-budget 0 1000) 900)
  (check-equal? (remaining-budget 500 1000) 400)
  (check-equal? (remaining-budget 900 1000) 0)
  (check-equal? (remaining-budget 1500 1000) -600))

;; ------------------------------------------------------------
;; 4. estimate-context-tokens heuristic is reasonable
;; ------------------------------------------------------------

(test-case "estimate-context-tokens heuristic is roughly chars/4"
  (define test-text "Hello world") ; 11 chars → ~3 tokens
  (define msgs-heuristic (list (hash 'role "user" 'content test-text)))
  (define estimated (estimate-context-tokens msgs-heuristic))
  (check-true (>= estimated 2))
  (check-true (<= estimated 10)))

;; ============================================================
;; 5. New W0: per-message overhead constants
;; ============================================================

(test-case "PER-MESSAGE-OVERHEAD-TOKENS is reasonable"
  (check-true (>= PER-MESSAGE-OVERHEAD-TOKENS 2))
  (check-true (<= PER-MESSAGE-OVERHEAD-TOKENS 10)))

(test-case "TOOL-CALL-OVERHEAD-TOKENS is non-zero"
  (check-true (>= TOOL-CALL-OVERHEAD-TOKENS 5)))

(test-case "TOOL-RESULT-OVERHEAD-TOKENS is non-zero"
  (check-true (>= TOOL-RESULT-OVERHEAD-TOKENS 5)))

(test-case "IMAGE-OVERHEAD-TOKENS is non-zero"
  (check-true (>= IMAGE-OVERHEAD-TOKENS 10)))

;; ============================================================
;; 6. estimate-message-overhead
;; ============================================================

(test-case "estimate-message-overhead returns default constant"
  (check-equal? (estimate-message-overhead) PER-MESSAGE-OVERHEAD-TOKENS))

(test-case "estimate-message-overhead accepts role keyword"
  (check-true (> (estimate-message-overhead #:role "user") 0)))

;; ============================================================
;; 7. estimate-content-part-tokens
;; ============================================================

(test-case "text-part has zero non-text tokens"
  (check-equal? (estimate-content-part-tokens (hash "type" "text" "text" "hello")) 0))

(test-case "tool-call part has non-zero tokens"
  (define tc (hash "type" "tool-call" "name" "read" "arguments" (hash "path" "/test.txt")))
  (check-true (> (estimate-content-part-tokens tc) 0)))

(test-case "tool-result part has non-zero tokens"
  (define tr (hash "type" "tool-result" "content" "hello world" "isError" #f))
  (check-true (> (estimate-content-part-tokens tr) 0)))

(test-case "image part has non-zero tokens"
  (define img (hash "type" "image" "mimeType" "image/png" "data" "abcdefghijklmnop"))
  (check-true (> (estimate-content-part-tokens img) 0)))

(test-case "empty image part has minimal tokens"
  (define img (hash "type" "image" "mimeType" "image/png" "data" ""))
  (check-true (> (estimate-content-part-tokens img) 0)))

;; ============================================================
;; 8. estimate-context-tokens with mixed content
;; ============================================================

(test-case "mixed content message has more tokens than text-only"
  (define text-only (list (hash "role" "user" "content" (list (hash "type" "text" "text" "hello")))))
  (define mixed
    (list (hash "role"
                "user"
                "content"
                (list (hash "type" "text" "text" "hello")
                      (hash "type" "tool-call" "name" "read" "arguments" (hash "path" "/x"))))))
  (check-true (> (estimate-context-tokens mixed) (estimate-context-tokens text-only))
              "mixed message with tool-call costs more than text-only"))

;; ============================================================
;; 9. estimate-tool-definition-tokens
;; ============================================================

(test-case "estimate-tool-definition-tokens returns non-zero"
  (check-true (> (estimate-tool-definition-tokens "read-file" "{\"type\":\"string\"}") 0)))

;; ============================================================
;; 10. estimate-non-text-tokens
;; ============================================================

(test-case "text-only message has 0 non-text tokens"
  (define text-msg (hash "role" "user" "content" (list (hash "type" "text" "text" "hello"))))
  (check-equal? (estimate-non-text-tokens text-msg) 0))

(test-case "tool-call message has non-zero non-text tokens"
  (define tool-msg
    (hash "role"
          "assistant"
          "content"
          (list (hash "type" "tool-call" "name" "read" "arguments" (hash "path" "/x")))))
  (check-true (> (estimate-non-text-tokens tool-msg) 0)))

(test-case "tool-result message has non-zero non-text tokens"
  (define result-msg
    (hash "role" "tool" "content" (list (hash "type" "tool-result" "content" "done" "isError" #f))))
  (check-true (> (estimate-non-text-tokens result-msg) 0)))
