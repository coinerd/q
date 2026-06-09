#lang racket/base

;; tests/test-content-part-to-text.rkt
;; v0.97.6 W0: F3 — Extracted content-part->text behavioral tests

(require rackunit
         rackunit/text-ui
         (only-in "../runtime/context-assembly/turn-context.rkt" content-part->text)
         (only-in "../util/content/content-parts.rkt" make-text-part make-tool-result-part)
         racket/format)

(define suite
  (test-suite "content-part->text"

    ;; 1. text-part content extracted
    (test-case "text-part content extracted"
      (check-equal? (content-part->text (make-text-part "hello")) "hello"))

    ;; 2. tool-result-part string content
    (test-case "tool-result-part string content"
      (check-equal? (content-part->text (make-tool-result-part "tc1" "string result" #f))
                    "string result"))

    ;; 3. tool-result-part hash content
    (test-case "tool-result-part hash content"
      (define h (hasheq 'key 'value))
      (check-equal? (content-part->text (make-tool-result-part "tc2" h #f)) (~a h)))

    ;; 4. tool-result-part list content
    (test-case "tool-result-part list content"
      (check-equal? (content-part->text (make-tool-result-part "tc3" '(a b c) #f)) "a b c"))

    ;; 5. tool-result-part error skipped
    (test-case "tool-result-part error skipped"
      (check-equal? (content-part->text (make-tool-result-part "tc4" "error text" #t)) ""))

    ;; 6. long tool result truncated to 500 chars
    (test-case "long tool result truncated to 500 chars"
      (define long (make-string 600 #\x))
      (define result (content-part->text (make-tool-result-part "tc5" long #f)))
      (check-equal? (string-length result) 500)
      (check-equal? (substring result 497 500) "..."))

    ;; 7. unknown content type returns empty string
    (test-case "unknown content type returns empty string"
      (check-equal? (content-part->text 42) "")
      (check-equal? (content-part->text #t) ""))

    ;; 8. empty string content preserved
    (test-case "empty string content preserved"
      (check-equal? (content-part->text (make-text-part "")) ""))

    ;; 9. content exactly 500 chars not truncated
    (test-case "content exactly 500 chars not truncated"
      (define exact (make-string 500 #\y))
      (check-equal? (content-part->text (make-tool-result-part "tc6" exact #f)) exact))))

(run-tests suite)
