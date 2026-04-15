#lang racket

;;; tests/test-truncation.rkt — tests for standardized output truncation (#774)

(require rackunit
         rackunit/text-ui
         "../util/truncation.rkt")

(define (make-lines n)
  (string-join (for/list ([i (in-range n)]) (format "Line ~a" i)) "\n"))

(define (make-bytes n)
  (make-string n #\x))

(test-case "output within limits passes through unchanged"
  (define short "Hello, world!")
  (check-equal? (truncate-output short) short))

(test-case "output within line and byte limits unchanged"
  (define medium (make-lines 100))
  (check-equal? (truncate-output medium) medium))

(test-case "output exceeding MAX-OUTPUT-LINES is truncated"
  (define long (make-lines (+ MAX-OUTPUT-LINES 500)))
  (define result (truncate-output long))
  (check-true (string-contains? result "[Output truncated."))
  (check-true (string-contains? result "500 lines omitted"))
  ;; Should contain the first MAX-OUTPUT-LINES lines
  (check-true (string-contains? result "Line 0"))
  (check-true (string-contains? result (format "Line ~a" (- MAX-OUTPUT-LINES 1)))))

(test-case "output exceeding MAX-OUTPUT-BYTES is truncated"
  (define big (make-bytes (+ MAX-OUTPUT-BYTES 1000)))
  (define result (truncate-output big))
  (check-true (string-contains? result "[Output truncated."))
  (check-true (string-contains? result "bytes omitted")))

(test-case "truncate-to-n-lines keeps first N lines"
  (define text "a\nb\nc\nd\ne")
  (define result (truncate-to-n-lines text 3))
  (check-true (string-contains? result "a"))
  (check-true (string-contains? result "b"))
  (check-true (string-contains? result "c"))
  (check-true (string-contains? result "2 lines omitted")))

(test-case "truncate-to-n-lines unchanged when within limit"
  (define text "a\nb\nc")
  (check-equal? (truncate-to-n-lines text 5) text))

(test-case "output-exceeds-limits? returns #f for small output"
  (check-false (output-exceeds-limits? "small")))

(test-case "output-exceeds-limits? returns #t for large output"
  (define long (make-lines (+ MAX-OUTPUT-LINES 1)))
  (check-true (output-exceeds-limits? long)))

(test-case "constants have expected values"
  (check-equal? MAX-OUTPUT-BYTES 50000)
  (check-equal? MAX-OUTPUT-LINES 2000))
