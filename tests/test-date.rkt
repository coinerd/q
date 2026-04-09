#lang racket

;; tests/test-date.rkt — tests for tools/builtins/date.rkt

(require rackunit
         rackunit/text-ui
         "../tools/builtins/date.rkt"
         "../tools/tool.rkt")

;; ============================================================
;; Helper
;; ============================================================

(define (content-text result)
  (define content (tool-result-content result))
  (cond
    [(string? content) content]
    [(list? content)
     (string-join
      (for/list ([part (in-list content)])
        (cond
          [(string? part) part]
          [(hash? part) (hash-ref part 'text "")]
          [else ""]))
      "\n")]
    [else ""]))

;; ============================================================
;; Tests
;; ============================================================

(test-case "date tool returns ISO format by default"
  (define result (tool-date (hasheq)))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  ;; Should match YYYY-MM-DDTHH:MM:SS
  (check-true (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}$" text)
              (format "Expected ISO format, got: ~a" text)))

(test-case "date tool returns date format"
  (define result (tool-date (hasheq 'format "date")))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  (check-true (regexp-match? #px"^[0-9]{4}-[0-9]{2}-[0-9]{2}$" text)
              (format "Expected date format, got: ~a" text)))

(test-case "date tool returns time format"
  (define result (tool-date (hasheq 'format "time")))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  (check-true (regexp-match? #px"^[0-9]{2}:[0-9]{2}:[0-9]{2}$" text)
              (format "Expected time format, got: ~a" text)))

(test-case "date tool returns unix epoch"
  (define result (tool-date (hasheq 'format "unix")))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  (check-true (regexp-match? #px"^[0-9]+$" text)
              (format "Expected unix timestamp, got: ~a" text))
  ;; Should be > 1700000000 (after 2023)
  (check-true (> (string->number text) 1700000000)
              (format "Expected recent timestamp, got: ~a" text)))

(test-case "date tool returns weekday"
  (define result (tool-date (hasheq 'format "weekday")))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  (check-not-false (member text '("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))
              (format "Expected weekday name, got: ~a" text)))

(test-case "date tool returns iso-full format"
  (define result (tool-date (hasheq 'format "iso-full")))
  (check-false (tool-result-is-error? result))
  (define text (content-text result))
  ;; Should contain weekday name in parens
  (check-true (regexp-match? #px"\\(Sunday|Monday|Tuesday|Wednesday|Thursday|Friday|Saturday\\)" text)
              (format "Expected iso-full with weekday, got: ~a" text)))

(test-case "date tool returns error for unknown format"
  (define result (tool-date (hasheq 'format "bogus")))
  ;; Should still succeed (returns helpful message, not error struct)
  (define text (content-text result))
  (check-true (string-contains? text "Unknown format")
              (format "Expected 'Unknown format' message, got: ~a" text)))

(test-case "date tool result has details hash"
  (define result (tool-date (hasheq 'format "date")))
  (define details (tool-result-details result))
  (check-equal? (hash-ref details 'format) "date"))

(test-case "date tool accepts exec-ctx parameter"
  (define result (tool-date (hasheq) #f))
  (check-false (tool-result-is-error? result)))
