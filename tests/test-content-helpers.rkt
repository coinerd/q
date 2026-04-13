#lang racket

(require rackunit
         "../util/content-helpers.rkt")

;; ============================================================
;; Test suite: util/content-helpers.rkt — content helpers
;; ============================================================

;; --- result-content->string ---

(test-case "result-content->string: string input returns same string"
  (check-equal? (result-content->string "hello") "hello"))

(test-case "result-content->string: list of strings joined with newline"
  (check-equal? (result-content->string '("line1" "line2")) "line1\nline2"))

(test-case "result-content->string: list with hash extracts text"
  (define result (result-content->string (list (hasheq 'text "hello"))))
  (check-equal? result "hello"))

;; --- tool-result-content->string ---

(test-case "tool-result-content->string: hash input extracts text key"
  (check-equal? (tool-result-content->string (hasheq 'text "from-hash")) "from-hash"))

(test-case "tool-result-content->string: list of strings joined with newline"
  (check-equal? (tool-result-content->string '("a" "b")) "a\nb"))

(test-case "tool-result-content->string: other types formatted"
  (check-equal? (tool-result-content->string 42) "42"))
