#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-string-helpers.rkt — Canonical truncate-string tests

(require rackunit
         "../util/string-helpers.rkt")

(test-case "string-helpers: truncate-string: short string unchanged"
  (check-equal? (truncate-string "hello" 10) "hello"))

(test-case "string-helpers: truncate-string: exact length unchanged"
  (check-equal? (truncate-string "hello" 5) "hello"))

(test-case "truncate-string: long string truncated with default ellipsis"
  (define result (truncate-string "hello world!" 8))
  (check-equal? result "hello...")
  (check-true (<= (string-length result) 8)))

(test-case "truncate-string: custom ellipsis"
  (define result (truncate-string "hello world!" 7 #:ellipsis "…"))
  (check-equal? result "hello …")
  (check-true (<= (string-length result) 7)))

(test-case "truncate-string: very small max-len"
  ;; With max-len < ellipsis length, ellipsis still appears
  (check-equal? (truncate-string "hello" 0) "..."))

(test-case "truncate-string: empty string"
  (check-equal? (truncate-string "" 5) ""))
