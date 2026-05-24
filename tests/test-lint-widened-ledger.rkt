#lang racket

(require rackunit
         racket/runtime-path)

(define-runtime-path script-path "../scripts/lint-widened-ledger.rkt")

(test-case "version<=?: basic comparisons"
  (define v<=? (dynamic-require script-path 'version<=?))
  (check-true (v<=? "0.55.0" "0.56.0"))
  (check-true (v<=? "0.55.0" "0.55.0"))
  (check-false (v<=? "0.56.0" "0.55.0"))
  (check-true (v<=? "0.58.0" "1.0.0"))
  (check-true (v<=? "0.1.0" "0.2.0")))

(test-case "parse-ledger-table: skips headers and separators"
  (define parse (dynamic-require script-path 'parse-ledger-table))
  (define lines
    '("| File | Function | Original | Widened | Reason | Revisit | Status |"
      "|------|----------|----------|---------|--------|---------|--------|"
      "| foo.rkt | bar | string? | any/c | test | v0.58.0 | open |"
      "not a table row"))
  (define result (parse lines))
  (check-equal? (length result) 1))

(test-case "parse-row: extracts fields"
  (define pr (dynamic-require script-path 'parse-row))
  (define result (pr "| foo.rkt | bar | string? | any/c | test | v0.58.0 | open |"))
  (check-not-false result)
  (when result
    (check-equal? (list-ref result 0) "foo.rkt")
    (check-equal? (list-ref result 1) "bar")
    (check-equal? (list-ref result 6) "open")))
