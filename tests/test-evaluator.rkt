#lang racket

;; tests/test-evaluator.rkt — tests for sandbox/evaluator.rkt (#222)
;;
;; Covers:
;;   eval-result struct (transparent, fields)
;;   eval-in-sandbox: arithmetic, strings, stdout, errors, timeout defaults

(require rackunit
         "../sandbox/evaluator.rkt")

;; ============================================================
;; 1. Simple arithmetic
;; ============================================================

(test-case "eval-in-sandbox: simple arithmetic returns correct value"
  (define r (eval-in-sandbox "(+ 1 2)"))
  (check-equal? (eval-result-value r) 3)
  (check-false (eval-result-error r)))

;; ============================================================
;; 2. String result
;; ============================================================

(test-case "eval-in-sandbox: string expression returns string value"
  (define r (eval-in-sandbox "\"hello\""))
  (check-equal? (eval-result-value r) "hello")
  (check-false (eval-result-error r)))

;; ============================================================
;; 3. Print to stdout
;; ============================================================

(test-case "eval-in-sandbox: displayln captures stdout in output"
  (define r (eval-in-sandbox "(displayln \"hi\")"))
  (check-true (string-contains? (eval-result-output r) "hi"))
  (check-false (eval-result-error r)))

;; ============================================================
;; 4. Error handling
;; ============================================================

(test-case "eval-in-sandbox: type error sets error field, value is #f"
  (define r (eval-in-sandbox "(+ 1 #f)"))
  (check-false (eval-result-value r))
  (check-not-false (eval-result-error r))
  (check-true (string? (eval-result-error r))))

;; ============================================================
;; 5. Struct field accessors
;; ============================================================

(test-case "eval-result has value, output, error, elapsed-ms fields"
  (define r (eval-in-sandbox "42"))
  (check-equal? (eval-result-value r) 42)
  (check-true (string? (eval-result-output r)))
  (check-false (eval-result-error r))
  (check-true (number? (eval-result-elapsed-ms r))))

;; ============================================================
;; 6. elapsed-ms is an exact integer
;; ============================================================

(test-case "eval-result-elapsed-ms returns exact integer"
  (define r (eval-in-sandbox "(+ 10 20)"))
  (check-true (exact-integer? (eval-result-elapsed-ms r)))
  (check-true (>= (eval-result-elapsed-ms r) 0)))

;; ============================================================
;; 7. Default timeout works — basic expression completes
;; ============================================================

(test-case "eval-in-sandbox: default timeout allows simple expression"
  (define r (eval-in-sandbox "(* 6 7)"))
  (check-equal? (eval-result-value r) 42)
  (check-false (eval-result-error r)))

;; ============================================================
;; 8. Result struct is transparent
;; ============================================================

(test-case "eval-result is transparent and recognized by eval-result?"
  (define r (eval-in-sandbox "'x"))
  (check-true (eval-result? r))
  ;; Transparent structs can be equal to manually constructed ones
  (check-equal? r (eval-result (eval-result-value r)
                                (eval-result-output r)
                                (eval-result-error r)
                                (eval-result-elapsed-ms r))))

;; ============================================================
;; 9. Multiple expressions: begin block
;; ============================================================

(test-case "eval-in-sandbox: begin block returns last expression value"
  (define r (eval-in-sandbox "(begin (define x 10) (+ x 5))"))
  (check-equal? (eval-result-value r) 15)
  (check-false (eval-result-error r)))

;; ============================================================
;; 10. Undefined identifier error
;; ============================================================

(test-case "eval-in-sandbox: undefined identifier produces error"
  (define r (eval-in-sandbox "nonexistent-var"))
  (check-false (eval-result-value r))
  (check-not-false (eval-result-error r))
  (check-true (string-contains? (eval-result-error r) "nonexistent-var")))

;; ============================================================
;; 11. Output is empty string for expression with no output
;; ============================================================

(test-case "eval-in-sandbox: expression with no prints has empty output"
  (define r (eval-in-sandbox "99"))
  (check-equal? (eval-result-output r) ""))
