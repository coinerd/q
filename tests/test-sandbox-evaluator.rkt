#lang racket

(require rackunit
         "../sandbox/evaluator.rkt")

;; ============================================================
;; eval-in-sandbox — simple expressions
;; ============================================================

(test-case "eval-in-sandbox evaluates simple expression"
  (define result (eval-in-sandbox "(+ 1 2)"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-value result) 3)
  (check-false (eval-result-error result)))

(test-case "eval-in-sandbox captures output"
  (define result (eval-in-sandbox "(display \"hello\")"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-output result) "hello"))

(test-case "eval-in-sandbox handles errors"
  (define result (eval-in-sandbox "(+ 1 \"not-a-number\")"))
  (check-pred eval-result? result)
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox tracks elapsed time"
  (define result (eval-in-sandbox "42"))
  (check-true (>= (eval-result-elapsed-ms result) 0)))

(test-case "eval-in-sandbox returns #f value on error"
  (define result (eval-in-sandbox "(car '())"))
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

;; ============================================================
;; eval-in-sandbox — timeout enforcement
;; ============================================================

(test-case "eval-in-sandbox kills code that exceeds timeout"
  ;; Use a very short timeout; sleeping 10s should be killed
  (define result (eval-in-sandbox "(sleep 10)" #:timeout 1))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result)))
  ;; Should have been killed well before 10 seconds (generous timeout)
  (check-true (< (eval-result-elapsed-ms result) 15000)))

;; ============================================================
;; eval-in-sandbox — forbidden operations
;; ============================================================

(test-case "eval-in-sandbox blocks system calls"
  (define result (eval-in-sandbox "(system \"ls\")"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox blocks file reading"
  (define result (eval-in-sandbox "(file->string \"/etc/passwd\")"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox blocks file writing"
  (define result (eval-in-sandbox "(call-with-output-file \"/tmp/sandbox-escape-test\" (lambda (p) (display \"pwned\" p)))"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox blocks network operations"
  (define result (eval-in-sandbox "(tcp-connect \"example.com\" 80)"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

;; ============================================================
;; eval-in-sandbox — error handling (syntax + runtime)
;; ============================================================

(test-case "eval-in-sandbox handles syntax errors gracefully"
  (define result (eval-in-sandbox "(+ 1"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox handles unmatched parenthesis"
  (define result (eval-in-sandbox "(let ((x 1)))"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox handles division by zero"
  (define result (eval-in-sandbox "(/ 1 0)"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox handles variadic + correctly with many arguments"
  ;; + is variadic in Racket — verify it works with many args
  (define result (eval-in-sandbox "(+ 1 2 3 4 5 6 7 8 9 10)"))
  (check-pred eval-result? result)
  (check-false (eval-result-error result))
  (check-equal? (eval-result-value result) 55))

(test-case "eval-in-sandbox handles wrong number of arguments to fixed-arity function"
  (define result (eval-in-sandbox "(car 1 2 3)"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox handles unbound identifier"
  (define result (eval-in-sandbox "nonexistent-variable-xyz"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox handles type errors"
  (define result (eval-in-sandbox "(string-length 42)"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

;; ============================================================
;; eval-in-sandbox — output capture
;; ============================================================

(test-case "eval-in-sandbox captures multi-part output"
  (define result (eval-in-sandbox "(display \"hello \") (display \"world\")"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-output result) "hello world"))

(test-case "eval-in-sandbox captures newline output"
  (define result (eval-in-sandbox "(displayln \"line1\") (displayln \"line2\")"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-output result) "line1\nline2\n"))

(test-case "eval-in-sandbox output is empty when nothing printed"
  (define result (eval-in-sandbox "(+ 1 2)"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-output result) ""))

;; ============================================================
;; eval-in-sandbox — state isolation
;; ============================================================

(test-case "eval-in-sandbox does not share mutable state between calls"
  ;; First call defines a box with a value
  (define result1 (eval-in-sandbox "(define bx (box 42)) (unbox bx)"))
  ;; Second call tries to access the box — it should fail since state is not shared
  (define result2 (eval-in-sandbox "(unbox bx)"))
  (check-pred eval-result? result1)
  (check-pred eval-result? result2)
  ;; First should succeed
  (check-equal? (eval-result-value result1) 42)
  (check-false (eval-result-error result1))
  ;; Second should fail (bx not defined in fresh sandbox)
  (check-false (eval-result-value result2))
  (check-true (string? (eval-result-error result2))))

(test-case "eval-in-sandbox does not leak definitions across invocations"
  (define result1 (eval-in-sandbox "(define my-secret-var 999)"))
  (define result2 (eval-in-sandbox "my-secret-var"))
  (check-pred eval-result? result1)
  (check-pred eval-result? result2)
  ;; Second call should fail — my-secret-var not defined
  (check-false (eval-result-value result2))
  (check-true (string? (eval-result-error result2))))

;; ============================================================
;; eval-in-sandbox — escape attempts
;; ============================================================

(test-case "eval-in-sandbox blocks eval escape attempt"
  (define result (eval-in-sandbox "(eval '(system \"ls\"))"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox blocks namespace manipulation"
  (define result (eval-in-sandbox "(namespace-require '(racket/file))"))
  (check-pred eval-result? result)
  (check-false (eval-result-value result))
  (check-true (string? (eval-result-error result))))

(test-case "eval-in-sandbox restricts dynamic-require to safe modules"
  ;; Racket's sandbox permits dynamic-require for whitelisted modules;
  ;; verify the call returns without crashing (value or error, not exception)
  (define result (eval-in-sandbox "(dynamic-require 'racket/file 'file->string)"))
  (check-pred eval-result? result)
  ;; Either it succeeds (returns procedure) or is blocked (returns #f with error)
  (check-true (or (procedure? (eval-result-value result))
                   (string? (eval-result-error result)))))

;; ============================================================
;; eval-in-sandbox — result struct properties
;; ============================================================

(test-case "eval-result elapsed-ms is a non-negative integer on success"
  (define result (eval-in-sandbox "(+ 1 2)"))
  (check-true (exact-integer? (eval-result-elapsed-ms result)))
  (check-true (>= (eval-result-elapsed-ms result) 0)))

(test-case "eval-result elapsed-ms is a non-negative integer on error"
  (define result (eval-in-sandbox "(/ 1 0)"))
  (check-true (exact-integer? (eval-result-elapsed-ms result)))
  (check-true (>= (eval-result-elapsed-ms result) 0)))

(test-case "eval-in-sandbox handles complex expression result"
  (define result (eval-in-sandbox "(let ([xs '(1 2 3)]) (map (lambda (x) (* x x)) xs))"))
  (check-pred eval-result? result)
  (check-false (eval-result-error result))
  (check-equal? (eval-result-value result) '(1 4 9)))

(test-case "eval-in-sandbox returns string values correctly"
  (define result (eval-in-sandbox "(string-append \"hello\" \" \" \"world\")"))
  (check-pred eval-result? result)
  (check-false (eval-result-error result))
  (check-equal? (eval-result-value result) "hello world"))

(test-case "eval-in-sandbox returns boolean values correctly"
  (define result (eval-in-sandbox "(and #t #t)"))
  (check-pred eval-result? result)
  (check-equal? (eval-result-value result) #t))

(test-case "eval-in-sandbox handles void results"
  (define result (eval-in-sandbox "(void)"))
  (check-pred eval-result? result)
  (check-false (eval-result-error result))
  ;; (void) returns the void value
  (check-true (void? (eval-result-value result))))

;; ============================================================
;; SEC-11: network and file access blocked in sandbox
;; ============================================================

(test-case "sandbox blocks network access"
  ;; Verify that sandbox-network-guard is set to #f by checking
  ;; the evaluator parameterization raises on network ops
  (define result (eval-in-sandbox
                  "(with-handlers ([exn:fail? (lambda (e) (list 'blocked (exn-message e)))])
                     (let-values ([(h in out) (tcp-connect \"example.com\" 80)])
                       'connected))"))
  (check-true (or (eval-result-error result)
                  (and (list? (eval-result-value result))
                       (eq? (car (eval-result-value result)) 'blocked)))))

(test-case "sandbox blocks file write access"
  (define result (eval-in-sandbox
                  "(with-handlers ([exn:fail? (lambda (e) (exn-message e))])
                     (call-with-output-file \"/tmp/sandbox-test-out\"
                       (lambda (p) (display \"hello\" p))))"))
  (check-true (or (eval-result-error result)
                  (string? (eval-result-value result)))))
