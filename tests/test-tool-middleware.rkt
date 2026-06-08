#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-tool-middleware.rkt — Tests for tool middleware HOF
;;
;; v0.29.6 W1: Tests for composable middleware pipeline.

(require rackunit
         (only-in racket/string string-contains?)
         (only-in "../util/tool/tool-types.rkt"
                  tool-call
                  make-tool-call
                  tool-call-name
                  tool-call-arguments
                  tool-call-id)
         (only-in "../tools/tool.rkt"
                  tool-result?
                  tool-result-is-error?
                  tool-result-content
                  make-success-result
                  make-error-result)
         (only-in "../tools/middleware.rkt"
                  compose-middleware
                  make-hook-middleware
                  make-safe-mode-middleware
                  make-validation-middleware
                  make-permission-middleware
                  make-mutation-queue-middleware
                  make-default-pipeline))

;; Helper: create a fake tool-call
(define (make-test-tc name [args (hasheq)])
  (make-tool-call "tc-1" name args))

;; Helper: base executor that returns success (as tool-result)
(define (success-executor tc ctx)
  (make-success-result (format "executed ~a" (tool-call-name tc))))

;; ============================================================
;; 1. compose-middleware basics
;; ============================================================

(test-case "compose-middleware returns a procedure"
  (define pipeline (compose-middleware))
  (check-pred procedure? pipeline))

(test-case "compose-middleware with zero middleware calls base executor"
  (define pipeline (compose-middleware))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result)))

(test-case "compose-middleware with single middleware executes in order"
  (define log (box '()))
  (define (logging-mw tc ctx next)
    (set-box! log (cons 'pre (unbox log)))
    (define result (next tc ctx))
    (set-box! log (cons 'post (unbox log)))
    result)
  (define pipeline (compose-middleware logging-mw))
  (pipeline (make-test-tc "read") #f success-executor)
  (check-equal? (reverse (unbox log)) '(pre post)))

(test-case "compose-middleware with multiple middleware wraps correctly"
  (define log (box '()))
  (define (mw-outer tc ctx next)
    (set-box! log (cons 'outer-pre (unbox log)))
    (define result (next tc ctx))
    (set-box! log (cons 'outer-post (unbox log)))
    result)
  (define (mw-inner tc ctx next)
    (set-box! log (cons 'inner-pre (unbox log)))
    (define result (next tc ctx))
    (set-box! log (cons 'inner-post (unbox log)))
    result)
  (define pipeline (compose-middleware mw-outer mw-inner))
  (pipeline (make-test-tc "read") #f success-executor)
  (check-equal? (reverse (unbox log)) '(outer-pre inner-pre inner-post outer-post)))

;; ============================================================
;; 2. Short-circuit on block
;; ============================================================

(test-case "middleware block short-circuits chain"
  (define log (box '()))
  (define (blocking-mw tc ctx next)
    (make-error-result "blocked"))
  (define (counting-mw tc ctx next)
    (set-box! log (cons 'reached (unbox log)))
    (next tc ctx))
  (define pipeline (compose-middleware blocking-mw counting-mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-true (tool-result-is-error? result))
  (check-equal? (unbox log) '() "counting-mw should not be reached"))

;; ============================================================
;; 3. Error propagation
;; ============================================================

(test-case "middleware propagates errors through chain"
  (define (error-mw tc ctx next)
    (make-error-result "something went wrong"))
  (define pipeline (compose-middleware error-mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; 4. Hook middleware
;; ============================================================

(test-case "hook middleware passes through when no hook"
  (define mw (make-hook-middleware #f 'preflight))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result)))

(test-case "hook middleware blocks on block action"
  (define hook (lambda (phase tc) (hasheq 'action 'block)))
  (define mw (make-hook-middleware hook 'preflight))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-true (tool-result-is-error? result)))

(test-case "hook middleware amends tool-call"
  (define amended-tc (make-test-tc "write" (hasheq 'path "/tmp/test")))
  (define hook (lambda (phase tc) (hasheq 'action 'amend 'payload amended-tc)))
  (define mw (make-hook-middleware hook 'preflight))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result))
  (check-not-false (string-contains? (format "~a" (tool-result-content result)) "write")))

;; ============================================================
;; 5. Safe-mode middleware
;; ============================================================

(test-case "safe-mode middleware blocks restricted tool"
  (define mw (make-safe-mode-middleware (lambda () #t) (lambda (n) (equal? n "read"))))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "bash") #f success-executor))
  (check-true (tool-result-is-error? result)))

(test-case "safe-mode middleware allows permitted tool"
  (define mw (make-safe-mode-middleware (lambda () #t) (lambda (n) (equal? n "read"))))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result)))

(test-case "safe-mode middleware passes through when not in safe-mode"
  (define mw (make-safe-mode-middleware (lambda () #f) (lambda (_) #f)))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "bash") #f success-executor))
  (check-false (tool-result-is-error? result)))

;; ============================================================
;; 6. Validation middleware
;; ============================================================

(test-case "validation middleware passes valid args"
  (define mw (make-validation-middleware (lambda (t args) (void)) (lambda (_) 'some-tool)))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result)))

(test-case "validation middleware blocks invalid args"
  (define mw
    (make-validation-middleware (lambda (t args)
                                  (raise (exn:fail "invalid" (current-continuation-marks))))
                                (lambda (_) 'some-tool)))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-true (tool-result-is-error? result)))

(test-case "validation middleware blocks unknown tool"
  (define mw (make-validation-middleware (lambda (t args) (void)) (lambda (_) #f)))
  (define pipeline (compose-middleware mw))
  (define result (pipeline (make-test-tc "unknown") #f success-executor))
  (check-true (tool-result-is-error? result)))

;; ============================================================
;; 7. Default pipeline
;; ============================================================

(test-case "default-pipeline executes successfully"
  (define pipeline (make-default-pipeline #:lookup-fn (lambda (_) 'some-tool)))
  (define result (pipeline (make-test-tc "read") #f success-executor))
  (check-false (tool-result-is-error? result)))

(test-case "default-pipeline with safe-mode blocks restricted"
  (define pipeline
    (make-default-pipeline #:safe-mode? (lambda () #t)
                           #:allowed-tool? (lambda (n) (equal? n "read"))))
  (define result (pipeline (make-test-tc "bash") #f success-executor))
  (check-true (tool-result-is-error? result)))
