#lang racket/base

;; tests/test-hof-combinators.rkt — Tests for HOF combinators
;;
;; Tests the three core combinators from extensions/combinators.rkt:
;;   - with-timeout
;;   - with-error-policy
;;   - with-hook-validation

(require rackunit
         racket/contract
         "../extensions/combinators.rkt"
         "../util/hook-types.rkt")

;; ============================================================
;; Test: with-timeout combinator
;; ============================================================

(test-case "with-timeout: successful computation"
  (check-equal? (with-timeout 1000 (lambda () 42)) 42))

(test-case "with-timeout: timeout returns on-timeout value"
  (check-equal? (with-timeout 50
                              (lambda ()
                                (sleep 1)
                                'should-not-return)
                              #:on-timeout (lambda () 'timed-out))
                'timed-out))

(test-case "with-timeout: exception calls on-error"
  (define caught #f)
  (with-timeout 1000
                (lambda () (raise (exn:fail "test" (current-continuation-marks))))
                #:on-error (lambda (e)
                             (set! caught #t)
                             'error-handled))
  (check-true caught))

;; ============================================================
;; Test: with-error-policy combinator
;; ============================================================

(test-case "with-error-policy: successful computation (non-critical)"
  (check-equal? (with-error-policy #f (lambda () 'ok) (lambda () 'error)) 'ok))

(test-case "with-error-policy: error returns default (non-critical)"
  (check-equal? (with-error-policy #f
                                   (lambda () (raise (exn:fail "test" (current-continuation-marks))))
                                   (lambda () 'error-default))
                'error-default))

(test-case "with-error-policy: error returns default (critical)"
  (check-equal? (with-error-policy #t
                                   (lambda () (raise (exn:fail "test" (current-continuation-marks))))
                                   (lambda () 'critical-error))
                'critical-error))

;; ============================================================
;; Test: with-hook-validation combinator
;; ============================================================

(test-case "with-hook-validation: valid hook-result (pass)"
  (define result
    (with-hook-validation 'test-ext
                          'some-hook
                          (hook-pass 'payload)
                          (lambda () (hook-block "default"))))
  (check-true (hook-result? result))
  (check-equal? (hook-result-action result) 'pass))

(test-case "with-hook-validation: valid hook-result (block)"
  (define result
    (with-hook-validation 'test-ext
                          'some-hook
                          (hook-block "blocked")
                          (lambda () (hook-pass 'default))))
  (check-true (hook-result? result))
  (check-equal? (hook-result-action result) 'block))

(test-case "with-hook-validation: invalid action returns default"
  (define invalid-result (hook-result 'invalid-action 'payload))
  (define called-default #f)
  (define result
    (with-hook-validation 'test-ext
                          'some-hook
                          invalid-result
                          (lambda ()
                            (set! called-default #t)
                            (hook-pass 'default))))
  (check-true called-default)
  (check-equal? (hook-result-action result) 'pass))

(test-case "with-hook-validation: non-hook-result returns default"
  (define called-default #f)
  (define result
    (with-hook-validation 'test-ext
                          'some-hook
                          'not-a-hook-result
                          (lambda ()
                            (set! called-default #t)
                            (hook-pass 'default))))
  (check-true called-default)
  (check-equal? (hook-result-action result) 'pass))

;; ============================================================
;; Test: Integration — combinators work together
;; ============================================================

(test-case "Integration: timeout + error-policy"
  (define result
    (with-timeout 1000
                  (lambda () (with-error-policy #f (lambda () (+ 1 2)) (lambda () 'error)))
                  #:on-timeout (lambda () 'timeout)))
  (check-equal? result 3))

(test-case "Integration: error-policy + hook-validation"
  (define result
    (with-error-policy
     #f
     (lambda ()
       (with-hook-validation 'ext 'hook (hook-amend 'new-payload) (lambda () (hook-pass 'default))))
     (lambda () 'error-default)))
  (check-equal? (hook-result-action result) 'amend)
  (check-equal? (hook-result-payload result) 'new-payload))

;; ============================================================
;; Done
;; ============================================================
(provide (all-defined-out))
