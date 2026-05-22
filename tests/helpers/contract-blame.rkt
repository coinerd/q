#lang racket/base

;; q/tests/helpers/contract-blame.rkt — Contract blame assertion helpers
;;
;; Provides:
;;   check-contract-blame  — asserts expression triggers contract violation
;;   check-contract-accepts — asserts value passes a contract

(require rackunit
         racket/contract)

(provide check-contract-blame
         check-contract-accepts)

;; check-contract-blame : (->* (string? (-> any)) (#:message (or/c string? #f)) void?)
;; Asserts that calling thunk triggers a contract violation.
;; Optionally checks that the error message contains #:message.
(define (check-contract-blame label thunk #:message [msg #f])
  (with-check-info
   (['label label])
   (define raised? #f)
   (define caught-msg #f)
   (with-handlers ([exn:fail:contract? (lambda (e)
                                         (set! raised? #t)
                                         (set! caught-msg (exn-message e)))])
     (thunk))
   (with-check-info
    (['contract-violation-raised? raised?] ['error-message caught-msg])
    (check-true raised? (format "~a: expected contract violation but none raised" label))
    (when (and msg raised?)
      (check-not-false (and caught-msg (regexp-match? (regexp-quote msg) caught-msg))
                       (format "~a: expected message containing ~a, got ~a" label msg caught-msg))))))

;; check-contract-accepts : (-> string? contract? any/c void?)
;; Asserts that value satisfies the given contract.
(define (check-contract-accepts label ct val)
  (with-check-info (['label label] ['contract ct] ['value val])
                   (check-not-exn (lambda () (contract ct val 'positive 'negative))
                                  (format "~a: value should satisfy contract" label))))
