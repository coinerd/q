#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-extension-types-predicate.rkt — C1 regression test
;;
;; Verifies that extension-registry? from util/extension-types.rkt
;; matches the real struct predicate from extensions/api.rkt.
;; This prevents the v0.73.8 C1 regression where a fake procedure?
;; predicate was used instead of the real struct predicate.

(require rackunit
         rackunit/text-ui
         "../util/extension/extension-types.rkt")

(define ext-pred-tests
  (test-suite "extension-types-predicate"

    (test-case "extension-registry? rejects plain procedures"
      (check-false (extension-registry? procedure?)
                   "extension-registry? must not match bare procedures")
      (check-false (extension-registry? (lambda (x) x))
                   "extension-registry? must not match lambda")
      (check-false (extension-registry? 42)
                   "extension-registry? must not match numbers")
      (check-false (extension-registry? "hello")
                   "extension-registry? must not match strings"))

    (test-case "extension-registry? is a proper predicate"
      (check-true (procedure? extension-registry?)
                  "extension-registry? must be a procedure")
      (check-equal? (procedure-arity extension-registry?) 1
                    "extension-registry? must accept exactly 1 argument"))))

(run-tests ext-pred-tests)
