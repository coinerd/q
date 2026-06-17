#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-runtime-packages.rkt — T2-5: runtime/ sub-packages scaffolding
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/budgeting.rkt")

(define suite
  (test-suite "runtime/ Packages (T2-5)"

    (test-case "context-assembly config struct available"
      (check-true (procedure? make-context-assembly-config))
      (check-true (context-assembly-config? (make-context-assembly-config))))))

(run-tests suite)
