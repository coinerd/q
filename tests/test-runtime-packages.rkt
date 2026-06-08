#lang racket/base

;; @speed fast  ;; @suite runtime

;; tests/test-runtime-packages.rkt — T2-5: runtime/ sub-packages scaffolding
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../runtime/context-assembly/config.rkt")

(define suite
  (test-suite "runtime/ Packages (T2-5)"

    (test-case "context-assembly config struct available"
      (check-true (procedure? default-context-assembly-config))
      (check-true (context-assembly-config? (default-context-assembly-config))))))

(run-tests suite)
