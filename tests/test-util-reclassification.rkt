#lang racket/base

;; tests/test-util-reclassification.rkt — T2-4: util/ reclassification scaffolding
;; STABILITY: evolving

(require rackunit
         rackunit/text-ui
         "../util/version.rkt"
         "../util/ids.rkt")

(define suite
  (test-suite "util/ Reclassification (T2-4)"

    (test-case "core util modules load and export"
      (check-equal? q-version "0.86.4")
      (check-true (procedure? generate-id)))))

(run-tests suite)
