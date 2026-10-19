#lang racket
;; @speed fast
;; @suite extensions
;; @boundary unit
;; @timeout 120
(require rackunit
         "helpers/tracker-evidence.rkt")
(provide evidence-suite)
(define evidence-suite
  (test-suite "tracker normalization"
    (test-case "valid binding normalizes exactly"
      (check-true (valid-tracker-binding-normalizes?)))))
(module+ test
  (require rackunit/text-ui)
  (exit (run-tests evidence-suite)))
