#lang racket

;; BOUNDARY: unit

;; tests/test-deprecation-warning.rkt -- Tests for warn-deprecated! utility (S3)

(require rackunit
         rackunit/text-ui
         (only-in "../util/errors.rkt" warn-deprecated!))

(define deprecation-suite
  (test-suite "Deprecation warning utility"

    (test-case "warn-deprecated! does not raise"
      (check-not-exn
       (lambda ()
         (warn-deprecated! 'old-fn "v0.46.0" "Use new-fn instead"))))

    (test-case "warn-deprecated! works without extra notes"
      (check-not-exn
       (lambda ()
         (warn-deprecated! 'another-old-fn "v0.50.0"))))))

(run-tests deprecation-suite)
