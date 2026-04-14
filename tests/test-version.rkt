#lang racket

(require rackunit
         rackunit/text-ui
         "../util/version.rkt")

(define version-suite
  (test-suite
   "version tests"

   (test-case "q-version is a string"
     (check-true (string? q-version)))

   (test-case "q-version matches semver pattern"
     (check-true (regexp-match? #rx"^[0-9]+\\.[0-9]+\\.[0-9]+" q-version)))

   (test-case "q-version is non-empty"
     (check-true (> (string-length q-version) 0)))))

(run-tests version-suite)
