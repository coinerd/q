#lang racket

(require rackunit
         "../util/diff.rkt")

;; util/diff.rkt is currently a placeholder module with no exports.
;; This test file ensures the module loads without error.

(test-case "util/diff module loads cleanly"
  ;; The module has no exports; just verifying it can be required
  (check-true #t))
