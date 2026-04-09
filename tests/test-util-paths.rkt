#lang racket

(require rackunit
         "../util/paths.rkt")

;; util/paths.rkt is currently a placeholder module with no exports.
;; This test file ensures the module loads without error.

(test-case "util/paths module loads cleanly"
  ;; The module has no exports; just verifying it can be required
  (check-true #t))
