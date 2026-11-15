#lang racket/base
;; @suite testing
;; @speed fast
;; @boundary unit
(require rackunit
         "../scripts/run-tests/profiles.rkt"
         "../scripts/run-tests/parse.rkt")
(module+ test
  (define r (make-skipped-result "x" 'ci '(network)))
  (check-equal? (classify-test-result r) 'SKIPPED_BY_PROFILE)
  (check-regexp-match #rx"requires=network" (bytes->string/utf-8 (test-file-result-stdout-bytes r))))
