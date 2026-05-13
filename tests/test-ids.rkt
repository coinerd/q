#lang racket

;; BOUNDARY: integration

(require rackunit
         rackunit/text-ui
         "../util/ids.rkt")

(define ids-suite
  (test-suite "ids tests"

    (test-case "generate-id returns a string"
      (define id (generate-id))
      (check-pred string? id))

    (test-case "generate-id is non-empty"
      (define id (generate-id))
      (check-true (> (string-length id) 0)))

    (test-case "generate-id produces unique IDs"
      (define ids
        (for/list ([_ (in-range 1000)])
          (generate-id)))
      (check-equal? (length ids) (length (remove-duplicates ids))))

    (test-case "IDs are lexicographically sortable (monotonic)"
      (define ids
        (for/list ([_ (in-range 100)])
          (generate-id)))
      (define sorted (sort ids string<?))
      (check-equal? ids sorted))))

(run-tests ids-suite 'verbose)
