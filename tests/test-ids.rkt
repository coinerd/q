#lang racket

(require rackunit
         rackunit/text-ui
         "../util/ids.rkt")

(define ids-suite
  (test-suite
   "ids tests"

   (test-case "generate-id returns a string"
     (define id (generate-id))
     (check-pred string? id))

   (test-case "generate-id is non-empty"
     (define id (generate-id))
     (check-true (> (string-length id) 0)))

   (test-case "generate-id produces unique IDs"
     (define ids (for/list ([_ (in-range 1000)])
                   (generate-id)))
     (check-equal? (length ids) (length (remove-duplicates ids))))

   (test-case "IDs are lexicographically sortable (monotonic)"
     (define ids (for/list ([_ (in-range 100)])
                   (generate-id)))
     (define sorted (sort ids string<?))
     (check-equal? ids sorted))

   (test-case "id? returns #t for generated IDs"
     (for ([_ (in-range 10)])
       (check-true (id? (generate-id)))))

   (test-case "id? returns #f for non-ID values"
     (check-false (id? ""))
     (check-false (id? "abc"))
     (check-false (id? 42))
     (check-false (id? '()))
     (check-false (id? #f)))

   (test-case "id->string and string->id roundtrip"
     (define id (generate-id))
     (define s (id->string id))
     (check-pred string? s)
     (check-equal? (string->id s) id))

   (test-case "id->string returns the string itself"
     (define id (generate-id))
     (check-equal? (id->string id) id))

   (test-case "string->id returns #f for invalid strings"
     (check-false (string->id ""))
     (check-false (string->id "short"))
     (check-false (string->id "not-an-id-at-all")))))

(run-tests ids-suite 'verbose)
