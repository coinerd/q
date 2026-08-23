;; @suite unit
;; @speed fast
;; Fixture: deeply nested test file (two levels below tests/). MUST be
;; discovered in every invocation mode.
(module+ test
  (require rackunit)
  (check-true #t))
