;; @suite unit
;; @speed fast
;; Fixture: nested test directory (one level below tests/). MUST be
;; discovered in every invocation mode (in-directory walks recursively).
(module+ test
  (require rackunit)
  (check-true #t))
