;; Fixture: ordinary test file WITHOUT declarative metadata. Relies on
;; filename/path heuristics (classification: heuristic). Still a test file:
;; MUST be discovered in every invocation mode.
(module+ test
  (require rackunit)
  (check-true #t))
