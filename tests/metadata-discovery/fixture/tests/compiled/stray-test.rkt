;; Fixture: stray .rkt inside a compiled/ directory. The discovery walk
;; excludes any path containing "/compiled/" (stale bytecode areas), so this
;; file MUST NOT be discovered in any invocation mode.
(module+ test
  (require rackunit)
  (check-true #t))
