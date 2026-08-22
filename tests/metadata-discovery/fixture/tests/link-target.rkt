;; @suite unit
;; @speed fast
;; Fixture: symlink target. This file exists outside of any subdirectory and
;; is the canonical content that symlinked-test.rkt points at. Both the
;; target and the symlink are expected to be discovered as distinct paths
;; when the platform preserves symlinks; on platforms where the symlink
;; materializes as a regular file the copy still must be discovered.
(module+ test
  (require rackunit)
  (check-true #t))
