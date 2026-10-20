#lang info

;; Version: keep in sync with util/version.rkt (canonical source).
;; Run `racket scripts/sync-version.rkt --write` after bumping util/version.rkt.

(define collection "q")
(define pkg-name "q")
(define version "1.00.31")
(define pkg-desc "A local-first, extensible coding agent runtime")

;; Owned regression evidence authenticates structured RackUnit assertions at runtime.
(define deps '("base" "gui-easy-lib" "rackunit-lib"))

;; The TUI uses native ANSI escape sequences.
;; GUI mode requires gui-easy-lib (optional — graceful fallback if absent).

(define build-deps '("quickcheck" "fmt"))

;; Frozen discovery-parity fixtures (tests/metadata-discovery/fixture) are
;; data files for tests/ci/metadata-discovery-test.rkt: deliberately without
;; #lang (and one git symlink), so raco setup must not compile them.
(define compile-omit-paths
  (list "tests/metadata-discovery/fixture"
        ;; BUG-0088 probe fixtures are overlay payloads: their relative
        ;; requires resolve inside the fixture repository they are copied
        ;; into, never at this in-tree location.
        "tests/fixtures/supplemental-evidence"))

(define pkg-authors '("coinerd"))
(define pkg-license "MIT")
