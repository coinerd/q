#lang info

;; Version: keep in sync with util/version.rkt (canonical source).
;; Run `racket scripts/sync-version.rkt --write` after bumping util/version.rkt.

(define collection "q")
(define pkg-name "q")
(define version "1.00.21")
(define pkg-desc "A local-first, extensible coding agent runtime")

(define deps '("base" "gui-easy-lib"))

;; The TUI uses native ANSI escape sequences.
;; GUI mode requires gui-easy-lib (optional — graceful fallback if absent).

(define build-deps '("rackunit-lib" "quickcheck" "fmt"))

;; Frozen discovery-parity fixtures (tests/metadata-discovery/fixture) are
;; data files for tests/ci/metadata-discovery-test.rkt: deliberately without
;; #lang (and one git symlink), so raco setup must not compile them.
(define compile-omit-paths '("tests/metadata-discovery/fixture"))

(define pkg-authors '("coinerd"))
(define pkg-license "MIT")
