#lang info

;; Version: keep in sync with util/version.rkt (canonical source).
;; Run `racket scripts/sync-version.rkt --write` after bumping util/version.rkt.

(define collection "q")
(define pkg-name "q")
(define version "0.74.5")
(define pkg-desc "A local-first, extensible coding agent runtime")

(define deps '("base" "gui-easy-lib"))

;; The TUI uses native ANSI escape sequences.
;; GUI mode requires gui-easy-lib (optional — graceful fallback if absent).

(define build-deps '("rackunit-lib" "quickcheck" "fmt"))

(define pkg-authors '("coinerd"))
(define pkg-license "MIT")
