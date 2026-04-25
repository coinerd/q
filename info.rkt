#lang info

;; Version: keep in sync with util/version.rkt (canonical source).
;; Run `racket scripts/sync-version.rkt --write` after bumping util/version.rkt.

(define collection "q")
(define pkg-name "q")
(define version "0.19.10")
(define pkg-desc "A local-first, extensible coding agent runtime")

(define deps '("base"))

;; Optional — dynamically-required with fallback stubs.
;; Install separately for full TUI: raco pkg install tui-term tui-ubuf
;; These packages do NOT compile on Racket 8.10 (upstream bugs).
;; The TUI works without them using built-in fallbacks.

(define build-deps '("rackunit-lib" "quickcheck" "fmt"))

(define pkg-authors '("coinerd"))
(define pkg-license "MIT")
