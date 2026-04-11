#lang info

(define collection "q")
(define pkg-name "q")
(define version "0.6.5")
(define pkg-desc "A local-first, extensible coding agent runtime")

(define deps '("base"))

;; Optional — dynamically-required with fallback stubs.
;; Install separately for full TUI: raco pkg install tui-term tui-ubuf
;; These packages do NOT compile on Racket 8.10 (upstream bugs).
;; The TUI works without them using built-in fallbacks.

(define build-deps '("rackunit-lib"))

(define pkg-authors '("coinerd"))
(define pkg-license "MIT")
