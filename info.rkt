#lang info

(define collection "q")
(define pkg-name "q")
(define version "0.3.1")
(define pkg-desc "A local-first, extensible coding agent runtime")

;; Runtime dependencies — all needed for full TUI + CLI experience.
;; tui-term and tui-ubuf are dynamically-required with fallback stubs,
;; but listed as deps so the full TUI works out of the box.
(define deps '("base"
               "tui-term"
               "tui-ubuf"))

(define build-deps '("rackunit-lib"))
