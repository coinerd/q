#lang racket/base

;; extensions/racket-tooling-handlers.rkt — Tool handlers for Racket editing
;;
;; Thin facade: re-exports from racket-tooling/analysis, formatting, rewrite.
;; All handler logic lives in sub-modules.

(require "racket-tooling/analysis.rkt"
         "racket-tooling/formatting.rkt"
         "racket-tooling/rewrite.rkt")

(provide (all-from-out "racket-tooling/analysis.rkt")
         (all-from-out "racket-tooling/formatting.rkt")
         (all-from-out "racket-tooling/rewrite.rkt"))
