#lang racket/base

;; skills/types.rkt — facade re-exporting from sub-modules
;;
;; Split from the original monolithic types.rkt (Issue #205, QUAL-09).
;; Now re-exports everything from:
;;   - resource-loader.rkt (resource types, loading, merging)
;;   - template.rkt (template rendering)
;;
;; All existing consumers importing skills/types.rkt continue to work unchanged.

(require "resource-loader.rkt"
         "template.rkt")

(provide (all-from-out "resource-loader.rkt")
         (all-from-out "template.rkt"))
