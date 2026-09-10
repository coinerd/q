#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/helpers/metadata.rkt — stable in-tree accessor for per-test-file
;; metadata (@speed/@suite/@boundary/@covers parser results) used by census
;; and inventory consumers. Thin re-export over scripts/run-tests/classify.rkt
;; so tests never reach into script internals directly.

(provide get-file-metadata)

(require (only-in "../../scripts/run-tests/classify.rkt" get-file-metadata))
