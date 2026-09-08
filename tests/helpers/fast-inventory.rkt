#lang racket/base

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/helpers/fast-inventory.rkt — canonical resolved `fast` inventory for
;; v1.00.28 W0 (RUNTIME-AUDIT-SPEC §1: the fast tier is the governing
;; optimization scope). Thin adapter over scripts/run-tests/classify.rkt so
;; the census, its tests, and reports all cite one inventory source.

(provide fast-inventory-files
         fast-inventory-file?
         fast-inventory-metadata)

(require (only-in "../../scripts/run-tests/classify.rkt" collect-test-files get-file-metadata))

;; Canonical fast-tier inventory: sorted, root-relative paths.
(define (fast-inventory-files)
  (sort (collect-test-files 'fast) string<?))

;; Membership predicate over the canonical inventory.
(define (fast-inventory-file? path)
  (and (member path (fast-inventory-files)) #t))

;; Per-file metadata (@speed/@suite/@boundary/@covers ...) as a hash.
(define (fast-inventory-metadata path)
  (get-file-metadata path))
