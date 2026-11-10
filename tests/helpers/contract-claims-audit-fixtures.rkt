#lang racket/base
;; Support module for tests/test-contract-claims-audit.rkt — the contract-
;; claims audit content contract. The audit path is anchored to this file's
;; own load directory (tests/helpers/../../ = checkout root), so the checks
;; resolve under any runner cwd (direct racket, raco test, or
;; scripts/run-tests.rkt subprocess mode). Guarded reads keep a missing
;; audit a clean failure, not an error.
(require racket/file
         racket/runtime-path
         racket/string)
(provide audit-exists?
         audit-contains?)
(define-runtime-path repo-root "../..")
(define audit-path (build-path repo-root "docs" "reports" "CONTRACT-CLAIMS-AUDIT-2026-11-09.md"))
(define (audit-exists?)
  (file-exists? audit-path))
(define (audit-contains? needle)
  (and (file-exists? audit-path) (string-contains? (file->string audit-path) needle)))
