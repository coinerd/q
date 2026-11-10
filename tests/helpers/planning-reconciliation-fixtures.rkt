#lang racket/base
;; Support module for tests/test-planning-reconciliation-report.rkt — the
;; planning reconciliation report content contract. The report path is
;; anchored to this file's own load directory (tests/helpers/../../ =
;; checkout root), so the checks resolve under any runner cwd (direct
;; racket, raco test, or scripts/run-tests.rkt subprocess mode). Guarded
;; reads keep a missing report a clean failure, not an error.
(require racket/file
         racket/runtime-path
         racket/string)
(provide report-exists?
         report-contains?)
(define-runtime-path repo-root "../..")
(define report-path (build-path repo-root "docs" "reports" "PLANNING-RECONCILIATION-2026-10-31.md"))
(define (report-exists?)
  (file-exists? report-path))
(define (report-contains? needle)
  (and (file-exists? report-path) (string-contains? (file->string report-path) needle)))
