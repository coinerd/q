#lang racket/base

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

;; tests/helpers/gsd-test-helpers.rkt — Test helpers for GSD context and delivery fixtures

(require racket/file
         (only-in "../../extensions/gsd/session-state.rkt" make-gsd-context))

;; Bind the synthetic merge evidence required before a test campaign may
;; advance beyond an approved wave. The SHA is intentionally unmistakable
;; fixture data and the file is always rooted in the caller's temporary project.
(define (bind-test-wave-merge-sha! project-root plan-id wave-index)
  (define evidence-dir (build-path project-root "docs" "reports" "gsd-wave-evidence"))
  (define evidence-path (build-path evidence-dir (format "~a-w~a.rktd" plan-id wave-index)))
  (make-directory* evidence-dir)
  (call-with-output-file evidence-path
                         (lambda (out)
                           (write '((merge-sha . "0123456789abcdef0123456789abcdef01234567")) out)
                           (newline out))
                         #:exists 'replace)
  evidence-path)

(provide make-gsd-context
         bind-test-wave-merge-sha!)
