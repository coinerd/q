#lang racket/base

;; util/deprecation.rkt — Deprecation warning utilities
;; Extracted from errors.rkt (W14 v0.72.5)

(provide warn-deprecated!)

;; Deprecation warning
(define (warn-deprecated! symbol-name removal-version [extra-notes #f])
  (log-warning (format "DEPRECATED: ~a (will be removed in ~a).~a"
                       symbol-name
                       removal-version
                       (if extra-notes
                           (format " ~a" extra-notes)
                           ""))))
