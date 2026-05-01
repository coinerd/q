#lang racket/base

;; extensions/gsd-planning/plan-diff.rkt — plan comparison, wave docs summary, change detection
;;
;; Utilities for comparing plan versions and summarizing wave status.

(require racket/string
         (only-in "../gsd/plan-types.rkt"
                  gsd-plan-waves
                  gsd-wave-index
                  gsd-wave-title
                  gsd-wave-slug
                  gsd-wave-status
                  gsd-wave-root-cause
                  wave-status->string)
         )

(provide wave-docs-summary
         plan-wave-summary)

(define (wave-docs-summary plan)
  (define waves (gsd-plan-waves plan))
  (string-join
   (for/list ([w waves])
     (define idx (gsd-wave-index w))
     (define title (gsd-wave-title w))
     (define slug (gsd-wave-slug w))
     (define status (wave-status->string (gsd-wave-status w)))
     (format "## W~a: ~a (~a)\n~a"
             idx title status
             (if (and (string? (gsd-wave-root-cause w))
                      (> (string-length (gsd-wave-root-cause w)) 0))
                 (gsd-wave-root-cause w)
                 "(no details)")))
   "\n\n"))

(define (plan-wave-summary plan)
  (define waves (gsd-plan-waves plan))
  (define total (length waves))
  (define completed
    (for/sum ([w waves]
              #:when (eq? (gsd-wave-status w) 'completed))
      1))
  (format "Waves: ~a/~a completed" completed total))
