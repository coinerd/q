#lang racket/base

;; BOUNDARY: integration

;; tests/test-extension-cleanup-w0.rkt — Extension loader cleanup tests (W-12, I-10, I-22)

(require rackunit
         "../extensions/loader.rkt"
         "../sandbox/limits.rkt")

;; ============================================================
;; W-12: Exception classification in extension loader
;; ============================================================

(test-case "classify-exception classifies generic failures as unknown"
  (check-equal? (classify-exception (exn:fail "test" (current-continuation-marks))) 'unknown))

(test-case "extension-load-error struct has category field"
  (define err (extension-load-error "fake-path.rkt" "test error" 'syntax-error))
  (check-true (extension-load-error? err))
  (check-equal? (extension-load-error-path err) "fake-path.rkt")
  (check-equal? (extension-load-error-message err) "test error")
  (check-equal? (extension-load-error-category err) 'syntax-error))

(test-case "extension-load-error supports new categories"
  (check-equal? (extension-load-error-category (extension-load-error "/p" "m" 'filesystem-error))
                'filesystem-error)
  (check-equal? (extension-load-error-category (extension-load-error "/p" "m" 'contract-error))
                'contract-error))

;; ============================================================
;; I-22: Unified process tracking
;; ============================================================

(test-case "get-process-count reads from box"
  (reset-process-count!)
  (check-equal? (get-process-count) 0)
  (track-process!)
  (check-equal? (get-process-count) 1)
  (untrack-process!)
  (check-equal? (get-process-count) 0))

(test-case "reset-process-count! clears count"
  (reset-process-count!)
  (track-process!)
  (track-process!)
  (check-equal? (get-process-count) 2)
  (reset-process-count!)
  (check-equal? (get-process-count) 0))

(test-case "track-process! enforces max limit"
  (reset-process-count!)
  (parameterize ([current-max-processes 2])
    (track-process!)
    (track-process!)
    (check-exn exn:fail? (lambda () (track-process!)))
    (reset-process-count!)))
