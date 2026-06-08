#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-settings-w1.rkt — W1 tests for auto-extraction settings
(require rackunit
         "../runtime/settings.rkt")

(test-case "W1: setting-memory-auto-extraction-enabled? default #f"
  (define s (make-minimal-settings))
  (check-false (setting-memory-auto-extraction-enabled? s)))

(test-case "W1: setting-memory-auto-extraction-enabled? reads boolean #t"
  (define s (make-minimal-settings #:overrides (hash 'memory (hash 'auto-extraction (hash 'enabled #t)))))
  (check-true (setting-memory-auto-extraction-enabled? s)))

(test-case "W1: setting-memory-auto-extraction-enabled? coerces string"
  (define s (make-minimal-settings #:overrides (hash 'memory (hash 'auto-extraction (hash 'enabled "true")))))
  (check-true (setting-memory-auto-extraction-enabled? s)))

(test-case "W1: setting-memory-auto-extraction-min-confidence default 0.5"
  (define s (make-minimal-settings))
  (check-equal? (setting-memory-auto-extraction-min-confidence s) 0.5))

(test-case "W1: setting-memory-auto-extraction-min-confidence reads number"
  (define s (make-minimal-settings #:overrides (hash 'memory (hash 'auto-extraction (hash 'min-confidence 0.75)))))
  (check-equal? (setting-memory-auto-extraction-min-confidence s) 0.75))

(test-case "W1: setting-memory-auto-extraction-min-confidence coerces string"
  (define s (make-minimal-settings #:overrides (hash 'memory (hash 'auto-extraction (hash 'min-confidence "0.8")))))
  (check-equal? (setting-memory-auto-extraction-min-confidence s) 0.8))
