#lang racket/base

;; @speed fast
;; @suite default
;;; test-memory-wiring-w2.rkt — W2 tests verifying auto-extraction runtime wiring
(require rackunit
         "../runtime/memory/auto-extraction.rkt"
         "../runtime/settings.rkt")

;; W1 already wired settings into runtime parameters via run-modes.rkt.
;; This test verifies the parameter-setting functions work correctly.

(test-case "W2: current-auto-extraction-enabled parameter default is #f"
  (check-false (current-auto-extraction-enabled)))

(test-case "W2: current-auto-extraction-min-confidence default is 0.5"
  (check-equal? (current-auto-extraction-min-confidence) 0.5))

(test-case "W2: parameterize enables auto-extraction"
  (parameterize ([current-auto-extraction-enabled #t])
    (check-true (current-auto-extraction-enabled))))

(test-case "W2: parameterize sets min-confidence"
  (parameterize ([current-auto-extraction-min-confidence 0.8])
    (check-equal? (current-auto-extraction-min-confidence) 0.8)))

(test-case "W2: settings-to-parameter roundtrip for enabled"
  (define s
    (make-minimal-settings #:overrides (hash 'memory (hash 'auto-extraction (hash 'enabled #t)))))
  (parameterize ([current-auto-extraction-enabled (setting-memory-auto-extraction-enabled? s)])
    (check-true (current-auto-extraction-enabled))))

(test-case "W2: settings-to-parameter roundtrip for min-confidence"
  (define s
    (make-minimal-settings #:overrides
                           (hash 'memory (hash 'auto-extraction (hash 'min-confidence 0.75)))))
  (parameterize ([current-auto-extraction-min-confidence
                  (setting-memory-auto-extraction-min-confidence s)])
    (check-equal? (current-auto-extraction-min-confidence) 0.75)))

(test-case "W2: disabled settings reset parameter to #f"
  (parameterize ([current-auto-extraction-enabled #t])
    (check-true (current-auto-extraction-enabled))
    ;; Simulate rebuild with disabled settings
    (define s (make-minimal-settings))
    (parameterize ([current-auto-extraction-enabled (setting-memory-auto-extraction-enabled? s)])
      (check-false (current-auto-extraction-enabled)))))
