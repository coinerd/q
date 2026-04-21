#lang racket

;; tests/test-trace-config.rkt — v0.15.0 Wave 2
;;
;; TDD tests for trace config parsing.

(require rackunit
         "../runtime/settings.rkt")

;; Helper: build a q-settings with a specific merged hash
(define (settings-with-merged merged-hash)
  (q-settings (hash) (hash) merged-hash))

;; ============================================================
;; trace-enabled?
;; ============================================================

(test-case "trace-enabled? returns #f when logging section absent"
  (define s (settings-with-merged (hash)))
  (check-false (trace-enabled? s)))

(test-case "trace-enabled? returns #f when logging.trace absent"
  (define s (settings-with-merged (hash 'logging (hash 'level "info"))))
  (check-false (trace-enabled? s)))

(test-case "trace-enabled? returns #f when trace.enabled is false"
  (define s (settings-with-merged (hash 'logging (hash 'trace (hash 'enabled #f)))))
  (check-false (trace-enabled? s)))

(test-case "trace-enabled? returns #t when logging.trace.enabled is true"
  (define s (settings-with-merged (hash 'logging (hash 'trace (hash 'enabled #t)))))
  (check-true (trace-enabled? s)))

;; ============================================================
;; trace-max-files
;; ============================================================

(test-case "trace-max-files defaults to 10"
  (define s (settings-with-merged (hash)))
  (check-equal? (trace-max-files s) 10))

(test-case "trace-max-files reads from config"
  (define s (settings-with-merged (hash 'logging (hash 'trace (hash 'enabled #t 'max-files 5)))))
  (check-equal? (trace-max-files s) 5))

(test-case "trace-max-files defaults to 10 when only enabled set"
  (define s (settings-with-merged (hash 'logging (hash 'trace (hash 'enabled #t)))))
  (check-equal? (trace-max-files s) 10))
