#lang racket/base
;;; test-memory-session-config-w1.rkt — W1 tests for session-config auto-extraction accessors
(require rackunit
         "../runtime/session/session-config.rkt")

(test-case "W1: config-memory-auto-extraction-enabled? default #f"
  (define c (hash->session-config (hash)))
  (check-false (config-memory-auto-extraction-enabled? c)))

(test-case "W1: config-memory-auto-extraction-enabled? reads #t"
  (define c (hash->session-config (hash 'memory-auto-extraction-enabled #t)))
  (check-true (config-memory-auto-extraction-enabled? c)))

(test-case "W1: config-memory-auto-extraction-min-confidence default 0.5"
  (define c (hash->session-config (hash)))
  (check-equal? (config-memory-auto-extraction-min-confidence c) 0.5))

(test-case "W1: config-memory-auto-extraction-min-confidence reads value"
  (define c (hash->session-config (hash 'memory-auto-extraction-min-confidence 0.8)))
  (check-equal? (config-memory-auto-extraction-min-confidence c) 0.8))

(test-case "W1: config-memory-auto-extraction-min-confidence clamps out-of-range"
  (define c (hash->session-config (hash 'memory-auto-extraction-min-confidence 2.0)))
  (check-equal? (config-memory-auto-extraction-min-confidence c) 0.5))
