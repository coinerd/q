#lang racket/base

;; tests/test-token-estimate-cache.rkt — v0.70.6 W0

(require rackunit
         rackunit/text-ui
         "../util/token-estimate-cache.rkt")

;; Simple estimator for testing: tokens = string-length / 4 (rounded up)
(define (test-estimator text)
  (max 1 (ceiling (/ (string-length text) 4))))

(define-test-suite test-token-estimate-cache

  (test-case "cached-estimate: computes value on miss"
    (clear-token-estimate-cache!)
    (define result (cached-estimate-text-tokens test-estimator "hello world"))
    (check-equal? result 3)
    (define stats (token-estimate-cache-stats))
    (check-equal? (hash-ref stats 'misses) 1)
    (check-equal? (hash-ref stats 'hits) 0))

  (test-case "cached-estimate: returns cached value on hit"
    (clear-token-estimate-cache!)
    (cached-estimate-text-tokens test-estimator "repeat me")
    (define result (cached-estimate-text-tokens test-estimator "repeat me"))
    (check-equal? result 3)
    (define stats (token-estimate-cache-stats))
    (check-equal? (hash-ref stats 'misses) 1)
    (check-equal? (hash-ref stats 'hits) 1))

  (test-case "cached-estimate: different texts have separate entries"
    (clear-token-estimate-cache!)
    (cached-estimate-text-tokens test-estimator "text one")
    (cached-estimate-text-tokens test-estimator "text two")
    (define stats (token-estimate-cache-stats))
    (check-equal? (hash-ref stats 'entries) 2)
    (check-equal? (hash-ref stats 'misses) 2))

  (test-case "clear-token-estimate-cache! resets everything"
    (clear-token-estimate-cache!)
    (cached-estimate-text-tokens test-estimator "foo")
    (clear-token-estimate-cache!)
    (define stats (token-estimate-cache-stats))
    (check-equal? (hash-ref stats 'entries) 0)
    (check-equal? (hash-ref stats 'hits) 0)
    (check-equal? (hash-ref stats 'misses) 0))

  (test-case "make-token-estimate-cache: isolated from global cache"
    (clear-token-estimate-cache!)
    (define local (make-token-estimate-cache test-estimator))
    (local "local only")
    (check-equal? (hash-ref (token-estimate-cache-stats) 'entries) 0)
    (check-equal? (local "local only") 3))

  (test-case "make-token-estimate-cache: local cache accumulates hits"
    (define local (make-token-estimate-cache test-estimator))
    (local "hit me")
    (local "hit me")
    ;; Local caches don't expose stats, but we verify consistency
    (check-equal? (local "hit me") 2)))

(run-tests test-token-estimate-cache)
