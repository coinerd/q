#lang racket/base

;; util/token-estimate-cache.rkt — memoized token estimation cache
;;
;; Wraps text-based token estimation with a content-addressed cache.
;; Reduces redundant computation when the same message content is
;; estimated repeatedly (e.g. during context assembly iteration).

(require racket/contract
         racket/math)

(provide (contract-out
          [cached-estimate-text-tokens
           (-> (-> string? exact-nonnegative-integer?) string? exact-nonnegative-integer?)]
          [clear-token-estimate-cache! (-> void?)]
          [token-estimate-cache-stats (-> (hash/c symbol? exact-nonnegative-integer?))]
          [make-token-estimate-cache
           (-> (-> string? exact-nonnegative-integer?) (-> string? exact-nonnegative-integer?))]))

;; ============================================================
;; Cache implementation
;; ============================================================

;; Mutable hash: content-hash -> estimated-token-count
(define token-cache (make-hash))

;; Total number of cache hits since last clear
(define cache-hits (box 0))

;; Total number of cache misses since last clear
(define cache-misses (box 0))

;; Compute a content hash for cache keying.
;; Uses equal-hash-code for speed; collisions are acceptable for
;; an estimation heuristic.
(define (content-hash text)
  (equal-hash-code text))

;; cached-estimate-text-tokens : (string? -> exact-nonnegative-integer?) string? -> exact-nonnegative-integer?
;;
;; Looks up the token estimate for `text` in the cache. If absent,
;; calls `estimator` to compute the value, stores it, and returns it.
(define (cached-estimate-text-tokens estimator text)
  (define key (content-hash text))
  (cond
    [(hash-has-key? token-cache key)
     (set-box! cache-hits (add1 (unbox cache-hits)))
     (hash-ref token-cache key)]
    [else
     (define result (estimator text))
     (hash-set! token-cache key result)
     (set-box! cache-misses (add1 (unbox cache-misses)))
     result]))

;; clear-token-estimate-cache! : -> void?
;;
;; Clears all cached entries and resets hit/miss counters.
(define (clear-token-estimate-cache!)
  (set! token-cache (make-hash))
  (set-box! cache-hits 0)
  (set-box! cache-misses 0))

;; token-estimate-cache-stats : -> hash?
;;
;; Returns a hash with keys 'entries, 'hits, 'misses.
(define (token-estimate-cache-stats)
  (hash 'entries (hash-count token-cache) 'hits (unbox cache-hits) 'misses (unbox cache-misses)))

;; make-token-estimate-cache : (string? -> exact-nonnegative-integer?) -> (string? -> exact-nonnegative-integer?)
;;
;; Returns a self-contained cached estimator function. Each call
;; returns a fresh cache (isolated from the global one). Useful
;; for testing and per-session isolation.
(define (make-token-estimate-cache estimator)
  (define local-cache (make-hash))
  (define local-hits (box 0))
  (define local-misses (box 0))
  (lambda (text)
    (define key (content-hash text))
    (cond
      [(hash-has-key? local-cache key)
       (set-box! local-hits (add1 (unbox local-hits)))
       (hash-ref local-cache key)]
      [else
       (define result (estimator text))
       (hash-set! local-cache key result)
       (set-box! local-misses (add1 (unbox local-misses)))
       result])))
