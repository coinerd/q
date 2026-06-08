#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: integration

;; tests/test-summary-cache-eviction.rkt — LRU eviction for summary cache
;;
;; Verifies that the summary-cache evicts oldest entries when at capacity
;; and promotes accessed entries to prevent premature eviction.

(require rackunit
         rackunit/text-ui
         "../runtime/context/context-assembly.rkt")

(define eviction-tests
  (test-suite "summary-cache-eviction"

    (test-case "empty cache has count 0"
      (define c (make-summary-cache))
      (check-equal? (summary-cache-count c) 0))

    (test-case "store and lookup"
      (define c (make-summary-cache))
      (summary-cache-store! c "a" "b" "summary-ab")
      (check-equal? (summary-cache-lookup c "a" "b") "summary-ab")
      (check-equal? (summary-cache-count c) 1))

    (test-case "lookup misses return #f"
      (define c (make-summary-cache))
      (check-false (summary-cache-lookup c "x" "y")))

    (test-case "LRU eviction at capacity"
      (define c (make-summary-cache #:max-entries 3))
      (summary-cache-store! c "a" "b" "s1")
      (summary-cache-store! c "c" "d" "s2")
      (summary-cache-store! c "e" "f" "s3")
      ;; Cache is full (3 entries). Adding a 4th evicts oldest.
      (summary-cache-store! c "g" "h" "s4")
      (check-equal? (summary-cache-count c) 3)
      ;; Oldest (a/b) should be evicted
      (check-false (summary-cache-lookup c "a" "b"))
      ;; Others remain
      (check-equal? (summary-cache-lookup c "c" "d") "s2")
      (check-equal? (summary-cache-lookup c "e" "f") "s3")
      (check-equal? (summary-cache-lookup c "g" "h") "s4"))

    (test-case "lookup promotes entry (prevents eviction)"
      (define c (make-summary-cache #:max-entries 3))
      (summary-cache-store! c "a" "b" "s1")
      (summary-cache-store! c "c" "d" "s2")
      (summary-cache-store! c "e" "f" "s3")
      ;; Access a/b to promote it (now newest)
      (summary-cache-lookup c "a" "b")
      ;; Add new entry — should evict c/d (oldest not accessed)
      (summary-cache-store! c "g" "h" "s4")
      (check-equal? (summary-cache-count c) 3)
      ;; a/b survives because it was promoted
      (check-equal? (summary-cache-lookup c "a" "b") "s1")
      ;; c/d was evicted (was oldest before promotion of a/b)
      (check-false (summary-cache-lookup c "c" "d")))

    (test-case "update existing key does not increase count"
      (define c (make-summary-cache #:max-entries 3))
      (summary-cache-store! c "a" "b" "original")
      (summary-cache-store! c "a" "b" "updated")
      (check-equal? (summary-cache-count c) 1)
      (check-equal? (summary-cache-lookup c "a" "b") "updated"))

    (test-case "default capacity is 50"
      (define c (make-summary-cache))
      (check-equal? (summary-cache-max-entries c) DEFAULT-CACHE-MAX-ENTRIES))

    (test-case "stress: fill to capacity and verify eviction"
      (define c (make-summary-cache #:max-entries 10))
      (for ([i (in-range 20)])
        (summary-cache-store! c (format "f~a" i) (format "t~a" i) (format "s~a" i)))
      ;; Should only have last 10 entries
      (check-equal? (summary-cache-count c) 10)
      ;; First 10 should be evicted
      (check-false (summary-cache-lookup c "f0" "t0"))
      (check-false (summary-cache-lookup c "f9" "t9"))
      ;; Last 10 should remain
      (check-equal? (summary-cache-lookup c "f10" "t10") "s10")
      (check-equal? (summary-cache-lookup c "f19" "t19") "s19"))))

(run-tests eviction-tests)
