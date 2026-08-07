#lang racket/base

;; @speed fast
;; @suite default
;; test-hash-utils.rkt — Tests for util/hash-utils.rkt (W4)

(require rackunit
         "../util/hash-utils.rkt")

(test-case "hash-ref*: deep path access"
  (define h (hash 'a (hash 'b (hash 'c 42))))
  (check-equal? (hash-ref* h '(a b c)) 42))

(test-case "hash-ref*: equivalent to nested hash-ref"
  (define h (hash 'a (hash 'b (hash 'c 7))))
  (check-equal? (hash-ref* h '(a b c))
                (hash-ref (hash-ref (hash-ref h 'a) 'b) 'c)))

(test-case "hash-ref*: missing key returns explicit default"
  (define h (hash 'a (hash 'b 1)))
  (check-equal? (hash-ref* h '(a z) 'missing) 'missing))

(test-case "hash-ref*: missing key default is #f"
  (define h (hash 'a 1))
  (check-equal? (hash-ref* h '(x y)) #f))

(test-case "hash-ref*: non-hash intermediate returns default"
  (define h (hash 'a 5))
  (check-equal? (hash-ref* h '(a b) 'dflt) 'dflt))

(test-case "hash-ref*: string keys"
  (define h (hash "a" (hash "b" "deep")))
  (check-equal? (hash-ref* h '("a" "b")) "deep"))

(test-case "hash-ref*: mixed symbol/string keys"
  (define h (hash 'a (hash "b" (hash 'c "deep"))))
  (check-equal? (hash-ref* h '(a "b" c)) "deep"))

(test-case "hash-ref*: empty path returns the hash itself"
  (define h (hash 'a 1))
  (check-equal? (hash-ref* h '()) h))

(test-case "hash-ref*: single-level access"
  (define h (hash 'a 10))
  (check-equal? (hash-ref* h '(a)) 10))

(test-case "hash-ref*: top-level value found at depth 1"
  (define h (hash 'a (hash 'b (hash 'c (hash 'd 1)))))
  (check-equal? (hash-ref* h '(a b c d)) 1))
