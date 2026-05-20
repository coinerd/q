#lang racket/base
;; test-hash-helpers.rkt — Tests for util/hash-helpers.rkt (T3-6)

(require rackunit
         "../util/hash-helpers.rkt")

(test-case "key->string: symbol to string"
  (check-equal? (key->string 'foo) "foo"))

(test-case "key->string: string passthrough"
  (check-equal? (key->string "bar") "bar"))

(test-case "normalize-keys: symbols to strings"
  (define h (hasheq 'a 1 'b 2))
  (define result (normalize-keys h))
  (check-equal? (hash-ref result "a") 1)
  (check-equal? (hash-ref result "b") 2))

(test-case "normalize-keys: already strings"
  (define h (hash "x" 10 "y" 20))
  (define result (normalize-keys h))
  (check-equal? (hash-ref result "x") 10))

(test-case "flex-ref: string key in hash"
  (define h (hash "name" "alice"))
  (check-equal? (flex-ref h "name") "alice"))

(test-case "flex-ref: symbol key lookup in string-keyed hash"
  (define h (hash "name" "bob"))
  (check-equal? (flex-ref h 'name) "bob"))

(test-case "flex-ref: string key lookup in symbol-keyed hash"
  (define h (hasheq 'name "charlie"))
  (check-equal? (flex-ref h "name") "charlie"))

(test-case "flex-ref: missing key returns default"
  (check-equal? (flex-ref (hash) "missing" #f) #f))

(test-case "flex-ref: missing key default is #f"
  (check-equal? (flex-ref (hash) 'nope) #f))

(test-case "flex-ref: procedure default"
  (check-equal? (flex-ref (hash) 'missing (lambda () 42)) 42))

(test-case "normalize-keys: mixed keys"
  (define h (hash 'sym 1 "str" 2))
  (define result (normalize-keys h))
  (check-equal? (hash-ref result "sym") 1)
  (check-equal? (hash-ref result "str") 2))

(test-case "flex-ref: exact match preferred"
  (define h (hash "key" "string-val"))
  (check-equal? (flex-ref h "key") "string-val"))

(test-case "normalize-keys: empty hash"
  (check-equal? (normalize-keys (hash)) (hash)))
