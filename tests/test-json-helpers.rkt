#lang racket

(require rackunit
         "../util/json-helpers.rkt")

;; ============================================================
;; Test suite: util/json-helpers.rkt — ensure-hash-args
;; ============================================================

(test-case "ensure-hash-args: hash input returns same hash"
  (define h (hasheq 'a 1 'b 2))
  (check-equal? (ensure-hash-args h) h))

(test-case "ensure-hash-args: valid JSON string parses to hash"
  (define result (ensure-hash-args "{\"key\": \"val\"}"))
  (check-true (hash? result))
  (check-equal? (hash-ref result 'key #f) "val"))

(test-case "ensure-hash-args: empty string returns empty hash"
  (define result (ensure-hash-args ""))
  (check-true (hash? result))
  (check-true (hash-empty? result)))

(test-case "ensure-hash-args: empty JSON object string returns empty hash"
  (define result (ensure-hash-args "{}"))
  (check-true (hash? result))
  (check-true (hash-empty? result)))

(test-case "ensure-hash-args: invalid JSON string returns _parse_failed hash"
  (define result (ensure-hash-args "not json"))
  (check-true (hash? result))
  (check-true (hash-ref result '_parse_failed #f))
  (check-equal? (hash-ref result '_raw_args #f) "not json"))
