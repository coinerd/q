#lang racket/base

;; @speed fast  ;; @suite extensions

;; BOUNDARY: integration

(require rackunit
         "../util/hook-types.rkt")

(test-case "HOOK-SCHEMA-VERSION is 1"
  (check-equal? HOOK-SCHEMA-VERSION 1))

(test-case "hook-schema-version returns current version"
  (check-equal? (hook-schema-version) 1))

(test-case "validate-hook-result passes for valid action"
  (define result (hook-pass "payload"))
  (check-true (validate-hook-result 'model-request-pre result)))

(test-case "validate-hook-result fails for invalid action"
  (define result (hook-block "payload"))
  ;; 'turn-end only allows pass/amend
  (check-false (validate-hook-result 'turn-end (hook-block "x"))))

(test-case "validate-hook-result fails on schema version mismatch"
  (define result (hook-pass "payload"))
  (check-false (validate-hook-result 'model-request-pre result 999)))
