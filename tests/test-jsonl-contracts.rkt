#lang racket/base

;; tests/test-jsonl-contracts.rkt — Contract boundary tests for jsonl.rkt

(require rackunit
         "../util/jsonl.rkt")

(test-case "jsonl-append! rejects non-path"
  (check-exn exn:fail:contract? (lambda () (jsonl-append! 42 (hasheq)))))

(test-case "jsonl-write-to-port! rejects non-port"
  (check-exn exn:fail:contract? (lambda () (jsonl-write-to-port! "not-a-port" (hasheq)))))

(test-case "jsonl-append-entries! rejects non-list"
  (check-exn exn:fail:contract? (lambda () (jsonl-append-entries! "/tmp/test.jsonl" "not-a-list"))))

(test-case "jsonl-read-all rejects non-path"
  (check-exn exn:fail:contract? (lambda () (jsonl-read-all 42))))

(test-case "jsonl-line-valid? accepts any input, returns boolean"
  (check-false (jsonl-line-valid? 42)))
