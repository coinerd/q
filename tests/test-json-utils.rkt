#lang racket/base

;; Focused tests for the extracted JSON utility modules (v0.99.58 W1-4 / P1-G).
;; These complement tests/test-gsd-milestone-truth.rkt by exercising the new
;; module boundaries directly.

(require rackunit
         "../util/json/strict-json.rkt"
         "../util/json/canonical-json.rkt")

(test-case "strict-json-read: basic object round-trips"
  (check-equal? (strict-json-read "{\"a\":1,\"b\":2}") (hash "a" 1 "b" 2))
  (check-equal? (strict-json-read "{}") (hash))
  (check-equal? (strict-json-read "[]") '())
  (check-equal? (strict-json-read "null") 'null)
  (check-equal? (strict-json-read "true") #t)
  (check-equal? (strict-json-read "false") #f))

(test-case "strict-json-read: integers only (rejects floats)"
  (check-equal? (strict-json-read "42") 42)
  (check-equal? (strict-json-read "-1") -1)
  (check-equal? (strict-json-read "9007199254740991") 9007199254740991)
  (check-exn exn:fail? (lambda () (strict-json-read "3.14"))))

(test-case "strict-json-read: rejects trailing input"
  (check-exn exn:fail? (lambda () (strict-json-read "1 2"))))

(test-case "strict-json-read: surrogate pairs"
  (check-equal? (strict-json-read "\"\\uD83D\\uDE00\"") "😀")
  (check-exn exn:fail? (lambda () (strict-json-read "\"\\uD83D\""))))

(test-case "canonical-json-bytes: deterministic key ordering (UTF-16)"
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "b" 2 "a" 1))) "{\"a\":1,\"b\":2}")
  ;; ASCII 'z' (0x7A) sorts before emoji '😀' (0x1F600) in UTF-16
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "😀" 1 "z" 2))) "{\"z\":2,\"😀\":1}"))

(test-case "canonical-json-bytes: nested structures"
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "arr" (list 3 1 2))))
                "{\"arr\":[3,1,2]}")
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "inner" (hash "y" 1 "x" 2))))
                "{\"inner\":{\"x\":2,\"y\":1}}"))

(test-case "canonical-json-bytes: special characters escaped"
  (check-equal? (bytes->string/utf-8 (canonical-json-bytes (hash "key" "a\"b\\c")))
                "{\"key\":\"a\\\"b\\\\c\"}"))

(test-case "canonical-json-bytes: rejects unsafe integers"
  (check-exn exn:fail? (lambda () (canonical-json-bytes (hash "k" 9007199254740992))))
  (check-exn exn:fail? (lambda () (canonical-json-bytes (hash "k" 1.5)))))

(test-case "canonical-json-sha256: stable hash"
  (define h1 (canonical-json-sha256 (hash "b" 2 "a" 1)))
  (define h2 (canonical-json-sha256 (hash "a" 1 "b" 2)))
  (check-equal? h1 h2)
  (check-equal? (string-length h1) 64)
  ;; Only hex characters
  (check-true (regexp-match? #px"^[0-9a-f]{64}$" h1)))
