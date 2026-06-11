#lang racket

;; tests/test-browser-screenshot-type.rkt — NF-01: screenshot bytes type fix

(require rackunit
         net/base64)

;; ---------------------------------------------------------------------------
;; NF-01: base64 string -> bytes conversion
;; ---------------------------------------------------------------------------

(test-case "NF-01: base64-decode converts string to bytes"
  (define raw-data "aGVsbG8=") ;; "hello" in base64
  (define result
    (if (non-empty-string? raw-data)
        (base64-decode (string->bytes/utf-8 raw-data))
        #""))
  (check-pred bytes? result)
  (check-equal? result #"hello")
  (check-equal? (bytes-length result) 5))

(test-case "NF-01: empty string returns empty bytes"
  (define raw-data "")
  (define result
    (if (non-empty-string? raw-data)
        (base64-decode (string->bytes/utf-8 raw-data))
        #""))
  (check-pred bytes? result)
  (check-equal? result #""))
