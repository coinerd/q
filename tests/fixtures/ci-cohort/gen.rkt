#lang racket/base
;; Fixture generator for ci-cohort tests — run once to produce valid-20.json
(require json
         racket/format
         racket/list)

(define (make-sha i)
  (define sha-str
    (format "abc~a~a~a"
            (integer->char (+ 97 (modulo i 26)))
            (integer->char (+ 97 (modulo (+ i 5) 26)))
            (integer->char (+ 97 (modulo (+ i 10) 26)))))
  (define elapsed (+ 300.0 (* i 15.5)))
  (define file-count (+ 1162 i))
  (define test-count (+ 16808 (* i 3)))
  (hasheq 'sha
          sha-str
          'pr
          (+ 9550 i)
          'scheduler
          "batch"
          'ordering
          "fifo"
          'attempts
          (list (hasheq 'run-id
                        (number->string (+ 33000000001 (* i 100)))
                        'result
                        "success"
                        'elapsed-seconds
                        elapsed
                        'timing-sample
                        #t))
          'inventory-digest
          (format "sha256:~a000000000000000000000000000000000000000000000000000"
                  (number->string i 16))
          'file-count
          file-count
          'test-count
          test-count
          'pass
          file-count
          'fail
          0
          'timeout
          0
          'skip
          0
          'zero-test
          #f
          'flakes
          0
          'parallel-only-failures
          0
          'prepared-env
          "match"
          'queue-wait-seconds
          12
          'queue-depth
          0
          'runner-minutes
          7.5))

(define shas
  (for/list ([i (in-range 20)])
    (make-sha i)))
(define manifest
  (hasheq 'cohort-id
          "v1.00.23-cohort-1"
          'milestone
          "v1.00.23"
          'schema-version
          1
          'expected-count
          20
          'shas
          shas
          'exclusions
          '()))

(call-with-output-file "tests/fixtures/ci-cohort/valid-20.json"
                       #:exists 'replace
                       (lambda (out) (write-json manifest out)))
(displayln "created valid-20.json")
