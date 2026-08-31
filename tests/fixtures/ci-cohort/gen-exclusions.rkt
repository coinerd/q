#lang racket/base
;; Fixture generator: 15 SHAs + 5 named exclusions = 20 total (valid).
(require json
         racket/format
         racket/list
         (only-in "../../../util/version.rkt" q-version))

(define (make-sha i)
  (define sha-str
    (format "exc~a~a~a"
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
          (format "sha256:exc~a000000000000000000000000000000000000000000000000"
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
  (for/list ([i (in-range 15)])
    (make-sha i)))
(define exclusion-reasons
  '("missing-lane-artifact" "incompatible-scheduler"
                            "artifact-corrupt"
                            "inventory-mismatch"
                            "artifact-expired"))
(define exclusions
  (for/list ([i (in-range 5)])
    (hasheq 'sha
            (format "excluded~a" i)
            'reason
            (list-ref exclusion-reasons i)
            'detail
            (format "SHA excluded for ~a" (list-ref exclusion-reasons i)))))
(define manifest
  (hasheq 'cohort-id
          (format "v~a-cohort-exclusions" q-version)
          'milestone
          (format "v~a" q-version)
          'schema-version
          1
          'expected-count
          20
          'shas
          shas
          'exclusions
          exclusions))

(call-with-output-file "tests/fixtures/ci-cohort/valid-15-with-exclusions.json"
                       #:exists 'replace
                       (lambda (out) (write-json manifest out)))
(displayln "created valid-15-with-exclusions.json")
