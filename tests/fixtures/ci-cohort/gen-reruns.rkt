#lang racket/base
;; Fixture generator: 20 SHAs with some failed-then-passed reruns.
;; SHAs 0, 5, 10, 15 each have one failed attempt before the timing sample.
(require json
         racket/format
         racket/list
         (only-in "../../../util/version.rkt" q-version))

(define (make-sha i)
  (define sha-str
    (format "rerun~a~a~a"
            (integer->char (+ 97 (modulo i 26)))
            (integer->char (+ 97 (modulo (+ i 5) 26)))
            (integer->char (+ 97 (modulo (+ i 10) 26)))))
  (define elapsed (+ 300.0 (* i 15.5)))
  (define file-count (+ 1162 i))
  (define test-count (+ 16808 (* i 3)))
  (define has-rerun? (zero? (modulo i 5)))
  (define attempts
    (if has-rerun?
        (list (hasheq 'run-id
                      (number->string (+ 33000000001 (* i 100)))
                      'result
                      "failure"
                      'elapsed-seconds
                      (* elapsed 1.3)
                      'timing-sample
                      #f)
              (hasheq 'run-id
                      (number->string (+ 33000000002 (* i 100)))
                      'result
                      "success"
                      'elapsed-seconds
                      elapsed
                      'timing-sample
                      #t))
        (list (hasheq 'run-id
                      (number->string (+ 33000000001 (* i 100)))
                      'result
                      "success"
                      'elapsed-seconds
                      elapsed
                      'timing-sample
                      #t))))
  (hasheq 'sha
          sha-str
          'pr
          (+ 9550 i)
          'scheduler
          "batch"
          'ordering
          "fifo"
          'attempts
          attempts
          'inventory-digest
          (format "sha256:rerun~a000000000000000000000000000000000000000000000000"
                  (number->string i 16))
          'file-count
          file-count
          'test-count
          test-count
          'pass
          file-count
          'fail
          (if has-rerun? 0 0)
          'timeout
          0
          'skip
          0
          'zero-test
          #f
          'flakes
          (if has-rerun? 1 0)
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
          (format "v~a-cohort-reruns" q-version)
          'milestone
          (format "v~a" q-version)
          'schema-version
          1
          'expected-count
          20
          'shas
          shas
          'exclusions
          '()))

(call-with-output-file "tests/fixtures/ci-cohort/valid-20-with-reruns.json"
                       #:exists 'replace
                       (lambda (out) (write-json manifest out)))
(displayln "created valid-20-with-reruns.json")
