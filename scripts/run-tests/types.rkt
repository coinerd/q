#lang racket/base

;; Test file result struct and constructor.

(provide (struct-out test-file-result)
         make-test-file-result)

(struct test-file-result (path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total)
  #:transparent)

;; Constructor alias for cleaner test access
(define (make-test-file-result path
                               exit-code
                               stdout-bytes
                               stderr-bytes
                               elapsed-ms
                               passed
                               failed
                               total)
  (test-file-result path exit-code stdout-bytes stderr-bytes elapsed-ms passed failed total))
