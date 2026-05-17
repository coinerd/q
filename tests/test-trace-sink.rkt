#lang racket/base

;; tests/test-trace-sink.rkt — Trace sink protocol tests (F9)

(require rackunit
         rackunit/text-ui
         racket/class
         racket/file
         racket/string
         "../runtime/trace-sink.rkt")

(define trace-sink-tests
  (test-suite "trace-sink"

    (test-case "null-trace-sink discards"
      (define sink (new null-trace-sink%))
      (send sink trace-write! (hasheq 'test #t))
      (send sink trace-flush!)
      (send sink trace-close!)
      ;; No error = pass
      (check-true #t))

    (test-case "port-trace-sink writes to port"
      (define-values (in out) (make-pipe))
      (define sink (new port-trace-sink% [port out]))
      (send sink trace-write! (hasheq 'msg "hello"))
      (send sink trace-flush!)
      (define line (read in))
      (check-equal? (hash-ref line 'msg) "hello")
      (send sink trace-close!))

    (test-case "file-trace-sink writes to file"
      (define tmp (make-temporary-file "trace-sink-test-~a.jsonl"))
      (define sink (new file-trace-sink% [path tmp] [exists-mode 'truncate]))
      (send sink trace-write! (hasheq 'seq 1 'data "test"))
      (send sink trace-close!)
      (define content (file->string tmp))
      (check-true (string-contains? content "test"))
      (delete-file tmp))

    (test-case "trace-sink interface compliance"
      (define null-sink (new null-trace-sink%))
      (check-true (is-a? null-sink trace-sink<%>))
      (define port-sink (new port-trace-sink% [port (open-output-string)]))
      (check-true (is-a? port-sink trace-sink<%>)))))

(run-tests trace-sink-tests)
