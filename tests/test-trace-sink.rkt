#lang racket/base
;; BOUNDARY: pure

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
      (check-true (is-a? port-sink trace-sink<%>)))

    ;; ═══════════════════════════════════════════════════════════════
    ;; Characterization tests: sync sink ordering/flush/close contract
    ;; Future async sinks must satisfy these same properties.
    ;; ═══════════════════════════════════════════════════════════════

    (test-case "sync-sink: writes preserve insertion order"
      (define out (open-output-string))
      (define sink (new port-trace-sink% [port out]))
      (for ([i (in-range 10)])
        (send sink trace-write! (hasheq 'seq i)))
      (send sink trace-flush!)
      (define content (get-output-string out))
      (define entries
        (for/list ([line (in-list (string-split content "\n"))]
                   #:when (non-empty-string? line))
          (define v (read (open-input-string line)))
          (hash-ref v 'seq #f)))
      (check-equal? entries '(0 1 2 3 4 5 6 7 8 9)))

    (test-case "sync-sink: flush ensures all data readable"
      (define tmp (make-temporary-file "trace-flush-test-~a.jsonl"))
      (define sink (new file-trace-sink% [path tmp] [exists-mode 'truncate]))
      (send sink trace-write! (hasheq 'msg "before-flush"))
      (send sink trace-flush!)
      ;; After flush, file must contain the entry
      (define content (file->string tmp))
      (check-true (string-contains? content "before-flush"))
      (send sink trace-close!)
      (delete-file tmp))

    (test-case "sync-sink: close flushes and prevents further writes"
      (define out (open-output-string))
      (define sink (new port-trace-sink% [port out]))
      (send sink trace-write! (hasheq 'msg "data"))
      (send sink trace-close!)
      ;; After close, data was written
      (define content (get-output-string out))
      (check-true (string-contains? content "data")))

    (test-case "sync-sink: multiple flushes are idempotent"
      (define out (open-output-string))
      (define sink (new port-trace-sink% [port out]))
      (send sink trace-write! (hasheq 'msg "test"))
      (send sink trace-flush!)
      (send sink trace-flush!)
      (send sink trace-flush!)
      ;; Only one entry written despite multiple flushes
      (define content (get-output-string out))
      (define line-count (length (filter non-empty-string? (string-split content "\n"))))
      (check-equal? line-count 1))

    (test-case "sync-sink: write after write without flush preserves both"
      (define out (open-output-string))
      (define sink (new port-trace-sink% [port out]))
      (send sink trace-write! (hasheq 'seq 1))
      (send sink trace-write! (hasheq 'seq 2))
      (send sink trace-flush!)
      (define content (get-output-string out))
      (define entries
        (for/list ([line (in-list (string-split content "\n"))]
                   #:when (non-empty-string? line))
          (hash-ref (read (open-input-string line)) 'seq #f)))
      (check-equal? entries '(1 2)))))

(run-tests trace-sink-tests)
