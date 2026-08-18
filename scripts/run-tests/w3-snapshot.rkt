#lang racket/base
;; W3 ONE-OFF: snapshot per-suite file membership for before/after diff.
;; Deleted after the wave; not a deliverable.
(require (only-in "classify.rkt" collect-test-files))

(define suites
  '(all fast
        unit_fast
        slow
        tui
        smoke
        release_smoke
        security
        arch
        runtime
        extensions
        workflows
        platform
        mutating))

(for ([s (in-list suites)])
  (define files (collect-test-files s))
  (call-with-output-file (format "/tmp/w3-before-~a.txt" s)
                         #:exists 'replace
                         (lambda (out)
                           (for ([f (in-list files)])
                             (displayln f out))))
  (printf "~a: ~a files~n" s (length files)))
