#lang racket/base
;; W3 ONE-OFF: snapshot per-suite file membership for before/after diff.
;; Deleted after the wave; not a deliverable.
(require racket/path
         (only-in "classify.rkt" collect-test-files))

;; raco test runs every .rkt in this directory at module load; keep this
;; one-off's side effects out of the suite (W0: raco test must pass).
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "w3-snapshot.rkt"))))))

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

(when invoked-directly?
  (for ([s (in-list suites)])
    (define files (collect-test-files s))
    (call-with-output-file (format "/tmp/w3-before-~a.txt" s)
                           #:exists 'replace
                           (lambda (out)
                             (for ([f (in-list files)])
                               (displayln f out))))
    (printf "~a: ~a files~n" s (length files))))
