#lang racket/base
;; W3 one-off: list unit-fast files whose failure path is `(exit (run-tests ...))`
;; without a `module+ test` form (unsafe for grouped in-process execution).
(require racket/file
         racket/list
         racket/path
         racket/string
         (only-in "classify.rkt" base-dir collect-test-files get-file-metadata))

;; raco test runs every .rkt in this directory at module load; keep this
;; one-off's side effects out of the suite (W0: raco test must pass).
(define invoked-directly?
  (let ([run-file (find-system-path 'run-file)])
    (and (path? run-file)
         (let ([base (file-name-from-path run-file)])
           (and base (equal? (path->string base) "w3-exit-scan.rkt"))))))

(when invoked-directly?
  (for ([f (in-list (collect-test-files 'unit-fast))])
    (define content (file->string (build-path base-dir f)))
    (when (and (regexp-match? #rx"\\(exit \\(run-tests" content)
               (not (regexp-match? #px"\\(module\\+\\s+test\\b" content)))
      (printf "~a~n" f))))
