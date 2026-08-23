#lang racket/base
;; W3 one-off: list unit-fast files whose failure path is `(exit (run-tests ...))`
;; without a `module+ test` form (unsafe for grouped in-process execution).
(require racket/file
         racket/list
         racket/string
         (only-in "classify.rkt" base-dir collect-test-files get-file-metadata))

(for ([f (in-list (collect-test-files 'unit-fast))])
  (define content (file->string (build-path base-dir f)))
  (when (and (regexp-match? #rx"\\(exit \\(run-tests" content)
             (not (regexp-match? #px"\\(module\\+\\s+test\\b" content)))
    (printf "~a~n" f)))
