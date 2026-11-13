#lang racket/base
(require rackunit/log
         racket/path
         racket/runtime-path)

(define args (current-command-line-arguments))
(unless (= (vector-length args) 2)
  (error 'quick-child "expected test path and evidence path"))
(define path (simplify-path (path->complete-path (vector-ref args 0))))
(define evidence (vector-ref args 1))
(define-runtime-path root "../..")
(define test-dir (path-only path))
(define default-loader (current-load/use-compiled))
(define compiled-paths (use-compiled-file-paths))
(define (inside? dir candidate)
  (define relative
    (find-relative-path (simplify-path dir) (simplify-path (path->complete-path candidate))))
  (and (relative-path? relative) (not (memq 'up (explode-path relative)))))
;; The reviewed repository and fixture sources must never be replaced by .zo
;; files. Loads outside these directories (including installed collections)
;; may use compiled artifacts. This is not a sandbox against malicious or
;; concurrently modified code.
(define (source-loader file name)
  (parameterize ([use-compiled-file-paths
                  (if (or (inside? root file) (inside? test-dir file)) null compiled-paths)])
    (default-loader file name)))
;; An entry point may end with `(exit 0)` after running its suite (a common
;; repository convention: `(exit (run-tests suite))`). That is a CLEAN finish:
;; the RackUnit log already records what actually ran, so evidence remains
;; truthful. Non-zero exits (and exits before any test ran) stay failures.
(struct clean-exit exn:fail ())

(parameterize ([current-load/use-compiled source-loader]
               [exit-handler (lambda (code)
                               (if (equal? code 0)
                                   (raise (clean-exit "clean exit 0" (current-continuation-marks)))
                                   (error 'quick-child "unexpected exit: ~a" code)))]
               [current-directory (or test-dir (current-directory))])
  ;; Run the enclosing module once, then exactly one entry-point submodule: the
  ;; RackUnit test submodule when declared, otherwise the repository's main
  ;; script convention. Never both: 100+ repository tests declare the same
  ;; suite in test and main (often wrapping it in (exit (run-tests ...))), so
  ;; running both would execute - and count - every assertion twice. A file
  ;; declaring neither still just runs its module body.
  (define (run-entry!)
    (dynamic-require path #f)
    (define (entry-submodule name)
      (define sub `(submod ,path ,name))
      (and (module-declared? sub #t) sub))
    (define entry (or (entry-submodule 'test) (entry-submodule 'main)))
    (when entry
      (dynamic-require entry #f)))
  (with-handlers ([clean-exit? void])
    (run-entry!)))
(define counts (test-log))
(call-with-output-file evidence
                       #:exists 'truncate
                       (lambda (out)
                         (write (list (path->string path) (car counts) (cdr counts)) out)))
