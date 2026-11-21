#lang racket/base

;; scripts/cl3-benchmark-probe.rkt — CL3 Wave 1 evaluation harness.
;; Measures cold/warm one-shot execution, persistent-worker requests and the
;; combined navigate→inspect→evaluate→edit→reload→evaluate workflow. All work
;; is developer-only; no q tool, test runner or CI policy is involved.

(require racket/cmdline
         racket/file
         racket/list
         racket/port
         racket/runtime-path
         racket/string
         racket/system
         json
         "cl3-eval-probe.rkt"
         "semantic-navigator.rkt")

(provide percentile
         stats
         samples->report
         run-benchmark)

(define-runtime-path repo-root "..")

(define (percentile sorted fraction)
  (if (null? sorted)
      #f
      (let ([index (min (sub1 (length sorted))
                        (inexact->exact (floor (* fraction (sub1 (length sorted))))))])
        (list-ref sorted index))))

(define (stats label values)
  (define sorted (sort values <))
  (hash 'label
        label
        'n
        (length values)
        'p50
        (percentile sorted 0.5)
        'p90
        (percentile sorted 0.9)
        'max
        (if (null? sorted)
            #f
            (last sorted))
        'samples
        sorted))

(define (timed thunk)
  (define start (current-inexact-milliseconds))
  (define result (thunk))
  (values (inexact->exact (round (- (current-inexact-milliseconds) start))) result))

(define (elapsed thunk)
  (define-values (ms result) (timed thunk))
  (void result)
  ms)

(define (one-shot code)
  (define exe (or (find-executable-path "racket") (error 'cl3 "racket not found")))
  (define-values (elapsed result)
    (timed (lambda ()
             (parameterize ([current-output-port (open-output-string)]
                            [current-error-port (open-output-string)])
               (system* exe "-e" code)))))
  (void result)
  elapsed)

(define (run-benchmark #:n [n 10])
  (define one-shot-samples
    (for/list ([i (in-range n)])
      (one-shot "(displayln (+ 20 22))")))
  (define session (start-worker! #:directory repo-root))
  (define startup-session (start-worker! #:directory repo-root))
  ;; First request includes spawning plus initial module loading in the child.
  (define startup-ms (hash-ref (request! startup-session (hash 'op "reset")) 'wall-ms))
  (kill-worker! startup-session)
  (define worker-samples
    (for/list ([i (in-range n)])
      (hash-ref (request! session (hash 'op "eval" 'code "(+ 20 22)")) 'wall-ms)))
  (define module-file (build-path (find-system-path 'temp-dir) "cl3-benchmark-module.rkt"))
  (call-with-output-file module-file
                         (lambda (out)
                           (display "#lang racket/base\n(provide f)\n(define (f) 1)\n" out))
                         #:exists 'truncate/replace)
  (define load-ms
    (hash-ref (request! session (hash 'op "load" 'path (path->string module-file))) 'wall-ms))
  (define reload-ms
    (begin
      (call-with-output-file module-file
                             (lambda (out)
                               (display "#lang racket/base\n(provide f)\n(define (f) 2)\n" out))
                             #:exists 'truncate/replace)
      (hash-ref (request! session (hash 'op "reload" 'path (path->string module-file))) 'wall-ms)))
  (define reset-ms (hash-ref (request! session (hash 'op "reset")) 'wall-ms))
  (kill-worker! session)

  ;; Combined workflow: navigator query (source identity check included),
  ;; inspect via worker eval, edit a temp module, reload, and evaluate again.
  (define workflow-navigator
    (for/list ([i (in-range 5)])
      (elapsed (lambda () (module-query "agent/state.rkt" #:root repo-root)))))
  (define workflow-eval
    (for/list ([i (in-range 5)])
      (define s (start-worker! #:directory repo-root))
      (define eval-ms (hash-ref (request! s (hash 'op "eval" 'code "(+ 20 22)")) 'wall-ms))
      (define edit-ms
        (elapsed (lambda ()
                      (call-with-output-file
                       module-file
                       (lambda (out)
                         (display (format "#lang racket/base\n(provide f)\n(define (f) ~a)\n" i) out))
                       #:exists 'truncate/replace))))
      (define reload-ms*
        (hash-ref (request! s (hash 'op "reload" 'path (path->string module-file))) 'wall-ms))
      (define reeval-ms (hash-ref (request! s (hash 'op "eval" 'code "(f)")) 'wall-ms))
      (kill-worker! s)
      (hash 'navigate eval-ms 'edit edit-ms 'reload reload-ms* 'reeval reeval-ms)))
  (delete-file module-file)
  (hash
   'revision
   (string-trim (with-output-to-string
                 (lambda ()
                   (parameterize ([current-directory repo-root])
                     (system* (find-executable-path "git") "rev-parse" "HEAD")))))
   'host
   (format "~a/~a-bit" (system-type 'os) (system-type 'word))
   'n
   n
   'cold-one-shot
   (stats "cold one-shot (first sample)" (take one-shot-samples 1))
   'warm-one-shot
   (stats "warm one-shot (remaining samples)" (cdr one-shot-samples))
   'worker-cold-eval
   (stats "first persistent request (includes startup)" (take worker-samples 1))
   'worker-warm-eval
   (stats "subsequent persistent requests" (cdr worker-samples))
   'worker-startup-ms
   startup-ms
   'worker-load-ms
   load-ms
   'worker-reload-ms
   reload-ms
   'worker-reset-ms
   reset-ms
   'workflow
   (hash 'navigator-query
         (stats "navigator query incl. source identity" workflow-navigator)
         'worker-steps
         workflow-eval)
   'limitations
   (string-append
         "Developer-only timings on one host/revision; no fresh-context agent trials, "
         "no correctness or cognitive-load claim. Reload leaves captured closures stale; "
         "reset is the clean operation.")))

(define (samples->report result out)
  (write-json result out)
  (newline out))

(module+ main
  (define out-path #f)
  (define n 10)
  (define args (vector->list (current-command-line-arguments)))
  (let loop ([args args])
    (cond
      [(null? args) (void)]
      [(equal? (car args) "--out")
       (set! out-path (cadr args))
       (loop (cddr args))]
      [(equal? (car args) "--n")
       (set! n (string->number (cadr args)))
       (loop (cddr args))]
      [else (error 'cl3-benchmark "unknown option: ~a" (car args))]))
  (define report (run-benchmark #:n n))
  (if out-path
      (call-with-output-file out-path
                             (lambda (out) (samples->report report out))
                             #:exists 'truncate/replace)
      (write-json report))
  (newline))
