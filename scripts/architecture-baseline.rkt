#lang racket/base

(require racket/cmdline
         racket/file
         "architecture-baseline-helpers.rkt")

(define revision #f)
(define raw-path #f)
(define markdown-path #f)
(define last-n 200)

(command-line #:program "architecture-baseline.rkt"
              #:once-each [("--revision") sha "Pinned Git revision (commit-ish)" (set! revision sha)]
              [("--raw") path "Raw .rktd output path" (set! raw-path path)]
              [("--markdown") path "Markdown output path" (set! markdown-path path)]
              [("--last")
               count
               "Number of commits used as history evidence"
               (define parsed (string->number count))
               (unless (and (exact-integer? parsed) (positive? parsed))
                 (raise-user-error 'architecture-baseline "--last must be a positive integer"))
               (set! last-n parsed)])

(unless revision
  (raise-user-error 'architecture-baseline "missing required --revision SHA"))
(unless raw-path
  (raise-user-error 'architecture-baseline "missing required --raw PATH"))
(unless markdown-path
  (raise-user-error 'architecture-baseline "missing required --markdown PATH"))

(define snapshot (collect-architecture-snapshot revision #:last last-n))
(define raw (canonical-datum->string snapshot))
(define markdown (architecture-snapshot->markdown snapshot))

(call-with-output-file raw-path (lambda (output) (display raw output)) #:exists 'truncate/replace)
(call-with-output-file markdown-path
                       (lambda (output) (display markdown output))
                       #:exists 'truncate/replace)
