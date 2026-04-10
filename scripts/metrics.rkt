#!/usr/bin/env racket
#lang racket/base

;; scripts/metrics.rkt — Single-source-of-truth project metrics
;;
;; Usage:
;;   cd q/ && racket scripts/metrics.rkt           # static metrics
;;   cd q/ && racket scripts/metrics.rkt --tests   # also run test suite
;;
;; Output: markdown table suitable for copy-paste into README.md / CHANGELOG.md

(require racket/port
         racket/file
         racket/list
         racket/string
         racket/system)

(define run-tests? (member "--tests" (vector->list (current-command-line-arguments))))

;; --- Counting helpers ---

(define (rkt-files dir #:exclude-tests? [exclude? #f])
  (for/list ([f (in-directory dir)]
             #:when (and (file-exists? f)
                         (string-suffix? (path->string f) ".rkt")
                         (not (string-contains? (path->string f) "/compiled/"))
                         (not (string-contains? (path->string f) ".zo"))
                         (if exclude?
                             (not (string-contains? (path->string f) "/tests/"))
                             #t)))
    f))

(define (line-count files)
  (for/sum ([f files])
    (with-handlers ([exn:fail? (lambda (_) 0)])
      (length (file->lines f)))))

(define (assertion-count files)
  (for/sum ([f files])
    (with-handlers ([exn:fail? (lambda (_) 0)])
      (for/sum ([line (file->lines f)])
        (if (regexp-match? #rx"\\(check-" line) 1 0)))))

(define (version-string)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define info-content (file->string "info.rkt"))
    (define m (regexp-match #rx"[(]define version \"([^\"]+)\"[)]" info-content))
    (if m (cadr m) "unknown")))

;; --- Compute metrics ---

(define test-files   (rkt-files "tests"))
(define source-files (rkt-files "." #:exclude-tests? #t))
(define test-count   (line-count test-files))
(define src-count    (line-count source-files))
(define assertions   (assertion-count test-files))

(define test-pass-count
  (if run-tests?
      (let ()
        (displayln "Running test suite...")
        (define out (open-output-string))
        (parameterize ([current-output-port out]
                       [current-error-port out])
          (system* (find-executable-path "raco") "test" "tests/"))
        (define output (get-output-string out))
        (define m (regexp-match #rx"([0-9]+) tests passed" output))
        (if m (string->number (cadr m)) "unknown"))
      "— (use --tests to run)"))

;; --- Output ---

(printf "| Metric | Value |~n")
(printf "|--------|-------|~n")
(printf "| Version | ~a |~n" (version-string))
(printf "| Source modules | ~a |~n" (length source-files))
(printf "| Test files | ~a |~n" (length test-files))
(printf "| Source lines | ~a |~n" src-count)
(printf "| Test lines | ~a |~n" test-count)
(printf "| Test assertions | ~a |~n" assertions)
(printf "| Tests passing | ~a |~n" test-pass-count)
