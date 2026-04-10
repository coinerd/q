#!/usr/bin/env racket
#lang racket/base

;; scripts/metrics.rkt — Single-source-of-truth project metrics
;;
;; Usage:
;;   cd q/ && racket scripts/metrics.rkt           # static metrics
;;   cd q/ && racket scripts/metrics.rkt --tests   # also run test suite
;;   cd q/ && racket scripts/metrics.rkt --lint    # verify README.md metrics match reality
;;
;; Output: markdown table suitable for copy-paste into README.md / CHANGELOG.md

(require racket/port
         racket/file
         racket/list
         racket/string
         racket/dict
         racket/system)

(define args (vector->list (current-command-line-arguments)))
(define run-tests? (member "--tests" args))
(define lint?    (member "--lint" args))

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

;; --- Lint mode: compare computed metrics against README.md ---

;; Computed metric values keyed by the name used in the README table.
(define computed-metrics
  `(("Source modules"   . ,(number->string (length source-files)))
    ("Test files"       . ,(number->string (length test-files)))
    ("Source lines"     . ,(number->string src-count))
    ("Test lines"       . ,(number->string test-count))
    ("Test assertions"  . ,(number->string assertions))))

(define (lint-metrics)
  ;; Read README.md, locate the ## Test Suite section, parse its table,
  ;; and compare against computed-metrics.
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "ERROR: Could not read README.md: ~a~n" (exn-message e))
                               1)])
    (define readme-lines (file->lines "README.md"))
    ;; Collect lines between "## Test Suite" and the next "##" heading
    (define in-section? (box #f))
    (define section-lines
      (filter values
              (for/list ([line readme-lines])
                (cond
                  [(regexp-match? #rx"^## Test Suite" line)
                   (set-box! in-section? #t) #f]
                  [(and (unbox in-section?) (regexp-match? #rx"^##" line))
                   (set-box! in-section? #f) #f]
                  [(unbox in-section?) line]
                  [else #f]))))
    ;; Parse table rows: | <name> | <number> |
    (define readme-values (make-hash))
    (for ([line section-lines])
      (define m (regexp-match #rx"^\\| *([^|]+?) *\\| *([^|]+?) *\\|$" line))
      (when m
        (define name  (string-trim (cadr m)))
        (define value (string-trim (caddr m)))
        ;; Only store rows with numeric values (skip header separator, etc.)
        (when (regexp-match? #rx"^[0-9,.]+$" value)
          (hash-set! readme-values name (string-replace value "," "")))))
    ;; Compare each computed metric against README
    (define errors
      (for/fold ([errs '()])
                ([(name value) (in-dict computed-metrics)])
        (define readme-val (hash-ref readme-values name #f))
        (cond
          [(not readme-val)
           (cons (format "ERROR: README.md: ~a: metric not found in README table" name) errs)]
          [(not (equal? value readme-val))
           (cons (format "ERROR: README.md: ~a: expected ~a, found ~a" name value readme-val) errs)]
          [else errs])))
    ;; Report results
    (if (null? errors)
        (begin
          (displayln "All 5 static metrics match README.md.")
          0)
        (begin
          (for ([e (reverse errors)]) (displayln e))
          (printf "Metrics lint FAILED (~a errors)~n" (length errors))
          1))))

;; --- Output ---

(cond
  [lint?
   ;; --lint: lint only, skip normal table output
   (exit (lint-metrics))]
  [else
   (printf "| Metric | Value |~n")
   (printf "|--------|-------|~n")
   (printf "| Version | ~a |~n" (version-string))
   (printf "| Source modules | ~a |~n" (length source-files))
   (printf "| Test files | ~a |~n" (length test-files))
   (printf "| Source lines | ~a |~n" src-count)
   (printf "| Test lines | ~a |~n" test-count)
   (printf "| Test assertions | ~a |~n" assertions)
   (printf "| Tests passing | ~a |~n" test-pass-count)])
