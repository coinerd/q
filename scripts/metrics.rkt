#!/usr/bin/env racket
#lang racket/base

;; scripts/metrics.rkt — Single-source-of-truth project metrics
;;
;; Usage:
;;   cd q/ && racket scripts/metrics.rkt                # static metrics
;;   cd q/ && racket scripts/metrics.rkt --tests        # also run test suite
;;   cd q/ && racket scripts/metrics.rkt --lint         # verify README.md metrics match reality
;;   cd q/ && racket scripts/metrics.rkt --lint-prose [FILE]  # verify prose counts match table
;;   cd q/ && racket scripts/metrics.rkt --sync-readme [FILE] # replace METRICS markers in file
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
(define lint? (member "--lint" args))
(define lint-prose? (member "--lint-prose" args))
(define sync-readme? (member "--sync-readme" args))
(define sync-all? (member "--sync-all" args))

;; Resolve optional file argument (first non-flag arg)
(define (file-arg)
  (for/first ([a (in-list args)]
              #:when (not (string-prefix? a "--")))
    a))

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
             (for/sum ([line (file->lines f)]) (if (regexp-match? #rx"\\(check-" line) 1 0)))))

(define (version-string)
  (with-handlers ([exn:fail? (lambda (_) "unknown")])
    (define info-content (file->string "info.rkt"))
    (define m (regexp-match #rx"[(]define version \"([^\"]+)\"[)]" info-content))
    (if m
        (cadr m)
        "unknown")))

;; --- Compute metrics ---

(define test-files (rkt-files "tests"))
(define source-files (rkt-files "." #:exclude-tests? #t))
(define test-count (line-count test-files))
(define src-count (line-count source-files))
(define assertions (assertion-count test-files))

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
        (if m
            (string->number (cadr m))
            "unknown"))
      "— (use --tests to run)"))

;; --- Computed metric values keyed by the name used in the README table ---

(define computed-metrics
  `(("Source modules" . ,(number->string (length source-files)))
    ("Test files" . ,(number->string (length test-files)))
    ("Source lines" . ,(number->string src-count))
    ("Test lines" . ,(number->string test-count))
    ("Test assertions" . ,(number->string assertions))))

;; --- Lint mode: compare computed metrics against README.md ---

(define (lint-metrics)
  (with-handlers ([exn:fail? (lambda (e)
                               (printf "ERROR: Could not read README.md: ~a~n" (exn-message e))
                               1)])
    (define readme-lines (file->lines "README.md"))
    (define in-section? (box #f))
    (define section-lines
      (filter values
              (for/list ([line readme-lines])
                (cond
                  [(regexp-match? #rx"^## Test Suite" line)
                   (set-box! in-section? #t)
                   #f]
                  [(and (unbox in-section?) (regexp-match? #rx"^##" line))
                   (set-box! in-section? #f)
                   #f]
                  [(unbox in-section?) line]
                  [else #f]))))
    (define readme-values (make-hash))
    (for ([line section-lines])
      (define m (regexp-match #rx"^\\| *([^|]+?) *\\| *([^|]+?) *\\|$" line))
      (when m
        (define name (string-trim (cadr m)))
        (define value (string-trim (caddr m)))
        (when (regexp-match? #rx"^[0-9,.]+$" value)
          (hash-set! readme-values name (string-replace value "," "")))))
    (define errors
      (for/fold ([errs '()]) ([(name value) (in-dict computed-metrics)])
        (define readme-val (hash-ref readme-values name #f))
        (cond
          [(not readme-val)
           (cons (format "ERROR: README.md: ~a: metric not found in README table" name) errs)]
          [(not (equal? value readme-val))
           (cons (format "ERROR: README.md: ~a: expected ~a, found ~a" name value readme-val) errs)]
          [else errs])))
    (if (null? errors)
        (begin
          (displayln "All 5 static metrics match README.md.")
          0)
        (begin
          (for ([e (reverse errors)])
            (displayln e))
          (printf "Metrics lint FAILED (~a errors)~n" (length errors))
          1))))

;; --- Lint prose mode: check prose counts match computed metrics ---

(define (lint-prose-metrics path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  (define errors '())
  ;; Check for patterns like "Full test suite (NNN files)" in prose
  (define prose-rxs
    `(("test-files" . #rx"Full test suite \\(([0-9]+) files\\)")
      ("test-files" . #rx"tests? suite:? \\(([0-9]+) files\\)")
      ("source-modules" . #rx"([0-9]+) source modules")))
  (for ([pr (in-list prose-rxs)])
    (define key (car pr))
    (define rx (cdr pr))
    (define m (regexp-match rx content))
    (when m
      (define prose-val (cadr m))
      (define computed
        (cond
          [(equal? key "test-files") (number->string (length test-files))]
          [(equal? key "source-modules") (number->string (length source-files))]
          [else "unknown"]))
      (unless (equal? prose-val computed)
        (set! errors
              (cons (format "MISMATCH: prose says ~a but computed ~a is ~a" prose-val key computed)
                    errors)))))
  (if (null? errors)
      (begin
        (displayln "Prose metrics lint PASSED")
        0)
      (begin
        (for ([e (reverse errors)])
          (displayln e))
        (printf "Prose lint FAILED (~a errors)~n" (length errors))
        1)))

;; --- Sync readme markers: replace <!-- METRICS: key --> with actual values ---

(define metrics-map
  (hash "test-files"
        (number->string (length test-files))
        "source-modules"
        (number->string (length source-files))
        "source-lines"
        (number->string src-count)
        "test-lines"
        (number->string test-count)
        "test-assertions"
        (number->string assertions)))

(define (sync-readme-markers path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  (define marker-rx #rx"<!-- METRICS: ([a-z-]+) -->")
  (define updated
    (regexp-replace*
     marker-rx
     content
     (λ (match key) (hash-ref metrics-map key (λ () (format "<!-- METRICS: ~a (unknown) -->" key))))))
  (call-with-output-file path (λ (out) (display updated out)) #:exists 'replace)
  (printf "Synced METRICS markers in ~a~n" path)
  0)

;; --- Sync table values: update metrics table in README ---

(define (sync-table-values path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  (define updated
    (for/fold ([c content]) ([(name value) (in-dict computed-metrics)])
      (regexp-replace* (regexp (format "\\| ~a \\| [0-9,]+ \\|" (regexp-quote name)))
                       c
                       (format "| ~a | ~a |" name value))))
  (call-with-output-file path (λ (out) (display updated out)) #:exists 'replace)
  (printf "Synced metrics table in ~a~n" path)
  0)

;; --- Sync prose counts: update prose NNN counts in README ---

(define (sync-prose-counts path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  ;; Fix "Full test suite (NNN files)"
  (define test-file-count (number->string (length test-files)))
  (define updated
    (regexp-replace* #rx"Full test suite \\([0-9]+ files\\)"
                     content
                     (format "Full test suite (~a files)" test-file-count)))
  ;; Fix "NNN source modules"
  (define src-mod-count (number->string (length source-files)))
  (define updated2
    (regexp-replace* #rx"[0-9]+ source modules" updated (format "~a source modules" src-mod-count)))
  (call-with-output-file path (λ (out) (display updated2 out)) #:exists 'replace)
  (printf "Synced prose counts in ~a~n" path)
  0)

;; --- Sync all: table + prose + markers ---

(define (sync-all path)
  (define p (or path "README.md"))
  (sync-readme-markers p)
  (sync-table-values p)
  (sync-prose-counts p)
  (printf "--sync-all complete.~n")
  0)

;; --- Output ---

(cond
  [lint? (exit (lint-metrics))]
  [lint-prose? (exit (lint-prose-metrics (or (file-arg) "README.md")))]
  [sync-all? (exit (sync-all (file-arg)))]
  [sync-readme? (exit (sync-readme-markers (or (file-arg) "README.md")))]
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
