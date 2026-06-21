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
;;   cd q/ && racket scripts/metrics.rkt --sync-all [FILE]    # sync markers+table+prose
;;   cd q/ && racket scripts/metrics.rkt --check-only [FILE]  # dry-run: show what would change
;;
;; The --check-only flag can be combined with --sync-* flags to compute
;; the diff without writing to disk. Pure computation is in metrics-helpers.rkt.
;;
;; Output: markdown table suitable for copy-paste into README.md / CHANGELOG.md

(require racket/port
         racket/file
         racket/list
         racket/string
         racket/dict
         racket/system
         "metrics-helpers.rkt")

(define args (vector->list (current-command-line-arguments)))
(define run-tests? (member "--tests" args))
(define lint? (member "--lint" args))
(define lint-prose? (member "--lint-prose" args))
(define sync-readme? (member "--sync-readme" args))
(define sync-all? (member "--sync-all" args))
(define check-only? (member "--check-only" args))

;; Resolve optional file argument (first non-flag arg)
(define (file-arg)
  (for/first ([a (in-list args)]
              #:when (not (string-prefix? a "--")))
    a))

;; --- Counting helpers ---

;; Enumerate only git-tracked .rkt files to prevent untracked temp/debug
;; files from inflating source/test counts.  Returns a list of path strings,
;; or #f if git is unavailable or not in a repository.
(define (git-tracked-rkt-files)
  (define git-bin (find-executable-path "git"))
  (and git-bin
       (with-handlers ([exn:fail? (lambda (_) #f)])
         (define out (open-output-string))
         (define ok?
           (parameterize ([current-output-port out]
                          [current-error-port out])
             (system* git-bin "ls-files" "--cached" "*.rkt")))
         (and ok?
              (let ([output (get-output-string out)])
                (define parts (string-split output "\n"))
                (define non-empty (filter non-empty-string? parts))
                (and (pair? non-empty) non-empty))))))

;; Check whether a path string refers to a file under the tests/ directory.
;; Handles both "tests/foo.rkt" (git ls-files style) and
;; "./tests/foo.rkt" (in-directory style).
(define (path-under-tests? s)
  (or (string-prefix? s "tests/") (string-contains? s "/tests/")))

;; Predicate: should this file string be included in the count?
(define (rkt-file-keep? s dir exclude?)
  (and (string-suffix? s ".rkt")
       (not (string-contains? s "/compiled/"))
       (not (string-contains? s ".zo"))
       (not (string-contains? s "/__pycache__/"))
       (not (string-contains? s "/.git/"))
       (if exclude?
           (not (path-under-tests? s))
           (if (equal? dir "tests")
               (path-under-tests? s)
               #t))))

;; Return a list of .rkt file paths.
;;
;; When git is available, uses `git ls-files` so only **tracked** files are
;; counted — untracked temp/debug files (e.g. tmp-debug-*.rkt) are excluded.
;; Falls back to filesystem traversal via `in-directory` if git is absent.
;;
;; The `dir` argument controls the subset:
;;   "tests" → only files under tests/
;;   "."     → all tracked .rkt files
;; When `#:exclude-tests?` is #t, files under tests/ are excluded
;; (used for counting *source* modules only).
(define (rkt-files dir #:exclude-tests? [exclude? #f])
  (define tracked (git-tracked-rkt-files))
  (define candidates
    (or tracked
        ;; Fallback: walk the filesystem if git is unavailable
        (for/list ([f (in-directory dir)]
                   #:when (file-exists? f))
          (path->string f))))
  (filter (lambda (f)
            (define s
              (if (path? f)
                  (path->string f)
                  f))
            (rkt-file-keep? s dir exclude?))
          (map (lambda (f)
                 (if (path? f)
                     f
                     (string->path f)))
               candidates)))

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
    ;; Pure computation delegated to metrics-helpers.rkt
    (define content (file->string "README.md"))
    (define errors (lint-table-values content computed-metrics))
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
  ;; Pure computation delegated to metrics-helpers.rkt
  (define content (file->string path))
  (define test-count (number->string (length test-files)))
  (define src-count (number->string (length source-files)))
  (define errors (lint-prose-values content test-count src-count))
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

;; Thin I/O shell: read → compute (pure) → write or display
;; The --check-only flag prints the diff without writing.

(define (write-or-check path updated label)
  (if check-only?
      (begin
        (printf "[check-only] Would sync ~a in ~a~n" label path)
        0)
      (begin
        (call-with-output-file path (λ (out) (display updated out)) #:exists 'replace)
        (printf "Synced ~a in ~a~n" label path)
        0)))

(define (sync-readme-markers path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  ;; Pure computation delegated to metrics-helpers.rkt
  (define content (file->string path))
  (define updated (compute-marker-sync content metrics-map))
  (write-or-check path updated "METRICS markers"))

;; --- Sync table values: update metrics table in README ---

(define (sync-table-values path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  ;; Pure computation delegated to metrics-helpers.rkt
  (define updated (compute-table-sync content computed-metrics))
  (write-or-check path updated "metrics table"))

;; --- Sync prose counts: update prose NNN counts in README ---

(define (sync-prose-counts path)
  (unless (file-exists? path)
    (printf "ERROR: ~a not found~n" path)
    1)
  (define content (file->string path))
  ;; Pure computation delegated to metrics-helpers.rkt
  (define test-count (number->string (length test-files)))
  (define src-mod (number->string (length source-files)))
  (define updated (compute-prose-sync content test-count src-mod))
  (write-or-check path updated "prose counts"))

;; --- Sync all: table + prose + markers ---

(define (sync-all path)
  (define p (or path "README.md"))
  (unless (file-exists? p)
    (printf "ERROR: ~a not found~n" p)
    1)
  ;; Single read-compute-write cycle (was 3 separate reads/writes)
  (define content (file->string p))
  (define updated (compute-all-sync content metrics-map computed-metrics))
  (write-or-check p updated "all metrics"))

;; --- Output ---

(cond
  [lint? (exit (lint-metrics))]
  [lint-prose? (exit (lint-prose-metrics (or (file-arg) "README.md")))]
  [sync-all? (exit (sync-all (file-arg)))]
  [sync-readme? (exit (sync-readme-markers (or (file-arg) "README.md")))]
  [check-only?
   ;; --check-only without --sync-* : show what would change
   (let* ([p (or (file-arg) "README.md")])
     (unless (file-exists? p)
       (printf "ERROR: ~a not found~n" p)
       (exit 1))
     (let* ([content (file->string p)]
            [updated (compute-all-sync content metrics-map computed-metrics)])
       (if (equal? content updated)
           (begin
             (displayln "No changes needed.")
             (exit 0))
           (begin
             (displayln "Changes would be made (--check-only mode):")
             (let* ([old-lines (string-split content "\n")]
                    [new-lines (string-split updated "\n")])
               (for ([old-l (in-list old-lines)]
                     [new-l (in-list new-lines)]
                     [i (in-naturals)])
                 (unless (equal? old-l new-l)
                   (printf "  Line ~a:~n    - ~a~n    + ~a~n" (add1 i) old-l new-l)))
               (exit 0))))))]
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
