#!/usr/bin/env racket
#lang racket/base

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/port)

;; ---------------------------------------------------------------------------
;; Portable test linter — statically analyzes tests/*.rkt for portability issues
;; Exit 0 if clean, 1 if any ERROR found (warnings/infos are reported only).
;; ---------------------------------------------------------------------------

;;; --- helpers ---

(define (comment-line? line)
  (string-prefix? (string-trim line) ";;"))

(define (hardcoded-home-path? str)
  ;; strings containing /home/ or /Users/ — always a portability problem
  (or (string-contains? str "/home/") (string-contains? str "/Users/")))

(define allowed-abs-prefixes
  '("/tmp/" "/tmp"
            "/dev/null"
            "/usr/"
            "/etc/"
            "/proc/"
            "/sys/"
            "/bin/"
            "/sbin/"
            "/lib/"
            "/var/"
            "/opt/"
            "/run/"
            "/boot/"
            "/forbidden"))

(define (cli-command? str)
  ;; Single-segment /words like /help /quit /exit — not paths
  (regexp-match? #rx"^/[a-z][a-z-]*$" str))

(define (string-contains-char? str c)
  (for/or ([ch (in-string str)])
    (char=? ch c)))

(define (not-a-file-path? str)
  ;; Strings containing spaces, backslashes, or other non-path chars
  ;; are almost certainly not filesystem paths.
  (or (string-contains? str " ")
      (string-contains-char? str #\\)
      ;; Unicode right arrow (→)
      (regexp-match? #rx"\u2192" str)))

(define (test-fixture-path? str)
  ;; Test fixtures for nonexistent or fake paths — not real dependencies
  (or (string-contains? str "nonexistent")
      (string-contains? str "no-such")
      (string-contains? str "no/such")
      (string-contains? str "/my/project")
      (string-contains? str "/src/bar.rkt")
      (string-contains? str "/src/main.rkt")
      (string-contains? str "/src/util.rkt")
      (string-contains? str "/foo/bar.txt")
      ;; Single-char filenames like /a.rkt /b.rkt — clearly test fixtures
      (regexp-match? #rx"^/[a-z]\\.rkt$" str)
      ;; Multi-char fake test names /foo.rkt /bar.rkt
      (regexp-match? #rx"^/(foo|bar)\\.rkt$" str)))

(define (absolute-path-pattern? str)
  ;; matches #rx"^/[a-z]" but NOT allowed prefixes, CLI commands, or test fixtures
  (and (not (not-a-file-path? str))
       (regexp-match? #rx"^/[a-z]" str)
       (not (cli-command? str))
       (not (test-fixture-path? str))
       (not (for/or ([prefix (in-list allowed-abs-prefixes)])
              (string-prefix? str prefix)))))

(define (extract-strings-from-line line)
  ;; pull out double-quoted string literals from a line (best-effort)
  (let loop ([chars (string->list line)]
             [acc '()]
             [in-string #f]
             [buf '()])
    (cond
      [(null? chars)
       (if in-string
           (cons (list->string (reverse buf)) acc)
           acc)]
      [else
       (let ([c (car chars)]
             [rest (cdr chars)])
         (cond
           [(and in-string (char=? c #\")) (loop rest (cons (list->string (reverse buf)) acc) #f '())]
           [in-string (loop rest acc #t (cons c buf))]
           [(char=? c #\") (loop rest acc #t '())]
           [else (loop rest acc #f '())]))])))

;;; --- state ---

(define errors '())
(define warnings '())
(define infos '())

(define (add-error! msg)
  (set! errors (cons msg errors)))
(define (add-warning! msg)
  (set! warnings (cons msg warnings)))
(define (add-info! msg)
  (set! infos (cons msg infos)))

;;; --- checks per file ---

(define (check-hardcoded-paths filepath lines)
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (unless (comment-line? line)
      (for ([str (in-list (extract-strings-from-line line))])
        (when (hardcoded-home-path? str)
          (add-error! (format "ERROR: ~a:~a: hardcoded path \"~a\"" filepath lineno str)))
        (when (absolute-path-pattern? str)
          (add-error! (format "ERROR: ~a:~a: hardcoded path \"~a\"" filepath lineno str)))))))

(define (check-cwd-assumptions filepath lines)
  (define uses-current-directory? #f)
  (define has-runtime-path? #f)
  (for ([line (in-list lines)])
    (unless (comment-line? line)
      (when (string-contains? line "current-directory")
        (set! uses-current-directory? #t))
      (when (string-contains? line "define-runtime-path")
        (set! has-runtime-path? #t))))
  (when (and uses-current-directory? (not has-runtime-path?))
    ;; find the first non-comment line that uses current-directory
    (for ([line (in-list lines)]
          [lineno (in-naturals 1)])
      (when (and (not (comment-line? line))
                 (string-contains? line "current-directory")
                 (not (string-contains? line "define-runtime-path")))
        (add-warning! (format "WARNING: ~a:~a: current-directory used without define-runtime-path"
                              filepath
                              lineno))
        ;; only report the first occurrence per file
        (set! uses-current-directory? #f)))))

(define (check-hash-ordering filepath lines)
  (for ([line (in-list lines)]
        [lineno (in-naturals 1)])
    (unless (comment-line? line)
      (when (or (string-contains? line "hash-keys") (string-contains? line "hash-values"))
        (unless (string-contains? line "sort")
          (define which
            (cond
              [(string-contains? line "hash-keys") "hash-keys"]
              [else "hash-values"]))
          (add-info! (format "INFO: ~a:~a: ~a without sort (ordering may be nondeterministic)"
                             filepath
                             lineno
                             which)))))))

;;; --- main ---

(define tests-dir (build-path (current-directory) "tests"))

(define (lint-file filepath)
  (define lines (file->lines filepath))
  (define rel (path->string (find-relative-path (current-directory) filepath)))
  (check-hardcoded-paths rel lines)
  (check-cwd-assumptions rel lines)
  (check-hash-ordering rel lines))

(define (main)
  (unless (directory-exists? tests-dir)
    (displayln "tests/ directory not found. Run from q/ root.")
    (exit 2))

  (define rkt-files
    (sort (for/list ([f (in-directory tests-dir)]
                     #:when (string-suffix? (path->string f) ".rkt"))
            f)
          path<?))

  (for ([f (in-list rkt-files)])
    (lint-file f))

  ;; Print results in order: errors, warnings, infos
  (for ([e (reverse errors)])
    (displayln e))
  (for ([w (reverse warnings)])
    (displayln w))
  (for ([i (reverse infos)])
    (displayln i))

  (displayln "---")
  (printf "~a errors, ~a warnings, ~a infos\n" (length errors) (length warnings) (length infos))

  (if (null? errors)
      (begin
        (displayln "Lint PASSED")
        (exit 0))
      (begin
        (displayln "Lint FAILED")
        (exit 1))))

(main)
