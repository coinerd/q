#!/usr/bin/env racket
#lang racket/base

;; gen-release-manifest.rkt — Generate a structured release manifest (JSON).
;;
;; Usage:
;;   racket scripts/gen-release-manifest.rkt [TARBALL_PATH]
;;
;; If TARBALL_PATH is provided, includes file size and SHA-256 checksum.
;; Outputs JSON to stdout.

(require racket/file
         racket/port
         racket/string
         racket/system
         racket/path)

;; ---------------------------------------------------------------------------
;; Version parsing
;; ---------------------------------------------------------------------------

(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

;; ---------------------------------------------------------------------------
;; SHA-256 of a file (uses sha256sum command)
;; ---------------------------------------------------------------------------

(define (file-sha256 path)
  (define out
    (with-output-to-string (lambda ()
                             (system (format "sha256sum ~a 2>/dev/null || shasum -a 256 ~a"
                                             (path->string path)
                                             (path->string path))))))
  (define parts (string-split out))
  (if (pair? parts)
      (car parts)
      "unknown"))

;; ---------------------------------------------------------------------------
;; File size
;; ---------------------------------------------------------------------------

(define (file-size-bytes path)
  (file-size path))

;; ---------------------------------------------------------------------------
;; JSON output (manual — no dependency on json library)
;; ---------------------------------------------------------------------------

(define (emit-manifest version commit date tarball-path)
  (define size
    (if tarball-path
        (file-size-bytes tarball-path)
        0))
  (define sha
    (if tarball-path
        (file-sha256 tarball-path)
        "n/a"))
  (define tarball-name
    (if tarball-path
        (path->string (file-name-from-path tarball-path))
        (format "q-~a.tar.gz" version)))
  (displayln "{")
  (printf "  \"version\": \"~a\",~n" version)
  (printf "  \"tag\": \"v~a\",~n" version)
  (printf "  \"commit\": \"~a\",~n" (or commit "unknown"))
  (printf "  \"date\": \"~a\",~n" date)
  (printf "  \"assets\": [~n")
  (printf "    {~n")
  (printf "      \"name\": \"~a\",~n" tarball-name)
  (printf "      \"size\": ~a,~n" size)
  (printf "      \"sha256\": \"~a\"~n" sha)
  (printf "    }~n")
  (printf "  ],~n")
  (printf "  \"compatibility\": {~n")
  (printf "    \"min-racket\": \"8.10\"~n")
  (printf "  },~n")
  (printf "  \"verification\": \"racket main.rkt --version\"~n")
  (displayln "}"))

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define args (vector->list (current-command-line-arguments)))

  ;; Read version
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found")
    (exit 1))
  (define version (parse-q-version (file->string util-path)))
  (unless version
    (displayln "ERROR: could not parse version from util/version.rkt")
    (exit 1))

  ;; Get git commit
  (define commit
    (let ([out (with-output-to-string (lambda () (system "git rev-parse --short HEAD 2>/dev/null")))])
      (string-trim out)))

  ;; Get date
  (define date
    (let ([out (with-output-to-string (lambda () (system "date -u +%Y-%m-%d")))]) (string-trim out)))

  ;; Tarball path (optional)
  (define tarball-path
    (if (pair? args)
        (let ([p (string->path (car args))]) (if (file-exists? p) p #f))
        #f))

  (emit-manifest version commit date tarball-path))

(main)
