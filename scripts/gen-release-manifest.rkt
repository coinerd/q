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

(define (git-tag-sha tag)
  "Get the SHA that the tag points to (short)."
  (define out
    (with-output-to-string (lambda () (system (format "git rev-list -n 1 ~a 2>/dev/null" tag)))))
  (define trimmed (string-trim out))
  (if (string=? trimmed "") "unknown" trimmed))

(define (git-tag-object-sha tag)
  "Get the annotated tag object SHA if the tag is annotated, #f if lightweight."
  (define out
    (with-output-to-string (lambda () (system (format "git rev-parse ~a 2>/dev/null" tag)))))
  (define trimmed (string-trim out))
  (if (or (string=? trimmed "") (regexp-match? #rx"unknown revision" trimmed)) #f trimmed))

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
  ;; Traceability: tag SHA and tag object SHA
  (define tag-name (format "v~a" version))
  (define tag-commit-sha (git-tag-sha tag-name))
  (define tag-obj-sha (git-tag-object-sha tag-name))
  (displayln "{")
  (printf "  \"version\": \"~a\",~n" version)
  (printf "  \"tag\": \"v~a\",~n" version)
  (printf "  \"commit\": \"~a\",~n" (or commit "unknown"))
  (printf "  \"date\": \"~a\",~n" date)
  ;; W6: Traceability fields
  (printf "  \"traceability\": {~n")
  (printf "    \"tag_name\": \"~a\",~n" tag-name)
  (printf "    \"tag_commit_sha\": \"~a\",~n" tag-commit-sha)
  (when tag-obj-sha
    (printf "    \"tag_object_sha\": \"~a\",~n" tag-obj-sha))
  (printf "    \"manifest_commit_sha\": \"~a\",~n" (or commit "unknown"))
  (printf "    \"commit_matches_tag\": ~a~n"
          (if (and commit
                   (not (equal? commit "unknown"))
                   (not (equal? tag-commit-sha "unknown"))
                   (or (string=? commit tag-commit-sha)
                       (string-prefix? tag-commit-sha commit)
                       (string-prefix? commit tag-commit-sha)))
              "true"
              "false"))
  (printf "  },~n")
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

(module+ main
  (main))
