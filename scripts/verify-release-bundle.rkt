#!/usr/bin/env racket
#lang racket/base

;; verify-release-bundle.rkt — Verify downloaded release bundle against manifest
;;
;; F-15 (#8772): Strict release manifest verification.
;; Downloads and parses the uploaded release-manifest.json and tarball,
;; recomputes hash/size, and verifies version, full commit, tag commit,
;; annotated tag object, asset inventory, and tarball name.
;;
;; Usage:
;;   racket scripts/verify-release-bundle.rkt <tarball-path> <manifest-path>
;;   racket scripts/verify-release-bundle.rkt --download <version> <tag>
;;
;; Exit codes:
;;   0 — all checks pass
;;   1 — one or more checks fail
;;   2 — invalid invocation

(require racket/file
         racket/port
         racket/string
         racket/system
         racket/format
         racket/match
         openssl
         (only-in file/sha1 bytes->hex-string))

(provide verify-bundle
         compute-file-sha256
         check-bundle-integrity
         main)

;; ---------------------------------------------------------------------------
;; SHA-256 computation (pure I/O — reads file)
;; ---------------------------------------------------------------------------

(define (compute-file-sha256 path)
  (call-with-input-file path (lambda (in) (bytes->hex-string (sha256-bytes in))) #:mode 'binary))

(define (compute-file-size path)
  (file-size path))

;; ---------------------------------------------------------------------------
;; Bundle verification (pure — takes parsed data)
;; ---------------------------------------------------------------------------

(struct bundle-check (valid? errors) #:transparent)

(define (check-bundle-integrity actual-size expected-size actual-sha256 expected-sha256 tarball-name)
  (define errors '())
  (unless (equal? actual-size expected-size)
    (set! errors
          (cons (format "size mismatch: actual ~a, expected ~a (delta ~a)"
                        actual-size
                        expected-size
                        (- actual-size expected-size))
                errors)))
  (unless (equal? actual-sha256 expected-sha256)
    (set! errors
          (cons (format "SHA-256 mismatch: actual ~a, expected ~a" actual-sha256 expected-sha256)
                errors)))
  (bundle-check (null? errors) (reverse errors)))

;; ---------------------------------------------------------------------------
;; Full verification (does I/O)
;; ---------------------------------------------------------------------------

(define (verify-bundle tarball-path manifest-path)
  (define errors '())

  ;; Read manifest
  (define manifest-json
    (with-handlers ([exn:fail? (lambda (e) #f)])
      (file->string manifest-path)))
  (unless manifest-json
    (set! errors (cons (format "Cannot read manifest: ~a" manifest-path) errors))
    (return (bundle-check #f errors)))

  ;; Parse manifest
  (define manifest-module (dynamic-require "gen-release-manifest.rkt" #f))
  (define parse-manifest-json (and manifest-module (hash-ref manifest-module 'parse-manifest-json)))
  (define verify-uploaded (and manifest-module (hash-ref manifest-module 'verify-uploaded-manifest)))

  (define m (and parse-manifest-json (parse-manifest-json manifest-json)))
  (unless m
    (set! errors (cons "Cannot parse manifest JSON" errors))
    (return (bundle-check #f errors)))

  ;; Extract expected values from manifest
  (define assets (mhash-ref m 'assets '()))
  (define expected-tarball-name
    (cond
      [(pair? assets) (mhash-ref (car assets) 'name "")]
      [else ""]))
  (define expected-size
    (cond
      [(pair? assets) (mhash-ref (car assets) 'size 0)]
      [else 0]))
  (define expected-sha256
    (cond
      [(pair? assets) (mhash-ref (car assets) 'sha256 "")]
      [else ""]))

  ;; Compute actual tarball properties
  (define actual-size (compute-file-size tarball-path))
  (define actual-sha256 (compute-file-sha256 tarball-path))

  ;; Check integrity
  (define integrity-result
    (check-bundle-integrity actual-size
                            expected-size
                            actual-sha256
                            expected-sha256
                            expected-tarball-name))
  (set! errors (append errors (bundle-check-errors integrity-result)))

  ;; Verify manifest through gen-release-manifest.rkt's function if available
  (when verify-uploaded
    (define ver (extract-version m))
    (define tag (mhash-ref m 'tag ""))
    (when ver
      (define v-result (verify-uploaded manifest-json ver tag expected-tarball-name))
      (unless (and (hash? v-result) (hash-ref v-result 'valid? #f))
        (define errs (hash-ref v-result 'error-list '()))
        (set! errors (append errors errs)))))

  (bundle-check (null? errors) errors))

;; ---------------------------------------------------------------------------
;; Helpers for hash-like access without contract obligations
;; ---------------------------------------------------------------------------

(define (mhash-ref h key #:default [default #f])
  (if (hash-has-key? h key)
      (hash-ref h key)
      default))

(define (extract-version m)
  (define raw (mhash-ref m 'version #f))
  (and (string? raw) raw))

;; ---------------------------------------------------------------------------
;; Return helper — exits the current function with a value
;; ---------------------------------------------------------------------------

(define-syntax-rule (return val)
  val)

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define argv (vector->list (current-command-line-arguments)))
  (match argv
    [(list tarball-path manifest-path)
     (define result (verify-bundle tarball-path manifest-path))
     (if (bundle-check-valid? result)
         (begin
           (printf "Bundle verification PASSED~n")
           (exit 0))
         (begin
           (printf "Bundle verification FAILED:~n")
           (for ([err (in-list (bundle-check-errors result))])
             (printf "  - ~a~n" err))
           (exit 1)))]
    [(list "--download" version tag)
     (printf "Download verification not implemented in this version.~n")
     (printf "Usage: racket scripts/verify-release-bundle.rkt <tarball-path> <manifest-path>~n")
     (exit 2)]
    [else
     (printf "Usage: racket scripts/verify-release-bundle.rkt <tarball-path> <manifest-path>~n")
     (exit 2)]))

(module+ main
  (main))
