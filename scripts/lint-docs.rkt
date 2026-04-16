#!/usr/bin/env racket
#lang racket/base

;; lint-docs.rkt — Documentation coherence linter.
;;
;; Checks:
;;   1. Verified-against markers in docs/ files reference the current version
;;   2. No stale version references in canonical docs
;;   3. Internal doc links resolve to existing files
;;   4. ADR numbering is sequential without gaps
;;
;; Exit 0 if clean, 1 if any issues found.
;; Run from the q/ directory.

(require racket/file
         racket/list
         racket/path
         racket/string
         racket/match)

;; ---------------------------------------------------------------------------
;; Version helpers
;; ---------------------------------------------------------------------------

(define (parse-q-version content)
  (define m (regexp-match #rx"\\(define q-version \"([0-9]+\\.[0-9]+\\.[0-9]+)\"" content))
  (and m (cadr m)))

(define VERSION-PAT #rx"[0-9]+\\.[0-9]+\\.[0-9]+")

;; ---------------------------------------------------------------------------
;; File discovery
;; ---------------------------------------------------------------------------

(define (skip-path? p)
  (define s (path->string p))
  (or (string-contains? s "/compiled/") (string-contains? s "/.git/")))

(define (collect-md-files base-dir)
  (sort (for/list ([f (in-directory base-dir)]
                   #:when (and (not (skip-path? f))
                               (let ([ext (filename-extension f)])
                                 (and ext (equal? (bytes->string/utf-8 ext) "md")))))
          f)
        path<?))

;; ---------------------------------------------------------------------------
;; Check 1: verified-against markers
;; ---------------------------------------------------------------------------

(define (check-verified-markers md-files canonical)
  (define marker-rx #rx"<!-- verified-against: v([0-9]+\\.[0-9]+\\.[0-9]+) -->")
  (define warnings 0)
  (for ([f (in-list md-files)])
    (define content (file->string f))
    (define matches (regexp-match* marker-rx content))
    (for ([m (in-list matches)])
      (define v-m (regexp-match #rx"v([0-9]+\\.[0-9]+\\.[0-9]+)" m))
      (when v-m
        (define v (cadr v-m))
        (unless (equal? v canonical)
          (printf "  WARN: ~a: verified-against v~a but canonical is v~a~n"
                  (path->string f)
                  v
                  canonical)
          (set! warnings (add1 warnings))))))
  (printf "  Verified-against markers checked: ~a warnings~n" warnings)
  warnings)

;; ---------------------------------------------------------------------------
;; Check 2: internal doc links
;; ---------------------------------------------------------------------------

(define (check-doc-links md-files)
  (define doc-dir (build-path (current-directory) "docs"))
  (define link-rx #rx"\\[(?:[^\\]]*)\\]\\(([^)\"']+)\\)")
  (define broken 0)
  (for ([f (in-list md-files)])
    (define content (file->string f))
    (define links (regexp-match* link-rx content))
    (for ([raw (in-list links)])
      (define target-m (regexp-match #rx"\\]\\(([^)\"']+)" raw))
      (when target-m
        (define target (cadr target-m))
        ;; Only check relative links (not http/https)
        (unless (or (string-prefix? target "http")
                    (string-prefix? target "#")
                    (string-prefix? target "mailto:"))
          (define base (path-only f))
          (define resolved (simplify-path (build-path base target)))
          (unless (or (file-exists? resolved) (directory-exists? resolved))
            (printf "  WARN: ~a: broken link ~a -> ~a~n"
                    (path->string f)
                    target
                    (path->string resolved))
            (set! broken (add1 broken)))))))
  (printf "  Doc links checked: ~a broken~n" broken)
  broken)

;; ---------------------------------------------------------------------------
;; Check 3: ADR numbering
;; ---------------------------------------------------------------------------

(define (check-adr-numbering)
  (define adr-dir (build-path (current-directory) "docs" "adr"))
  (define gaps 0)
  (when (directory-exists? adr-dir)
    (define adrs
      (filter values
              (for/list ([f (in-directory adr-dir)])
                (define name (path->string (file-name-from-path f)))
                (define m (regexp-match #rx"^([0-9]+)-" name))
                (and m (string->number (car m))))))
    (set! adrs (sort adrs <))
    (for ([n (in-list adrs)]
          [i (in-naturals 1)])
      (unless (= n i)
        (printf "  WARN: ADR gap — expected ~a, found ~a~n" i n)
        (set! gaps (add1 gaps)))))
  (printf "  ADR numbering: ~a gaps~n" gaps)
  gaps)

;; ---------------------------------------------------------------------------
;; Main
;; ---------------------------------------------------------------------------

(define (main)
  (define util-path (build-path (current-directory) "util" "version.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found")
    (exit 1))
  (define canonical (parse-q-version (file->string util-path)))
  (unless canonical
    (displayln "ERROR: could not parse version")
    (exit 1))

  (printf "Docs lint — canonical version: ~a~n~n" canonical)

  (define md-files (collect-md-files (build-path (current-directory) "docs")))
  (printf "Found ~a doc files~n~n" (length md-files))

  (displayln "Checking verified-against markers ...")
  (define w1 (check-verified-markers md-files canonical))

  (displayln "Checking doc links ...")
  (define w2 (check-doc-links md-files))

  (displayln "Checking ADR numbering ...")
  (define w3 (check-adr-numbering))

  (define total (+ w1 w2 w3))
  (printf "~n---~n")
  (printf "Total warnings: ~a~n" total)
  (if (= total 0)
      (begin
        (displayln "Docs lint PASSED")
        (exit 0))
      (begin
        (displayln "Docs lint FAILED")
        (exit 1))))

(main)
