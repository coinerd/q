#!/usr/bin/env racket
#lang racket/base

;; scripts/check-version-expectations.rkt — Version-expectation lint (BUG-0009).
;;
;; Fails the *introducing* PR when a test under tests/ hard-codes the current
;; release version string instead of deriving it from the version module.
;; Canonical source: util/version.rkt → q-version (info.rkt must agree —
;; scripts/sync-version.rkt keeps them in lockstep).
;;
;; Usage:
;;   cd q/ && racket scripts/check-version-expectations.rkt
;;
;; Exit 0 if clean, 1 if any hard-coded version literal found (or the
;; canonical version itself is unparseable/out of sync).

(require racket/file
         racket/list
         racket/path
         racket/string)

(provide canonical-version-from-content
         find-version-literals)

;; Extract the canonical version from util/version.rkt content:
;; (define q-version "X.Y.Z") → "X.Y.Z", or #f if absent.
(define (canonical-version-from-content content)
  (define m (regexp-match #px"define\\s+q-version\\s+\"([^\"]+)\"" content))
  (and m (cadr m)))

;; List of 1-based line numbers in `lines` containing the literal
;; `canonical` verbatim (comments included — a literal is a literal).
(define (find-version-literals lines canonical)
  (define pat (regexp (regexp-quote canonical)))
  (for/list ([line (in-list lines)]
             [n (in-naturals 1)]
             #:when (regexp-match? pat line))
    n))

;; Extract (define version "X.Y.Z") from info.rkt content, or #f.
(define (info-version-from-content content)
  (define m (regexp-match #px"define\\s+version\\s+\"([^\"]+)\"" content))
  (and m (cadr m)))

(define (test-files)
  (sort (for/list ([p (in-directory "tests")]
                   #:when (and (file-exists? p)
                               (regexp-match? #rx"\\.rkt$" (path->string p))
                               (not (regexp-match? #rx"(^|/)compiled/" (path->string p)))))
          p)
        path<?))

(define (main)
  (printf "=== Version Expectation Check (BUG-0009) ===~n")
  (define util-path (build-path "util" "version.rkt"))
  (define info-path (build-path "info.rkt"))
  (unless (file-exists? util-path)
    (displayln "ERROR: util/version.rkt not found — run from repo root (q/)")
    (exit 1))
  (define canonical (canonical-version-from-content (file->string util-path)))
  (cond
    [(not canonical)
     (displayln "ERROR: cannot parse (define q-version \"...\") from util/version.rkt")
     (exit 1)]
    [else
     ;; info.rkt must agree with the canonical module; drift here is exactly
     ;; the duplication class this lint exists to catch.
     (define info-v (and (file-exists? info-path)
                         (info-version-from-content (file->string info-path))))
     (when (and info-v (not (string=? info-v canonical)))
       (printf "ERROR: info.rkt version ~a != canonical q-version ~a~n" info-v canonical)
       (displayln "       run: racket scripts/sync-version.rkt --write")
       (exit 1))
     (define files (test-files))
     (define hits
       (for/fold ([acc '()])
                 ([f (in-list files)])
         (append acc
                 (for/list ([n (in-list (find-version-literals
                                         (file->lines f) canonical))])
                   (cons (path->string f) n)))))
     (cond
       [(null? hits)
        (printf "Version-expectation lint PASSED — ~a test files scanned, 0 hard-coded \"~a\" literals~n"
                (length files) canonical)
        (exit 0)]
       [else
        (printf "FAIL: ~a hard-coded version literal(s) \"~a\" in tests/ (derive from q-version instead):~n"
                (length hits) canonical)
        (for ([h (in-list hits)])
          (printf "  ERROR: ~a:~a~n" (car h) (cdr h)))
        (displayln "  Fix: (require (only-in \"../util/version.rkt\" q-version)) and (format ...) the expectation.")
        (displayln "  Rationale (BUG-0009): a literal here goes green on the branch that adds it and")
        (displayln "  red on main/release after the next version bump.")
        (exit 1)])]))

(module+ main
  (main))
