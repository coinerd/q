#lang racket/base

;; scripts/metrics-report.rkt — Codebase metrics scorecard for q
;;
;; Produces a summary of key health metrics.
;; Run: racket scripts/metrics-report.rkt
;;
;; v0.28.10: Initial version — maintainability scorecard.

(require racket/file
         racket/list)

;; Count how many .rkt files match a pattern in content
;; Exclude test, script, and compiled directories from source metrics.
(define (source-file? f)
  (define s (path->string f))
  (and (regexp-match? #rx"\\.rkt$" s)
       (not (regexp-match? #rx"compiled" s))
       (not (regexp-match? #rx"(?:^|/)tests/" s))
       (not (regexp-match? #rx"(?:^|/)scripts/" s))))

(define (count-files-matching dir pattern)
  (for/sum ([f (in-directory dir)] #:when (file-exists? f) #:when (source-file? f))
           (with-handlers ([exn:fail? (lambda (_) 0)])
             (define content (file->string f))
             (if (regexp-match? pattern content) 1 0))))

;; Count total occurrences of a pattern across all .rkt files
(define (count-occurrences dir pattern)
  (for/sum ([f (in-directory dir)] #:when (file-exists? f) #:when (source-file? f))
           (with-handlers ([exn:fail? (lambda (_) 0)])
             (define content (file->string f))
             (length (regexp-match* pattern content)))))

;; Count total .rkt files
(define (count-rkt-files dir)
  (for/sum ([f (in-directory dir)] #:when (file-exists? f) #:when (source-file? f)) 1))

(printf "=== q Codebase Metrics Report ===~n~n")

(printf "Module counts:~n")
(define total-rkt (count-rkt-files "."))
(define tr-count (count-files-matching "." #rx"#lang typed/racket"))
(printf "  Total .rkt files:       ~a~n" total-rkt)
(printf "  Typed Racket files:     ~a~n" tr-count)
(printf "  TR coverage:            ~a%~n"
        (if (> total-rkt 0)
            (real->decimal-string (* 100.0 (/ tr-count total-rkt)) 1)
            "0.0"))
(newline)

(printf "Pattern counts:~n")
(printf "  cond forms:             ~a~n" (count-occurrences "." #px"[(]cond[ \n([]"))
(printf "  match forms:            ~a~n" (count-occurrences "." #px"[(]match[ \n([]"))
(printf "  with-handlers:          ~a~n" (count-occurrences "." #rx"with-handlers"))
(printf "  contract-out modules:   ~a~n" (count-files-matching "." #rx"contract-out"))
(printf "  hasheq constructions:   ~a~n" (count-occurrences "." #rx"hasheq"))
(newline)

(printf "Architecture metrics:~n")
(printf "  Error domain files:     ~a~n" (count-files-matching "." #rx"q-error"))
(printf "  Hook schema versioned:  ~a~n" (count-files-matching "." #rx"HOOK-SCHEMA-VERSION"))
(printf "  Event codec:            ~a~n" (count-files-matching "." #rx"hash->payload"))
(newline)

(define match-n (count-occurrences "." #px"[(]match[ \n([]"))
(define cond-n (count-occurrences "." #px"[(]cond[ \n([]"))
(printf "Quality ratios:~n")
(printf "  match/cond ratio:       ~a~n"
        (if (> cond-n 0)
            (real->decimal-string (/ match-n cond-n) 2)
            "N/A"))
(newline)
(printf "=== End Report ===~n")
