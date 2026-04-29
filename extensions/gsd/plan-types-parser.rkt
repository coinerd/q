#lang racket/base

;; extensions/gsd/plan-types-parser.rkt — Markdown parsing for GSD plans
;;
;; Extracted from plan-types.rkt to keep the parsing code in untyped Racket
;; while plan-types.rkt itself is #lang typed/racket.
;; Returns raw hash tables and values — plan-types.rkt wraps into structs.

(require racket/string
         racket/list)

;; ============================================================
;; Parsing helpers
;; ============================================================

;; Clean file path: strip surrounding backticks and whitespace.
(define (clean-file-path s)
  (define trimmed (string-trim s))
  (cond
    [(>= (string-length trimmed) 6)
     (define triple-back (string-prefix? trimmed "```"))
     (define triple-end (string-suffix? trimmed "```"))
     (if (and triple-back triple-end)
         (string-trim (substring trimmed 3 (- (string-length trimmed) 3)))
         (if (and (string-prefix? trimmed "`") (string-suffix? trimmed "`"))
             (string-trim (substring trimmed 1 (- (string-length trimmed) 1)))
             trimmed))]
    [(>= (string-length trimmed) 2)
     (if (and (string-prefix? trimmed "`") (string-suffix? trimmed "`"))
         (string-trim (substring trimmed 1 (- (string-length trimmed) 1)))
         trimmed)]
    [else trimmed]))

;; Parse structured fields from wave document content.
(define (parse-wave-content content)
  (define lines (string-split content "\n"))
  (define n (length lines))
  (define root-cause "")
  (define files '())
  (define verify-cmd "")
  (define done-criteria '())
  (define in-files-section #f)
  (for ([line lines]
        [i (in-naturals)])
    (define trimmed (string-trim line))
    (when (regexp-match? #rx"^## " trimmed)
      (set! in-files-section (string-prefix? trimmed "## Files")))
    (cond
      [(string-prefix? trimmed "## Verify")
       (define after
         (for/list ([j (in-range (add1 i) (min n (+ i 5)))]
                    #:when (and (> (string-length (string-trim (list-ref lines j))) 0)
                                (not (string-contains? (list-ref lines j) "```"))))
           (string-trim (list-ref lines j))))
       (when (and (string=? verify-cmd "") (not (null? after)))
         (set! verify-cmd (string-join after "; ")))]
      [(regexp-match #rx"^- +[Rr]oot *[Cc]ause *: *(.+)$" line)
       =>
       (lambda (m) (set! root-cause (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Ff]iles *: *(.+)$" line)
       =>
       (lambda (m)
         (define paths (string-split (string-trim (cadr m)) ","))
         (set! files (append files (map (lambda (p) (clean-file-path (string-trim p))) paths))))]
      [(regexp-match #rx"^- +[Ff]ile *: *(.+)$" line)
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      [(and in-files-section (regexp-match #rx"^- +(.+)$" line))
       =>
       (lambda (m) (set! files (append files (list (clean-file-path (string-trim (cadr m)))))))]
      [(regexp-match #rx"^- +[Vv]erify *: *(.+)$" line)
       =>
       (lambda (m) (set! verify-cmd (string-trim (cadr m))))]
      [(regexp-match #rx"^- +[Dd]one *: *(.+)$" line)
       =>
       (lambda (m) (set! done-criteria (append done-criteria (list (string-trim (cadr m))))))]))
  (hasheq 'root-cause root-cause 'files files 'verify verify-cmd 'done done-criteria))

;; Parse a single wave section → raw data hash (not a gsd-wave struct).
;; plan-types.rkt wraps this into a gsd-wave.
(define (parse-wave-section-raw lines)
  (define header (car lines))
  (define body-lines (cdr lines))
  (define header-match (regexp-match #rx"^## +[Ww]ave +([0-9]+) *: *(.+)$" header))
  (define idx
    (if header-match
        (string->number (cadr header-match))
        0))
  (define title
    (if header-match
        (string-trim (caddr header-match))
        ""))
  (define fields (parse-wave-content (string-join body-lines "\n")))
  (hasheq 'index
          idx
          'title
          title
          'root-cause
          (hash-ref fields 'root-cause "")
          'files
          (hash-ref fields 'files '())
          'verify
          (hash-ref fields 'verify "")
          'done
          (hash-ref fields 'done '())))

;; Parse PLAN.md content → list of raw data hashes.
(define (parse-waves-from-markdown-raw md-text)
  (define lines (string-split md-text "\n"))
  (define wave-starts
    (for/list ([line lines]
               [idx (in-naturals)]
               #:when (regexp-match #rx"^## +[Ww]ave +[0-9]+" line))
      idx))
  (define wave-end-idxs
    (if (< (length wave-starts) 2)
        (list (sub1 (length lines)))
        (append (map sub1 (cdr wave-starts)) (list (sub1 (length lines))))))
  (for/list ([start wave-starts]
             [end wave-end-idxs])
    (parse-wave-section-raw (take (drop lines start) (add1 (- end start))))))

;; ============================================================
;; Provide
;; ============================================================

(provide parse-waves-from-markdown-raw
         parse-wave-content
         clean-file-path)
