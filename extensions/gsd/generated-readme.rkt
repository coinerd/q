#lang racket/base
;; Pure, versioned interpretation of the recognized metrics generator. Never
;; load candidate code. Only two numeric table cells are correction-owned.
(require racket/list racket/port racket/string)
(provide generated-readme-correction generated-metrics recognized-metrics-generators)
(define recognized-metrics-generators
  '(("scripts/metrics.rkt" . "9a2fcda109e36762d03cbe324df45a1e020785cb425ac925c45431de5b40e607")
    ("scripts/metrics-helpers.rkt" . "90a0235867ddb2585f382d8aa1b47dffaaacc1656e95c9b91af4c90431d6636a")))
;; Inputs are (path . strict UTF-8 contents), selected from the committed tree.
;; Same tracked-Racket filters, file->lines semantics, assertion-BEARING lines
;; (not occurrences) as the pinned generator. No filesystem fallback.
(define (generated-metrics inputs)
  (define kept
    (filter (lambda (p)
              (and (string-suffix? (car p) ".rkt")
                   (not (ormap (lambda (s) (string-contains? (car p) s))
                               '("/compiled/" ".zo" "/__pycache__/" "/.git/"))))) inputs))
  (define (test? p) (or (string-prefix? (car p) "tests/") (string-contains? (car p) "/tests/")))
  (define tests (filter test? kept))
  (define sources (filter (lambda (p) (not (test? p))) kept))
  (define (lines p) (port->lines (open-input-string (cdr p))))
  (list (cons "Source modules" (length sources))
        (cons "Test files" (length tests))
        (cons "Source lines" (for/sum ([p (in-list sources)]) (length (lines p))))
        (cons "Test lines" (for/sum ([p (in-list tests)]) (length (lines p))))
        (cons "Test assertions" (for*/sum ([p (in-list tests)] [l (in-list (lines p))])
                                  (if (regexp-match? #rx"\\(check-" l) 1 0)))))
(define (generated-readme-correction before inputs)
  (define metrics (generated-metrics inputs))
  ;; Exactly one recognized row per metric, inside the actual lint section.
  ;; Preserve separators, newlines, modes, and all prose outside those cells.
  (define section
    (let loop ([ls (string-split before "\n" #:trim? #f)] [in? #f] [out '()])
      (cond [(null? ls) (reverse out)]
            [(regexp-match? #rx"^## Test Suite" (car ls)) (loop (cdr ls) #t out)]
            [(and in? (regexp-match? #rx"^##" (car ls))) (loop (cdr ls) #f out)]
            [else (loop (cdr ls) in? (if in? (cons (car ls) out) out))])))
  (for/fold ([text before]) ([metric (in-list metrics)])
    (define name (car metric))
    (define rx (regexp (string-append "^\\| " name " \\| ([0-9,]+) \\|$")))
    (define rows
      (filter (lambda (l)
                (define m (regexp-match #rx"^\\| *([^|]+?) *\\| *([^|]+?) *\\|$" l))
                (and m (equal? (string-trim (cadr m)) name))) section))
    (unless (and (= (length rows) 1) (regexp-match? rx (car rows))
                 (= (length (regexp-match* (regexp (string-append "\\| " name " \\| [0-9,]+ \\|")) before)) 1))
      (error 'generated-readme "ambiguous/missing generated row: ~a" name))
    (define row (car rows))
    (define old (string-replace (cadr (regexp-match rx row)) "," ""))
    (cond [(member name '("Test lines" "Test assertions"))
           (string-replace text row (format "| ~a | ~a |" name (cdr metric)))]
          [else
           (unless (equal? old (number->string (cdr metric)))
             (error 'generated-readme "non-correction metric drift: ~a" name))
           text])))
