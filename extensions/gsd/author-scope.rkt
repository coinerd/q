#lang racket/base
;; Canonical literal author scope from the authenticated frozen declaration.
(require racket/file
         racket/list
         racket/string
         "plan-snapshot.rkt"
         (only-in "plan-types.rkt" parse-wave-content)
         (only-in "wave-docs.rkt" parse-plan-index wave-index-entry-idx wave-index-entry-ref-path))
(provide frozen-author-files
         literal-author-scope?)
(define (literal-author-scope? files)
  (and (list? files)
       (pair? files)
       (andmap (lambda (file)
                 (and (string? file)
                      (positive? (string-length file))
                      (equal? file (string-trim file))
                      (not (regexp-match? #px"[\\\\:*?\\[\\]{}\u0000\r\n]" file))
                      (for/and ([part (in-list (string-split file "/" #:trim? #f))])
                        (and (positive? (string-length part)) (not (member part '("." "..")))))))
               files)
       (= (length files) (length (remove-duplicates files)))
       #t))
(define (frozen-author-files base plan wave)
  (define (need value message)
    (unless value
      (error 'author-scope "~a" message))
    value)
  (define manifest (load-snapshot-manifest base plan))
  (need (and manifest (null? (snapshot-drift? base plan))) "missing or drifting frozen snapshot")
  (define root (snapshot-dir base plan))
  (define declared
    (for/first ([entry (in-list (parse-plan-index (file->string (build-path root "PLAN.md"))))]
                #:when (= wave (wave-index-entry-idx entry)))
      (wave-index-entry-ref-path entry)))
  (define entries
    (filter (lambda (entry)
              (if declared
                  (equal? declared (snapshot-file-path entry))
                  (regexp-match? (regexp (format "^waves/W~a-" wave)) (snapshot-file-path entry))))
            (plan-snapshot-manifest-files manifest)))
  (need (= (length entries) 1) "ambiguous frozen wave")
  (define files
    (hash-ref (parse-wave-content (file->string (build-path root (snapshot-file-path (car entries)))))
              'files
              '()))
  (need (literal-author-scope? files) "frozen FILES is not literal")
  (define relative
    (map (lambda (p)
           (if (string-prefix? p "q/")
               (substring p 2)
               p))
         files))
  (need (literal-author-scope? relative) "ambiguous repository-relative FILES")
  relative)
