#lang racket/base

;; extensions/racket-tooling/rewrite.rkt — codemod/rewrite operations
;;
;; Extracted from racket-tooling-handlers.rkt.
;; handle-racket-codemod: pattern/template-based structural rewrites.

(require racket/string
         racket/list
         (only-in "../racket-tooling-helpers.rkt"
                  raco-fmt
                  raco-make
                  read-file-string
                  write-file-string!
                  backup-file
                  restore-backup!
                  read-all-forms
                  form->string
                  pattern-matches?
                  apply-template)
         (only-in "../tool-api.rkt" make-success-result make-error-result))

(provide handle-racket-codemod)

;; Validate string before read - reject #reader or #lang injections
(define (safe-read-string s context)
  (when (or (regexp-match? #rx"#reader" s) (regexp-match? #rx"#lang" s))
    (error 'racket-codemod (format "~a contains forbidden #reader or #lang directive" context)))
  (read (open-input-string s)))

(define (handle-racket-codemod args [exec-ctx #f])
  (define path (hash-ref args 'file ""))
  (define pattern (hash-ref args 'pattern ""))
  (define template (hash-ref args 'template ""))
  (define write? (hash-ref args 'write #f))

  (when (string=? path "")
    (error 'racket-codemod "file is required"))
  (when (string=? pattern "")
    (error 'racket-codemod "pattern is required"))
  (when (string=? template "")
    (error 'racket-codemod "template is required"))
  (unless (file-exists? path)
    (error 'racket-codemod (format "File not found: ~a" path)))

  (define content (read-file-string path))
  (define lang-line
    (let ([lines (string-split content "\n" #:trim? #f)])
      (if (and (pair? lines) (regexp-match? #rx"^#lang" (car lines)))
          (string-append (car lines) "\n\n")
          "")))
  (define forms (read-all-forms content))
  (define pat-form (safe-read-string pattern "pattern"))
  (define tmpl-form (safe-read-string template "template"))

  (define matches
    (for/list ([f (in-list forms)]
               [i (in-naturals)]
               #:when (pattern-matches? pat-form f))
      (cons i f)))

  (cond
    [(null? matches)
     (make-success-result (list (hasheq 'type "text" 'text "No matches found for pattern.")))]
    [(not write?)
     (define preview-text
       (string-join (for/list ([m (in-list matches)])
                      (format "  Form ~a: ~a => ~a"
                              (car m)
                              (form->string (cdr m))
                              (form->string (apply-template pat-form tmpl-form (cdr m)))))
                    "\n"))
     (make-success-result
      (list (hasheq 'type
                    "text"
                    'text
                    (format "Dry run: ~a matches.\n~a" (length matches) preview-text))))]
    [else
     (define new-forms
       (for/list ([f (in-list forms)]
                  [i (in-naturals)])
         (define match (assoc i matches))
         (if match
             (apply-template pat-form tmpl-form (cdr match))
             f)))
     (define new-content
       (string-append
        lang-line
        (string-join (map (lambda (f) (string-append (form->string f) "\n\n")) new-forms) "")))
     (define bak (backup-file path))
     (write-file-string! path new-content)
     (define-values (fmt-ec _ign1 _ign2) (raco-fmt path))
     (define-values (make-ec _ign3 make-err) (raco-make path))
     (cond
       [(and (zero? fmt-ec) (zero? make-ec))
        (delete-file bak)
        (make-success-result
         (list (hasheq 'type
                       "text"
                       'text
                       (format "Codemod applied: ~a forms replaced in ~a" (length matches) path))))]
       [else
        (restore-backup! bak path)
        (make-error-result (format "Validation failed after codemod. Reverted.\n~a" make-err))])]))
