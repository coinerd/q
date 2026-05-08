#lang racket/base

;; extensions/racket-tooling/formatting.rkt — format, syntax, repair operations
;;
;; Extracted from racket-tooling-handlers.rkt.
;; Edit modes: replace, form, skeleton, struct-add-field, provide-append,
;; cond-insert-clause, match-insert-clause, rewrite-form.

(require racket/string
         (only-in "../../util/errors.rkt" raise-extension-error)
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
                  find-form-end
                  pattern-matches?
                  apply-template)
         (only-in "../tool-api.rkt" make-success-result make-error-result))

(provide handle-racket-edit)

;; Validate string before read - reject #reader or #lang injections
(define (safe-read-string s context)
  (when (or (regexp-match? #rx"#reader" s) (regexp-match? #rx"#lang" s))
    (raise-extension-error (format "~a contains forbidden #reader or #lang directive" context) 'racket-tooling 'edit))
  (read (open-input-string s)))

(define (handle-racket-edit args [exec-ctx #f])
  (define path (hash-ref args 'file (hash-ref args 'path "")))
  (define mode (hash-ref args 'mode "replace"))
  (when (string=? path "")
    (raise-extension-error "file is required" 'racket-tooling 'edit))
  (unless (or (string=? mode "skeleton") (file-exists? path))
    (raise-extension-error (format "File not found: ~a" path) 'racket-tooling 'edit))
  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "racket-edit error: ~a" (exn-message e))))])
    (cond
      [(string=? mode "replace") (handle-edit-replace path args)]
      [(string=? mode "form") (handle-edit-form path args)]
      [(string=? mode "skeleton") (handle-edit-skeleton path args)]
      [(string=? mode "struct-add-field") (handle-edit-struct-add-field path args)]
      [(string=? mode "provide-append") (handle-edit-provide-append path args)]
      [(string=? mode "cond-insert-clause") (handle-edit-cond-insert path args)]
      [(string=? mode "match-insert-clause") (handle-edit-match-insert path args)]
      [(string=? mode "rewrite-form") (handle-edit-rewrite-form path args)]
      [else (make-error-result (format "Unknown mode: ~a" mode))])))

(define (string-index-of haystack needle)
  (define m (regexp-match-positions (regexp-quote needle) haystack))
  (and m (caar m)))

(define (handle-edit-replace path args)
  (define old-text (hash-ref args 'oldText ""))
  (define new-text (hash-ref args 'newText ""))
  (when (string=? old-text "")
    (error 'racket-edit "oldText is required for replace mode"))
  (define content (read-file-string path))
  (define idx (string-index-of content old-text))
  (unless idx
    (error 'racket-edit "oldText not found in file"))
  (define rest (substring content (+ idx (string-length old-text))))
  (when (string-index-of rest old-text)
    (error 'racket-edit
           (string-append "oldText is not unique in file. "
                          "Use racket-codemod for structural pattern matching, "
                          "or narrow the match context.")))
  (define before (substring content 0 idx))
  (define after (substring content (+ idx (string-length old-text))))
  (define new-content (string-append before new-text after))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _fmt-out fmt-err) (raco-fmt path))
  (define-values (make-ec _make-out make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Replaced in ~a. Format+compile passed." path))))]
    [else
     (restore-backup! bak path)
     (make-error-result
      (format "Validation failed after edit. Reverted.\nfmt: ~a\ncompile: ~a" fmt-err make-err))]))

(define (handle-edit-form path args)
  (define pattern (hash-ref args 'pattern ""))
  (define template (hash-ref args 'template ""))
  (when (string=? pattern "")
    (error 'racket-edit "pattern is required for form mode"))
  (define content (read-file-string path))
  (define forms (read-all-forms content))
  (define pattern-form (safe-read-string pattern "pattern"))
  (define match-idx
    (for/first ([i (in-range (length forms))]
                #:when (equal? (list-ref forms i) pattern-form))
      i))
  (unless match-idx
    (error 'racket-edit "Pattern form not found in file"))
  (when (string=? template "")
    (error 'racket-edit "template is required for form mode"))
  (define template-form (safe-read-string template "template"))
  (define new-forms (list-set forms match-idx template-form))
  (define new-content
    (string-join (map (lambda (f) (string-append (form->string f) "\n\n")) new-forms) ""))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _fmt-out fmt-err) (raco-fmt path))
  (define-values (make-ec _make-out make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Form replaced in ~a. Format+compile passed." path))))]
    [else
     (restore-backup! bak path)
     (make-error-result (format "Validation failed after form edit. Reverted.\nfmt: ~a\ncompile: ~a"
                                fmt-err
                                make-err))]))

(define (handle-edit-skeleton path args)
  (define lang (hash-ref args 'lang "racket"))
  (define requires (hash-ref args 'requires ""))
  (define provides (hash-ref args 'provides ""))
  (define signatures (hash-ref args 'signatures ""))
  (define req-list (filter (lambda (s) (not (string=? s ""))) (string-split requires ",")))
  (define prov-list (filter (lambda (s) (not (string=? s ""))) (string-split provides ",")))
  (define sig-lines (string-split signatures "\n"))
  (define lines
    (append (list (format "#lang ~a" lang) "")
            (if (null? req-list)
                '()
                (append (list "(require")
                        (map (lambda (r) (format "  ~a" (string-trim r))) req-list)
                        (list ")")))
            (if (null? prov-list)
                '()
                (append (list "(provide")
                        (map (lambda (p) (format "  ~a" (string-trim p))) prov-list)
                        (list ")")))
            '("")
            sig-lines))
  (define content (string-join lines "\n"))
  (write-file-string! path content)
  (define-values (fmt-ec _1 _2) (raco-fmt path))
  (define-values (make-ec _3 _4) (raco-make path))
  (make-success-result (list (hasheq 'type
                                     "text"
                                     'text
                                     (format "Skeleton created: ~a (fmt: ~a, compile: ~a)"
                                             path
                                             (if (zero? fmt-ec) "pass" "fail")
                                             (if (zero? make-ec) "pass" "fail"))))))

(define (handle-edit-struct-add-field path args)
  (define struct-name (hash-ref args 'structName ""))
  (define field-name (hash-ref args 'fieldName ""))
  (when (or (string=? struct-name "") (string=? field-name ""))
    (error 'racket-edit "structName and fieldName required"))
  (define content (read-file-string path))
  (define pattern (format "(define ~a" struct-name))
  (define lines (string-split content "\n" #:trim? #f))
  (define struct-line-idx
    (for/first ([i (in-range (length lines))]
                #:when (string-contains? (list-ref lines i) pattern))
      i))
  (unless struct-line-idx
    (error 'racket-edit (format "Struct ~a not found" struct-name)))
  (define default-expr (hash-ref args 'defaultExpr #f))
  (define field-text
    (if default-expr
        (format "  [~a ~a]" field-name default-expr)
        (format "  ~a" field-name)))
  (define-values (end-idx _) (find-form-end lines struct-line-idx))
  (define new-lines (append (take lines end-idx) (list field-text) (drop lines end-idx)))
  (define new-content (string-join new-lines "\n"))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _ign1a _fmt-err) (raco-fmt path))
  (define-values (make-ec _ign2a make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq 'type
                    "text"
                    'text
                    (format "Added field ~a to ~a in ~a" field-name struct-name path))))]
    [else
     (restore-backup! bak path)
     (make-error-result (format "Validation failed after struct edit. Reverted.\n~a" make-err))]))

(define (handle-edit-provide-append path args)
  (define ids-str (hash-ref args 'ids ""))
  (when (string=? ids-str "")
    (error 'racket-edit "ids is required for provide-append"))
  (define ids (map string-trim (string-split ids-str ",")))
  (define content (read-file-string path))
  (define lines (string-split content "\n" #:trim? #f))
  (define prov-line-idx
    (for/first ([i (in-range (length lines))]
                #:when (regexp-match? #rx"^\\(provide" (string-trim (list-ref lines i))))
      i))
  (unless prov-line-idx
    (error 'racket-edit "No (provide ...) form found"))
  (define-values (end-idx _) (find-form-end lines prov-line-idx))
  (define id-lines (map (lambda (id) (format "         ~a" id)) ids))
  (define new-lines (append (take lines end-idx) id-lines (drop lines end-idx)))
  (define new-content (string-join new-lines "\n"))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _ign3a _ign3b) (raco-fmt path))
  (define-values (make-ec _ign4a make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Appended to provide in ~a: ~a" path ids-str))))]
    [else
     (restore-backup! bak path)
     (make-error-result (format "Validation failed after provide-append. Reverted.\n~a" make-err))]))

(define (handle-edit-cond-insert path args)
  (define clause (hash-ref args 'clause ""))
  (define anchor (hash-ref args 'anchorClause ""))
  (define position (hash-ref args 'insertPosition "before"))
  (when (or (string=? clause "") (string=? anchor ""))
    (error 'racket-edit "clause and anchorClause required for cond-insert-clause"))
  (simple-form-insert path clause anchor position "cond"))

(define (handle-edit-match-insert path args)
  (define clause (hash-ref args 'clause ""))
  (define anchor (hash-ref args 'anchorClause ""))
  (define position (hash-ref args 'insertPosition "before"))
  (when (or (string=? clause "") (string=? anchor ""))
    (error 'racket-edit "clause and anchorClause required for match-insert-clause"))
  (simple-form-insert path clause anchor position "match"))

(define (simple-form-insert path clause anchor position form-type)
  (define content (read-file-string path))
  (define lines (string-split content "\n" #:trim? #f))
  (define anchor-idx
    (for/first ([i (in-range (length lines))]
                #:when (string-contains? (list-ref lines i) anchor))
      i))
  (unless anchor-idx
    (error 'racket-edit (format "Anchor clause not found: ~a" anchor)))
  (define insert-idx
    (if (string=? position "after")
        (+ anchor-idx 1)
        anchor-idx))
  (define clause-lines (string-split clause "\n"))
  (define new-lines (append (take lines insert-idx) clause-lines (drop lines insert-idx)))
  (define new-content (string-join new-lines "\n"))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _ign5a _ign5b) (raco-fmt path))
  (define-values (make-ec _ign6a make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq 'type "text" 'text (format "Inserted clause in ~a. Validation passed." path))))]
    [else
     (restore-backup! bak path)
     (make-error-result (format "Validation failed after clause insert. Reverted.\n~a" make-err))]))

(define (handle-edit-rewrite-form path args)
  (define start-line (hash-ref args 'startLine #f))
  (define end-line (hash-ref args 'endLine #f))
  (define new-content-str (hash-ref args 'content ""))
  (unless (and start-line end-line)
    (error 'racket-edit "startLine and endLine required for rewrite-form"))
  (define content (read-file-string path))
  (define lines (string-split content "\n" #:trim? #f))
  (define new-lines (string-split new-content-str "\n"))
  (define new-all-lines (append (take lines (sub1 start-line)) new-lines (drop lines end-line)))
  (define new-content (string-join new-all-lines "\n"))
  (define bak (backup-file path))
  (write-file-string! path new-content)
  (define-values (fmt-ec _ign7a _ign7b) (raco-fmt path))
  (define-values (make-ec _ign8a make-err) (raco-make path))
  (cond
    [(and (zero? fmt-ec) (zero? make-ec))
     (delete-file bak)
     (make-success-result
      (list (hasheq
             'type
             "text"
             'text
             (format "Rewrote lines ~a-~a in ~a. Validation passed." start-line end-line path))))]
    [else
     (restore-backup! bak path)
     (make-error-result (format "Validation failed after rewrite. Reverted.\n~a" make-err))]))
