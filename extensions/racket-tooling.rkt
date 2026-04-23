#lang racket/base

;; extensions/racket-tooling.rkt — Racket Editing Extension
;;
;; Wave B1: Registers racket-edit, racket-codemod, racket-check tools
;; for structural s-expression editing with validation.
;; Also registers /fmt, /check, /expand slash commands.

(require racket/port
         racket/string
         racket/file
         racket/list
         racket/match
         racket/system
         json
         "define-extension.rkt"
         "dynamic-tools.rkt"
         "ext-commands.rkt"
         "context.rkt"
         "hooks.rkt"
         "../tools/tool.rkt")

(provide racket-tooling-extension
         handle-racket-check
         handle-racket-edit
         handle-racket-codemod
         register-racket-tools
         register-racket-commands
         raco-fmt
         raco-make
         raco-test
         run-raco)

;; ============================================================
;; Raco command helpers
;; ============================================================

;; Run a raco command, return (values exit-code stdout stderr)
(define (run-raco args [cwd #f])
  (define raco-bin (find-raco))
  (unless raco-bin
    (error 'run-raco "raco not found on PATH"))
  (run-command raco-bin args cwd))

(define (find-raco)
  (let loop ([dirs (string-split (or (getenv "PATH") "") ":")])
    (cond
      [(null? dirs) #f]
      [else
       (define candidate (build-path (car dirs) "raco"))
       (if (file-exists? candidate)
           candidate
           (loop (cdr dirs)))])))

;; Run an arbitrary command, return (values exit-code stdout stderr)
(define (run-command cmd args [cwd #f])
  (define cmd-str
    (if (string? cmd)
        cmd
        (path->string cmd)))
  (define arg-str
    (string-join (map (lambda (a)
                        (if (string? a)
                            a
                            (format "~a" a)))
                      args)
                 " "))
  (define full-cmd (string-append cmd-str " " arg-str))
  (define stdout (open-output-string))
  (define stderr (open-output-string))
  (define exit-code
    (parameterize ([current-output-port stdout]
                   [current-error-port stderr]
                   [current-directory (or cwd (current-directory))])
      (system/exit-code full-cmd)))
  (values exit-code (get-output-string stdout) (get-output-string stderr)))

;; Specific raco commands
(define (raco-fmt path)
  (run-raco `("fmt" "-i" ,path)))

(define (raco-make path)
  (run-raco `("make" ,path)))

(define (raco-test path)
  (run-raco `("test" ,path)))

(define (raco-expand path)
  (run-raco `("expand" ,path)))

;; ============================================================
;; File I/O helpers
;; ============================================================

(define (read-file-string path)
  (call-with-input-file path port->string))

(define (write-file-string! path content)
  (call-with-output-file path (lambda (out) (display content out)) #:exists 'replace))

(define (backup-file path)
  (define bak (string-append path ".bak"))
  (copy-file path bak #t)
  bak)

(define (restore-backup! bak-path orig-path)
  (when (file-exists? bak-path)
    (copy-file bak-path orig-path #t)
    (delete-file bak-path)))

;; ============================================================
;; S-expression helpers
;; ============================================================

;; Read all top-level forms from a string.
;; Strips #lang line if present (read can't handle #lang inside a module).
(define (read-all-forms str)
  (define lines (string-split str "\n" #:trim? #f))
  (define stripped
    (string-join (filter (lambda (l) (not (regexp-match? #rx"^#lang" (string-trim l)))) lines) "\n"))
  (define inp (open-input-string stripped))
  (let loop ([forms '()])
    (define form (read inp))
    (if (eof-object? form)
        (reverse forms)
        (loop (cons form forms)))))

;; Write a form to string
(define (form->string form)
  (define out (open-output-string))
  (write form out)
  (get-output-string out))

;; Find all top-level form boundaries (start-line, end-line) in file content
(define (find-form-boundaries content)
  (define lines (string-split content "\n" #:trim? #f))
  (define inp (open-input-string content))
  (port-count-lines! inp)
  (let loop ([boundaries '()])
    (define start-pos (port-next-location inp))
    (define form (read inp))
    (cond
      [(eof-object? form) (reverse boundaries)]
      [else
       (define end-pos (port-next-location inp))
       ;; start-pos and end-pos are (line column position) or #f
       (define start-line (and start-pos (car start-pos)))
       (define end-line (and end-pos (car end-pos)))
       (loop (cons (cons (or start-line 1) (or end-line start-line)) boundaries))])))

;; ============================================================
;; Handler: racket-check
;; ============================================================

(define (handle-racket-check args [exec-ctx #f])
  (define path (hash-ref args 'path ""))
  (define mode (hash-ref args 'mode "syntax"))
  (when (string=? path "")
    (error 'racket-check "path is required"))

  (unless (file-exists? path)
    (error 'racket-check (format "File not found: ~a" path)))

  (define results '())

  ;; Format check
  (when (member mode '("format" "all"))
    (define-values (ec out err) (raco-fmt path))
    (set!
     results
     (cons (hasheq 'step "format" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
           results)))

  ;; Syntax/compile check
  (when (member mode '("syntax" "all"))
    (define-values (ec out err) (raco-make path))
    (set!
     results
     (cons (hasheq 'step "compile" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
           results)))

  ;; Test
  (when (member mode '("test" "all"))
    (define-values (ec out err) (raco-test path))
    (set! results
          (cons (hasheq 'step "test" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
                results)))

  ;; Expand
  (when (member mode '("expand" "all"))
    (define-values (ec out err) (raco-expand path))
    (set!
     results
     (cons (hasheq 'step "expand" 'pass? (zero? ec) 'output (string-trim (string-append out err)))
           results)))

  ;; If no mode matched, default to syntax
  (when (null? results)
    (define-values (ec out err) (raco-make path))
    (set!
     results
     (list (hasheq 'step "compile" 'pass? (zero? ec) 'output (string-trim (string-append out err))))))

  (define all-pass? (andmap (lambda (r) (hash-ref r 'pass? #f)) results))
  (make-success-result
   (list (hasheq 'type
                 "text"
                 'text
                 (jsexpr->string
                  (hasheq 'file path 'all-pass? all-pass? 'results (reverse results)))))))

;; ============================================================
;; Handler: racket-edit
;; ============================================================

(define (handle-racket-edit args [exec-ctx #f])
  (define path (hash-ref args 'file (hash-ref args 'path "")))
  (define mode (hash-ref args 'mode "replace"))

  (when (string=? path "")
    (error 'racket-edit "file is required"))

  ;; skeleton mode creates new files; other modes require existing files
  (unless (or (string=? mode "skeleton") (file-exists? path))
    (error 'racket-edit (format "File not found: ~a" path)))

  (with-handlers ([exn:fail? (lambda (e)
                               (make-error-result (format "racket-edit error: ~a" (exn-message e))))])
    (cond
      ;; replace mode: exact text replacement with validation
      [(string=? mode "replace") (handle-edit-replace path args)]

      ;; form mode: structural form editing
      [(string=? mode "form") (handle-edit-form path args)]

      ;; skeleton mode: generate new file from signature
      [(string=? mode "skeleton") (handle-edit-skeleton path args)]

      ;; struct-add-field mode
      [(string=? mode "struct-add-field") (handle-edit-struct-add-field path args)]

      ;; provide-append mode
      [(string=? mode "provide-append") (handle-edit-provide-append path args)]

      ;; cond-insert-clause mode
      [(string=? mode "cond-insert-clause") (handle-edit-cond-insert path args)]

      ;; match-insert-clause mode
      [(string=? mode "match-insert-clause") (handle-edit-match-insert path args)]

      ;; rewrite-form mode: replace lines by range
      [(string=? mode "rewrite-form") (handle-edit-rewrite-form path args)]

      [else (make-error-result (format "Unknown mode: ~a" mode))])))

;; Find first index of needle in haystack, or #f
(define (string-index-of haystack needle)
  (define m (regexp-match-positions (regexp-quote needle) haystack))
  (and m (caar m)))

;; Replace mode: exact old→new text replacement with fmt+compile validation
(define (handle-edit-replace path args)
  (define old-text (hash-ref args 'oldText ""))
  (define new-text (hash-ref args 'newText ""))
  (when (string=? old-text "")
    (error 'racket-edit "oldText is required for replace mode"))

  (define content (read-file-string path))

  ;; Check oldText exists and is unique
  (define idx (string-index-of content old-text))
  (unless idx
    (error 'racket-edit "oldText not found in file"))

  ;; Check uniqueness
  (define rest (substring content (+ idx (string-length old-text))))
  (when (string-index-of rest old-text)
    (error 'racket-edit
           (string-append "oldText is not unique in file. "
                          "Use racket-codemod for structural pattern matching, "
                          "or narrow the match context.")))

  ;; Apply replacement
  (define before (substring content 0 idx))
  (define after (substring content (+ idx (string-length old-text))))
  (define new-content (string-append before new-text after))

  ;; Backup, write, validate
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
     ;; Revert
     (restore-backup! bak path)
     (make-error-result
      (format "Validation failed after edit. Reverted.\nfmt: ~a\ncompile: ~a" fmt-err make-err))]))

;; Form mode: insert/remove sibling forms
(define (handle-edit-form path args)
  (define pattern (hash-ref args 'pattern ""))
  (define template (hash-ref args 'template ""))
  (when (string=? pattern "")
    (error 'racket-edit "pattern is required for form mode"))

  (define content (read-file-string path))
  (define forms (read-all-forms content))
  (define pattern-form (read (open-input-string pattern)))

  ;; Find matching form
  (define match-idx
    (for/first ([i (in-range (length forms))]
                #:when (equal? (list-ref forms i) pattern-form))
      i))

  (unless match-idx
    (error 'racket-edit "Pattern form not found in file"))

  ;; If template provided, replace; otherwise error
  (when (string=? template "")
    (error 'racket-edit "template is required for form mode"))

  (define template-form (read (open-input-string template)))
  (define new-forms (list-set forms match-idx template-form))

  ;; Reconstruct file
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

;; Skeleton mode: generate new file
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

;; struct-add-field mode
(define (handle-edit-struct-add-field path args)
  (define struct-name (hash-ref args 'structName ""))
  (define field-name (hash-ref args 'fieldName ""))
  (when (or (string=? struct-name "") (string=? field-name ""))
    (error 'racket-edit "structName and fieldName required"))

  (define content (read-file-string path))
  (define pattern (format "(define ~a" struct-name))

  ;; Find struct definition and add field
  (define lines (string-split content "\n" #:trim? #f))
  (define struct-line-idx
    (for/first ([i (in-range (length lines))]
                #:when (string-contains? (list-ref lines i) pattern))
      i))

  (unless struct-line-idx
    (error 'racket-edit (format "Struct ~a not found" struct-name)))

  ;; Simple approach: find closing paren of struct def and add field before it
  (define default-expr (hash-ref args 'defaultExpr #f))
  (define field-text
    (if default-expr
        (format "  [~a ~a]" field-name default-expr)
        (format "  ~a" field-name)))

  ;; Find the struct's closing paren by tracking paren depth
  (define-values (end-idx _) (find-form-end lines struct-line-idx))

  ;; Insert field before closing paren
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

;; provide-append mode
(define (handle-edit-provide-append path args)
  (define ids-str (hash-ref args 'ids ""))
  (when (string=? ids-str "")
    (error 'racket-edit "ids is required for provide-append"))

  (define ids (map string-trim (string-split ids-str ",")))
  (define content (read-file-string path))
  (define lines (string-split content "\n" #:trim? #f))

  ;; Find (provide ...) form
  (define prov-line-idx
    (for/first ([i (in-range (length lines))]
                #:when (regexp-match? #rx"^\\(provide" (string-trim (list-ref lines i))))
      i))

  (unless prov-line-idx
    (error 'racket-edit "No (provide ...) form found"))

  ;; Find closing paren of provide form
  (define-values (end-idx _) (find-form-end lines prov-line-idx))

  ;; Insert IDs before closing paren
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

;; cond-insert-clause mode
(define (handle-edit-cond-insert path args)
  (define clause (hash-ref args 'clause ""))
  (define anchor (hash-ref args 'anchorClause ""))
  (define position (hash-ref args 'insertPosition "before"))

  (when (or (string=? clause "") (string=? anchor ""))
    (error 'racket-edit "clause and anchorClause required for cond-insert-clause"))

  (simple-form-insert path clause anchor position "cond"))

;; match-insert-clause mode
(define (handle-edit-match-insert path args)
  (define clause (hash-ref args 'clause ""))
  (define anchor (hash-ref args 'anchorClause ""))
  (define position (hash-ref args 'insertPosition "before"))

  (when (or (string=? clause "") (string=? anchor ""))
    (error 'racket-edit "clause and anchorClause required for match-insert-clause"))

  (simple-form-insert path clause anchor position "match"))

;; Generic form insertion helper (cond/match clauses)
(define (simple-form-insert path clause anchor position form-type)
  (define content (read-file-string path))
  (define lines (string-split content "\n" #:trim? #f))

  ;; Find anchor line
  (define anchor-idx
    (for/first ([i (in-range (length lines))]
                #:when (string-contains? (list-ref lines i) anchor))
      i))

  (unless anchor-idx
    (error 'racket-edit (format "Anchor clause not found: ~a" anchor)))

  ;; Insert before or after anchor
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

;; rewrite-form mode: replace lines by startLine/endLine range
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

;; ============================================================
;; Handler: racket-codemod
;; ============================================================

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
  (define pat-form (read (open-input-string pattern)))
  (define tmpl-form (read (open-input-string template)))

  ;; Find forms matching pattern (structural equality)
  (define matches
    (for/list ([f (in-list forms)]
               [i (in-naturals)]
               #:when (pattern-matches? pat-form f))
      (cons i f)))

  (cond
    [(null? matches)
     (make-success-result (list (hasheq 'type "text" 'text "No matches found for pattern.")))]
    [(not write?)
     ;; Dry run: show what would change
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
     ;; Apply: replace matching forms
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

     (define-values (fmt-ec _ign9a _ign9b) (raco-fmt path))
     (define-values (make-ec _ign10a make-err) (raco-make path))

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

;; Pattern matching: check if a pattern form matches a source form.
;; Placeholder atoms starting with @@ match anything.
(define (pattern-matches? pattern source)
  (cond
    [(and (symbol? pattern)
          (let ([s (symbol->string pattern)]) (and (> (string-length s) 2) (string-prefix? s "@@"))))
     #t]
    [(and (symbol? pattern) (symbol? source)) (eq? pattern source)]
    [(and (pair? pattern) (pair? source))
     (and (= (length pattern) (length source)) (andmap pattern-matches? pattern source))]
    [(and (null? pattern) (null? source)) #t]
    [(and (string? pattern) (string? source)) (string=? pattern source)]
    [(and (number? pattern) (number? source)) (= pattern source)]
    [else #f]))

;; Apply template: replace placeholder atoms in template with matched values.
(define (apply-template pattern template source)
  (define bindings (collect-bindings pattern source))
  (subst-bindings bindings template))

(define (collect-bindings pattern source)
  (cond
    [(and (symbol? pattern)
          (let ([s (symbol->string pattern)]) (and (> (string-length s) 2) (string-prefix? s "@@"))))
     (list (cons pattern source))]
    [(and (list? pattern) (list? source)) (apply append (map collect-bindings pattern source))]
    [else '()]))

(define (subst-bindings bindings form)
  (cond
    [(and (symbol? form) (assoc form bindings))
     =>
     cdr]
    [(list? form) (map (lambda (f) (subst-bindings bindings f)) form)]
    [else form]))

;; ============================================================
;; Helper: find form end by paren tracking
;; ============================================================

(define (find-form-end lines start-idx)
  (define depth 0)
  (let loop ([i start-idx]
             [col 0]
             [in-string #f])
    (cond
      [(>= i (length lines)) (values i depth)]
      [else
       (define line (list-ref lines i))
       (define chars (string->list line))
       (let char-loop ([cs (if (= i start-idx)
                               (drop chars col)
                               chars)]
                       [d depth]
                       [in-str in-string])
         (cond
           [(null? cs)
            (if (zero? d)
                (values (+ i 1) d)
                (loop (+ i 1) 0 in-str))]
           [else
            (define c (car cs))
            (define new-d
              (cond
                [in-str d]
                [(char=? c #\() (+ d 1)]
                [(char=? c #\)) (- d 1)]
                [else d]))
            (if (and (= i start-idx) (= new-d 0) (char=? c #\)))
                (values (+ i 1) 0)
                (char-loop (cdr cs)
                           new-d
                           (if (and (not in-str) (char=? c #\"))
                               #t
                               (if (and in-str (char=? c #\")) #f in-str))))]))])))

;; ============================================================
;; Extension definition
;; ============================================================

(define (register-racket-tools ctx)
  (ext-register-tool!
   ctx
   (make-tool
    "racket-check"
    (string-append "Run Racket validation on a file. "
                   "Modes: format (raco fmt), syntax/compile (raco make), "
                   "test (raco test), expand (raco expand), all. "
                   "Default: syntax.")
    (hasheq
     'type
     "object"
     'required
     '("path")
     'properties
     (hasheq 'path
             (hasheq 'type "string" 'description "Path to .rkt file")
             'mode
             (hasheq 'type "string" 'description "Check mode: format, syntax, test, expand, all")))
    handle-racket-check)
   (make-tool
    "racket-edit"
    (string-append "Structural s-expression editing for Racket files. "
                   "Modes: replace (exact text), form (pattern/template), "
                   "skeleton (new file), struct-add-field, provide-append, "
                   "cond-insert-clause, match-insert-clause, rewrite-form. "
                   "Validates with raco fmt + raco make after edit, reverts on failure.")
    (hasheq 'type
            "object"
            'required
            '("file")
            'properties
            (hasheq 'file
                    (hasheq 'type "string" 'description "Path to .rkt file")
                    'mode
                    (hasheq 'type "string" 'description "Edit mode")
                    'oldText
                    (hasheq 'type "string" 'description "Text to find (replace mode)")
                    'newText
                    (hasheq 'type "string" 'description "Replacement text")
                    'pattern
                    (hasheq 'type "string" 'description "S-expression pattern (form mode)")
                    'template
                    (hasheq 'type "string" 'description "S-expression template (form mode)")
                    'startLine
                    (hasheq 'type "integer" 'description "Start line (rewrite-form)")
                    'endLine
                    (hasheq 'type "integer" 'description "End line (rewrite-form)")
                    'content
                    (hasheq 'type "string" 'description "New content (rewrite-form, skeleton)")
                    'structName
                    (hasheq 'type "string" 'description "Struct name (struct-add-field)")
                    'fieldName
                    (hasheq 'type "string" 'description "Field name (struct-add-field)")
                    'ids
                    (hasheq 'type "string" 'description "Comma-separated IDs (provide-append)")
                    'clause
                    (hasheq 'type "string" 'description "Clause to insert (cond/match-insert)")
                    'anchorClause
                    (hasheq 'type "string" 'description "Anchor clause text")
                    'insertPosition
                    (hasheq 'type "string" 'description "before or after")
                    'lang
                    (hasheq 'type "string" 'description "Language for skeleton")
                    'requires
                    (hasheq 'type "string" 'description "Comma-separated requires (skeleton)")
                    'provides
                    (hasheq 'type "string" 'description "Comma-separated provides (skeleton)")
                    'signatures
                    (hasheq 'type "string" 'description "Newline-separated signatures (skeleton)")))
    handle-racket-edit)
   (make-tool "racket-codemod"
              (string-append "Pattern/template codemod for Racket files. "
                             "Uses bar-quoted @@PLACEHOLDER atoms to match any subtree. "
                             "Dry run by default; set write=true to apply. "
                             "Validates with raco fmt + raco make, reverts on failure.")
              (hasheq 'type
                      "object"
                      'required
                      '("file" "pattern" "template")
                      'properties
                      (hasheq 'file
                              (hasheq 'type "string" 'description "Path to .rkt file")
                              'pattern
                              (hasheq 'type "string" 'description "Pattern with @@PLACEHOLDER atoms")
                              'template
                              (hasheq 'type "string" 'description "Replacement template")
                              'write
                              (hasheq 'type "boolean" 'description "Apply changes (default: false)")))
              handle-racket-codemod))
  (hook-pass ctx))

(define (register-racket-commands ctx)
  (ext-register-command! ctx "/fmt" "Format a Racket file" 'general '() '("f"))
  (ext-register-command! ctx "/check" "Compile-check a Racket file" 'general '() '("c"))
  (ext-register-command! ctx "/expand" "Expand a Racket file" 'general '() '("e"))
  (hook-pass ctx))

(define-q-extension racket-tooling-extension
                    #:version "1.0.0"
                    #:api-version "1"
                    #:on register-tools
                    register-racket-tools
                    #:on register-shortcuts
                    register-racket-commands)
