#lang racket/base

;; q/scripts/ci/validate-workflows.rkt — testing-framework refactor §11.5.
;;
;; Syntax/structure validation for every GitHub declaration file, replacing
;; the former `pip install pyyaml` + `python3 -c "import yaml"` step. The
;; pinned Racket toolchain is the only interpreter CI may install for this
;; check, so the PR-critical gate carries no interpreter of its own.
;;
;; No YAML library is assumed, so this is a PRAGMATIC validator. What matters
;; is that it is FAIL CLOSED: the classes of malformation that produced
;; unreadable workflows (a duplicated top-level key after a bad merge, a tab
;; where the file needs spaces, an unterminated flow collection or quote, an
;; indentation level that matches no open block) are all rejected, a read
;; failure is a failure, and success is printed only after every file parsed
;; structurally. It never claims success after a read it could not complete.
;;
;; Scope: the mapping/sequence STRUCTURE of the file (indentation levels,
;; top-level keys, duplicate top-level keys) plus per-line quote and flow
;; balance. Block scalars (`run: |`, `description: >-`) are opaque programs,
;; not YAML structure, and are skipped exactly as a real parser would treat
;; them — otherwise every shell `if [ … ]` would look like an unclosed flow
;; sequence.
;;
;; Usage:
;;   racket scripts/ci/validate-workflows.rkt [--root <checkout>]
;;
;; Exit 0 and one `workflow-yaml-ok:` line when every file is structurally
;; sound; exit 1, one `ERROR: <file>: <problem>` line per finding, and no
;; success line otherwise.

(require racket/cmdline
         racket/file
         racket/list
         racket/path
         racket/string)

(provide validate-workflow-files
         workflow-declaration-files
         yaml-problems
         validate-text)

;; ---------------------------------------------------------------------------
;; Line helpers
;; ---------------------------------------------------------------------------

(define (leading-spaces s)
  (let loop ([i 0])
    (cond
      [(>= i (string-length s)) i]
      [(char=? (string-ref s i) #\space) (loop (add1 i))]
      [else i])))

(define tab-indent-rx #rx"^ *\t")

(define (sequence-item? trimmed)
  (regexp-match? #rx"^-( |$)" trimmed))

(define (strip-dash trimmed)
  (if (sequence-item? trimmed)
      (string-trim (substring trimmed 1))
      trimmed))

;; A `#` comment starts at a `#` that opens the line or follows whitespace,
;; and only outside a quoted scalar. Returns the prefix before the comment.
(define (strip-comment s)
  (let loop ([i 0] [quote-char #f])
    (cond
      [(>= i (string-length s)) s]
      [else
       (define ch (string-ref s i))
       (cond
         [quote-char
          (cond
            [(and (char=? quote-char #\") (char=? ch #\\)) (loop (+ i 2) quote-char)]
            [(char=? ch quote-char) (loop (add1 i) #f)]
            [else (loop (add1 i) quote-char)])]
         [(and (char=? ch #\#)
               (or (zero? i) (memv (string-ref s (sub1 i)) (list #\space #\tab))))
          (substring s 0 i)]
         [(or (char=? ch #\") (char=? ch #\')) (loop (add1 i) ch)]
         [else (loop (add1 i) #f)])])))

;; The mapping key of a `key: value` (or `- key: value`) line, or #f.
(define (key-of trimmed)
  (define m (regexp-match #rx"^(\"[^\"]*\"|'[^']*'|[^:#][^:]*):( |$)" (strip-dash trimmed)))
  (and m (cadr m)))

;; A block scalar header: `key: |`, `key: >-`, `- run: |2`, …
(define (block-scalar-header? trimmed)
  (define m (regexp-match #rx"^[^:]+: +(.*)$" (strip-dash trimmed)))
  (and m
       (regexp-match? #rx"^[|>]([1-9][-+]?|[-+][1-9]?)?$" (string-trim (strip-comment (cadr m))))))

;; ---------------------------------------------------------------------------
;; Per-line quote / flow-collection scan
;; ---------------------------------------------------------------------------

;; Returns (values flow-stack unterminated-quote-description-or-#f) where
;; flow-stack is the list of still-open `[`/`{` characters (innermost first,
;; carried across lines because flow collections may span lines).
(define (scan-flow-and-quotes line flow line-no problem!)
  (define len (string-length line))
  (let loop ([i 0] [quote-char #f] [flow flow])
    (cond
      [(>= i len) (values flow quote-char)]
      [else
       (define ch (string-ref line i))
       (cond
         [quote-char
          (cond
            [(and (char=? quote-char #\") (char=? ch #\\)) (loop (+ i 2) quote-char flow)]
            ;; `''` inside a single-quoted scalar is an escaped quote.
            [(and (char=? quote-char #\')
                  (char=? ch #\')
                  (< (add1 i) len)
                  (char=? (string-ref line (add1 i)) #\'))
             (loop (+ i 2) quote-char flow)]
            [(char=? ch quote-char) (loop (add1 i) #f flow)]
            [else (loop (add1 i) quote-char flow)])]
         [(and (char=? ch #\#)
               (or (zero? i) (memv (string-ref line (sub1 i)) (list #\space #\tab))))
          (values flow #f)]
         [(or (char=? ch #\") (char=? ch #\')) (loop (add1 i) ch flow)]
         [(or (char=? ch #\[) (char=? ch #\{)) (loop (add1 i) #f (cons ch flow))]
         [(or (char=? ch #\]) (char=? ch #\}))
          (define opener (if (char=? ch #\]) #\[ #\{))
          (cond
            [(null? flow)
             (problem! line-no (format "unmatched closing '~a' with no open flow collection" ch))
             (loop (add1 i) #f flow)]
            [(char=? (car flow) opener) (loop (add1 i) #f (cdr flow))]
            [else
             (problem! line-no
                       (format "unclosed flow collection '~a' closed by '~a'" (car flow) ch))
             (loop (add1 i) #f (cdr flow))])]
         [else (loop (add1 i) #f flow)])])))

(define (quote-name quote-char)
  (if (char=? quote-char #\") "double" "single"))

;; ---------------------------------------------------------------------------
;; The validator
;; ---------------------------------------------------------------------------

;; Structural problems of one YAML document, as a list of
;; "<line N: problem>" strings (possibly empty). Deterministic order.
(define (yaml-problems text)
  (define problems-rev '())
  (define (problem! line-no message)
    (set! problems-rev (cons (format "line ~a: ~a" line-no message) problems-rev)))
  ;; stack of open block indents, innermost first
  (define levels '())
  (define top-keys (make-hash))
  (define flow '())
  ;; indent of the key line that opened an opaque block scalar, or #f
  (define block-scalar-indent #f)
  (define lines (string-split text "\n"))

  (define (push-level! indent)
    (set! levels (cons indent levels)))

  ;; Keep the open-block stack consistent with this line's indentation.
  (define (track-indent! idx indent)
    (cond
      [(null? levels)
       (if (zero? indent)
           (push-level! 0)
           (begin
             (problem! idx
                       (format "top-level content must not be indented (found column ~a)"
                               (add1 indent)))
             (push-level! 0)))]
      [else
       (define top (car levels))
       (cond
         [(> indent top) (push-level! indent)]
         [(= indent top) (void)]
         [else
          (let pop! ()
            (when (and (pair? levels) (> (car levels) indent))
              (set! levels (cdr levels))
              (pop!)))
          (cond
            [(null? levels)
             (problem! idx
                       (format "inconsistent indentation: column ~a has no open parent block"
                               (add1 indent)))
             (push-level! indent)]
            [(= (car levels) indent) (void)]
            [else
             (problem! idx
                       (format "inconsistent indentation: found column ~a, expected column ~a"
                               (add1 indent)
                               (add1 (car levels))))
             (push-level! indent)])])]))

  (for ([line (in-list lines)]
        [idx (in-naturals 1)])
    (define indent (leading-spaces line))
    (define trimmed (string-trim line))
    (cond
      ;; inside a block scalar: everything more indented than its key is opaque
      [(and block-scalar-indent
            (or (string=? trimmed "") (> indent block-scalar-indent)))
       (void)]
      [else
       (when block-scalar-indent (set! block-scalar-indent #f))
       (cond
         [(string=? trimmed "") (void)]
         [(string-prefix? trimmed "#") (void)]
         ;; document markers carry no structure
         [(or (string=? trimmed "---") (string=? trimmed "...")) (void)]
         [else
          (when (regexp-match? tab-indent-rx line)
            (problem! idx "tab indentation is not allowed in YAML"))
          (define-values (flow* quote-char) (scan-flow-and-quotes line flow idx problem!))
          (set! flow flow*)
          (when quote-char
            (problem! idx (format "unclosed ~a quote" (quote-name quote-char))))
          (track-indent! idx indent)
          (define key (key-of trimmed))
          (when (zero? indent)
            (cond
              [(not key)
               (unless (sequence-item? trimmed)
                 (problem! idx "top-level line is not a mapping entry"))]
              [(hash-ref top-keys key #f)
               (problem! idx (format "duplicate top-level key ~s" key))]
              [else (hash-set! top-keys key #t)]))
          (when (block-scalar-header? trimmed)
            (set! block-scalar-indent indent))])]))

  (unless (null? flow)
    (problem! (length lines)
              (format "unclosed flow collection: ~a '~a' still open at end of file"
                      (length flow)
                      (car flow))))
  (reverse problems-rev))

;; Problems of one file's text (used by the tests to keep the pure part pure).
(define (validate-text text) (yaml-problems text))

;; ---------------------------------------------------------------------------
;; File discovery + whole-tree validation
;; ---------------------------------------------------------------------------

(define workflow-rx #rx"\\.ya?ml$")

(define (directory-entries dir)
  (cond
    [(directory-exists? dir) (sort (map path->string (directory-list dir)) string<?)]
    [else '()]))

;; Repo-relative declaration files in deterministic order:
;; `.github/workflows/*.yml(yaml)` then `.github/actions/*/action.yml(yaml)`.
(define (workflow-declaration-files root)
  (define repo (simplify-path (path->complete-path root)))
  (define workflows
    (for/list ([name (in-list (directory-entries (build-path repo ".github" "workflows")))]
               #:when (regexp-match? workflow-rx name))
      (build-path ".github" "workflows" name)))
  (define actions
    (append*
     (for/list ([name (in-list (directory-entries (build-path repo ".github" "actions")))]
                #:when (directory-exists? (build-path repo ".github" "actions" name)))
       (for/list ([entry (in-list (directory-entries (build-path repo ".github" "actions" name)))]
                  #:when (regexp-match? #rx"^action\\.ya?ml$" entry))
         (build-path ".github" "actions" name entry)))))
  (append workflows actions))

;; Returns (values problems files-validated-count) where problems are
;; "<file>: <problem>" strings. A read failure, an unreadable path and an
;; empty declaration tree are all problems: a check that inspected nothing
;; must never report success.
(define (validate-workflow-files root)
  (define repo (simplify-path (path->complete-path root)))
  (define files (workflow-declaration-files repo))
  (define problems
    (if (null? files)
        (list "no .github/workflows/*.yml or .github/actions/*/action.yml found: nothing was validated")
        (append*
         (for/list ([rel (in-list files)])
           (define path (build-path repo rel))
           (define shown (path->string rel))
           (cond
             [(not (file-exists? path))
              (list (format "~a: not a regular file (cannot read)" shown))]
             [else
              (define text
                (with-handlers ([exn:fail? (lambda (e) e)])
                  (file->string path)))
              (cond
                [(exn:fail? text)
                 (list (format "~a: cannot read: ~a"
                               shown
                               (if (exn-message text) (exn-message text) "read failed")))]
                [else
                 (for/list ([p (in-list (yaml-problems text))])
                   (format "~a: ~a" shown p))])])))))
  (values problems (length files)))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(define (main argv)
  (define root (current-directory))
  (command-line
   #:program "validate-workflows.rkt"
   #:once-each
   [("--root") dir "Repository root to validate (default: current directory)" (set! root dir)]
   #:args ()
   (void))
  (define-values (problems count) (validate-workflow-files root))
  (cond
    [(null? problems)
     (printf "workflow-yaml-ok: ~a workflow/action files structurally valid\n" count)
     0]
    [else
     (for ([p (in-list problems)])
       (eprintf "ERROR: ~a\n" p))
     (eprintf "workflow-yaml-invalid: ~a problem(s) in ~a workflow/action file(s)\n"
              (length problems)
              count)
     1]))

(module+ main
  (exit (main (vector->list (current-command-line-arguments)))))
