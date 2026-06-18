#lang racket/base

;; skills/frontmatter.rkt — Agent Skills standard frontmatter parsing (#1157)
;;
;; Parses YAML-like frontmatter from SKILL.md files (between --- markers).
;; Validates skill names and required fields per the Agent Skills standard.
;;
;; Provides:
;;   valid-skill-name?                 — check if a skill name conforms to the standard
;;   parse-skill-frontmatter           — extract flat frontmatter hash from SKILL.md content
;;   parse-skill-frontmatter-extended  — parse YAML-subset (lists, nested maps, inline arrays)
;;   validate-frontmatter              — validate parsed frontmatter fields
;;   parse-fm-inline-array             — parse "[a, b, c]" → ("a" "b" "c")

(require racket/string
         racket/list)

;; ============================================================
;; Skill name validation
;; ============================================================

;; valid-skill-name? : any/c -> boolean?
;; Returns #t if name is a valid skill name: 1-64 chars, lowercase a-z + 0-9 + hyphens,
;; must not start or end with a hyphen.
(define (valid-skill-name? name)
  (and (string? name)
       (>= (string-length name) 1)
       (<= (string-length name) 64)
       (regexp-match? #rx"^[a-z0-9]([a-z0-9-]*[a-z0-9])?$" name)))

;; ============================================================
;; Frontmatter parsing
;; ============================================================

;; parse-skill-frontmatter : string? -> (or/c hash? #f)
;; Parses YAML-like frontmatter from SKILL.md content.
;; Frontmatter is delimited by --- markers at the start of the file.
;; Returns a hash of parsed key-value fields, or #f if no valid frontmatter.
;;
;; LIMITATION (I-21): This is a simple regex-based parser, not a full YAML parser.
;; It only supports flat key: value pairs. It does NOT support:
;;   - Nested structures (objects, nested maps)
;;   - Lists/arrays (YAML - item syntax)
;;   - Multi-line values (|, >, folded scalars)
;;   - Quoted strings with embedded colons
;;   - Comments after values (# inline comments)
;; For skill frontmatter, only flat string values are needed.
(define (parse-skill-frontmatter content)
  (define lines (string-split content "\n"))
  (cond
    [(< (length lines) 3) #f]
    [(not (string=? (string-trim (car lines)) "---")) #f]
    [else
     (define rest (cdr lines))
     (define end-idx
       (for/first ([line (in-list rest)]
                   [i (in-naturals)]
                   #:when (string=? (string-trim line) "---"))
         i))
     (if (not end-idx)
         #f
         (let ()
           (define fm-lines (take rest end-idx))
           (define result (make-hash))
           (for ([line (in-list fm-lines)])
             (define match (regexp-match #rx"^([a-zA-Z_-]+):\\s*(.+)$" line))
             (when match
               (define key (string->symbol (string-trim (cadr match))))
               (define val (string-trim (caddr match)))
               ;; Remove surrounding quotes if present
               (hash-set! result key (string-trim val "\""))))
           (for/hash ([(k v) (in-hash result)])
             (values k v))))]))

;; ============================================================
;; Extended frontmatter parsing (v0.99.26 W1)
;; ============================================================

;; parse-skill-frontmatter-extended : string? -> (or/c hash? #f)
;; Parses YAML-subset frontmatter supporting:
;;   - Flat key: value (strings) — backward compatible
;;   - Lists (- item syntax) with string items
;;   - Nested maps within list items (- key: value + continuations)
;;   - Inline arrays ([a, b, c])
;; Returns a hash with mixed value types:
;;   - Flat values: string?
;;   - List values: (listof string?) or (listof hash?)
;;   - Inline arrays: (listof string?)
(define (parse-skill-frontmatter-extended content)
  (define lines (string-split content "\n"))
  (cond
    [(< (length lines) 3) #f]
    [(not (string=? (string-trim (car lines)) "---")) #f]
    [else
     (define rest (cdr lines))
     (define end-idx
       (for/first ([line (in-list rest)]
                   [i (in-naturals)]
                   #:when (string=? (string-trim line) "---"))
         i))
     (if (not end-idx)
         #f
         (let ()
           (define fm-lines (take rest end-idx))
           (define tokens (tokenize-fm-lines fm-lines))
           (if (null? tokens)
               #f
               (let-values ([(result _remaining) (parse-fm-mapping tokens 0)])
                 result))))]))

;; --- Tokenizer ---

;; tokenize-fm-lines : (listof string?) -> (listof (list/c exact-nonnegative-integer? string?))
;; Converts raw frontmatter lines into (indent content) pairs,
;; filtering out blank lines and comments.
(define (tokenize-fm-lines lines)
  (filter-map (lambda (line)
                (define trimmed (string-trim line))
                (cond
                  [(string=? trimmed "") #f]
                  [(regexp-match? #rx"^#" trimmed) #f]
                  [else (list (fm-count-indent line) trimmed)]))
              lines))

;; fm-count-indent : string? -> exact-nonnegative-integer?
;; Counts leading space characters (tabs not supported, per YAML spec).
(define (fm-count-indent line)
  (- (string-length line) (string-length (string-trim line #:left? #t #:right? #f))))

;; --- Inline array parser ---

;; parse-fm-inline-array : string? -> (listof string?)
;; Parses "[a, b, c]" → ("a" "b" "c").
;; Strips surrounding quotes from each element.
(define (parse-fm-inline-array str)
  (define trimmed (string-trim str))
  (cond
    [(and (string-prefix? trimmed "[") (string-suffix? trimmed "]"))
     (define inner (substring trimmed 1 (- (string-length trimmed) 1)))
     (define inner-trimmed (string-trim inner))
     (if (string=? inner-trimmed "")
         '()
         (map (lambda (s) (fm-strip-quotes (string-trim s))) (string-split inner-trimmed ",")))]
    [else (list trimmed)]))

;; --- Key-value parser ---

;; parse-fm-kv : string? -> (or/c (cons/c symbol? string?) #f)
;; Parses "key: value" → (cons 'key "value").
;; Returns #f if the string doesn't match key: value syntax.
(define (parse-fm-kv content)
  (define m (regexp-match #rx"^([a-zA-Z_-]+):\\s*(.*)$" content))
  (and m (cons (string->symbol (cadr m)) (fm-strip-quotes (string-trim (caddr m))))))

;; fm-strip-quotes : string? -> string?
;; Removes surrounding single or double quotes if present.
(define (fm-strip-quotes s)
  (define len (string-length s))
  (cond
    [(and (>= len 2)
          (or (and (string-prefix? s "\"") (string-suffix? s "\""))
              (and (string-prefix? s "'") (string-suffix? s "'"))))
     (substring s 1 (- len 1))]
    [else s]))

;; --- Recursive descent parser ---

;; parse-fm-mapping : tokens exact-nonnegative-integer? -> (values hash? tokens)
;; Parses key:value pairs at indent >= min-indent until dedent or list item.
(define (parse-fm-mapping tokens min-indent)
  (define result (make-hash))
  (let loop ([ts tokens])
    (cond
      [(null? ts) (values (fm-freeze result) '())]
      [else
       (define tok (car ts))
       (define indent (car tok))
       (define content (cadr tok))
       (cond
         ;; Dedent → end of mapping
         [(< indent min-indent) (values (fm-freeze result) ts)]
         ;; List item → not part of this mapping
         [(string-prefix? content "- ") (values (fm-freeze result) ts)]
         ;; Key: value or key: (children)
         [else
          (define kv (parse-fm-kv content))
          (cond
            [(not kv) (loop (cdr ts))]
            [else
             (define key (car kv))
             (define value (cdr kv))
             (cond
               ;; Inline array: key: [a, b, c]
               [(string-prefix? value "[")
                (hash-set! result key (parse-fm-inline-array value))
                (loop (cdr ts))]
               ;; Scalar: key: value
               [(not (string=? value ""))
                (hash-set! result key value)
                (loop (cdr ts))]
               ;; Children: key: (empty → indented block)
               [else
                (define-values (child-val remaining) (parse-fm-children (cdr ts) indent))
                (hash-set! result key child-val)
                (loop remaining)])])])])))

;; parse-fm-children : tokens exact-nonnegative-integer? -> (values any/c tokens)
;; Parses children of a key with empty value.
;; Returns a list (for `- ` items) or a nested hash (for indented key:value).
(define (parse-fm-children tokens parent-indent)
  (cond
    [(null? tokens) (values "" '())]
    [else
     (define tok (car tokens))
     (define indent (car tok))
     (define content (cadr tok))
     (cond
       [(<= indent parent-indent) (values "" tokens)]
       [(string-prefix? content "- ") (parse-fm-list tokens parent-indent)]
       [else
        ;; Nested mapping
        (define-values (child-map remaining) (parse-fm-mapping tokens (+ parent-indent 1)))
        (values child-map remaining)])]))

;; parse-fm-list : tokens exact-nonnegative-integer? -> (values list tokens)
;; Parses a list of `- ` items until dedent.
(define (parse-fm-list tokens parent-indent)
  (let loop ([ts tokens]
             [items '()])
    (cond
      [(null? ts) (values (reverse items) '())]
      [else
       (define tok (car ts))
       (define indent (car tok))
       (define content (cadr tok))
       (cond
         [(<= indent parent-indent) (values (reverse items) ts)]
         [(not (string-prefix? content "- ")) (values (reverse items) ts)]
         [else
          (define-values (item remaining) (parse-fm-list-item ts))
          (loop remaining (cons item items))])])))

;; parse-fm-list-item : tokens -> (values any/c tokens)
;; Parses one `- ...` item.
;; A list item is either a plain string or a map (key:value on same line
;; with continuation lines at the same column).
(define (parse-fm-list-item tokens)
  (define tok (car tokens))
  (define indent (car tok))
  (define content (cadr tok))
  (define item-content (substring content 2))
  (define continuation-indent (+ indent 2))
  (define kv (parse-fm-kv item-content))
  (cond
    ;; Map item: synthesize token at continuation-indent and parse mapping
    [kv
     (define synthetic-tokens (cons (list continuation-indent item-content) (cdr tokens)))
     (define-values (item-map remaining) (parse-fm-mapping synthetic-tokens continuation-indent))
     (values item-map remaining)]
    ;; Plain string item
    [else (values item-content (cdr tokens))]))

;; fm-freeze : mutable-hash? -> immutable-hash?
(define (fm-freeze h)
  (for/hash ([(k v) (in-hash h)])
    (values k v)))

;; ============================================================
;; Frontmatter validation
;; ============================================================

;; validate-frontmatter : hash? string? -> (values (or/c 'ok 'warning 'error) string?)
;; Validates parsed frontmatter against the Agent Skills standard.
;; Returns two values: status symbol and message string.
(define (validate-frontmatter fm directory-name)
  (define name (hash-ref fm 'name #f))
  (define desc (hash-ref fm 'description #f))
  (cond
    [(not name) (values 'error "Missing required 'name' field in frontmatter")]
    [(not (valid-skill-name? name)) (values 'error (format "Invalid skill name: ~a" name))]
    [(not (string=? name directory-name))
     (values 'warning (format "Skill name '~a' doesn't match directory '~a'" name directory-name))]
    [(not desc) (values 'error "Missing required 'description' field")]
    [(> (string-length desc) 1024) (values 'error "Description exceeds 1024 characters")]
    [else (values 'ok "Valid")]))

(provide valid-skill-name?
         parse-skill-frontmatter
         parse-skill-frontmatter-extended
         validate-frontmatter
         parse-fm-inline-array)
