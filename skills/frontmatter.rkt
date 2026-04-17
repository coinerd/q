#lang racket/base

;; skills/frontmatter.rkt — Agent Skills standard frontmatter parsing (#1157)
;;
;; Parses YAML-like frontmatter from SKILL.md files (between --- markers).
;; Validates skill names and required fields per the Agent Skills standard.
;;
;; Provides:
;;   valid-skill-name?         — check if a skill name conforms to the standard
;;   parse-skill-frontmatter   — extract frontmatter hash from SKILL.md content
;;   validate-frontmatter      — validate parsed frontmatter fields

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
         validate-frontmatter)
