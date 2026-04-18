#lang racket/base

;; skills/template.rkt — template rendering
;;
;; Split from skills/types.rkt (Issue #205, QUAL-09).
;; Contains:
;;   render-template — substitute {{var}} placeholders in a template string
;;   render-template-with-positional-args — bash-style $1, $@, ${@:N:L} expansion

(require racket/string)

(provide render-template
         render-template-with-positional-args)

;; ============================================================
;; Template rendering
;; ============================================================

(define (render-template template-string vars)
  ;; Replace {{var-name}} with values from vars hash.
  ;; vars keys can be symbols or strings.
  ;; Missing variables are left as-is.
  (define var-map
    (for/hash ([(k v) (in-hash vars)])
      (values (if (symbol? k) (symbol->string k) k) v)))
  (regexp-replace*
   #rx"\\{\\{([^}]+)\\}\\}"
   template-string
   (λ (match var-name)
     (hash-ref var-map (string-trim var-name) match))))

;; ============================================================
;; Bash-style positional arg expansion (#1187)
;; ============================================================

;; Expand bash-style positional arguments in a template string.
;; Supports: $1, $2, ... $9, $@, ${@:N:L}
;; - $N → Nth positional arg (1-indexed)
;; - $@ → all args joined with space
;; - ${@:N:L} → L args starting from position N (1-indexed)
;; Missing args expand to empty string.
(define (render-template-with-positional-args template-string args)
  (define args-vec (list->vector args))
  (define args-count (vector-length args-vec))
  ;; Build regex for ${@:N:L} using string-append to avoid escape issues
  (define slice-rx (string-append "\\$" "\\{@:([0-9]+):([0-9]+)\\}"))
  ;; ${@:N:L} → L args starting from 1-indexed N
  (define expanded
    (regexp-replace*
     (regexp slice-rx)
     template-string
     (lambda (match start-str len-str)
       (define start (string->number start-str))
       (define len (string->number len-str))
       (string-join
        (for/list ([i (in-range (sub1 start) (min (+ (sub1 start) len) args-count))])
          (vector-ref args-vec i))
        " "))))
  ;; $@ → all args
  (define expanded2
    (regexp-replace*
     #rx"\\$@"
     expanded
     (lambda (match)
       (string-join args " "))))
  ;; $N → Nth arg (1-9)
  (regexp-replace*
   #rx"\\$([1-9])"
   expanded2
   (lambda (match n-str)
     (define n (string->number n-str))
     (if (<= n args-count)
         (vector-ref args-vec (sub1 n))
         ""))))
