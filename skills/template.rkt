#lang racket/base

;; skills/template.rkt — template rendering
;;
;; Split from skills/types.rkt (Issue #205, QUAL-09).
;; Contains:
;;   render-template — substitute {{var}} placeholders in a template string

(require racket/string)

(provide render-template)

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
