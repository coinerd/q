#lang racket/base

;; tools/define-tool.rkt — Macro for declarative tool definition
;;
;; Provides `define-tool` which generates make-tool + schema + handler
;; from a declarative specification. Eliminates boilerplate for simple
;; tools where the schema, description, and handler are closely coupled.
;;
;; Usage:
;;   (define-tool my-tool
;;     #:description "Does something useful"
;;     #:required ("path")
;;     #:properties
;;       [("path" "string" "Path to file")
;;        ("verbose" "boolean" "Show extra output")]
;;     (lambda (args exec-ctx)
;;       (make-success-result "done")))

(require (for-syntax racket/base
                     racket/list
                     syntax/parse)
         "tool.rkt")

(provide define-tool)

(define-syntax (define-tool stx)
  (syntax-parse stx
    [(_ name:id
        #:description desc:str
        #:required (req:str ...)
        #:properties [(prop-name:id prop-type:str prop-desc:str) ...]
        handler:expr)

     #:with tool-name-str #`#,(symbol->string (syntax->datum #'name))
     #:with tool-id (datum->syntax stx (string->symbol (format "tool-~a" (syntax->datum #'name))))
     ;; Build property pairs at expansion time
     #:with (prop-pair ...) (for/list ([pn (syntax->list #'(prop-name ...))]
                                       [pt (syntax->list #'(prop-type ...))]
                                       [pd (syntax->list #'(prop-desc ...))])
                              #`(cons '#,(syntax->datum pn) (hasheq 'type #,pt 'description #,pd)))

     #'(begin
         (define tool-id handler)
         (provide tool-id)
         (define name
           (make-tool tool-name-str
                      desc
                      (hasheq 'type
                              "object"
                              'required
                              '(req ...)
                              'properties
                              (make-immutable-hasheq (list prop-pair ...)))
                      tool-id)))]))
