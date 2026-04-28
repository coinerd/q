#lang racket

;; tools/schema-macro.rkt — Declarative tool schema construction
;;
;; Provides `define-tool-schema` macro for building JSON Schema objects
;; used in tool definitions. Eliminates repetitive hasheq nests.
;;
;; Example:
;;   (define-tool-schema my-schema
;;     #:description "My tool description"
;;     #:required ("path" "content")
;;     #:properties
;;     [("path" "string" "File path")
;;      ("content" "string" "Content to write")
;;      ("timeout" "integer" "Timeout in seconds")])
;;
;; Also provides `tool-schema` function for programmatic construction.

(require (for-syntax racket/base
                     syntax/parse))

(provide define-tool-schema
         tool-schema)

;; ============================================================
;; Programmatic schema builder
;; ============================================================

;; Build a JSON Schema object from required/properties lists.
;; properties: (listof (list name type description))
;; required: (listof string)
;; optional: description string
(define (tool-schema required-list properties #:description [desc #f])
  (define base
    (hasheq 'type
            "object"
            'required
            required-list
            'properties
            (for/hasheq ([prop (in-list properties)])
              (match-define (list name type prop-desc) prop)
              (values (string->symbol name) (hasheq 'type type 'description prop-desc)))))
  (if desc
      (hash-set base 'description desc)
      base))

;; ============================================================
;; Schema macro
;; ============================================================

;; Helper: build properties hash from parallel lists of names/types/descs
(define (build-properties-hash names types descs)
  (for/hasheq ([n (in-list names)]
               [t (in-list types)]
               [d (in-list descs)])
    (values (if (symbol? n)
                n
                (string->symbol n))
            (hasheq 'type t 'description d))))

(begin-for-syntax
  (define-splicing-syntax-class schema-desc
                                #:attributes (desc)
                                (pattern (~seq #:description d:expr)
                                  #:with desc #'d)
                                (pattern (~seq)
                                  #:with desc #'#f)))

(define-syntax (define-tool-schema stx)
  (syntax-parse stx
    [(_ id:id
        desc:schema-desc
        #:required (req:str ...)
        #:properties [(pname:str ptype:str pdesc:str) ...])
     #'(define id
         (let ([base-schema
                (hasheq 'type
                        "object"
                        'required
                        (list req ...)
                        'properties
                        (build-properties-hash (list pname ...) (list ptype ...) (list pdesc ...)))])
           (if desc.desc
               (hash-set base-schema 'description desc.desc)
               base-schema)))]))

(require racket/match)
