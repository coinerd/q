#lang racket/base

;; extensions/define-extension.rkt — macro DSL for extension definition
;;
;; Provides:
;;   define-q-extension: a macro that creates an extension struct
;;   with name, version, api-version, and hook handlers.
;;
;; Usage:
;;   (define-q-extension my-ext
;;     #:version "1.0.0"
;;     #:api-version "1"
;;     #:on hook-point handler
;;     ...)

(require (for-syntax racket/base
                     syntax/parse
                     (only-in "../util/hook-types.rkt" valid-hook-name?))
         "api.rkt")

(provide define-q-extension)

;; ============================================================
;; define-q-extension macro (syntax-parse)
;; ============================================================

(define-syntax (define-q-extension stx)
  (syntax-parse stx
    [(_ name-sym:id
        (~alt (~optional (~seq #:version version:str))
              (~optional (~seq #:api-version api-version:str))
              (~seq #:on hook-point:id handler-expr)) ...)
     (define ext-name (symbol->string (syntax-e #'name-sym)))
     (define ext-version
       (if (attribute version)
           (syntax-e #'version)
           "0.1.0"))
     (define ext-api-version
       (if (attribute api-version)
           (syntax-e #'api-version)
           "1"))
     ;; Validate hook points at expansion time
     (for ([hp (in-list (syntax->list #'(hook-point ...)))])
       (define point-sym (syntax-e hp))
       (unless (valid-hook-name? point-sym)
         (raise-syntax-error 'define-q-extension (format "unknown hook point '~a'" point-sym) hp)))
     ;; Build hasheq args from hook pairs
     (define hasheq-args
       (apply append
              (for/list ([hp (in-list (syntax->list #'(hook-point ...)))]
                         [he (in-list (syntax->list #'(handler-expr ...)))])
                (list (datum->syntax stx (list 'quote (syntax-e hp))) he))))
     (with-syntax ([ext-name-str ext-name]
                   [ext-version-str ext-version]
                   [ext-api-version-str ext-api-version]
                   [(hasheq-arg ...) hasheq-args])
       #'(define name-sym
           (extension ext-name-str ext-version-str ext-api-version-str (hasheq hasheq-arg ...))))]))
