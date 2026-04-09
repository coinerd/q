#lang racket

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

(require "api.rkt")

(provide define-q-extension)

;; ============================================================
;; define-q-extension macro
;; ============================================================

(define-syntax (define-q-extension stx)
  (syntax-case stx ()
    [(_ name-sym opts ...)
     (identifier? #'name-sym)
     (let* ([args (syntax->list #'(opts ...))]
            [ext-name (symbol->string (syntax-e #'name-sym))])
       ;; Parse keyword arguments
       (define-values (version api-version hook-pairs)
         (let loop ([remaining args]
                    [version "0.1.0"]
                    [api-version "1"]
                    [hooks '()])
           (cond
             [(null? remaining)
              (values version api-version (reverse hooks))]
             [(null? (cdr remaining))
              (raise-syntax-error 'define-q-extension
                                  "expected value after keyword"
                                  (car remaining))]
             [else
              (define kw (syntax-e (car remaining)))
              (case kw
                [(#:version)
                 (loop (cddr remaining)
                       (syntax-e (cadr remaining))
                       api-version hooks)]
                [(#:api-version)
                 (loop (cddr remaining)
                       version
                       (syntax-e (cadr remaining))
                       hooks)]
                [(#:on)
                 (when (null? (cddr remaining))
                   (raise-syntax-error 'define-q-extension
                                       "expected handler after #:on hook-point"
                                       (cadr remaining)))
                 (define point-stx (cadr remaining))
                 (define handler-stx (caddr remaining))
                 (loop (cdddr remaining)
                       version api-version
                       (cons (cons point-stx handler-stx) hooks))]
                [else
                 (raise-syntax-error 'define-q-extension
                                     "unknown keyword"
                                     (car remaining))])])))
       ;; Build pairs of (quoted-symbol handler-expr) for hasheq
       (define hasheq-args
         (apply append
                (for/list ([hp hook-pairs])
                  (list (datum->syntax stx (list 'quote (syntax-e (car hp))))
                        (cdr hp)))))
       (with-syntax ([ext-name ext-name]
                     [ext-version version]
                     [ext-api-version api-version]
                     [(hasheq-arg ...) hasheq-args])
         #'(define name-sym
             (extension ext-name
                        ext-version
                        ext-api-version
                        (hasheq hasheq-arg ...)))))]))
