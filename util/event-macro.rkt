#lang racket/base

;; util/event-macro.rkt — event struct definition macros
;;
;; Provides:
;;   define-typed-event  — full code generation: struct, constructor, type, fields, provide
;;
;; Usage:
;;   (define-typed-event turn-start-event "turn.started"
;;     (model provider))
;;   (define-typed-event injection-event "injection"
;;     (source content-type content-length)
;;     #:optional ([message #f]))
;;
;; Generates: struct, make-*-event constructor, *-event-type, *-event-fields, provide

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (only-in "../agent/event-structs/base.rkt" typed-event))

(provide define-typed-event)

;; ===========================================================
;; define-typed-event macro
;; ===========================================================

(begin-for-syntax
  (define (make-kw-arg-list fields defaults)
    ;; fields: list of identifier syntax
    ;; defaults: list of #f or default-expression syntax
    ;; Returns flat list: (#:f1 f1 #:f2 [f2 default2] ...)
    (apply append
           (for/list ([f fields]
                      [d defaults])
             (define kw (datum->syntax f (string->keyword (symbol->string (syntax-e f)))))
             (if d
                 (list kw #`[#,f #,d])
                 (list kw f))))))

(define-syntax (define-typed-event stx)
  (syntax-parse stx
    ;; With optional fields
    [(_ event-name:id
        type-str:str
        (req-field:id ...)
        (~seq #:optional ([opt-field:id opt-default:expr] ...)))
     (define all-fields (append (syntax->list #'(req-field ...)) (syntax->list #'(opt-field ...))))
     (define all-defaults
       (append (map (λ (_) #f) (syntax->list #'(req-field ...))) (syntax->list #'(opt-default ...))))
     (define make-name (format-id #'event-name "make-~a" #'event-name))
     (define type-name (format-id #'event-name "~a-type" #'event-name))
     (define fields-name (format-id #'event-name "~a-fields" #'event-name))
     (define kw-args (make-kw-arg-list all-fields all-defaults))
     (with-syntax ([make-name make-name]
                   [type-name type-name]
                   [fields-name fields-name]
                   [(all-field ...) all-fields]
                   [(kw-arg ...) kw-args])
       #'(begin
           (struct event-name typed-event (all-field ...) #:transparent)
           (define type-name type-str)
           (define fields-name '(all-field ...))
           (define (make-name #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp [timestamp (current-seconds)]
                              kw-arg ...)
             (event-name type-str timestamp session-id turn-id all-field ...))
           (provide (struct-out event-name)
                    make-name
                    type-name
                    fields-name)))]
    ;; Without optional fields
    [(_ event-name:id type-str:str (req-field:id ...))
     (define all-fields (syntax->list #'(req-field ...)))
     (define make-name (format-id #'event-name "make-~a" #'event-name))
     (define type-name (format-id #'event-name "~a-type" #'event-name))
     (define fields-name (format-id #'event-name "~a-fields" #'event-name))
     (define kw-args (make-kw-arg-list all-fields (map (λ (_) #f) all-fields)))
     (with-syntax ([make-name make-name]
                   [type-name type-name]
                   [fields-name fields-name]
                   [(all-field ...) all-fields]
                   [(kw-arg ...) kw-args])
       #'(begin
           (struct event-name typed-event (all-field ...) #:transparent)
           (define type-name type-str)
           (define fields-name '(all-field ...))
           (define (make-name #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp [timestamp (current-seconds)]
                              kw-arg ...)
             (event-name type-str timestamp session-id turn-id all-field ...))
           (provide (struct-out event-name)
                    make-name
                    type-name
                    fields-name)))]))
