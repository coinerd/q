#lang racket/base

;; util/event-macro.rkt — event struct definition macros
;;
;; Provides:
;;   define-typed-event  — full code generation: struct, constructor, type, fields, provide
;;   lookup-event-fields — runtime lookup of field names by struct name symbol
;;
;; Usage:
;;   (define-typed-event turn-start-event "turn.started"
;;     (model provider))
;;   (define-typed-event injection-event "injection"
;;     (source content-type content-length)
;;     #:optional ([message #f]))
;;
;; Generates: struct, make-*-event constructor, *-event-type, *-event-fields, provide
;; Also auto-registers into *event-field-registry* at module load time.

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (only-in "../agent/event-structs/base.rkt" typed-event))

(provide define-typed-event
         lookup-event-fields
         register-event-fields!)

;; ===========================================================
;; Event field registry (I-12, I-23)
;; ===========================================================

;; Mutable hash: struct-name symbol -> field-name list
;; Populated automatically by define-typed-event at module load time.
(define *event-field-registry* (make-hasheq))

(define (register-event-fields! name fields)
  (hash-set! *event-field-registry* name fields))

(define (lookup-event-fields name)
  (hash-ref *event-field-registry* name '()))

;; ===========================================================
;; define-typed-event macro (unified clause, I-23)
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

;; Single unified clause handling both with-optional and without-optional.
(define-syntax (define-typed-event stx)
  (syntax-parse stx
    [(_ event-name:id
        type-str:str
        (req-field:id ...)
        (~optional (~seq #:optional ([opt-field:id opt-default:expr] ...))
                   #:defaults ([(opt-field 1) '()] [(opt-default 1) '()])))
     (define opt-raw (attribute opt-field))
     (define opt-default-raw (attribute opt-default))
     (define opt-fields
       (cond
         [(not opt-raw) '()]
         [(syntax? opt-raw) (syntax->list opt-raw)]
         [(list? opt-raw) opt-raw]
         [else '()]))
     (define opt-defaults
       (cond
         [(not opt-default-raw) '()]
         [(syntax? opt-default-raw) (syntax->list opt-default-raw)]
         [(list? opt-default-raw) opt-default-raw]
         [else '()]))
     (define all-fields (append (syntax->list #'(req-field ...)) opt-fields))
     (define all-defaults
       (append (map (lambda (_) #f) (syntax->list #'(req-field ...))) opt-defaults))
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
           (register-event-fields! 'event-name fields-name)
           (provide (struct-out event-name)
                    make-name
                    type-name
                    fields-name)))]))
