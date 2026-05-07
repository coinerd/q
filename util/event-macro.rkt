#lang racket/base

;; util/event-macro.rkt — event struct definition macros
;;
;; Provides:
;;   define-event        — basic struct+provide (DEPRECATED, kept for compat)
;;   define-typed-event  — full code generation: struct, constructor, type, fields, provide
;;
;; v0.32.2 Wave 0: define-typed-event macro using syntax-parse.
;; Generates struct + keyword-arg constructor + type string + field list + provide.

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse)
         (only-in "../agent/event-structs/base.rkt" typed-event))

(provide define-event
         define-typed-event)

;; ===========================================================
;; define-event macro (DEPRECATED — use define-typed-event)
;; ===========================================================

(define-syntax (define-event stx)
  (syntax-case stx ()
    [(_ event-name (field ...))
     #'(begin
         (struct event-name (field ...) #:transparent)
         (provide (struct-out event-name)))]
    [(_ event-name parent (field ...))
     #'(begin
         (struct event-name parent (field ...) #:transparent)
         (provide (struct-out event-name)))]))

;; ===========================================================
;; define-typed-event macro
;; ===========================================================
;;
;; Usage:
;;   (define-typed-event turn-start-event "turn.started"
;;     (model provider))

(begin-for-syntax
  (define (make-kw-arg-list fields)
    ;; Build alternating keyword-identifier list: (#:model model #:provider provider ...)
    (apply append
           (for/list ([f fields])
             (define kw (datum->syntax f (string->keyword (symbol->string (syntax-e f)))))
             (list kw f)))))

(define-syntax (define-typed-event stx)
  (syntax-parse stx
    [(_ event-name:id type-str:str (user-field:id ...))
     (define make-name (format-id #'event-name "make-~a" #'event-name))
     (define type-name (format-id #'event-name "~a-type" #'event-name))
     (define fields-name (format-id #'event-name "~a-fields" #'event-name))
     (define kw-args (make-kw-arg-list (syntax->list #'(user-field ...))))
     (with-syntax ([make-name make-name]
                   [type-name type-name]
                   [fields-name fields-name]
                   [(kw-arg ...) kw-args])
       #'(begin
           (struct event-name typed-event (user-field ...) #:transparent)
           (define type-name type-str)
           (define fields-name '(user-field ...))
           (define (make-name #:session-id session-id
                              #:turn-id turn-id
                              #:timestamp [timestamp (current-seconds)]
                              kw-arg ...)
             (event-name type-str timestamp session-id turn-id user-field ...))
           (provide (struct-out event-name)
                    make-name
                    type-name
                    fields-name)))]))
