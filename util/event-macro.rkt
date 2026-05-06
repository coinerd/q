#lang racket/base

;; util/event-macro.rkt — define-event macro prototype
;;
;; Provides:
;;   define-event — macro to define event structs with predicates

(require (for-syntax racket/base
                     racket/syntax)
         racket/contract)

(provide define-event)

;; ===========================================================
;; define-event macro
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
