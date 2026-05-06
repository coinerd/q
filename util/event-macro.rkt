#lang racket/base

;; util/event-macro.rkt — define-event macro prototype
;;
;; Provides:
;;   define-event — macro to define event structs with predicates

(require (for-syntax racket/base racket/syntax)
         racket/contract)

(provide define-event)

;; ============================================================
;; define-event macro
;; ============================================================

(define-syntax (define-event stx)
  (syntax-case stx ()
    [(_ event-name (field ...))
     #'(begin
         (struct event-name (field ...) #:transparent)
         (provide (struct-out event-name)))]))

;; ============================================================
;; Example usage (commented out):
;; ============================================================
#|
(define-event message-sent-event (content sender timestamp))
;; Expands to:
;;   (struct message-sent-event (content sender timestamp) #:transparent)
;;   (provide (struct-out message-sent-event))
;;   (define message-sent-event? (lambda (x) (message-sent-event? x)))
;;   (provide message-sent-event?)

;; Usage:
;; (define ev (message-sent-event "Hello" "Alice" (current-seconds)))
;; (message-sent-event? ev) => #t
|#
