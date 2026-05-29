#lang racket/base

;; ui-core/event-types.rkt — Struct types for UI events
;;
;; Replaces raw hash constructions in ui-core/dispatch.rkt with
;; typed structs to prevent key-typo bugs.

(require racket/contract)

(provide (struct-out ui-event)
         (contract-out
          [make-ui-event (->* (string?) () #:rest any/c ui-event?)]
          [ui-event->hash (-> ui-event? hash?)]))

;; A UI event with a type string and optional payload fields.
;; Payload is stored as a hash for flexibility since UI events
;; have varying payloads.
(struct ui-event (type payload) #:transparent)

;; Constructor that builds payload from keyword-style rest args
(define (make-ui-event type . payload-args)
  (define payload
    (let loop ([args payload-args] [h (hasheq)])
      (if (null? args)
          h
          (if (null? (cdr args))
              h
              (loop (cddr args) (hash-set h (car args) (cadr args)))))))
  (ui-event type payload))

;; Convert to hash for backward compatibility with event bus
(define (ui-event->hash evt)
  (hash-set (ui-event-payload evt) 'type (ui-event-type evt)))
