#lang racket/base

;; util/custom-entries.rkt — extension state persistence entries
;;
;; Custom entries are message structs with kind 'custom-message
;; and extension metadata in the meta field.

(require "message.rkt")

(provide make-custom-entry
         custom-entry?
         custom-entry-extension
         custom-entry-key
         custom-entry-data)

;; Creates a message struct with kind 'custom-message and extension metadata.
(define (make-custom-entry extension-name key data)
  (make-message (format "custom-~a-~a-~a" extension-name key (current-inexact-milliseconds))
                #f
                'system
                'custom-message
                '()
                (current-seconds)
                (hasheq 'extension extension-name 'key key 'data data)))

(define (custom-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'custom-message)))

(define (custom-entry-extension msg)
  (hash-ref (message-meta msg) 'extension #f))

(define (custom-entry-key msg)
  (hash-ref (message-meta msg) 'key #f))

(define (custom-entry-data msg)
  (hash-ref (message-meta msg) 'data #f))
