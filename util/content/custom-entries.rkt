#lang racket/base

;; util/custom-entries.rkt — extension state persistence entries
;;
;; Custom entries are message structs with kind 'custom-message
;; and extension metadata in the meta field.

(require racket/contract
         "../message/message.rkt")

(provide (contract-out [make-custom-entry (-> string? string? any/c any/c)]
                       [custom-entry? (-> any/c boolean?)]
                       [custom-entry-extension (-> any/c any/c)]
                       [custom-entry-key (-> any/c any/c)]
                       [custom-entry-data (-> any/c any/c)]))

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
  (hash-ref (message-meta-safe msg) 'extension #f))

(define (custom-entry-key msg)
  (hash-ref (message-meta-safe msg) 'key #f))

(define (custom-entry-data msg)
  (hash-ref (message-meta-safe msg) 'data #f))
