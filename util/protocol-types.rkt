#lang racket/base

;; util/protocol-types.rkt — Backward-compat re-export facade
;;
;; ARCH-05: This module was split into focused sub-modules:
;;   - content-parts.rkt — text-part, tool-call-part, tool-result-part
;;   - message.rkt       — message struct, serialization
;;   - event.rkt         — event envelope, serialization
;;   - entry-predicates.rkt — entry kind predicates
;;   - tree-entries.rkt  — branch-entry, tree-navigation-entry, etc.
;;   - loop-result.rkt   — loop-result struct
;;
;; This file re-exports everything for backward compatibility.
;; New code should import the focused sub-modules directly.

(require racket/contract
         "content-parts.rkt"
         "message.rkt"
         "event.rkt"
         "entry-predicates.rkt"
         "tree-entries.rkt"
         "loop-result.rkt")

;; Content parts
;; Re-export all from sub-modules
(provide (all-from-out "content-parts.rkt")
         (all-from-out "message.rkt")
         (all-from-out "event.rkt")
         (all-from-out "entry-predicates.rkt")
         (all-from-out "tree-entries.rkt")
         (all-from-out "loop-result.rkt")

         ;; Custom entry for extension state persistence (#1147)
         make-custom-entry
         custom-entry?
         custom-entry-extension
         custom-entry-key
         custom-entry-data

         ;; Tool-call (standalone — canonical definition)
         (struct-out tool-call)

         ;; Tool-result (standalone — canonical definition)
         (struct-out tool-result))

;; ============================================================
;; Custom entry for extension state persistence (#1147)
;; ============================================================

;; Creates a message struct with kind 'custom-message and extension
;; metadata in the meta field. Used by extensions to persist state.
(define (make-custom-entry extension-name key data)
  (make-message (format "custom-~a-~a-~a" extension-name key (current-inexact-milliseconds))
                #f
                'system
                'custom-message
                '()
                (current-seconds)
                (hasheq 'extension extension-name 'key key 'data data)))

;; Predicate: is this message a custom entry?
(define (custom-entry? msg)
  (and (message? msg) (eq? (message-kind msg) 'custom-message)))

;; Accessor: get extension name from a custom entry
(define (custom-entry-extension msg)
  (hash-ref (message-meta msg) 'extension #f))

;; Accessor: get key from a custom entry
(define (custom-entry-key msg)
  (hash-ref (message-meta msg) 'key #f))

;; Accessor: get data from a custom entry
(define (custom-entry-data msg)
  (hash-ref (message-meta msg) 'data #f))

;; ============================================================
;; Tool-call struct (standalone — canonical definition)
;; ============================================================

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

;; ============================================================
;; Tool-result struct (standalone — canonical definition)
;; ============================================================

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
