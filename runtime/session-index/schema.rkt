#lang racket/base

;; runtime/session-index/schema.rkt — index struct, bookmarks, invariants
;;
;; Core data definitions for session-index.

(require racket/contract)

(provide session-index
         session-index?
         session-index-by-id
         session-index-children
         session-index-entry-order
         session-index-bookmarks
         session-index-active-leaf-id
         session-index-bookmark-sem
         bookmark
         bookmark?
         bookmark-id
         bookmark-entry-id
         bookmark-label
         bookmark-timestamp
         make-bookmark
         navigate-result
         navigate-result?
         navigate-result-entry
         navigate-result-branch
         navigate-result-children
         navigate-result-leaf?
         make-empty-index)

;; Per-index semaphore for bookmark mutations (#115)
(struct session-index (by-id children entry-order bookmarks active-leaf-id bookmark-sem)
  #:transparent)

;; A bookmark holds user-defined labels for quick navigation
(struct bookmark (id entry-id label timestamp) #:transparent)

;; navigate-result : entry branch children leaf?
(struct navigate-result (entry branch children leaf?) #:transparent)

(define (make-bookmark id entry-id label timestamp)
  (bookmark id entry-id label timestamp))

(define (make-empty-index)
  (session-index (make-hash) (make-hash) (vector) (make-hash) (box #f) (make-semaphore 1)))
