#lang racket/base

;; runtime/session-index.rkt — sidecar derived indexes for session logs
;;
;; Thin facade: re-exports from session-index/schema, mutations, query.
;; All struct definitions live in schema.rkt (single source of truth).

(require racket/contract
         (only-in "../util/message/message.rkt" message?)
         "session-index/schema.rkt"
         "session-index/mutations.rkt"
         "session-index/query.rkt")

;; Explicit re-exports from session-index/schema.rkt — do NOT use all-from-out (ADR-0028)
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
         make-empty-index
         (contract-out
          [build-index! (-> path-string? path-string? session-index?)]
          [load-index (-> path-string? session-index?)]
          [save-index! (-> path-string? session-index? void?)]
          [switch-leaf! (-> session-index? string? (or/c string? #f))]
          [mark-active-leaf! (-> session-index? string? (or/c string? #f))]
          [navigate-to-entry! (-> session-index? string? (or/c navigate-result? #f))]
          [navigate-to-leaf! (-> session-index? string? (or/c navigate-result? #f))]
          [navigate-next-leaf! (-> session-index? (or/c navigate-result? #f))]
          [navigate-prev-leaf! (-> session-index? (or/c navigate-result? #f))]
          [branch! (-> session-index? string? (or/c message? #f))]
          [branch-with-summary!
           (-> session-index? string? string? (values session-index? (or/c message? #f)))]
          [reset-leaf! (-> session-index? void?)]
          [append-to-leaf! (-> session-index? message? (values session-index? message?))]
          [add-bookmark! (-> session-index? string? string? (values session-index? string?))]
          [remove-bookmark! (-> session-index? string? (values session-index? boolean?))]
          [list-bookmarks (-> session-index? list?)]
          [find-bookmark-by-label (-> session-index? string? (or/c bookmark? #f))]
          [get-bookmark (-> session-index? string? (or/c bookmark? #f))]
          [load-bookmarks (-> path-string? list?)]
          [save-bookmarks! (-> path-string? session-index? void?)]
          [load-index-with-bookmarks (-> path-string? path-string? session-index?)]
          [bookmarks-path (-> path-string? path?)]
          [lookup-entry (-> session-index? string? (or/c message? #f))]
          [children-of (-> session-index? string? list?)]
          [leaf-nodes (-> session-index? list?)]
          [resolve-active-leaf (-> session-index? (or/c message? #f))]
          [active-leaf (-> session-index? (or/c message? #f))]
          [get-branch (-> session-index? string? (or/c list? #f))]
          [find-common-ancestor (-> session-index? string? string? (or/c string? #f))]
          [collect-branch-entries
           (-> session-index? string? string? exact-nonnegative-integer? list?)]
          [leaf-depth (-> session-index? string? (or/c exact-nonnegative-integer? #f))]
          [estimate-entry-tokens (-> (or/c hash? message?) exact-nonnegative-integer?)]))
