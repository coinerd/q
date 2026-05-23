#lang racket/base

;; runtime/session-index.rkt — sidecar derived indexes for session logs
;;
;; Thin facade: re-exports from session-index/schema, mutations, query.
;; All struct definitions live in schema.rkt (single source of truth).

(require racket/contract
         "session-index/schema.rkt"
         "session-index/mutations.rkt"
         "session-index/query.rkt")

(provide (all-from-out "session-index/schema.rkt")
         (contract-out [build-index! (-> path-string? path-string? session-index?)]
                       [load-index (-> path-string? session-index?)]
                       [save-index! (-> path-string? session-index? void?)]
                       [switch-leaf! (-> session-index? string? (or/c string? #f))]
                       [mark-active-leaf! (-> session-index? string? (or/c string? #f))]
                       [navigate-to-entry! (-> session-index? string? (or/c navigate-result? #f))]
                       [navigate-to-leaf! (-> session-index? string? (or/c navigate-result? #f))]
                       [navigate-next-leaf! (-> session-index? (or/c navigate-result? #f))]
                       [navigate-prev-leaf! (-> session-index? (or/c navigate-result? #f))]
                       [branch! (-> session-index? string? (or/c any/c #f))]
                       [branch-with-summary! (-> session-index? string? string? (or/c any/c #f))]
                       [reset-leaf! (-> session-index? void?)]
                       [append-to-leaf! (-> session-index? any/c any/c)]
                       [add-bookmark! (-> session-index? string? string? string?)]
                       [remove-bookmark! (-> session-index? string? boolean?)]
                       [list-bookmarks (-> session-index? list?)]
                       [find-bookmark-by-label (-> session-index? string? (or/c bookmark? #f))]
                       [get-bookmark (-> session-index? string? (or/c bookmark? #f))]
                       [load-bookmarks (-> path-string? list?)]
                       [save-bookmarks! (-> path-string? session-index? void?)]
                       [load-index-with-bookmarks (-> path-string? path-string? session-index?)]
                       [bookmarks-path (-> path-string? path?)]
                       [lookup-entry (-> session-index? string? (or/c any/c #f))]
                       [children-of (-> session-index? string? list?)]
                       [leaf-nodes (-> session-index? list?)]
                       [resolve-active-leaf (-> session-index? (or/c any/c #f))]
                       [active-leaf (-> session-index? (or/c any/c #f))]
                       [get-branch (-> session-index? string? (or/c list? #f))]
                       [find-common-ancestor (-> session-index? string? string? (or/c string? #f))]
                       [collect-branch-entries
                        (-> session-index? string? string? exact-nonnegative-integer? list?)]
                       [leaf-depth (-> session-index? string? (or/c exact-nonnegative-integer? #f))]
                       [estimate-entry-tokens (-> (or/c hash? any/c) exact-nonnegative-integer?)]))
