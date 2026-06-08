#lang racket/base
;; tools/builtins/memory-tools.rkt — memory tool registration facade
;;
;; v0.96.2 (F4): Thin facade importing handlers from domain modules.
;; Shared helpers in memory-tools-shared.rkt.
;; Handlers in memory-tools-{store,query,manage}.rkt.

(require "../tool.rkt"
         "../define-tool.rkt"
         (only-in "memory-tools-store.rkt" store-memory-handler update-memory-handler)
         (only-in "memory-tools-query.rkt" search-memory-handler list-memory-handler)
         (only-in "memory-tools-manage.rkt"
                  delete-memory-handler
                  clear-memory-handler
                  consolidate-memory-handler
                  cleanup-expired-handler))

(define-tool
 store-memory
 #:description "Store a piece of information in memory for later retrieval."
 #:required ("content")
 #:properties
 [(content "string" "The information to store")
  (type "string" "Memory type: episodic, semantic, or procedural (default: semantic)")
  (scope
   "string"
   "Visibility: session, project, or user (default: project when project root exists, else session; user disabled by default)")
  (tags "array" "List of string tags for categorization")
  (sensitivity "string" "Sensitivity: public, internal, or sensitive (default: public)")]
 store-memory-handler)

(define-tool
 search-memory
 #:description
 "Search stored memories by query, scope, type, or tags. Defaults to project scope when project root exists, else session."
 #:required ("query")
 #:properties
 [(query "string" "Search query text")
  (scope "string" "Filter by scope: session, project, or user (user disabled by default)")
  (types "array" "Filter by memory types")
  (tags "array" "Filter by tags")
  (limit "integer" "Maximum results (default: 5, max: 20)")]
 search-memory-handler)

(define-tool delete-memory
             #:description "Delete a stored memory item by ID after scoped visibility preflight."
             #:required ("id")
             #:properties
             [(id "string" "The memory item ID to delete")
              (scope "string" "Scope of the item (defaults safely; user disabled by default)")]
             delete-memory-handler)

(define-tool
 list-memory
 #:description
 "List stored memories, optionally filtered by scope and type. Defaults to project scope when project root exists, else session."
 #:required ()
 #:properties
 [(scope "string" "Filter by scope: session, project, or user (user disabled by default)")
  (types "array" "Filter by memory types")
  (limit "integer" "Maximum results (default: 5, max: 20)")]
 list-memory-handler)

(define-tool
 clear-memory
 #:description
 "Delete ALL memories in a given scope. Destructive — requires confirm=true. Use list_memory first to review."
 #:required ("scope" "confirm")
 #:properties [(scope "string" "Scope to clear: session, project, or user (user disabled by default)")
               (confirm "boolean" "Must be true to confirm destructive operation")]
 clear-memory-handler)

(define-tool
 update-memory
 #:description
 "Update an existing memory item by ID. Supports updating content, type, tags, sensitivity, and supersedes."
 #:required ("id")
 #:properties [(id "string" "The memory item ID to update")
               (content "string" "New content for the memory item")
               (type "string" "New type: episodic, semantic, or procedural")
               (tags "array" "New list of string tags")
               (sensitivity "string" "New sensitivity: public, internal, or sensitive")
               (supersedes "array" "List of memory IDs this item supersedes")
               (scope "string" "Scope context for visibility check")]
 update-memory-handler)

(define-tool
 cleanup-expired-memory
 #:description
 "Clean up expired memory items. Use dry-run=true first to inspect, then confirm=true to delete."
 #:required ("scope")
 #:properties [(scope "string" "Scope to clean: session, project, or user")
               (dry-run "boolean" "If true, only report what would be deleted (no deletion)")
               (confirm "boolean" "Must be true to confirm deletion of expired items")]
 cleanup-expired-handler)

(define-tool
 consolidate-memory
 #:description
 "Consolidate multiple memory items into a single merged item. The merged item supersedes the originals."
 #:required ("ids" "merged-content")
 #:properties
 [(ids "array" "List of memory item IDs to consolidate (minimum 2)")
  (merged-content "string" "The merged content for the new consolidated item")
  (scope "string" "Scope context for visibility check: session, project, or user")
  (target-type "string" "Type for merged item: semantic (default), episodic, or procedural")
  (target-scope "string" "Scope for merged item: defaults to input scope")
  (keep-originals? "boolean"
                   "If true (default), keep original items. If false, delete them after merge.")]
 consolidate-memory-handler)

(provide store-memory
         search-memory
         delete-memory
         list-memory
         clear-memory
         update-memory
         cleanup-expired-memory
         consolidate-memory)
