#lang racket/base

;; extensions/github/tool-schemas.rkt — GitHub tool JSON schemas
;;
;; Extracted from github-integration.rkt for modularity.
;; Provides the 6 tool schema definitions used by the GitHub integration.

(provide gh-issue-schema
         gh-pr-schema
         gh-milestone-schema
         gh-board-schema
         gh-wave-start-schema
         gh-wave-finish-schema)

;; ============================================================
;; Tool schemas
;; ============================================================

(define gh-issue-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "Issue action: create, close, update, get, list, close_tree")
    'title
    (hasheq 'type "string" 'description "Issue title (create/update)")
    'body
    (hasheq 'type "string" 'description "Issue body text (create/update)")
    'number
    (hasheq 'type "number" 'description "Issue number")
    'labels
    (hasheq 'type "array" 'items (hasheq 'type "string") 'description "Label names (create)")
    'state
    (hasheq 'type "string" 'description "State filter for list: open, closed, all (default: open)")
    'milestone_number
    (hasheq 'type "number" 'description "Milestone number filter (list)")
    'milestone_id
    (hasheq 'type "string" 'description "Milestone GraphQL ID (create)")
    'board_fields
    (hasheq 'type "object" 'description "Project board fields to set (create)"))
   'required
   '("action")))

(define gh-pr-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "PR action: create, merge, list, get")
    'title
    (hasheq 'type "string" 'description "PR title (create)")
    'body
    (hasheq 'type "string" 'description "PR body text (create)")
    'head
    (hasheq 'type "string" 'description "Head branch name (create)")
    'base
    (hasheq 'type "string" 'description "Base branch name (create, default: main)")
    'number
    (hasheq 'type "number" 'description "PR number (merge/get)")
    'method
    (hasheq 'type "string" 'description "Merge method: squash, merge, rebase (default: squash)")
    'commit_title
    (hasheq 'type "string" 'description "Squash commit title (merge)")
    'state
    (hasheq 'type "string" 'description "State filter for list: open, closed, all (default: open)"))
   'required
   '("action")))

(define gh-milestone-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type "string" 'description "Milestone action: create, close, list, create_from_spec")
    'title
    (hasheq 'type "string" 'description "Milestone title (create)")
    'description
    (hasheq 'type "string" 'description "Milestone description (create)")
    'due_on
    (hasheq 'type "string" 'description "Due date ISO timestamp (create)")
    'number
    (hasheq 'type "number" 'description "Milestone number (close)")
    'state
    (hasheq 'type "string" 'description "State filter: open, closed, all (list, default: open)")
    'spec_file
    (hasheq 'type "string" 'description "Path to JSON spec file (create_from_spec)")
    'dry_run
    (hasheq 'type "boolean" 'description "Preview only, do not create (create_from_spec)"))
   'required
   '("action")))

(define gh-board-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq
    'action
    (hasheq 'type
            "string"
            'description
            "Board action: status, stale, autofix, verify, batch_set, reconfigure")
    'milestone_number
    (hasheq 'type "number" 'description "Milestone number (status/stale/autofix/verify)")
    'issue_numbers
    (hasheq 'type "array" 'items (hasheq 'type "number") 'description "Issue numbers (batch_set)")
    'issue_number
    (hasheq 'type "number" 'description "Single issue number (reconfigure)")
    'fields
    (hasheq 'type "object" 'description "Board fields to set: status, priority, area, effort, risk"))
   'required
   '("action")))

(define gh-wave-start-schema
  (hasheq
   'type
   "object"
   'properties
   (hasheq 'issue_number
           (hasheq 'type "number" 'description "GitHub issue number for the wave parent")
           'branch_name
           (hasheq 'type "string" 'description "Override branch name (default: auto-generated)"))
   'required
   '("issue_number")))

(define gh-wave-finish-schema
  (hasheq 'type
          "object"
          'properties
          (hasheq 'issue_number
                  (hasheq 'type "number" 'description "GitHub issue number for the wave parent")
                  'files
                  (hasheq 'type
                          "array"
                          'items
                          (hasheq 'type "string")
                          'description
                          "File paths to commit (relative to repo root)")
                  'commit_msg
                  (hasheq 'type "string" 'description "Commit message")
                  'branch
                  (hasheq 'type "string" 'description "Branch name (default: auto-detected)")
                  'pr_title
                  (hasheq 'type "string" 'description "PR title override (default: commit message)")
                  'pr_body
                  (hasheq 'type "string" 'description "PR body override (default: references issue)")
                  'close_sub_issues
                  (hasheq 'type "boolean" 'description "Close sub-issues (default: true)"))
          'required
          '("issue_number" "files" "commit_msg")))
