#lang racket/base

;; tools/registry-table/skill-tools.rkt — Skill-related built-in tool specs
;; STABILITY: internal

(require racket/base
         "spec.rkt"
         "../tool.rkt"
         "../builtins/spawn-subagent.rkt"
         "../builtins/skill-router.rkt"
         "../builtins/session-recall.rkt"
         "../builtins/save-conclusion.rkt"
         "../builtins/set-task-state.rkt"
         "../builtins/record-conclusion.rkt"
         "../builtins/delete-lines.rkt")

(provide skill-tool-specs)

(define skill-tool-specs
  (list
   ;; save-conclusion
   (tool-spec
    "save-conclusion"
    "Save a distilled insight or conclusion about the current task. Use after discovering important facts, making decisions, identifying patterns, finding error causes, or getting test results."
    (hasheq
     'type
     "object"
     'required
     '("content")
     'properties
     (hasheq 'content
             (hasheq 'type "string" 'description "The conclusion text")
             'category
             (hasheq 'type
                     "string"
                     'description
                     "Category: fact, decision, pattern, error-cause, test-result (default: fact)")
             'tags
             (hasheq 'type
                     "array"
                     'description
                     "List of relevance tags (symbols) for state-aware filtering")))
    tool-save-conclusion
    #f)
   ;; record_conclusion
   (tool-spec
    "record_conclusion"
    (string-append
     "Record a distilled insight or conclusion about the current task. "
     "Use after discovering important facts, making decisions, identifying patterns, "
     "finding error causes, or getting test results. "
     "Conclusions replace raw file contents in the prompt when the agent moves past exploration, "
     "enabling context optimization.")
    (hasheq
     'type
     "object"
     'required
     '("text")
     'properties
     (hasheq 'text
             (hasheq 'type "string" 'description "The conclusion text")
             'category
             (hasheq 'type
                     "string"
                     'description
                     "Category: fact, decision, pattern, error-cause, test-result (default: fact)")
             'tags
             (hasheq 'type "array" 'description "List of relevance tags for state-aware filtering")))
    tool-record_conclusion
    #f)
   ;; set-task-state
   (tool-spec
    "set-task-state"
    "Transition the current task state. Valid states: idle, exploration, planning, implementation, verification, debugging."
    (hasheq
     'type
     "object"
     'required
     '("state" "event")
     'properties
     (hasheq
      'state
      (hasheq 'type
              "string"
              'description
              "Target state: idle, exploration, planning, implementation, verification, debugging")
      'event
      (hasheq
       'type
       "string"
       'description
       "Transition event: begin-explore, begin-plan, begin-implement, begin-verify, begin-debug, task-complete, revisit, force-transition")))
    tool-set-task-state
    #f)
   ;; spawn-subagent
   (tool-spec
    "spawn-subagent"
    "Spawn an isolated child agent to execute a delegated task"
    (hasheq 'type
            "object"
            'required
            '("task")
            'properties
            (hasheq 'task
                    (hasheq 'type "string" 'description "Task description for the child agent")
                    'role
                    (hasheq 'type "string" 'description "Role prompt to load")
                    'model
                    (hasheq 'type "string" 'description "Model override for child")
                    'max-turns
                    (hasheq 'type "integer" 'description "Max turns (default 5)")
                    'tools
                    (hasheq 'type
                            "array"
                            'items
                            (hasheq 'type "string")
                            'description
                            "Allowed tool names for child")))
    tool-spawn-subagent
    #f)
   ;; spawn-subagents
   (tool-spec
    "spawn-subagents"
    "Run multiple subagent tasks in parallel with bounded concurrency."
    (hasheq
     'type
     "object"
     'required
     '("jobs")
     'properties
     (hasheq
      'jobs
      (hasheq 'type
              "array"
              'description
              "Array of job objects, each with task (required), role, model, max-turns, jobId"
              'items
              (hasheq 'type "object"))
      'maxParallel
      (hasheq 'type "integer" 'description "Max concurrent subagents (1-3, default 3)")
      'aggregate
      (hasheq 'type "boolean" 'description "Include aggregated summary in response (default true)")))
    tool-spawn-subagents
    #f)
   ;; session_recall
   (tool-spec
    "session_recall"
    (string-append
     "Retrieve earlier session entries that are not in your current context. "
     "Use the Session History Catalog to find relevant entry IDs. "
     "Provide one of: entry_ids (list of IDs), query (text search), or range (from/to IDs). "
     "Returns up to 5 entries per call. Results are ephemeral — only in context for this turn.")
    (hasheq 'type
            "object"
            'required
            '()
            'properties
            (hasheq 'entry_ids
                    (hasheq 'type
                            "array"
                            'items
                            (hasheq 'type "string")
                            'description
                            "Entry IDs to retrieve from session history")
                    'query
                    (hasheq 'type "string" 'description "Text search query to find relevant entries")
                    'range
                    (hasheq 'type
                            "object"
                            'properties
                            (hasheq 'from
                                    (hasheq 'type "string" 'description "Start entry ID")
                                    'to
                                    (hasheq 'type "string" 'description "End entry ID"))
                            'description
                            "Retrieve a range of entries by ID (inclusive)")))
    tool-session-recall
    #f)
   ;; skill-route
   (tool-spec
    "skill-route"
    "Discover, search, and load skill instructions by name or description."
    (hasheq
     'type
     "object"
     'required
     '()
     'properties
     (hasheq
      'action
      (hasheq 'type
              "string"
              'description
              "Action: list (all skills), match (search by query), load (full content by name)")
      'query
      (hasheq 'type "string" 'description "Search query for match action")
      'name
      (hasheq 'type "string" 'description "Skill name for load action")))
    tool-skill-route
    #f)
   ;; delete-lines
   (tool-spec
    "delete-lines"
    "Delete a range of lines from a file by line number. Use instead of edit for removing 3+ consecutive lines."
    (hasheq 'type
            "object"
            'required
            '("path" "start-line" "end-line")
            'properties
            (hasheq 'path
                    (hasheq 'type "string" 'description "Path to file")
                    'start-line
                    (hasheq 'type "integer" 'description "1-based start line (inclusive)")
                    'end-line
                    (hasheq 'type "integer" 'description "1-based end line (inclusive)")))
    tool-delete-lines
    "Prefer delete-lines over edit for removing 3+ consecutive lines. Read the file first to identify exact line numbers.")))
