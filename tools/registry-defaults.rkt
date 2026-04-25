#lang racket/base

;; tools/registry-defaults.rkt — Register built-in tools into a tool registry
;;
;; Extracted from main.rkt. Contains register-default-tools! which
;; registers all 13 built-in tools (read, write, edit, bash, grep,
;; find, ls, date, firecrawl, spawn-subagent, spawn-subagents,
;; session_recall, skill-route) into the given registry.

(require "tool.rkt"
         "builtins/read.rkt"
         "builtins/write.rkt"
         "builtins/edit.rkt"
         "builtins/bash.rkt"
         "builtins/grep.rkt"
         "builtins/find.rkt"
         "builtins/ls.rkt"
         "builtins/date.rkt"
         "builtins/firecrawl.rkt"
         "builtins/spawn-subagent.rkt"
         "builtins/session-recall.rkt"
         "builtins/skill-router.rkt")

(provide register-default-tools!)

;; Register the built-in tools into the given tool registry.
;; #:only — optional list of tool name strings to register; #f means all.
(define (register-default-tools! registry #:only [only #f])
  (define (should-register? name)
    (or (not only) (member name only)))
  (when (should-register? "read")
    (register-tool!
     registry
     (make-tool
      "read"
      "Read file contents"
      (hasheq 'type
              "object"
              'required
              '("path")
              'properties
              (hasheq 'path
                      (hasheq 'type "string" 'description "Path to file to read")
                      'offset
                      (hasheq 'type "integer" 'description "Line offset (1-indexed)")
                      'limit
                      (hasheq 'type "integer" 'description "Max lines to return")))
      tool-read
      #:prompt-guidelines
      "When making parallel read calls, include the 'path' parameter in EVERY call. Never pass a bare line number without a path.")))
  (when (should-register? "write")
    (register-tool!
     registry
     (make-tool "write"
                "Write content to a file"
                (hasheq 'type
                        "object"
                        'required
                        '("path" "content")
                        'properties
                        (hasheq 'path
                                (hasheq 'type "string" 'description "Path to file to write")
                                'content
                                (hasheq 'type "string" 'description "Content to write")))
                tool-write)))
  (when (should-register? "edit")
    (register-tool!
     registry
     (make-tool
      "edit"
      "Edit a file by replacing exact text. old-text MUST be copied verbatim from a prior read result — do not guess."
      (hasheq
       'type
       "object"
       'required
       '("path" "old-text" "new-text")
       'properties
       (hasheq
        'path
        (hasheq 'type "string" 'description "Path to file to edit")
        'old-text
        (hasheq 'type
                "string"
                'description
                "EXACT text to replace, copied verbatim from a read result. Do not type from memory.")
        'new-text
        (hasheq 'type "string" 'description "Replacement text")))
      tool-edit
      #:prompt-guidelines
      (string-append
       "IMPORTANT: old-text must match the file content exactly. Copy it verbatim from a prior "
       "read tool result. If edit fails, re-read the file first. "
       "Keep old-text short — ideally under 500 chars (~20 lines). "
       "If you need to change a large block, split into multiple smaller edits."))))
  (when (should-register? "bash")
    (register-tool!
     registry
     (make-tool "bash"
                "Execute a shell command"
                (hasheq 'type
                        "object"
                        'required
                        '("command")
                        'properties
                        (hasheq 'command
                                (hasheq 'type "string" 'description "Shell command to run")
                                'timeout
                                (hasheq 'type "number" 'description "Timeout in seconds")
                                'working-directory
                                (hasheq 'type "string" 'description "Working directory")))
                tool-bash)))
  (when (should-register? "grep")
    (register-tool!
     registry
     (make-tool "grep"
                "Search for text patterns in files"
                (hasheq 'type
                        "object"
                        'required
                        '("pattern" "path")
                        'properties
                        (hasheq 'pattern
                                (hasheq 'type "string" 'description "Regex pattern to search for")
                                'path
                                (hasheq 'type "string" 'description "File or directory to search")
                                'glob
                                (hasheq 'type "string" 'description "File glob filter")
                                'case-insensitive?
                                (hasheq 'type "boolean" 'description "Case insensitive match")
                                'max-results
                                (hasheq 'type "integer" 'description "Max matches")
                                'context-lines
                                (hasheq 'type "integer" 'description "Context lines around match")))
                tool-grep)))
  (when (should-register? "find")
    (register-tool!
     registry
     (make-tool "find"
                "Find files and directories by name or type"
                (hasheq 'type
                        "object"
                        'required
                        '("path")
                        'properties
                        (hasheq 'path
                                (hasheq 'type "string" 'description "Root directory")
                                'name
                                (hasheq 'type "string" 'description "Filename glob pattern")
                                'type
                                (hasheq 'type "string" 'description "\"file\", \"dir\", or \"any\"")
                                'max-depth
                                (hasheq 'type "integer" 'description "Max recursion depth")
                                'max-results
                                (hasheq 'type "integer" 'description "Max results")))
                tool-find)))
  (when (should-register? "ls")
    (register-tool!
     registry
     (make-tool
      "ls"
      "List directory contents"
      (hasheq 'type
              "object"
              'required
              '("path")
              'properties
              (hasheq 'path
                      (hasheq 'type "string" 'description "Directory to list")
                      'all?
                      (hasheq 'type "boolean" 'description "Show hidden files")
                      'long?
                      (hasheq 'type "boolean" 'description "Long format with size/type")
                      'sort-by
                      (hasheq 'type "string" 'description "\"name\", \"size\", or \"date\"")))
      tool-ls)))

  (when (should-register? "date")
    (register-tool!
     registry
     (make-tool
      "date"
      "Returns the current date and time. Use this tool to learn today's date before answering time-dependent questions."
      (hasheq 'type
              "object"
              'required
              '()
              'properties
              (hasheq 'format
                      (hasheq 'type
                              "string"
                              'description
                              "Output format: iso (default), date, time, unix, weekday, iso-full")))
      tool-date)))

  ;; Firecrawl web search tool (requires FIRECRAWL_API_KEY env var or config)
  (when (should-register? "firecrawl")
    (register-tool!
     registry
     (make-tool
      "firecrawl"
      "Search the web, scrape/crawl/map websites via Firecrawl. Actions: search (web search with query), scrape (extract
                       content from URL), crawl (crawl multiple pages from URL), map (list all URLs at a site). Returns markdown by default.
                              Requires FIRECRAWL_API_KEY environment variable."
      (hasheq
       'type
       "object"
       'required
       '("action")
       'properties
       (hasheq
        'action
        (hasheq 'type "string" 'description "Action to perform: search, scrape, crawl, or map")
        'query
        (hasheq 'type "string" 'description "Search query (for search action)")
        'url
        (hasheq 'type "string" 'description "URL to scrape, crawl, or map")
        'formats
        (hasheq 'type
                "array"
                'items
                (hasheq 'type "string")
                'description
                "Output formats: markdown (default), html, rawHtml")
        'limit
        (hasheq 'type "integer" 'description "Max results for search/crawl (default 5)")
        'onlyMainContent
        (hasheq 'type "boolean" 'description "Extract main content only (default true)")
        'timeout
        (hasheq 'type "integer" 'description "Timeout in seconds for crawl polling (default 30)")))
      tool-firecrawl)))
  (when (should-register? "spawn-subagent")
    (register-tool!
     registry
     (make-tool
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
      tool-spawn-subagent)))

  ;; spawn-subagents: batch parallel subagent execution
  (when (should-register? "spawn-subagents")
    (register-tool!
     registry
     (make-tool
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
        (hasheq 'type
                "boolean"
                'description
                "Include aggregated summary in response (default true)")))
      tool-spawn-subagents)))

  ;; Session recall tool — retrieve earlier session entries by ID, query, or range (#1391)
  (when (should-register? "session_recall")
    (register-tool!
     registry
     (make-tool
      "session_recall"
      (string-append
       "Retrieve earlier session entries that are not in your current context. "
       "Use the Session History Catalog to find relevant entry IDs. "
       "Provide one of: entry_ids (list of IDs), query (text search), or range (from/to IDs). "
       "Returns up to 5 entries per call. Results are ephemeral — only in context for this turn.")
      (hasheq
       'type
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
      tool-session-recall)))

  ;; skill-route: discover and search skills by description match
  (when (should-register? "skill-route")
    (register-tool!
     registry
     (make-tool
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
      tool-skill-route)))

  (void))
