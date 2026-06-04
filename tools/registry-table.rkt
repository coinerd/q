#lang racket/base

;; tools/registry-table.rkt — Declarative tool spec table
;;
;; Encodes all 14 built-in tools as tool-spec structs.
;;
;; register-tools-from-specs! converts specs → make-tool calls.

(require "tool.rkt"
         "builtins/read.rkt"
         "builtins/write.rkt"
         "builtins/edit.rkt"
         "builtins/delete-lines.rkt"
         "builtins/bash.rkt"
         "builtins/grep.rkt"
         "builtins/find.rkt"
         "builtins/ls.rkt"
         "builtins/date.rkt"
         "builtins/firecrawl.rkt"
         "builtins/spawn-subagent.rkt"
         "builtins/session-recall.rkt"
         "builtins/skill-router.rkt"
         "builtins/save-conclusion.rkt"
         "builtins/set-task-state.rkt"
         "builtins/record-conclusion.rkt"
         "builtins/browser-tools.rkt"
         "../browser/workflow.rkt")

;; M-03: Named struct replacing raw list access.
;; Each spec was a list: (name description schema handler [prompt-guidelines])
;; Now a named struct with clear field accessors.
(struct tool-spec (name description schema handler prompt-guidelines) #:transparent)

(provide register-tools-from-specs!
         dangerous-tool-names
         tool-specs
         tool-spec
         tool-spec?
         tool-spec-name
         tool-spec-description
         tool-spec-schema
         tool-spec-handler
         tool-spec-prompt-guidelines)

;; ============================================================
;; Tool spec table
;; ============================================================

;; Each entry is a (tool-spec ...) struct — see tool-specs below.
(define tool-specs
  (list
   ;; read
   (tool-spec
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
    "When making parallel read calls, include the 'path' parameter in EVERY call. Never pass a bare line number without a path.")
   ;; write
   (tool-spec "write"
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
              tool-write
              #f)
   ;; edit
   (tool-spec
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
    (string-append
     "IMPORTANT: old-text must match the file content exactly. Copy it verbatim from a prior "
     "read tool result. If edit fails, re-read the file first. "
     "Keep old-text short — ideally under 500 chars (~20 lines). "
     "If you need to change a large block, split into multiple smaller edits."))
   ;; bash
   (tool-spec "bash"
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
              tool-bash
              #f)
   ;; grep
   (tool-spec "grep"
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
              tool-grep
              #f)
   ;; find
   (tool-spec "find"
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
              tool-find
              #f)
   ;; ls
   (tool-spec "ls"
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
              tool-ls
              #f)
   ;; date
   (tool-spec
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
    tool-date
    #f)
   ;; firecrawl
   (tool-spec
    "firecrawl"
    (string-append "Search the web, scrape/crawl/map websites via Firecrawl. "
                   "Actions: search (web search with query), scrape (extract content from URL), "
                   "crawl (crawl multiple pages from URL), map (list all URLs at a site). "
                   "Returns markdown by default. Requires FIRECRAWL_API_KEY environment variable.")
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
    tool-firecrawl
    #f)
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
   ;; set-task-state
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
    "Prefer delete-lines over edit for removing 3+ consecutive lines. Read the file first to identify exact line numbers.")
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
   ;; ============================================================
   ;; Browser tools (9)
   ;; ============================================================
   ;; browser_open (MEDIUM risk)
   (tool-spec "browser_open"
              "Open a browser session to a URL"
              (hasheq 'type
                      "object"
                      'required
                      '("url")
                      'properties
                      (hasheq 'url
                              (hasheq 'type "string" 'description "URL to open")
                              'viewport_width
                              (hasheq 'type "integer" 'description "Viewport width in pixels")
                              'viewport_height
                              (hasheq 'type "integer" 'description "Viewport height in pixels")))
              handle-browser-open
              "Use browser_open to open web pages. Always close sessions when done.")
   ;; browser_observe (LOW risk)
   (tool-spec
    "browser_observe"
    "Observe current page state: URL, title, text content, console errors"
    (hasheq 'type
            "object"
            'required
            '("session-id")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'selector
                    (hasheq 'type "string" 'description "CSS selector to focus observation")))
    handle-browser-observe
    #f)
   ;; browser_click (HIGH risk)
   (tool-spec
    "browser_click"
    "Click an element on the page"
    (hasheq 'type
            "object"
            'required
            '("session-id" "selector")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'selector
                    (hasheq 'type "string" 'description "CSS selector of element to click")
                    'button
                    (hasheq 'type "string" 'description "Mouse button: left, right, middle")))
    handle-browser-click
    "Clicking may trigger side effects. Prefer observe before click.")
   ;; browser_type (HIGH risk)
   (tool-spec "browser_type"
              "Type text into an input element"
              (hasheq 'type
                      "object"
                      'required
                      '("session-id" "selector" "text")
                      'properties
                      (hasheq 'session-id
                              (hasheq 'type "string" 'description "Browser session ID")
                              'selector
                              (hasheq 'type "string" 'description "CSS selector of input element")
                              'text
                              (hasheq 'type "string" 'description "Text to type")
                              'clear-first?
                              (hasheq 'type "boolean" 'description "Clear existing text first")))
              handle-browser-type
              "Typing submits form data. Verify the correct input before typing.")
   ;; browser_press (HIGH risk)
   (tool-spec
    "browser_press"
    "Press a keyboard key"
    (hasheq 'type
            "object"
            'required
            '("session-id" "key")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'key
                    (hasheq 'type "string" 'description "Key name (e.g. Enter, Tab, Escape)")
                    'modifiers
                    (hasheq 'type "array" 'description "Modifier keys (ctrl, alt, shift, meta)")))
    handle-browser-press
    "Pressing keys may trigger form submissions or page navigation.")
   ;; browser_extract (LOW risk)
   (tool-spec
    "browser_extract"
    "Extract structured data from a page element"
    (hasheq
     'type
     "object"
     'required
     '("session-id" "selector")
     'properties
     (hasheq 'session-id
             (hasheq 'type "string" 'description "Browser session ID")
             'selector
             (hasheq 'type "string" 'description "CSS selector of element to extract from")
             'extract-type
             (hasheq 'type "string" 'description "Extraction type: text, html, accessibility")))
    handle-browser-extract
    #f)
   ;; browser_screenshot (LOW risk)
   (tool-spec
    "browser_screenshot"
    "Take a screenshot of the current page"
    (hasheq
     'type
     "object"
     'required
     '("session-id")
     'properties
     (hasheq 'session-id
             (hasheq 'type "string" 'description "Browser session ID")
             'selector
             (hasheq 'type "string" 'description "CSS selector to screenshot specific element")))
    handle-browser-screenshot
    #f)
   ;; browser_scroll (LOW risk)
   (tool-spec
    "browser_scroll"
    "Scroll the page"
    (hasheq 'type
            "object"
            'required
            '("session-id")
            'properties
            (hasheq 'session-id
                    (hasheq 'type "string" 'description "Browser session ID")
                    'direction
                    (hasheq 'type "string" 'description "Scroll direction: up or down")
                    'amount
                    (hasheq 'type "integer" 'description "Number of viewport heights to scroll")))
    handle-browser-scroll
    #f)
   ;; browser_close (LOW risk)
   (tool-spec "browser_close"
              "Close a browser session"
              (hasheq 'type
                      "object"
                      'required
                      '("session-id")
                      'properties
                      (hasheq 'session-id
                              (hasheq 'type "string" 'description "Browser session ID to close")))
              handle-browser-close
              "Always close browser sessions when done to free resources.")
   ;; browser_check_local_app (MEDIUM risk — read-only navigation)
   (tool-spec "browser_check_local_app"
              "Quick health check for a local web app: open, screenshot, extract, close."
              (hasheq 'type
                      "object"
                      'required
                      '("url")
                      'properties
                      (hasheq 'url
                              (hasheq 'type "string" 'description "Local app URL")
                              'timeout_ms
                              (hasheq 'type "integer" 'description "Timeout ms")
                              'selector
                              (hasheq 'type "string" 'description "CSS selector")
                              'screenshot
                              (hasheq 'type "boolean" 'description "Capture screenshot")))
              handle-browser-check-local-app
              "Quick local app health check.")))

;; ============================================================
;; Registration from specs
;; ============================================================

;; R-03/R-22: Metadata-driven dangerous tool classification
(define dangerous-tool-names
  '("write" "edit" "bash" "delete-lines" "browser_click" "browser_type" "browser_press"))

;; Register tools from tool-spec structs.
(define (register-tools-from-specs! registry specs #:only [only #f])
  (for ([spec (in-list specs)])
    (cond
      [(tool-spec? spec)
       (define name (tool-spec-name spec))
       (when (or (not only) (member name only))
         (define pg (tool-spec-prompt-guidelines spec))
         (define dangerous? (and (member name dangerous-tool-names) #t))
         (if pg
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:prompt-guidelines pg
                                        #:dangerous? dangerous?))
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:dangerous? dangerous?))))]))
  (void))
