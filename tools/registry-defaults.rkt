#lang racket/base

;; tools/registry-defaults.rkt — Register built-in tools into a tool registry
;;
;; Extracted from main.rkt. Contains register-default-tools! which
;; registers all 9 built-in tools (read, write, edit, bash, grep,
;; find, ls, date, firecrawl) into the given registry.

(require "tool.rkt"
         "builtins/read.rkt"
         "builtins/write.rkt"
         "builtins/edit.rkt"
         "builtins/bash.rkt"
         "builtins/grep.rkt"
         "builtins/find.rkt"
         "builtins/ls.rkt"
         "builtins/date.rkt"
         "builtins/firecrawl.rkt")

(provide register-default-tools!)

;; Register the built-in tools into the given tool registry.
;; #:only — optional list of tool name strings to register; #f means all.
(define (register-default-tools! registry #:only [only #f])
  (define (should-register? name)
    (or (not only) (member name only)))
  (when (should-register? "read")
    (register-tool! registry
      (make-tool "read"
                 "Read file contents"
                 (hasheq 'type "object"
                         'required '("path")
                         'properties (hasheq 'path (hasheq 'type "string"
                                                            'description "Path to file to read")
                                             'offset (hasheq 'type "integer"
                                                             'description "Line offset (1-indexed)")
                                             'limit (hasheq 'type "integer"
                                                            'description "Max lines to return")))
                 tool-read)))
  (when (should-register? "write")
    (register-tool! registry
      (make-tool "write"
                 "Write content to a file"
                 (hasheq 'type "object"
                         'required '("path" "content")
                         'properties (hasheq 'path (hasheq 'type "string"
                                                            'description "Path to file to write")
                                             'content (hasheq 'type "string"
                                                              'description "Content to write")))
                 tool-write)))
  (when (should-register? "edit")
    (register-tool! registry
      (make-tool "edit"
                 "Edit a file with exact text replacement"
                 (hasheq 'type "object"
                         'required '("path" "old-text" "new-text")
                         'properties (hasheq 'path (hasheq 'type "string"
                                                            'description "Path to file to edit")
                                             'old-text (hasheq 'type "string"
                                                               'description "Exact text to find")
                                             'new-text (hasheq 'type "string"
                                                               'description "Replacement text")))
                 tool-edit)))
  (when (should-register? "bash")
    (register-tool! registry
      (make-tool "bash"
                 "Execute a shell command"
                 (hasheq 'type "object"
                         'required '("command")
                         'properties (hasheq 'command (hasheq 'type "string"
                                                                'description "Shell command to run")
                                             'timeout (hasheq 'type "number"
                                                              'description "Timeout in seconds")
                                             'working-directory (hasheq 'type "string"
                                                                        'description "Working directory")))
                 tool-bash)))
  (when (should-register? "grep")
    (register-tool! registry
      (make-tool "grep"
                 "Search for text patterns in files"
                 (hasheq 'type "object"
                         'required '("pattern" "path")
                         'properties (hasheq 'pattern (hasheq 'type "string"
                                                                'description "Regex pattern to search for")
                                             'path (hasheq 'type "string"
                                                           'description "File or directory to search")
                                             'glob (hasheq 'type "string"
                                                           'description "File glob filter")
                                             'case-insensitive? (hasheq 'type "boolean"
                                                                        'description "Case insensitive match")
                                             'max-results (hasheq 'type "integer"
                                                                  'description "Max matches")
                                             'context-lines (hasheq 'type "integer"
                                                                    'description "Context lines around match")))
                 tool-grep)))
  (when (should-register? "find")
    (register-tool! registry
      (make-tool "find"
                 "Find files and directories by name or type"
                 (hasheq 'type "object"
                         'required '("path")
                         'properties (hasheq 'path (hasheq 'type "string"
                                                           'description "Root directory")
                                             'name (hasheq 'type "string"
                                                           'description "Filename glob pattern")
                                             'type (hasheq 'type "string"
                                                          'description "\"file\", \"dir\", or \"any\"")
                                             'max-depth (hasheq 'type "integer"
                                                                'description "Max recursion depth")
                                             'max-results (hasheq 'type "integer"
                                                                  'description "Max results")))
                 tool-find)))
  (when (should-register? "ls")
    (register-tool! registry
      (make-tool "ls"
                 "List directory contents"
                 (hasheq 'type "object"
                         'required '("path")
                         'properties (hasheq 'path (hasheq 'type "string"
                                                           'description "Directory to list")
                                             'all? (hasheq 'type "boolean"
                                                          'description "Show hidden files")
                                             'long? (hasheq 'type "boolean"
                                                          'description "Long format with size/type")
                                             'sort-by (hasheq 'type "string"
                                                              'description "\"name\", \"size\", or \"date\"")))
                 tool-ls)))

  (when (should-register? "date")
    (register-tool! registry
      (make-tool "date"
                 "Returns the current date and time. Use this tool to learn today's date before answering time-dependent questions."
                 (hasheq 'type "object"
                         'required '()
                         'properties (hasheq 'format (hasheq 'type "string"
                                                             'description "Output format: iso (default), date, time, unix, weekday, iso-full")))
                 tool-date)))

  ;; Firecrawl web search tool (requires FIRECRAWL_API_KEY env var or config)
  (when (should-register? "firecrawl")
    (register-tool! registry
      (make-tool "firecrawl"
                 "Search the web, scrape/crawl/map websites via Firecrawl. Actions: search (web search with query), scrape (extract
                       content from URL), crawl (crawl multiple pages from URL), map (list all URLs at a site). Returns markdown by default.
                              Requires FIRECRAWL_API_KEY environment variable."
                 (hasheq 'type "object"
                         'required '("action")
                         'properties (hasheq 'action (hasheq 'type "string"
                                                                    'description "Action to perform: search, scrape, crawl, or map")
                                             'query (hasheq 'type "string"
                                                            'description "Search query (for search action)")
                                             'url (hasheq 'type "string"
                                                          'description "URL to scrape, crawl, or map")
                                             'formats (hasheq 'type "array"
                                                                      'items (hasheq 'type "string")
                                                                      'description "Output formats: markdown (default), html, rawHtml")
                                             'limit (hasheq 'type "integer"
                                                            'description "Max results for search/crawl (default 5)")
                                             'onlyMainContent (hasheq 'type "boolean"
                                                                      'description "Extract main content only (default true)")
                                             'timeout (hasheq 'type "integer"
                                                              'description "Timeout in seconds for crawl polling (default 30)")))
                 tool-firecrawl)))
  (void))
