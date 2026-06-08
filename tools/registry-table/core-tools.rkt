#lang racket/base

;; tools/registry-table/core-tools.rkt — Core built-in tool specs
;; STABILITY: internal

(require "../tool.rkt"
         "../builtins/read.rkt"
         "../builtins/write.rkt"
         "../builtins/edit.rkt"
         "../builtins/delete-lines.rkt"
         "../builtins/bash.rkt"
         "../builtins/grep.rkt"
         "../builtins/find.rkt"
         "../builtins/ls.rkt"
         "../builtins/date.rkt")

(provide core-tool-specs)

;; Re-export tool-spec struct from parent (imported via ../tool.rkt's re-export)
(require (only-in "spec.rkt" tool-spec))

(define core-tool-specs
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
    #f)))
