#lang racket/base
;; tools/tool-struct.rkt — Tool struct definition
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(provide tool?
         tool
         tool-name
         tool-description
         tool-schema
         ;; NOTE: tool-execute is exported for test/internal use only.
         ;;       Production code should use tools/scheduler.rkt for invocation.
         ;;       A future breaking change may gate this behind a test-only module.
         tool-execute
         tool-prompt-snippet
         tool-prompt-guidelines
         tool-dangerous?
         tool-render-call
         tool-render-result)

(struct tool
        (name description
              schema
              execute
              prompt-snippet
              prompt-guidelines
              render-call
              render-result
              dangerous?)
  #:transparent)
