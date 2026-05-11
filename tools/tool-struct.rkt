#lang racket/base
;; tools/tool-struct.rkt — Tool struct definition
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(provide tool?
         tool
         tool-name
         tool-description
         tool-schema
         tool-execute
         tool-prompt-snippet
         tool-prompt-guidelines
         tool-dangerous?)

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
