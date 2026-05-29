#lang racket/base

(require racket/contract)
;; tools/tool-struct.rkt -- Tool struct definition
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(provide (contract-out [tool? (-> any/c boolean?)]
                       [tool-name (-> tool? string?)]
                       [tool-description (-> tool? string?)]
                       [tool-schema (-> tool? hash?)]
                       [tool-execute (-> tool? procedure?)]
                       [tool-prompt-snippet (-> tool? (or/c string? #f))]
                       [tool-prompt-guidelines (-> tool? (or/c string? #f))]
                       [tool-dangerous? (-> tool? boolean?)]
                       [tool-render-call (-> tool? (or/c procedure? #f))]
                       [tool-render-result (-> tool? (or/c procedure? #f))]
                       [tool-timeout-seconds (-> tool? (or/c exact-nonnegative-integer? #f))])
         ;; Raw struct constructor -- ONLY for use by tools/tool.rkt make-tool.
         ;; All external construction MUST use make-tool from tools/tool.rkt.
         tool)

(struct tool
        (name description
              schema
              execute
              prompt-snippet
              prompt-guidelines
              render-call
              render-result
              dangerous?
              timeout-seconds)
  #:transparent)
