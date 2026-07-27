#lang racket/base

(require racket/contract)
;; tools/tool-struct.rkt -- Tool struct definition
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable
;;
;; W0 (v0.99.72): Removed raw `tool` constructor from public exports.
;; External modules must use make-tool from tools/tool.rkt.
;; The constructor is available via (submod "tool-struct.rkt" internal).

(provide (contract-out [tool? (-> any/c boolean?)]
                       [tool-name (-> tool? string?)]
                       [tool-description (-> tool? string?)]
                       [tool-schema (-> tool? hash?)]
                       [tool-execute (-> tool? procedure?)]
                       [tool-prompt-snippet (-> tool? (or/c string? #f))]
                       [tool-prompt-guidelines (-> tool? (or/c string? #f))]
                       [tool-dangerous? (-> tool? boolean?)]
                       [tool-mutates-filesystem? (-> tool? boolean?)]
                       [tool-render-call (-> tool? (or/c procedure? #f))]
                       [tool-render-result (-> tool? (or/c procedure? #f))]
                       [tool-timeout-seconds (-> tool? (or/c exact-nonnegative-integer? #f))]
                       [tool-required-capability (-> tool? symbol?)]
                       [tool-externalizable? (-> tool? boolean?)]))

(struct tool
        (name description
              schema
              execute
              prompt-snippet
              prompt-guidelines
              render-call
              render-result
              dangerous?
              mutates-filesystem?
              timeout-seconds
              required-capability
              externalizable?)
  #:transparent)

;; Internal submodule -- exports the raw tool constructor for make-tool only.
(module+ internal
  (provide tool))
