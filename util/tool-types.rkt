#lang racket/base

;; util/tool-types.rkt — standalone tool-call and tool-result structs
;;
;; Canonical definitions for tool-call and tool-result data types.
;; These are shared across the tool execution pipeline.

(provide tool-call
         tool-call?
         tool-call-id
         tool-call-name
         tool-call-arguments
         make-tool-call
         tool-result
         tool-result?
         tool-result-content
         tool-result-details
         tool-result-is-error?
         make-tool-result)

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
