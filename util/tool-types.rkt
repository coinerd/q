#lang racket/base

;; util/tool-types.rkt — standalone tool-call and tool-result structs
;;
;; Canonical definitions for tool-call and tool-result data types.
;; These are shared across the tool execution pipeline.

(provide (struct-out tool-call)
         (struct-out tool-result))

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
