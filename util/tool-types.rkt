#lang racket/base

;; util/tool-types.rkt — standalone tool-call and tool-result structs
;;
;; Canonical definitions for tool-call and tool-result data types.
;; These are shared across the tool execution pipeline.

(require racket/contract)

(provide (contract-out [tool-call? (-> any/c boolean?)]
                       [tool-call-id (-> tool-call? any/c)]
                       [tool-call-name (-> tool-call? any/c)]
                       [tool-call-arguments (-> tool-call? any/c)]
                       [make-tool-call (-> any/c any/c any/c tool-call?)]
                       [tool-result? (-> any/c boolean?)]
                       [tool-result-content (-> tool-result? any/c)]
                       [tool-result-details (-> tool-result? any/c)]
                       [tool-result-is-error? (-> tool-result? any/c)]
                       [make-tool-result (-> any/c any/c any/c tool-result?)])
         tool-call
         tool-result)

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
