#lang racket/base

;; util/tool-types.rkt — standalone tool-call and tool-result structs
;;
;; Canonical definitions for tool-call and tool-result data types.
;; These are shared across the tool execution pipeline.

(require racket/contract)

(provide (contract-out [tool-call? (-> any/c boolean?)]
                       [tool-call-id (-> tool-call? (or/c string? #f))]
                       [tool-call-name (-> tool-call? string?)]
                       [tool-call-arguments (-> tool-call? (or/c hash? list?))]
                       [make-tool-call (-> (or/c string? #f) string? (or/c hash? list?) tool-call?)]
                       [tool-result? (-> any/c boolean?)]
                       [tool-result-content (-> tool-result? (or/c string? hash? list?))]
                       [tool-result-details (-> tool-result? (or/c hash? #f))]
                       [tool-result-is-error? (-> tool-result? boolean?)]
                       [make-tool-result (-> (or/c string? hash? list?) (or/c hash? #f) boolean? tool-result?)])
         tool-call
         tool-result)

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
