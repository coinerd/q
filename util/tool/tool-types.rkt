#lang racket/base
;; STABILITY: public

;; util/tool-types.rkt -- standalone tool-call and tool-result structs + tool? predicate
;;
;; Canonical definitions for tool data types.
;; tool? re-exported from tools/tool-struct.rkt (A-2: avoids agent->tools layer violation).

(require racket/contract
         (only-in "../../tools/tool-struct.rkt" tool?))

(provide (contract-out [tool? (-> any/c boolean?)]
                       [tool-call? (-> any/c boolean?)]
                       [tool-call-id (-> tool-call? (or/c string? #f))]
                       [tool-call-name (-> tool-call? (or/c string? #f))]
                       [tool-call-arguments (-> tool-call? any/c)]
                       [make-tool-call (-> (or/c string? #f) (or/c string? #f) any/c tool-call?)]
                       [tool-result? (-> any/c boolean?)]
                       [tool-result-content (-> tool-result? (or/c string? hash? list?))]
                       [tool-result-details (-> tool-result? (or/c hash? #f))]
                       [tool-result-is-error? (-> tool-result? boolean?)]
                       [make-tool-result
                        (-> (or/c string? hash? list?) (or/c hash? #f) boolean? tool-result?)])
         tool-call
         tool-result)

(struct tool-call (id name arguments) #:transparent #:extra-constructor-name make-tool-call)

(struct tool-result (content details is-error?)
  #:transparent
  #:extra-constructor-name make-tool-result)
