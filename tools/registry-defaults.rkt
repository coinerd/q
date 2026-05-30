#lang racket/base

;; tools/registry-defaults.rkt — Register built-in tools into a tool registry
;;
;; Delegates to registry-table.rkt for the declarative spec table.
;; All tool schemas, descriptions, and handlers are defined there.

(require racket/contract
         "registry-table.rkt"
         "registry.rkt")                            ; for tool-registry?

(provide (contract-out [register-default-tools!
                        (->* (tool-registry?) (#:only (or/c (listof string?) #f)) void?)]))

;; Register the built-in tools into the given tool registry.
;; #:only — optional list of tool name strings to register; #f means all.
(define (register-default-tools! registry #:only [only #f])
  (register-tools-from-specs! registry tool-specs #:only only))
