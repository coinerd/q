#lang racket/base

;; tools/registry-defaults.rkt — Register built-in tools into a tool registry
;;
;; Delegates to registry-table.rkt for the declarative spec table.
;; All tool schemas, descriptions, and handlers are defined there.

(require "registry-table.rkt")

(provide register-default-tools!)

;; Register the built-in tools into the given tool registry.
;; #:only — optional list of tool name strings to register; #f means all.
(define (register-default-tools! registry #:only [only #f])
  (register-tools-from-specs! registry tool-specs #:only only))
