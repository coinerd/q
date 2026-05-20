#lang racket/base
;; util/tool-registry-struct.rkt — Tool registry type definition
;; Extracted from tools/registry.rkt to break runtime→tools reverse dependency (H1).
;; Both runtime/ and tools/ can import from util/ without layer violation.
;; STABILITY: stable

(require racket/contract)

(provide (contract-out [tool-registry? (-> any/c boolean?)]
                       [make-tool-registry-internal (-> any/c any/c any/c tool-registry?)]
                       [tool-registry-tools-box (-> tool-registry? any/c)]
                       [tool-registry-active-set-box (-> tool-registry? any/c)]
                       [tool-registry-sem (-> tool-registry? any/c)])
         tool-registry)

;; Thread-safe tool registry struct. Fields are internal; use accessor
;; functions from tools/registry.rkt for safe access.
(struct tool-registry (tools-box active-set-box sem) #:constructor-name make-tool-registry-internal)
