#lang racket/base
;; util/tool-registry-struct.rkt — Tool registry type definition
;; Extracted from tools/registry.rkt to break runtime→tools reverse dependency (H1).
;; Both runtime/ and tools/ can import from util/ without layer violation.
;; STABILITY: stable

(provide tool-registry?
         make-tool-registry-internal
         tool-registry-tools-box
         tool-registry-active-set-box
         tool-registry-sem)

;; Thread-safe tool registry struct. Fields are internal; use accessor
;; functions from tools/registry.rkt for safe access.
(struct tool-registry (tools-box active-set-box sem) #:constructor-name make-tool-registry-internal)
