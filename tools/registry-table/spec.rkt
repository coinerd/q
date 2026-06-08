#lang racket/base

;; tools/registry-table/spec.rkt — Tool spec struct definition
;; STABILITY: internal
;;
;; Extracted from registry-table.rkt to avoid circular imports
;; between the facade and domain-specific spec modules.

(provide tool-spec
         tool-spec?
         tool-spec-name
         tool-spec-description
         tool-spec-schema
         tool-spec-handler
         tool-spec-prompt-guidelines)

(struct tool-spec (name description schema handler prompt-guidelines) #:transparent)
