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
         tool-spec-prompt-guidelines
         tool-spec-required-capability
         make-tool-spec*)

;; tool-spec with 6th field required-capability.
;; All existing call sites pass 5 args — the struct constructor
;; requires 6, so we use make-tool-spec* which defaults to 'any.
(struct tool-spec (name description schema handler prompt-guidelines required-capability)
  #:transparent)

;; make-tool-spec* — backward-compatible constructor with default required-capability
(define (make-tool-spec* name description schema handler prompt-guidelines [required-capability 'any])
  (tool-spec name description schema handler prompt-guidelines required-capability))
