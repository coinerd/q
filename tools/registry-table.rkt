#lang racket/base

;; tools/registry-table.rkt — Declarative tool spec table (thin facade)
;; STABILITY: internal
;;
;; Encodes all built-in tools as tool-spec structs.
;; Domain-specific specs live in registry-table/*.rkt.
;;
;; register-tools-from-specs! converts specs → make-tool calls.

(require "tool.rkt"
         "registry-table/spec.rkt"
         "registry-table/core-tools.rkt"
         "registry-table/browser-tools.rkt"
         "registry-table/memory-tools.rkt"
         "registry-table/skill-tools.rkt")

(provide register-tools-from-specs!
         dangerous-tool-names
         tool-specs
         tool-spec
         tool-spec?
         tool-spec-name
         tool-spec-description
         tool-spec-schema
         tool-spec-handler
         tool-spec-prompt-guidelines)

;; ============================================================
;; Combined tool spec table
;; ============================================================

(define tool-specs (append core-tool-specs browser-tool-specs memory-tool-specs skill-tool-specs))

;; R-03/R-22: Metadata-driven dangerous tool classification
(define dangerous-tool-names
  '("write" "edit" "bash" "delete-lines" "browser_click" "browser_type" "browser_press"))

;; Register tools from tool-spec structs.
(define (register-tools-from-specs! registry specs #:only [only #f])
  (for ([spec (in-list specs)])
    (cond
      [(tool-spec? spec)
       (define name (tool-spec-name spec))
       (when (or (not only) (member name only))
         (define pg (tool-spec-prompt-guidelines spec))
         (define dangerous? (and (member name dangerous-tool-names) #t))
         (if pg
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:prompt-guidelines pg
                                        #:dangerous? dangerous?))
             (register-tool! registry
                             (make-tool name
                                        (tool-spec-description spec)
                                        (tool-spec-schema spec)
                                        (tool-spec-handler spec)
                                        #:dangerous? dangerous?))))]))
  (void))
