#lang racket/base

;; tools/registry-table/memory-tools.rkt — Memory tool specs
;; STABILITY: internal

(require "spec.rkt"
         "../tool.rkt"
         "../builtins/memory-tools.rkt")

(provide memory-tool-specs)

;; ============================================================
;; Memory tool specs (8) — v0.95.15 activation
;; ============================================================
;; Each tool's handler already gates on (current-memory-backend) —
;; if no backend, returns "memory not enabled" error. Safe to register always.

(define memory-tool-specs
  (list (tool-spec (tool-name store-memory)
                   (tool-description store-memory)
                   (tool-schema store-memory)
                   (tool-execute store-memory)
                   (tool-prompt-guidelines store-memory))
        (tool-spec (tool-name search-memory)
                   (tool-description search-memory)
                   (tool-schema search-memory)
                   (tool-execute search-memory)
                   (tool-prompt-guidelines search-memory))
        (tool-spec (tool-name delete-memory)
                   (tool-description delete-memory)
                   (tool-schema delete-memory)
                   (tool-execute delete-memory)
                   (tool-prompt-guidelines delete-memory))
        (tool-spec (tool-name list-memory)
                   (tool-description list-memory)
                   (tool-schema list-memory)
                   (tool-execute list-memory)
                   (tool-prompt-guidelines list-memory))
        (tool-spec (tool-name clear-memory)
                   (tool-description clear-memory)
                   (tool-schema clear-memory)
                   (tool-execute clear-memory)
                   (tool-prompt-guidelines clear-memory))
        (tool-spec (tool-name update-memory)
                   (tool-description update-memory)
                   (tool-schema update-memory)
                   (tool-execute update-memory)
                   (tool-prompt-guidelines update-memory))
        (tool-spec (tool-name cleanup-expired-memory)
                   (tool-description cleanup-expired-memory)
                   (tool-schema cleanup-expired-memory)
                   (tool-execute cleanup-expired-memory)
                   (tool-prompt-guidelines cleanup-expired-memory))
        (tool-spec (tool-name consolidate-memory)
                   (tool-description consolidate-memory)
                   (tool-schema consolidate-memory)
                   (tool-execute consolidate-memory)
                   (tool-prompt-guidelines consolidate-memory))))
