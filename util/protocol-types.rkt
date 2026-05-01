#lang racket/base

;; util/protocol-types.rkt -- Backward-compat re-export facade
;;
;; ARCH-05: Decomposed into focused sub-modules:
;;   - content-parts.rkt      -- text-part, tool-call-part, tool-result-part
;;   - message.rkt            -- message struct, serialization
;;   - event.rkt              -- event envelope, serialization
;;   - entry-predicates.rkt   -- entry kind predicates
;;   - tree-entries.rkt       -- branch-entry, tree-navigation-entry, etc.
;;   - loop-result.rkt        -- loop-result struct
;;   - custom-entries.rkt     -- extension state persistence entries
;;   - tool-types.rkt         -- tool-call, tool-result structs
;;
;; This file re-exports everything for backward compatibility.

(require "content-parts.rkt"
         "message.rkt"
         "event.rkt"
         "entry-predicates.rkt"
         "tree-entries.rkt"
         "loop-result.rkt"
         "custom-entries.rkt"
         "tool-types.rkt")

(provide (all-from-out "content-parts.rkt")
         (all-from-out "message.rkt")
         (all-from-out "event.rkt")
         (all-from-out "entry-predicates.rkt")
         (all-from-out "tree-entries.rkt")
         (all-from-out "loop-result.rkt")
         (all-from-out "custom-entries.rkt")
         (all-from-out "tool-types.rkt"))
