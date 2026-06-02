#lang racket/base

;; util/protocol-types.rkt -- DEPRECATED backward-compat re-export facade
;;
;; DEPRECATED (v0.81.1): New source code must import the focused protocol
;; sub-modules directly. This facade remains only for tests, external
;; compatibility, and staged removal. Target removal: v0.82+.
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
;;
;; Migration status (v0.81.1): non-test source consumers have been migrated.
;; Tests may still require the facade to verify compatibility until removal.

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
