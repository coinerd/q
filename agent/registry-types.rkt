#lang racket/base

;; agent/registry-types.rkt — Type definitions for agent registry
;; STABILITY: evolving
;;
;; MAS Schritt 5: Modular Hot-Swapping & Dynamic Evolution
;;
;; This module defines the pure data structures for the agent registry.
;; It depends on nothing except racket/contract, making it a stable
;; foundation that all other registry modules can build upon.

(require racket/contract)

;; ============================================================
;; Data Structures
;; ============================================================

;; An agent descriptor records how to construct an agent role.
;;   role-name: symbol — e.g., 'planner, 'verifier, 'tool-gateway
;;   version: string — semantic version (e.g., "1.0.0")
;;   factory: procedure — factory that creates a fresh role instance
;;   module-path: (or/c #f string?) — for future dynamic-require loading
;;   active?: boolean — whether this version is currently active
(struct agent-descriptor (role-name version factory module-path active?) #:transparent)

;; A version pin records which version a session is locked to.
;; This prevents mid-session version switches that could cause inconsistency.
;;   role-name: symbol
;;   pinned-version: string
;;   pinned-at: exact-nonnegative-integer? — unix timestamp
(struct version-pin (role-name pinned-version pinned-at) #:transparent)

;; A registry entry maps role-name → list of agent-descriptor (all versions).
;; The active version has active? = #t.
;; Multiple versions can coexist for A/B testing and rollback.
(struct registry-entry (role-name descriptors) #:transparent)

;; ============================================================
;; Provides
;; ============================================================

(provide (contract-out (struct agent-descriptor
                               ([role-name symbol?] [version string?]
                                                    [factory procedure?]
                                                    [module-path (or/c #f string?)]
                                                    [active? boolean?]))
                       (struct version-pin
                               ([role-name symbol?] [pinned-version string?]
                                                    [pinned-at exact-nonnegative-integer?]))
                       (struct registry-entry
                               ([role-name symbol?] [descriptors (listof agent-descriptor?)]))))
