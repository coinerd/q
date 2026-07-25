#lang racket/base

;;; tools/tool-classification.rkt — Authoritative tool classification
;;;
;;; Single source of truth for tool-name → classification queries.
;;; Replaces the hardcoded sets that previously lived inline in
;;; permission-gate.rkt and directly drives dangerous metadata in
;;; registry-table.rkt.  Only worker eligibility remains registry-local.
;;;
;;; Classification policy (v0.99.66):
;;;   - Every registered built-in tool is classified as 'auto-approved,
;;;     'needs-approval, or 'tool-owned-approval.
;;;   - Unknown tool names FAIL CLOSED: classified as 'needs-approval.
;;;
;;; NOTE on naming: the struct accessor `tool-dangerous?` (tool-struct.rkt)
;;; operates on a tool? struct.  This module's functions operate on tool
;;; NAME STRINGS, so they use the `tool-name-` prefix to avoid collisions
;;; when both are imported into the same module (e.g. scheduler-execution.rkt).

(require racket/contract
         racket/set)

;; ============================================================
;; Classification table
;; ============================================================

;; Canonical classification:
;; tool-name-string -> 'auto-approved | 'needs-approval | 'tool-owned-approval
;; This is the single authoritative source.  It MUST be kept in sync with
;; the tools registered in registry-table.rkt.  The completeness test
;; (test-tool-classification.rkt) enforces this.
;;
;; Policy:
;;   auto-approved  — read-only / safe-state tools (no destructive side effects)
;;   needs-approval — tools that mutate files, execute commands, spawn agents,
;;                    drive a browser, or touch the network.
(define classification-table
  ;; ── Core read-only tools ──
  (hash "read"
        'auto-approved
        "ls"
        'auto-approved
        "find"
        'auto-approved
        "grep"
        'auto-approved
        "date"
        'auto-approved
        ;; ── Session / state (safe state mutation) ──
        "session_recall"
        'auto-approved
        "save-conclusion"
        'auto-approved
        "record_conclusion"
        'auto-approved
        "set-task-state"
        'auto-approved
        ;; ── Browser — observation only (no side effects) ──
        "browser_observe"
        'auto-approved
        "browser_extract"
        'auto-approved
        "browser_screenshot"
        'auto-approved
        "browser_scroll"
        'auto-approved
        "browser_close"
        'auto-approved
        ;; ── Memory — read-only / safe-state tools ──
        "list-memory"
        'auto-approved
        "search-memory"
        'auto-approved
        "store-memory"
        'auto-approved
        "update-memory"
        'auto-approved
        "consolidate-memory"
        'auto-approved
        "cleanup-expired-memory"
        'auto-approved
        ;; ── File mutation tools ──
        "write"
        'needs-approval
        "edit"
        'needs-approval
        "delete-lines"
        'needs-approval
        ;; ── Shell execution ──
        "bash"
        'needs-approval
        ;; ── Network tools ──
        "firecrawl"
        'needs-approval
        ;; ── Browser — side-effecting actions ──
        "browser_open"
        'needs-approval
        "browser_click"
        'needs-approval
        "browser_type"
        'needs-approval
        "browser_press"
        'needs-approval
        "browser_check_local_app"
        'needs-approval
        ;; ── Workflow / sub-agent spawning ──
        ;; skill-route can execute mas-workflows, which spawn child work.
        "skill-route"
        'needs-approval
        "spawn-subagent"
        'tool-owned-approval
        "spawn-subagents"
        'tool-owned-approval
        ;; ── Destructive memory tools ──
        "delete-memory"
        'needs-approval
        "clear-memory"
        'needs-approval))

;; ============================================================
;; Sets (derived, for permission-gate consumption)
;; ============================================================

(define auto-approved-tool-names
  (for/set ([(name cls) (in-hash classification-table)]
            #:when (eq? cls 'auto-approved))
    name))

(define needs-approval-tool-names
  (for/set ([(name cls) (in-hash classification-table)]
            #:when (eq? cls 'needs-approval))
    name))

(define tool-owned-approval-tool-names
  (for/set ([(name cls) (in-hash classification-table)]
            #:when (eq? cls 'tool-owned-approval))
    name))

;; ============================================================
;; Query functions
;; ============================================================

;; tool-name-string -> (or/c 'auto-approved 'needs-approval 'tool-owned-approval)
;; Unknown names fail closed → 'needs-approval.
(define (classify-tool-by-name name)
  (hash-ref classification-table name 'needs-approval))

;; tool-name-string -> boolean?
;; True for tools that are dangerous (generic or tool-owned approval).
;; Unknown names → #t (fail closed).
(define (tool-name-needs-approval? name)
  (and (memq (classify-tool-by-name name) '(needs-approval tool-owned-approval)) #t))

;; True when the tool's implementation owns its broker approval lifecycle and
;; the generic scheduler gate must not run.
(define (tool-name-tool-owned-approval? name)
  (eq? 'tool-owned-approval (classify-tool-by-name name)))

;; tool-name-string -> boolean?
;; True for tools explicitly classified as auto-approved.
;; Unknown names → #f.
(define (tool-name-auto-approved? name)
  (eq? 'auto-approved (classify-tool-by-name name)))

;; ============================================================
;; Provides
;; ============================================================

;; Classification enum values (documentation)
(provide AUTO-APPROVED
         NEEDS-APPROVAL
         TOOL-OWNED-APPROVAL
         ;; Query functions
         classify-tool-by-name
         tool-name-needs-approval?
         tool-name-auto-approved?
         tool-name-tool-owned-approval?
         ;; Derived sets (for permission-gate.rkt defaults)
         auto-approved-tool-names
         needs-approval-tool-names
         tool-owned-approval-tool-names
         ;; Completeness / introspection (for tests)
         (contract-out [all-classified-tool-names (-> (listof string?))]))

(define AUTO-APPROVED 'auto-approved)
(define NEEDS-APPROVAL 'needs-approval)
(define TOOL-OWNED-APPROVAL 'tool-owned-approval)

;; Returns all tool names present in the classification table.
;; Used by the completeness test to verify parity with registry-table.
(define (all-classified-tool-names)
  (hash-keys classification-table))
