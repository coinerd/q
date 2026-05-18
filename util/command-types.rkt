#lang racket/base

;; util/command-types.rkt — Command entry struct, pure registry operations,
;;                           and shared command AST types (F14)
;;
;; Extracted from tui/palette.rkt so that extensions/ can use cmd-entry
;; without importing from the TUI layer (ARCH-02 layer boundary fix).
;;
;; F14: Added shared-command for cross-subsystem type sharing.
;; NOTE: shared-command is GSD-specific (used by extensions/gsd/ subsystems).
;; The TUI layer uses cmd-entry and its own local parsed-command struct.

(provide (struct-out cmd-entry)
         register-command!
         lookup-command
         ;; F14: Shared command AST types
         (struct-out shared-command))
;; ---------------------------------------------------------------------------
;; TUI command registry types (ARCH-02)
;; ---------------------------------------------------------------------------

;; A command entry in the registry. Pure data — no rendering dependency.
(struct cmd-entry
        (name ; string — e.g. "/help"
         summary ; string — one-line description
         category ; symbol — 'general | 'session | 'model | 'debug
         args-spec ; (listof string) — arg names for display
         aliases ; (listof string) — short forms
         )
  #:transparent)

;; Adds or replaces a command in the registry hash.
(define (register-command! reg entry)
  (hash-set reg (cmd-entry-name entry) entry))

;; Returns cmd-entry or #f.
(define (lookup-command reg name)
  (hash-ref reg name #f))

;; ---------------------------------------------------------------------------
;; F14: Shared command AST types
;; ---------------------------------------------------------------------------

;; Shared command: unified across TUI slash commands and GSD commands.
;; name: symbol — canonical command name (e.g. 'help, 'gsd-go)
;; args: string — raw argument string after command name
;; kind: symbol — 'none, 'optional, 'required (arg expectation)
;; source: symbol — 'tui or 'gsd (where the command originated)
(struct shared-command (name args kind source) #:transparent)
