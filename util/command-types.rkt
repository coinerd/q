#lang racket/base

;; util/command-types.rkt — Command entry struct and pure registry operations
;;
;; Extracted from tui/palette.rkt so that extensions/ can use cmd-entry
;; without importing from the TUI layer (ARCH-02 layer boundary fix).

(provide (struct-out cmd-entry)
         register-command!
         lookup-command)

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
