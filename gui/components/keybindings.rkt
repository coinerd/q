#lang racket/base

;; q/gui/components/keybindings.rkt — Keyboard shortcut registry
;;
;; Extracted from rich-transcript-view.rkt for SRP separation.
;; Pure functions, headless-testable, no racket/gui dependencies.

(require racket/contract)

(provide (contract-out
          [default-keybindings hash?]
          [lookup-keybinding (-> char? boolean? (or/c symbol? #f))]
          [key-event->action (-> char? boolean? (or/c symbol? #f))]
          [list-keybindings (-> (listof pair?))]))

;; ──────────────────────────────
;; Keyboard shortcut registry (pure, headless-testable)
;; ──────────────────────────────

;; Default keyboard shortcuts for the GUI transcript
(define default-keybindings
  (hash #\l 'clear
        #\k 'compact
        #\c 'interrupt
        #\s 'save
        #\q 'quit))

;; Look up a keybinding by key character + ctrl modifier
(define (lookup-keybinding key-char ctrl?)
  (if ctrl?
      (hash-ref default-keybindings key-char #f)
      #f))

;; Get action symbol for a key event
(define (key-event->action key-char ctrl?)
  (lookup-keybinding key-char ctrl?))

;; List all registered shortcuts as (char . action) pairs
(define (list-keybindings)
  (hash->list default-keybindings))
