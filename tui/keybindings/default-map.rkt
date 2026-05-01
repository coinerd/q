#lang racket/base

;; q/tui/keybindings/default-map.rkt — default key → command mappings
;;
;; Re-exports the default keymap and user keymap loading from keymap.rkt.
;; Provides action dispatch table.

(require "../keymap.rkt")

(provide default-keymap
         load-user-keymap
         load-keybindings)

;; The default keymap and loading functions are already in keymap.rkt.
;; This module serves as the logical home for "default mapping" concerns.
