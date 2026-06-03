#lang racket/base
;; runtime/command-registry-bridge.rkt — Bridge for command-registry functions
;; A1-03: Re-exports TUI command-registry functions so wiring/ doesn't import tui/ directly.
;; The actual implementations live in tui/palette.rkt and tui/keymap.rkt.
;; This module exists solely to break the wiring→tui layer dependency.

(require (only-in "../tui/palette.rkt"
                  commands-from-hashes
                  merge-extension-commands
                  make-command-registry)
         (only-in "../tui/keymap.rkt"
                  shortcut-specs->keymap
                  keymap-merge))

(provide commands-from-hashes
         merge-extension-commands
         make-command-registry
         shortcut-specs->keymap
         keymap-merge)
