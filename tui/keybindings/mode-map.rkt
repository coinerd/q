#lang racket/base

;; q/tui/keybindings/mode-map.rkt — mode-specific keybinding overlays
;;
;; Placeholder for insert/normal/command mode overlays.
;; Currently a passthrough — all modes share the same keymap.

(require "../keymap.rkt")

(provide make-mode-keymap
         mode-overlay->keymap)

;; Create a mode-specific keymap overlay.
;; For now, returns the default keymap (no mode-specific overlays yet).
(define (make-mode-keymap mode base-keymap)
  base-keymap)

;; Resolve the effective keymap for a given mode.
(define (mode-overlay->keymap mode overlays)
  (if (hash? overlays)
      (hash-ref overlays mode (lambda () #f))
      #f))
