#lang racket/base

;; examples/extensions/keyboard-shortcut.rkt — keyboard shortcut registration (#1218)
;;
;; Demonstrates how to register custom keyboard shortcuts with namespace.
;; Uses the 'register-shortcuts hook to declare keybindings that the
;; TUI keymap system will pick up and dispatch.
;;
;; Usage:
;;   (load-extension! registry "examples/extensions/keyboard-shortcut.rkt")
;;
;; Note: Shortcuts are registered via the 'register-shortcuts hook.
;; Key specs use Emacs-style notation: C=Ctrl, M=Alt, S=Shift.
;; Each shortcut has a 'key and 'action field.

(require "../../extensions/api.rkt"
         "../../extensions/hooks.rkt")

(provide the-extension)

;; The register-shortcuts hook fires once during initialization.
;; Return (hook-amend (hasheq 'shortcuts (list shortcut-spec ...)))
;; where each shortcut-spec is (hasheq 'key "key-spec" 'action "action-name").
;;
;; Key spec format (Emacs-style):
;;   C-x     — Ctrl+X
;;   M-x     — Alt+X
;;   C-S-e   — Ctrl+Shift+E
;;   C-c C-r — Ctrl+C then Ctrl+R (chord)
;;   <f5>    — Function key F5
;;
;; Action names should be namespaced to avoid collisions with built-in
;; actions. Convention: "ext.<extension-name>.<action>"
;;
;; The TUI keymap system converts these specs into active bindings via
;; shortcut-specs->keymap. Extensions can also use keymap-merge to
;; layer their bindings on top of the default keymap.

(define the-extension
  (extension "keyboard-shortcut"
             "1.0.0"
             "1"
             (hasheq 'register-shortcuts
                     (lambda (payload)
                       (hook-amend
                        (hasheq 'shortcuts
                                (list
                                 ;; Quick deploy action: Ctrl+Shift+D
                                 (hasheq 'key "C-S-d"
                                         'action "ext.keyboard-shortcut.deploy")
                                 ;; Toggle debug: Ctrl+Shift+E
                                 (hasheq 'key "C-S-e"
                                         'action "ext.keyboard-shortcut.toggle-debug")
                                 ;; Refresh status: F5
                                 (hasheq 'key "<f5>"
                                         'action "ext.keyboard-shortcut.refresh-status"))))))))

;; Key concepts:
;;   1. Use 'register-shortcuts hook — fires during TUI initialization
;;   2. Return (hook-amend (hasheq 'shortcuts (list ...))) with shortcut specs
;;   3. Each spec: (hasheq 'key "C-S-d" 'action "ext.my-ext.my-action")
;;   4. Namespace actions with "ext.<name>." to avoid collisions
;;   5. Key specs: C=Ctrl, M=Alt, S=Shift, chords with space (C-c C-r)
;;   6. This is an advisory hook — errors default to pass (not critical)
;;   7. The TUI keymap system merges extension shortcuts into the base keymap
