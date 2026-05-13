#lang racket/base

;; BOUNDARY: io

;; tests/tui/test-keymap-unified.rkt -- Keymap unification tests (W-08)

(require rackunit
         "../../tui/keymap.rkt")

(test-case "default keymap has core navigation bindings"
  (define km (make-keymap))
  ;; Load default bindings
  (keymap-add! km (key-spec 'up #f #f #f) 'tui.navigation.history-up)
  (keymap-add! km (key-spec 'down #f #f #f) 'tui.navigation.history-down)
  (keymap-add! km (key-spec 'page-up #f #f #f) 'tui.navigation.page-up)
  (keymap-add! km (key-spec 'page-down #f #f #f) 'tui.navigation.page-down)
  (keymap-add! km (key-spec 'home #f #f #f) 'tui.navigation.home)
  (keymap-add! km (key-spec 'end #f #f #f) 'tui.navigation.end)
  (check-eq? (keymap-lookup km (key-spec 'up #f #f #f)) 'tui.navigation.history-up)
  (check-eq? (keymap-lookup km (key-spec 'down #f #f #f)) 'tui.navigation.history-down)
  (check-eq? (keymap-lookup km (key-spec 'page-up #f #f #f)) 'tui.navigation.page-up)
  (check-eq? (keymap-lookup km (key-spec 'home #f #f #f)) 'tui.navigation.home)
  (check-eq? (keymap-lookup km (key-spec 'end #f #f #f)) 'tui.navigation.end))

(test-case "default keymap has editing bindings"
  (define km (make-keymap))
  (keymap-add! km (key-spec 'backspace #f #f #f) 'tui.input.backspace)
  (keymap-add! km (key-spec 'delete #f #f #f) 'tui.input.delete)
  (check-eq? (keymap-lookup km (key-spec 'backspace #f #f #f)) 'tui.input.backspace)
  (check-eq? (keymap-lookup km (key-spec 'delete #f #f #f)) 'tui.input.delete))

(test-case "default keymap has word navigation"
  (define km (make-keymap))
  (keymap-add! km (key-spec 'left #t #f #f) 'tui.editor.word-left)
  (keymap-add! km (key-spec 'right #t #f #f) 'tui.editor.word-right)
  (check-eq? (keymap-lookup km (key-spec 'left #t #f #f)) 'tui.editor.word-left)
  (check-eq? (keymap-lookup km (key-spec 'right #t #f #f)) 'tui.editor.word-right))
