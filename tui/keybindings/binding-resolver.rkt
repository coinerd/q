#lang racket/base

;; q/tui/keybindings/binding-resolver.rkt — keymap resolution, keycode conversion, dispatch
;;
;; Pure keymap resolution logic. No TUI state mutation.

(require racket/string
         "../keymap.rkt")

(provide keycode->key-spec-from-msg
         resolve-key-action
         reload-keymap!
         current-keybindings-path
         get-active-keymap
         cached-keymap)

;; Current user keybindings file path override
(define current-keybindings-path (make-parameter #f))

;; Cached merged keymap
(define cached-keymap #f)

;; Get the active keymap (default + user overrides merged).
(define (get-active-keymap)
  (cond
    [cached-keymap cached-keymap]
    [else
     (define base
       (if (current-keybindings-path)
           (load-keybindings (current-keybindings-path))
           (default-keymap)))
     (when (not (current-keybindings-path))
       (define user (load-user-keymap))
       (when user
         (keymap-merge base user)))
     (set! cached-keymap base)
     base]))

;; Force reload of keymap (e.g., after user edits keybindings.json)
(define (reload-keymap!)
  (set! cached-keymap #f)
  (void))

;; Convert a raw keycode (char/symbol) from the terminal to a key-spec
;; for keymap lookup. Handles modifier-prefixed symbols like 'ctrl-z.
(define (keycode->key-spec-from-msg keycode)
  (cond
    [(char? keycode) (key-spec keycode #f #f #f)]
    [(symbol? keycode)
     (define s (symbol->string keycode))
     (cond
       [(and (> (string-length s) 5) (string-prefix? s "ctrl-"))
        (define rest (substring s 5))
        (key-spec (string->symbol rest) #t #f #f)]
       [(and (> (string-length s) 6) (string-prefix? s "shift-"))
        (define rest (substring s 6))
        (key-spec (string->symbol rest) #f #t #f)]
       [else (key-spec keycode #f #f #f)])]
    [else #f]))

;; Resolve a keycode to a keymap action, or #f if not found.
(define (resolve-key-action keycode)
  (define km (get-active-keymap))
  (define ks (keycode->key-spec-from-msg keycode))
  (and ks (keymap-lookup km ks)))
