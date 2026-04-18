#lang racket/base

;; q/tui/keymap.rkt — Configurable keybindings system (Issue #411)
;;
;; Provides a data-driven keymap that replaces hardcoded key handling.
;; Users can override keybindings via ~/.q/keybindings.json.

(require racket/string
         racket/port
         racket/file
         racket/match
         racket/function
         json
         "../util/config-paths.rkt")

(provide key-spec
         key-spec?
         key-spec-name
         key-spec-ctrl
         key-spec-shift
         key-spec-alt
         keycode->key-spec
         key-spec->keycode
         key-spec=?
         parse-key-string

         make-keymap
         keymap-add!
         keymap-remove!
         keymap-lookup
         keymap-list
         keymap-find-conflicts
         keymap-merge

         default-keymap
         load-user-keymap
         load-keybindings
         shortcut-specs->keymap

         ;; #1189: Namespaced actions
         namespaced-action?
         namespace-action
         migrate-action
         parse-keybindings-content)

;; ============================================================
;; Key specification
;; ============================================================

;; A key-spec identifies a key combination.
;; name: char? or symbol? (e.g. #\a, 'up, 'return)
;; ctrl, shift, alt: booleans for modifier keys
(struct key-spec (name ctrl shift alt) #:transparent)

;; Convert a terminal keycode to a key-spec.
;; keycode: char?, symbol?, or 'ctrl-c etc.
(define (keycode->key-spec keycode #:ctrl [ctrl #f] #:shift [shift #f] #:alt [alt #f])
  (key-spec keycode ctrl shift alt))

;; Convert key-spec back to a terminal-style keycode symbol.
(define (key-spec->keycode ks)
  (define base (key-spec-name ks))
  (define mods
    (string-append (if (key-spec-ctrl ks) "C-" "")
                   (if (key-spec-alt ks) "M-" "")
                   (if (key-spec-shift ks) "S-" "")))
  (string->symbol (string-append mods
                                 (cond
                                   [(char? base) (string base)]
                                   [(symbol? base) (symbol->string base)]
                                   [else "?"]))))

;; Check equality of two key-specs
(define (key-spec=? a b)
  (and (equal? (key-spec-name a) (key-spec-name b))
       (eq? (key-spec-ctrl a) (key-spec-ctrl b))
       (eq? (key-spec-shift a) (key-spec-shift b))
       (eq? (key-spec-alt a) (key-spec-alt b))))

;; ============================================================
;; #1189: Namespaced actions with auto-migration
;; ============================================================

;; Check if an action symbol uses dot-namespace format (e.g. 'tui.editor.copy)
(define (namespaced-action? action)
  (and (symbol? action)
       (regexp-match? #rx"^[a-z][a-z0-9-]*\\." (symbol->string action))))

;; Build a namespaced action from parts: (namespace 'editor 'copy) → 'app.editor.copy
(define (namespace-action . parts)
  (string->symbol (string-join (map (lambda (p)
                                      (if (symbol? p) (symbol->string p) p))
                                    parts) ".")))

;; Migration table: flat action → namespaced equivalent
(define flat->namespaced
  (hasheq 'history-up 'tui.navigation.history-up
          'history-down 'tui.navigation.history-down
          'page-up 'tui.navigation.page-up
          'page-down 'tui.navigation.page-down
          'home 'tui.navigation.home
          'end 'tui.navigation.end
          'scroll-up 'tui.navigation.scroll-up
          'scroll-down 'tui.navigation.scroll-down
          'submit 'tui.input.submit
          'backspace 'tui.input.backspace
          'delete 'tui.input.delete
          'cancel 'tui.input.cancel
          'word-left 'tui.editor.word-left
          'word-right 'tui.editor.word-right
          'copy 'tui.editor.copy
          'cut 'tui.editor.cut
          'paste 'tui.editor.paste
          'select-all 'tui.editor.select-all
          'clear-input 'tui.editor.clear-input
          'clear-screen 'tui.display.clear-screen))

;; Auto-migrate a flat action to its namespaced equivalent.
;; Returns the namespaced symbol if a mapping exists, otherwise the original.
(define (migrate-action action)
  (hash-ref flat->namespaced action action))

;; ============================================================
;; Keymap data structure
;; ============================================================

;; A keymap maps key-specs to action symbols.
;; Stored as a mutable list of (cons key-spec action).
(struct keymap (entries) #:mutable)

(define (make-keymap)
  (keymap '()))

(define (keymap-add! km ks action)
  ;; Remove existing binding for this key-spec
  (set-keymap-entries!
   km
   (cons (cons ks action) (filter (lambda (e) (not (key-spec=? (car e) ks))) (keymap-entries km)))))

(define (keymap-remove! km ks)
  (set-keymap-entries! km (filter (lambda (e) (not (key-spec=? (car e) ks))) (keymap-entries km))))

(define (keymap-lookup km ks)
  (define entry
    (for/first ([e (in-list (keymap-entries km))]
                #:when (key-spec=? (car e) ks))
      e))
  (and entry (cdr entry)))

(define (keymap-list km)
  (keymap-entries km))

;; Find conflicting bindings (same key-spec, different actions)
(define (keymap-find-conflicts km)
  (define entries (keymap-entries km))
  (for/list ([e (in-list entries)]
             [i (in-naturals)]
             #:when
             (for/or ([e2 (in-list entries)]
                      [j (in-naturals)]
                      #:when (and (< i j) (key-spec=? (car e) (car e2)) (not (eq? (cdr e) (cdr e2)))))
               #t))
    (car e)))

;; Merge a source keymap into a target (source wins on conflict)
(define (keymap-merge target source)
  ;; Source wins on conflict — user keybindings override defaults
  (for ([e (in-list (keymap-entries source))])
    (keymap-add! target (car e) (cdr e))))

;; ============================================================
;; Default keymap
;; ============================================================

(define (default-keymap)
  (define km (make-keymap))
  ;; Navigation
  (keymap-add! km (key-spec 'up #f #f #f) 'tui.navigation.history-up)
  (keymap-add! km (key-spec 'down #f #f #f) 'tui.navigation.history-down)
  (keymap-add! km (key-spec 'page-up #f #f #f) 'tui.navigation.page-up)
  (keymap-add! km (key-spec 'page-down #f #f #f) 'tui.navigation.page-down)
  (keymap-add! km (key-spec 'home #f #f #f) 'tui.navigation.home)
  (keymap-add! km (key-spec 'end #f #f #f) 'tui.navigation.end)
  ;; Ctrl+up/down for scrolling
  (keymap-add! km (key-spec 'up #t #f #f) 'tui.navigation.scroll-up)
  (keymap-add! km (key-spec 'down #t #f #f) 'tui.navigation.scroll-down)
  ;; Input
  (keymap-add! km (key-spec 'return #f #f #f) 'tui.input.submit)
  (keymap-add! km (key-spec 'backspace #f #f #f) 'tui.input.backspace)
  (keymap-add! km (key-spec 'delete #f #f #f) 'tui.input.delete)
  (keymap-add! km (key-spec 'escape #f #f #f) 'tui.input.cancel)
  ;; Word navigation
  (keymap-add! km (key-spec 'left #t #f #f) 'tui.editor.word-left)
  (keymap-add! km (key-spec 'right #t #f #f) 'tui.editor.word-right)
  ;; Line navigation
  (keymap-add! km (key-spec 'left #f #t #f) 'tui.navigation.home)
  (keymap-add! km (key-spec 'right #f #t #f) 'tui.navigation.end)
  ;; Ctrl-C for copy
  (keymap-add! km (key-spec #\c #t #f #f) 'tui.editor.copy)
  ;; Ctrl-X for cut
  (keymap-add! km (key-spec #\x #t #f #f) 'tui.editor.cut)
  ;; Ctrl-V for paste
  (keymap-add! km (key-spec #\v #t #f #f) 'tui.editor.paste)
  ;; Ctrl-A for select-all
  (keymap-add! km (key-spec #\a #t #f #f) 'tui.editor.select-all)
  ;; Ctrl-U for clear input
  (keymap-add! km (key-spec #\u #t #f #f) 'tui.editor.clear-input)
  ;; Ctrl-L for clear screen
  (keymap-add! km (key-spec #\l #t #f #f) 'tui.display.clear-screen)
  km)

;; ============================================================
;; User keymap loading from JSON
;; ============================================================

;; Load user keymap overrides from ~/.q/keybindings.json
;; Format: [{ "key": "C-a", "action": "select-all" }, ...]
;; Key format: C- = ctrl, M- = alt, S- = shift, then key name
;; Returns #f if file doesn't exist, otherwise a keymap
(define (load-user-keymap)
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (define config-dir (global-config-dir))
    (define kb-path (build-path config-dir "keybindings.json"))
    (if (file-exists? kb-path)
        (let ([bindings (load-keybindings-file kb-path)])
          (if (list? bindings)
              (let ([km (make-keymap)])
                (for ([b (in-list bindings)])
                  (when (and b (pair? b))
                    (keymap-add! km (car b) (cdr b))))
                km)
              #f))
        #f)))

;; Parse a key string like "C-M-a", "C-up", "S-right"
(define (parse-key-string s)
  (define ctrl #f)
  (define alt #f)
  (define shift #f)
  (define rest s)
  ;; Parse modifier prefixes
  (cond
    [(string-prefix? rest "C-M-")
     (begin
       (set! ctrl #t)
       (set! alt #t)
       (set! rest (substring rest 4)))]
    [(string-prefix? rest "M-C-")
     (begin
       (set! ctrl #t)
       (set! alt #t)
       (set! rest (substring rest 4)))]
    [(string-prefix? rest "C-S-")
     (begin
       (set! ctrl #t)
       (set! shift #t)
       (set! rest (substring rest 4)))]
    [(string-prefix? rest "C-")
     (begin
       (set! ctrl #t)
       (set! rest (substring rest 2)))]
    [(string-prefix? rest "M-")
     (begin
       (set! alt #t)
       (set! rest (substring rest 2)))]
    [(string-prefix? rest "S-")
     (begin
       (set! shift #t)
       (set! rest (substring rest 2)))]
    [else (void)])
  ;; Parse the key name
  (define key-name
    (cond
      [(= (string-length rest) 1) (string-ref rest 0)]
      [else (string->symbol rest)]))
  (key-spec key-name ctrl shift alt))

;; JSON keybindings loader.
;; Keybindings file format: [{ "key": "C-a", "action": "select-all" }, ...]
;; Key format: C- = ctrl, M- = alt, S- = shift, then key name.
;; Uses the `json` library for proper JSON parsing — key order in objects
;; is not significant.

(define (load-keybindings-file path)
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (log-warning (format "keymap: failed to load ~a: ~a" path (exn-message e)))
                     #f)])
    (if (file-exists? path)
        (let ([content (file->string path)]) (parse-keybindings-content content path))
        #f)))

(define (parse-keybindings-content content [source-path "<keybindings>"])
  (with-handlers ([exn:fail? (lambda (e)
                               (log-warning
                                (format "keymap: invalid JSON in ~a: ~a" source-path (exn-message e)))
                               #f)])
    (define data (string->jsexpr content))
    (unless (list? data)
      (raise (exn:fail (format "keymap: expected JSON array in ~a" source-path)
                       (current-continuation-marks))))
    (define bindings
      (for/list ([entry (in-list data)]
                 #:when (and (hash? entry) (hash-has-key? entry 'key) (hash-has-key? entry 'action)))
        (define key-str (hash-ref entry 'key #f))
        (define action-str (hash-ref entry 'action #f))
        (if (and (string? key-str) (string? action-str))
            (cons (parse-key-string key-str) (migrate-action (string->symbol action-str)))
            (begin
              (log-warning (format "keymap: skipping invalid entry in ~a: ~v" source-path entry))
              #f))))
    (define valid-bindings (filter identity bindings))
    (if (null? valid-bindings) #f valid-bindings)))

;; ============================================================
;; Extension shortcut integration (#678)
;; ============================================================

;; Convert a list of shortcut hash descriptors to a keymap.
;; Each hash should have keys: "key" (string), "action" (string).
;; Returns a keymap with all extension shortcuts bound.
;; ============================================================
;; Public keybindings loading API (#1117)
;; ============================================================

;; Load keybindings from a specific path or the default location.
;; Returns a merged keymap (default + user overrides).
;; If path is #f, uses ~/.q/keybindings.json.
;; If the file doesn't exist or is invalid, returns default keymap only.
(define (load-keybindings [path #f])
  (define base (default-keymap))
  (define kb-path (or path (build-path (global-config-dir) "keybindings.json")))
  (define user-bindings
    (with-handlers ([exn:fail?
                     (lambda (e)
                       (log-warning (format "keymap: failed to load ~a: ~a" kb-path (exn-message e)))
                       #f)])
      (and (file-exists? kb-path) (load-keybindings-file kb-path))))
  (when user-bindings
    (define user-km (make-keymap))
    (for ([b (in-list user-bindings)])
      (when (and b (pair? b))
        (keymap-add! user-km (car b) (cdr b))))
    (keymap-merge base user-km))
  base)

(define (shortcut-specs->keymap specs)
  (define km (make-keymap))
  (for ([spec (in-list specs)])
    (define key-str (hash-ref spec 'key #f))
    (define action-str (hash-ref spec 'action #f))
    (when (and (string? key-str) (string? action-str))
      (keymap-add! km (parse-key-string key-str) (string->symbol action-str))))
  km)
