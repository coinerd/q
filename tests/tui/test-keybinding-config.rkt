#lang racket

;; tests/tui/test-keybinding-config.rkt — Keybinding JSON loading/merge tests (#1117, #1118)
;;
;; Tests for:
;;   - load-keybindings: load from custom path
;;   - load-keybindings: fallback to defaults on invalid JSON
;;   - load-keybindings: merge user overrides with defaults
;;   - --keybindings CLI flag parsing
;;   - current-keybindings-path parameter

(require rackunit
         rackunit/text-ui
         racket/file
         "../../tui/keymap.rkt"
         "../../tui/tui-keybindings.rkt"
         "../../cli/args.rkt")

(define temp-dir (make-temporary-file "keybinding-test-~a" 'directory))

(define (make-temp-keybindings-json content)
  (define path (build-path temp-dir (format "kb-~a.json" (current-milliseconds))))
  (call-with-output-file path (lambda (out) (display content out)) #:exists 'truncate)
  path)

(define keybinding-config-tests
  (test-suite "Keybinding Configuration"

    ;; ── load-keybindings from custom path ──
    (test-case "load-keybindings with valid JSON merges with defaults"
      (define json-file (make-temp-keybindings-json "[{\"key\":\"C-z\",\"action\":\"undo\"}]"))
      (define km (load-keybindings json-file))
      ;; User binding present
      (check-equal? (keymap-lookup km (key-spec #\z #t #f #f)) 'undo)
      ;; Default binding still present
      (check-equal? (keymap-lookup km (key-spec 'up #f #f #f)) 'tui.navigation.history-up)
      (delete-file json-file))

    (test-case "load-keybindings with empty JSON array returns defaults"
      (define json-file (make-temp-keybindings-json "[]"))
      (define km (load-keybindings json-file))
      ;; Defaults still present
      (check-equal? (keymap-lookup km (key-spec 'return #f #f #f)) 'tui.input.submit)
      (delete-file json-file))

    (test-case "load-keybindings with invalid JSON returns defaults"
      (define json-file (make-temp-keybindings-json "this is not json"))
      (define km (load-keybindings json-file))
      ;; Defaults still present
      (check-equal? (keymap-lookup km (key-spec 'return #f #f #f)) 'tui.input.submit)
      (delete-file json-file))

    (test-case "load-keybindings with missing file returns defaults"
      (define km (load-keybindings "/nonexistent/path/keybindings.json"))
      (check-equal? (keymap-lookup km (key-spec 'return #f #f #f)) 'tui.input.submit))

    (test-case "load-keybindings user overrides take precedence"
      (define json-file (make-temp-keybindings-json "[{\"key\":\"C-c\",\"action\":\"cancel\"}]"))
      (define km (load-keybindings json-file))
      ;; User override wins
      (check-equal? (keymap-lookup km (key-spec #\c #t #f #f)) 'tui.input.cancel)
      (delete-file json-file))

    (test-case "load-keybindings with multiple bindings"
      (define json-file
        (make-temp-keybindings-json
         "[{\"key\":\"C-z\",\"action\":\"undo\"},\
                         {\"key\":\"C-y\",\"action\":\"redo\"},\
                         {\"key\":\"C-w\",\"action\":\"kill-word-backward\"}]"))
      (define km (load-keybindings json-file))
      (check-equal? (keymap-lookup km (key-spec #\z #t #f #f)) 'undo)
      (check-equal? (keymap-lookup km (key-spec #\y #t #f #f)) 'redo)
      (check-equal? (keymap-lookup km (key-spec #\w #t #f #f)) 'kill-word-backward)
      (delete-file json-file))

    (test-case "load-keybindings with modifier combos"
      (define json-file
        (make-temp-keybindings-json
         "[{\"key\":\"C-M-x\",\"action\":\"special\"},\
                         {\"key\":\"S-right\",\"action\":\"select-forward\"}]"))
      (define km (load-keybindings json-file))
      (check-equal? (keymap-lookup km (key-spec #\x #t #f #t)) 'special)
      (check-equal? (keymap-lookup km (key-spec 'right #f #t #f)) 'select-forward)
      (delete-file json-file))

    ;; ── current-keybindings-path parameter ──
    (test-case "current-keybindings-path defaults to #f"
      (check-false (current-keybindings-path)))

    (test-case "current-keybindings-path can be set"
      (parameterize ([current-keybindings-path "/tmp/test.json"])
        (check-equal? (current-keybindings-path) "/tmp/test.json")))

    ;; ── CLI flag parsing ──
    (test-case "--keybindings CLI flag sets keybindings-path"
      (define cfg (parse-cli-args (list "--keybindings" "/tmp/my-keys.json" "--tui")))
      (check-equal? (cli-config-mode cfg) 'tui)
      (check-equal? (cli-config-keybindings-path cfg) "/tmp/my-keys.json"))

    (test-case "no --keybindings flag leaves keybindings-path as #f"
      (define cfg (parse-cli-args (list "--tui")))
      (check-equal? (cli-config-mode cfg) 'tui)
      (check-false (cli-config-keybindings-path cfg)))

    ;; ── parse-key-string edge cases ──
    (test-case "parse-key-string handles C-up"
      (define ks (parse-key-string "C-up"))
      (check-equal? (key-spec-name ks) 'up)
      (check-true (key-spec-ctrl ks)))

    (test-case "parse-key-string handles C-S-delete"
      (define ks (parse-key-string "C-S-delete"))
      (check-equal? (key-spec-name ks) 'delete)
      (check-true (key-spec-ctrl ks))
      (check-true (key-spec-shift ks)))

    ;; ── keymap-merge behavior ──
    (test-case "keymap-merge: source overrides target"
      (define base (make-keymap))
      (keymap-add! base (key-spec #\a #t #f #f) 'select-all)
      (define override (make-keymap))
      (keymap-add! override (key-spec #\a #t #f #f) 'new-action)
      (keymap-merge base override)
      (check-equal? (keymap-lookup base (key-spec #\a #t #f #f)) 'new-action))))

(run-tests keybinding-config-tests)

;; Cleanup temp directory
(with-handlers ([exn:fail? void])
  (delete-directory/files temp-dir))
