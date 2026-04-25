#lang racket

;; tests/test-command-shortcut-reg.rkt — tests for Command & Shortcut Registration (#679)
;;
;; Covers:
;;   #677: register-command! with autocompletion
;;   #678: register-shortcut! for keybinding injection
;;   Integration: Extension hooks dispatch for commands and shortcuts

(require rackunit
         "../tui/palette.rkt"
         "../tui/keymap.rkt"
         "../extensions/api.rkt"
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt")

;; ============================================================
;; #677: Command registration from extensions
;; ============================================================

(test-case "commands-from-hashes converts valid hashes"
  (define hashes
    (list (hasheq 'name
                  "/deploy"
                  'summary
                  "Deploy the project"
                  'category
                  'general
                  'args-spec
                  '("<target>")
                  'aliases
                  '("dp"))))
  (define cmds (commands-from-hashes hashes))
  (check-equal? (length cmds) 1)
  (define c (car cmds))
  (check-equal? (cmd-entry-name c) "/deploy")
  (check-equal? (cmd-entry-summary c) "Deploy the project")
  (check-equal? (cmd-entry-category c) 'general)
  (check-equal? (cmd-entry-args-spec c) '("<target>"))
  (check-equal? (cmd-entry-aliases c) '("dp")))

(test-case "commands-from-hashes handles empty list"
  (check-equal? (commands-from-hashes '()) '()))

(test-case "commands-from-hashes fills defaults for missing keys"
  (define hashes (list (hasheq 'name "/test")))
  (define cmds (commands-from-hashes hashes))
  (check-equal? (length cmds) 1)
  (define c (car cmds))
  (check-equal? (cmd-entry-name c) "/test")
  (check-equal? (cmd-entry-summary c) "")
  (check-equal? (cmd-entry-category c) 'general)
  (check-equal? (cmd-entry-args-spec c) '())
  (check-equal? (cmd-entry-aliases c) '()))

(test-case "merge-extension-commands adds new commands to registry"
  (define reg (make-command-registry))
  (define ext-cmds (list (cmd-entry "/deploy" "Deploy" 'general '("<target>") '())))
  (define merged (merge-extension-commands reg ext-cmds))
  (check-not-false (lookup-command merged "/deploy")))

(test-case "merge-extension-commands overrides existing command"
  (define reg (make-command-registry))
  (check-not-false (lookup-command reg "/help"))
  (define ext-cmds (list (cmd-entry "/help" "Enhanced help" 'general '() '())))
  (define merged (merge-extension-commands reg ext-cmds))
  (check-equal? (cmd-entry-summary (lookup-command merged "/help")) "Enhanced help"))

(test-case "extension can register commands via register-commands hook"
  (define ext-reg (make-extension-registry))
  (register-extension! ext-reg
                       (extension "cmd-ext"
                                  "0.1"
                                  "1.0"
                                  (hasheq 'register-commands
                                          (lambda (payload)
                                            (hook-amend (hasheq 'commands
                                                                (list (hasheq 'name
                                                                              "/ext-cmd"
                                                                              'summary
                                                                              "Extension command"
                                                                              'category
                                                                              'general
                                                                              'args-spec
                                                                              '()
                                                                              'aliases
                                                                              '()))))))))
  (define result (dispatch-hooks 'register-commands (hasheq) ext-reg))
  (check-equal? (hook-result-action result) 'amend)
  (define cmd-list (hash-ref (hook-result-payload result) 'commands))
  (check-equal? (length cmd-list) 1)
  (define converted (commands-from-hashes cmd-list))
  (check-equal? (cmd-entry-name (car converted)) "/ext-cmd"))

(test-case "register-commands is advisory — errors default to pass"
  (check-false (critical-hook? 'register-commands)))

;; ============================================================
;; #678: Shortcut registration from extensions
;; ============================================================

(test-case "shortcut-specs->keymap creates keymap from specs"
  (define specs
    (list (hasheq 'key "C-S-d" 'action "deploy") (hasheq 'key "M-x" 'action "ext-command")))
  (define km (shortcut-specs->keymap specs))
  (define entries (keymap-list km))
  ;; Check the keymap has the bindings
  (check-true (ormap (lambda (e) (eq? (cdr e) 'deploy)) entries))
  (check-true (ormap (lambda (e) (eq? (cdr e) 'ext-command)) entries)))

(test-case "shortcut-specs->keymap handles empty list"
  (define km (shortcut-specs->keymap '()))
  (check-equal? (keymap-list km) '()))

(test-case "shortcut-specs->keymap skips invalid entries"
  (define specs
    (list (hasheq 'key "C-t" 'action "test")
          (hasheq 'key 123 'action "bad") ; invalid key
          (hasheq 'action "no-key"))) ; missing key
  (define km (shortcut-specs->keymap specs))
  (define entries (keymap-list km))
  (check-equal? (length entries) 1)
  (check-eq? (cdr (car entries)) 'test))

(test-case "extension shortcuts can be merged into base keymap"
  (define base (default-keymap))
  (define ext-specs (list (hasheq 'key "C-S-d" 'action "deploy")))
  (define ext-km (shortcut-specs->keymap ext-specs))
  (keymap-merge base ext-km)
  ;; Verify the shortcut was added
  (define deploy-spec (parse-key-string "C-S-d"))
  (check-eq? (keymap-lookup base deploy-spec) 'deploy))

(test-case "extension shortcuts override base keymap"
  (define base (default-keymap))
  ;; Ctrl-A is "select-all" by default
  (define orig-spec (parse-key-string "C-a"))
  (check-eq? (keymap-lookup base orig-spec) 'tui.editor.select-all)
  ;; Override via extension
  (define ext-specs (list (hasheq 'key "C-a" 'action "ext-select-all")))
  (define ext-km (shortcut-specs->keymap ext-specs))
  (keymap-merge base ext-km)
  (check-eq? (keymap-lookup base orig-spec) 'ext-select-all))

(test-case "extension can register shortcuts via register-shortcuts hook"
  (define ext-reg (make-extension-registry))
  (register-extension!
   ext-reg
   (extension "shortcut-ext"
              "0.1"
              "1.0"
              (hasheq 'register-shortcuts
                      (lambda (payload)
                        (hook-amend (hasheq 'shortcuts
                                            (list (hasheq 'key "C-S-e" 'action "ext-edit"))))))))
  (define result (dispatch-hooks 'register-shortcuts (hasheq) ext-reg))
  (check-equal? (hook-result-action result) 'amend)
  (define shortcuts (hash-ref (hook-result-payload result) 'shortcuts))
  (check-equal? (length shortcuts) 1)
  (check-equal? (hash-ref (car shortcuts) 'key) "C-S-e"))

(test-case "register-shortcuts is advisory — errors default to pass"
  (check-false (critical-hook? 'register-shortcuts)))

;; ============================================================
;; Combined: Command autocompletion with extension commands
;; ============================================================

(test-case "extension commands appear in autocomplete"
  (define base-reg (make-command-registry))
  (define ext-cmds (list (cmd-entry "/deploy" "Deploy" 'general '("<target>") '("dp"))))
  (define merged (merge-extension-commands base-reg ext-cmds))
  ;; Autocomplete for "/de" should find /deploy and /deactivate
  (define matches (filter-commands merged "/de"))
  (check-true (>= (length matches) 1))
  (check-true (ormap (lambda (c) (equal? (cmd-entry-name c) "/deploy")) matches)))

(test-case "extension commands appear in all-commands"
  (define base-reg (make-command-registry))
  (define ext-cmds (list (cmd-entry "/test-ext" "Test extension" 'general '() '())))
  (define merged (merge-extension-commands base-reg ext-cmds))
  (define all (all-commands merged))
  (check-true (ormap (lambda (c) (equal? (cmd-entry-name c) "/test-ext")) all)))

(test-case "extension commands appear in help output"
  (define base-reg (make-command-registry))
  (define ext-cmds (list (cmd-entry "/custom" "Custom command" 'general '() '())))
  (define merged (merge-extension-commands base-reg ext-cmds))
  (define all (all-commands merged))
  (define custom-entry
    (for/first ([c (in-list all)]
                #:when (equal? (cmd-entry-name c) "/custom"))
      c))
  (check-not-false custom-entry)
  (check-equal? (cmd-entry-summary custom-entry) "Custom command"))
