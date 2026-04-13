#lang racket

;; tests/test-cli-args.rkt — tests for cli/args.rkt (#224)
;;
;; Covers:
;;   parse-cli-args: flags, subcommands, positional prompt
;;   cli-config struct and accessors
;;   cli-config->runtime-config: hash conversion

(require rackunit
         (only-in "../cli/args.rkt"
                  parse-cli-args
                  cli-config
                  cli-config?
                  cli-config-command
                  cli-config-session-id
                  cli-config-prompt
                  cli-config-model
                  cli-config-mode
                  cli-config-project-dir
                  cli-config-config-path
                  cli-config-verbose?
                  cli-config-max-turns
                  cli-config-no-tools?
                  cli-config-tools
                  cli-config-session-dir
                  cli-config-sessions-subcommand
                  cli-config-sessions-args
                  cli-config->runtime-config))

;; ============================================================
;; 1. No args → command is 'chat (default interactive)
;; ============================================================

(test-case "no args → command is 'chat, mode is 'interactive"
  (define cfg (parse-cli-args '()))
  (check-equal? (cli-config-command cfg) 'chat)
  (check-equal? (cli-config-mode cfg) 'interactive))

;; ============================================================
;; 2. --help → command is 'help
;; ============================================================

(test-case "--help → command is 'help"
  (define cfg (parse-cli-args '("--help")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "-h → command is 'help"
  (define cfg (parse-cli-args '("-h")))
  (check-equal? (cli-config-command cfg) 'help))

;; ============================================================
;; 3. --version → command is 'version
;; ============================================================

(test-case "--version → command is 'version"
  (define cfg (parse-cli-args '("--version")))
  (check-equal? (cli-config-command cfg) 'version))

;; ============================================================
;; 4. --model and prompt positional → correct fields
;; ============================================================

(test-case "--model gpt-4 \"hello\" → model, prompt, mode=single"
  (define cfg (parse-cli-args '("--model" "gpt-4" "hello")))
  (check-equal? (cli-config-model cfg) "gpt-4")
  (check-equal? (cli-config-prompt cfg) "hello")
  (check-equal? (cli-config-mode cfg) 'single)
  (check-equal? (cli-config-command cfg) 'prompt))

;; ============================================================
;; 5. --json flag
;; ============================================================

(test-case "--json → mode is 'json"
  (define cfg (parse-cli-args '("--json")))
  (check-equal? (cli-config-mode cfg) 'json))

;; ============================================================
;; 6. --rpc flag
;; ============================================================

(test-case "--rpc → mode is 'rpc"
  (define cfg (parse-cli-args '("--rpc")))
  (check-equal? (cli-config-mode cfg) 'rpc))

;; ============================================================
;; 7. --verbose / -v
;; ============================================================

(test-case "--verbose → verbose? is #t"
  (define cfg (parse-cli-args '("--verbose")))
  (check-true (cli-config-verbose? cfg)))

(test-case "-v → verbose? is #t"
  (define cfg (parse-cli-args '("-v")))
  (check-true (cli-config-verbose? cfg)))

;; ============================================================
;; 8. --max-turns
;; ============================================================

(test-case "--max-turns 5 → max-turns is 5"
  (define cfg (parse-cli-args '("--max-turns" "5")))
  (check-equal? (cli-config-max-turns cfg) 5))

(test-case "default max-turns is 10"
  (define cfg (parse-cli-args '()))
  (check-equal? (cli-config-max-turns cfg) 10))

(test-case "--max-turns with non-number → command is 'help"
  (define cfg (parse-cli-args '("--max-turns" "abc")))
  (check-equal? (cli-config-command cfg) 'help))

;; ============================================================
;; 9. --no-tools
;; ============================================================

(test-case "--no-tools → no-tools? is #t"
  (define cfg (parse-cli-args '("--no-tools")))
  (check-true (cli-config-no-tools? cfg)))

(test-case "default no-tools? is #f"
  (define cfg (parse-cli-args '()))
  (check-false (cli-config-no-tools? cfg)))

;; ============================================================
;; 10. --tool (repeatable)
;; ============================================================

(test-case "--tool bash --tool grep → tools is '(\"bash\" \"grep\")"
  (define cfg (parse-cli-args '("--tool" "bash" "--tool" "grep")))
  (check-equal? (cli-config-tools cfg) '("bash" "grep")))

(test-case "default tools is empty list"
  (define cfg (parse-cli-args '()))
  (check-equal? (cli-config-tools cfg) '()))

;; ============================================================
;; 11. sessions list
;; ============================================================

(test-case "sessions list → command 'sessions, subcommand 'list"
  (define cfg (parse-cli-args '("sessions" "list")))
  (check-equal? (cli-config-command cfg) 'sessions)
  (check-equal? (cli-config-sessions-subcommand cfg) 'list)
  (check-equal? (cli-config-sessions-args cfg) '()))

;; ============================================================
;; 12. sessions info <id>
;; ============================================================

(test-case "sessions info abc123 → subcommand 'info, args '(\"abc123\")"
  (define cfg (parse-cli-args '("sessions" "info" "abc123")))
  (check-equal? (cli-config-sessions-subcommand cfg) 'info)
  (check-equal? (cli-config-sessions-args cfg) '("abc123")))

;; ============================================================
;; 13. sessions delete <id>
;; ============================================================

(test-case "sessions delete xyz → subcommand 'delete, args '(\"xyz\")"
  (define cfg (parse-cli-args '("sessions" "delete" "xyz")))
  (check-equal? (cli-config-sessions-subcommand cfg) 'delete)
  (check-equal? (cli-config-sessions-args cfg) '("xyz")))

;; ============================================================
;; 14. --resume / --session <id>
;; ============================================================

(test-case "--session sess1 → session-id is \"sess1\", command is 'resume"
  (define cfg (parse-cli-args '("--session" "sess1")))
  (check-equal? (cli-config-session-id cfg) "sess1")
  (check-equal? (cli-config-command cfg) 'resume))

;; ============================================================
;; 15. cli-config->runtime-config returns hash with expected keys
;; ============================================================

(test-case "cli-config->runtime-config returns hash with required keys"
  (define cfg (parse-cli-args '("--model" "claude-3" "--max-turns" "3")))
  (define rt (cli-config->runtime-config cfg))
  (check-true (hash? rt))
  (check-equal? (hash-ref rt 'max-iterations) 3)
  (check-equal? (hash-ref rt 'model) "claude-3")
  (check-false (hash-ref rt 'no-tools? #f)))

(test-case "cli-config->runtime-config omits unset optional keys"
  (define cfg (parse-cli-args '()))
  (define rt (cli-config->runtime-config cfg))
  (check-true (hash? rt))
  (check-false (hash-has-key? rt 'model))
  (check-false (hash-has-key? rt 'session-id)))

;; ============================================================
;; 16. --tui flag
;; ============================================================

(test-case "--tui → mode is 'tui"
  (define cfg (parse-cli-args '("--tui")))
  (check-equal? (cli-config-mode cfg) 'tui))

;; ============================================================
;; 17. --project-dir
;; ============================================================

(test-case "--project-dir /tmp/proj → project-dir is \"/tmp/proj\""
  (define cfg (parse-cli-args '("--project-dir" "/tmp/proj")))
  (check-equal? (cli-config-project-dir cfg) "/tmp/proj"))

;; ============================================================
;; 18. --config
;; ============================================================

(test-case "--config my.conf → config-path is \"my.conf\""
  (define cfg (parse-cli-args '("--config" "my.conf")))
  (check-equal? (cli-config-config-path cfg) "my.conf"))

;; ============================================================
;; 19. --session-dir
;; ============================================================

(test-case "--session-dir /tmp/sessions → session-dir is \"/tmp/sessions\""
  (define cfg (parse-cli-args '("--session-dir" "/tmp/sessions")))
  (check-equal? (cli-config-session-dir cfg) "/tmp/sessions"))

;; ============================================================
;; 20. Unknown flag → help
;; ============================================================

(test-case "unknown flag → command is 'help"
  (define cfg (parse-cli-args '("--bogus-flag")))
  (check-equal? (cli-config-command cfg) 'help))

;; ============================================================
;; 21. cli-config struct is transparent
;; ============================================================

(test-case "cli-config is recognized by cli-config?"
  (define cfg (parse-cli-args '()))
  (check-true (cli-config? cfg)))

;; ============================================================
;; 22. doctor subcommand
;; ============================================================

(test-case "doctor → command is 'doctor"
  (define cfg (parse-cli-args '("doctor")))
  (check-equal? (cli-config-command cfg) 'doctor))
