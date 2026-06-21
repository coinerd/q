#lang racket

;; @speed fast
;; @suite fast

;; W7 (#8481): RED-module first-slice characterization tests.
;; Tests ALL 4 RED modules via READ-ONLY characterization:
;; §1 cli/args.rkt — Table-driven flag fixture tests (Green, risk 7)
;; §2 agent/event-structs.rkt — Facade export inventory (Yellow, risk 8)
;; §3 wiring/run-modes.rkt — Mode resolution characterization (Yellow, risk 9)
;; §4 scripts/run-tests.rkt — Sub-module boundary characterization (Yellow, risk 10)
;;
;; RED modules are NOT modified — tests only.

(require rackunit)

;; ============================================================
;; §1 cli/args.rkt — Table-driven flag fixture tests
;; ============================================================

(require (only-in "../cli/args.rkt"
                  parse-cli-args
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
                  cli-config-keybindings-path
                  cli-config-print-mode?
                  cli-config-context-profile
                  cli-config-memory
                  cli-config-agent-pool
                  cli-config-parallel?
                  cli-config->runtime-config
                  q-version))

;; --- All 22 flags in FLAG-DEFINITIONS are tested individually ---

(test-case "F1: --help produces help command"
  (define cfg (parse-cli-args '("--help")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "F2: --version produces version command"
  (define cfg (parse-cli-args '("--version")))
  (check-equal? (cli-config-command cfg) 'version))

(test-case "F3: --session <id> sets command to resume"
  (define cfg (parse-cli-args '("--session" "abc123")))
  (check-equal? (cli-config-command cfg) 'resume)
  (check-equal? (cli-config-session-id cfg) "abc123"))

(test-case "F4: --model <name> sets model"
  (define cfg (parse-cli-args '("--model" "claude-3")))
  (check-equal? (cli-config-model cfg) "claude-3"))

(test-case "F5: --project-dir <path> sets project-dir"
  (define cfg (parse-cli-args '("--project-dir" "/tmp/proj")))
  (check-equal? (cli-config-project-dir cfg) "/tmp/proj"))

(test-case "F6: --config <path> sets config-path"
  (define cfg (parse-cli-args '("--config" "/etc/q.json")))
  (check-equal? (cli-config-config-path cfg) "/etc/q.json"))

(test-case "F7: --verbose sets verbose? to #t"
  (define cfg (parse-cli-args '("--verbose")))
  (check-true (cli-config-verbose? cfg)))

(test-case "F8: --max-turns <n> sets max-turns"
  (define cfg (parse-cli-args '("--max-turns" "20")))
  (check-equal? (cli-config-max-turns cfg) 20))

(test-case "F9: --max-turns with non-integer produces help"
  (define cfg (parse-cli-args '("--max-turns" "abc")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "F10: --max-turns with 0 produces help (must be positive)"
  (define cfg (parse-cli-args '("--max-turns" "0")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "F11: --no-tools sets no-tools? to #t"
  (define cfg (parse-cli-args '("--no-tools")))
  (check-true (cli-config-no-tools? cfg)))

(test-case "F12: --tool <name> accumulates into list"
  (define cfg (parse-cli-args '("--tool" "bash" "--tool" "edit")))
  (check-equal? (cli-config-tools cfg) '("bash" "edit")))

(test-case "F13: --session-dir <path> sets session-dir"
  (define cfg (parse-cli-args '("--session-dir" "/tmp/sess")))
  (check-equal? (cli-config-session-dir cfg) "/tmp/sess"))

(test-case "F14: --tui sets mode to tui"
  (define cfg (parse-cli-args '("--tui")))
  (check-equal? (cli-config-mode cfg) 'tui))

(test-case "F15: --json sets mode to json"
  (define cfg (parse-cli-args '("--json")))
  (check-equal? (cli-config-mode cfg) 'json))

(test-case "F16: --rpc sets mode to rpc"
  (define cfg (parse-cli-args '("--rpc")))
  (check-equal? (cli-config-mode cfg) 'rpc))

(test-case "F17: --gui sets mode to gui"
  (define cfg (parse-cli-args '("--gui")))
  (check-equal? (cli-config-mode cfg) 'gui))

(test-case "F18: --keybindings <path> sets keybindings-path"
  (define cfg (parse-cli-args '("--keybindings" "/etc/q/keys.json")))
  (check-equal? (cli-config-keybindings-path cfg) "/etc/q/keys.json"))

(test-case "F19: --print sets mode to print and print-mode? to #t"
  (define cfg (parse-cli-args '("--print")))
  (check-equal? (cli-config-mode cfg) 'print)
  (check-true (cli-config-print-mode? cfg)))

(test-case "F20: --memory hash sets memory to 'hash"
  (define cfg (parse-cli-args '("--memory" "hash")))
  (check-equal? (cli-config-memory cfg) 'hash))

(test-case "F20b: --memory file-jsonl sets memory to 'file-jsonl"
  (define cfg (parse-cli-args '("--memory" "file-jsonl")))
  (check-equal? (cli-config-memory cfg) 'file-jsonl))

(test-case "F20c: --memory disabled sets memory to 'disabled"
  (define cfg (parse-cli-args '("--memory" "disabled")))
  (check-equal? (cli-config-memory cfg) 'disabled))

(test-case "F20d: --memory invalid sets memory to #f (silent rejection)"
  (define cfg (parse-cli-args '("--memory" "bad-backend")))
  (check-false (cli-config-memory cfg)))

(test-case "F21: --context-profile bounded sets context-profile"
  (define cfg (parse-cli-args '("--context-profile" "bounded")))
  (check-equal? (cli-config-context-profile cfg) 'bounded))

(test-case "F21b: --context-profile full sets context-profile"
  (define cfg (parse-cli-args '("--context-profile" "full")))
  (check-equal? (cli-config-context-profile cfg) 'full))

(test-case "F21c: --context-profile invalid defaults to 'off"
  (define cfg (parse-cli-args '("--context-profile" "nonsense")))
  (check-equal? (cli-config-context-profile cfg) 'off))

(test-case "F22: --agent-pool <n> sets agent-pool"
  (define cfg (parse-cli-args '("--agent-pool" "5")))
  (check-equal? (cli-config-agent-pool cfg) 5))

(test-case "F22b: --parallel sets parallel? to #t"
  (define cfg (parse-cli-args '("--parallel")))
  (check-true (cli-config-parallel? cfg)))

;; --- Short flags ---

(test-case "S1: -h produces help command"
  (define cfg (parse-cli-args '("-h")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "S2: -v sets verbose?"
  (define cfg (parse-cli-args '("-v")))
  (check-true (cli-config-verbose? cfg)))

(test-case "S3: -p sets print mode"
  (define cfg (parse-cli-args '("-p")))
  (check-equal? (cli-config-mode cfg) 'print)
  (check-true (cli-config-print-mode? cfg)))

(test-case "S4: -m hash sets memory"
  (define cfg (parse-cli-args '("-m" "hash")))
  (check-equal? (cli-config-memory cfg) 'hash))

;; --- Subcommands ---

(test-case "CMD1: doctor subcommand"
  (define cfg (parse-cli-args '("doctor")))
  (check-equal? (cli-config-command cfg) 'doctor))

(test-case "CMD2: init subcommand"
  (define cfg (parse-cli-args '("init")))
  (check-equal? (cli-config-command cfg) 'init))

(test-case "CMD3: sessions list subcommand"
  (define cfg (parse-cli-args '("sessions" "list")))
  (check-equal? (cli-config-command cfg) 'sessions)
  (check-equal? (cli-config-sessions-subcommand cfg) 'list))

(test-case "CMD4: sessions info subcommand with args"
  (define cfg (parse-cli-args '("sessions" "info" "session-id-123")))
  (check-equal? (cli-config-sessions-subcommand cfg) 'info)
  (check-equal? (cli-config-sessions-args cfg) '("session-id-123")))

(test-case "CMD5: sessions delete subcommand"
  (define cfg (parse-cli-args '("sessions" "delete" "abc")))
  (check-equal? (cli-config-sessions-subcommand cfg) 'delete))

(test-case "CMD6: sessions verify subcommand"
  (define cfg (parse-cli-args '("sessions" "verify")))
  (check-equal? (cli-config-sessions-subcommand cfg) 'verify))

(test-case "CMD7: sessions with invalid subcommand produces help"
  (define cfg (parse-cli-args '("sessions" "bogus")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "CMD8: verify-session top-level command"
  (define cfg (parse-cli-args '("verify-session" "/path/to/sess.json")))
  (check-equal? (cli-config-command cfg) 'sessions)
  (check-equal? (cli-config-sessions-subcommand cfg) 'verify)
  (check-true (pair? (cli-config-sessions-args cfg))))

;; --- Edge cases ---

(test-case "E1: unknown -- flag produces help"
  (define cfg (parse-cli-args '("--bogus-flag")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "E2: --max-turns at end without value produces help"
  (define cfg (parse-cli-args '("--max-turns")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "E3: --model at end without value produces help"
  (define cfg (parse-cli-args '("--model")))
  (check-equal? (cli-config-command cfg) 'help))

(test-case "E4: default config has max-turns 10"
  (define cfg (parse-cli-args '()))
  (check-equal? (cli-config-max-turns cfg) 10))

(test-case "E5: default config has empty tools list"
  (define cfg (parse-cli-args '()))
  (check-equal? (cli-config-tools cfg) '()))

(test-case "E6: cli-config->runtime-config includes context-profile when set"
  (define cfg (parse-cli-args '("--context-profile" "bounded")))
  (define rt (cli-config->runtime-config cfg))
  ;; context-profile is NOT propagated in current cli-config->runtime-config
  ;; This documents the CURRENT behavior (gap)
  (check-false (hash-has-key? rt 'context-profile)))

(test-case "E7: cli-config->runtime-config includes agent-pool when set"
  (define cfg (parse-cli-args '("--agent-pool" "5")))
  (define rt (cli-config->runtime-config cfg))
  ;; agent-pool is NOT propagated — documents the gap
  (check-false (hash-has-key? rt 'agent-pool)))

(test-case "E8: q-version is a non-empty string"
  (check-true (and (string? q-version) (> (string-length q-version) 0))))

;; ============================================================
;; §2 agent/event-structs.rkt — Facade export inventory
;; ============================================================

;; Test that ALL exported identifiers from the facade resolve as procedures.
;; This is a golden-master test: if any identifier is removed or renamed,
;; this test will fail.

(define event-facade-exports
  ;; base
  '(typed-event typed-event?
                typed-event-type
                ;; turn-events
                turn-start-event
                turn-start-event?
                make-turn-start-event
                turn-end-event
                turn-end-event?
                make-turn-end-event
                ;; message-events
                message-start-event
                message-start-event?
                make-message-start-event
                message-end-event
                message-end-event?
                make-message-end-event
                ;; tool-events
                tool-call-event
                tool-call-event?
                make-tool-call-event
                tool-result-event
                tool-result-event?
                make-tool-result-event
                ;; provider-events
                provider-request-event
                provider-request-event?
                make-provider-request-event
                provider-response-event
                provider-response-event?
                make-provider-response-event))

(test-case "EV1: all event struct identifiers resolve as procedures"
  (for ([sym (in-list event-facade-exports)])
    (define v (dynamic-require "../agent/event-structs.rkt" sym))
    (check-true (or (procedure? v) (struct-predicate-procedure? v) (struct-accessor-procedure? v))
                (format "~a should be a procedure" sym))))

(test-case "EV2: typed-event-type accessor returns symbol for constructed event"
  (define make-tse (dynamic-require "../agent/event-structs.rkt" 'make-turn-start-event))
  (define te-type (dynamic-require "../agent/event-structs.rkt" 'typed-event-type))
  (define evt (make-tse #:session-id "s1" #:turn-id "t1" #:model "test" #:provider #f))
  ;; typed-event-type is the accessor; returns string type tag
  (check-equal? (te-type evt) "turn.started"))

(test-case "EV3: typed-event? predicate accepts event subtypes"
  (define make-tse (dynamic-require "../agent/event-structs.rkt" 'make-turn-start-event))
  (define te? (dynamic-require "../agent/event-structs.rkt" 'typed-event?))
  (define evt (make-tse #:session-id "s1" #:turn-id "t1" #:model "test" #:provider #f))
  (check-true (te? evt)))

;; ============================================================
;; §3 wiring/run-modes.rkt — Mode resolution characterization
;; ============================================================

(require (only-in "../wiring/run-modes.rkt" mode-for-config))

(test-case "M1: mode-for-config interactive (default)"
  (define cfg (parse-cli-args '()))
  (check-equal? (mode-for-config cfg) 'interactive))

(test-case "M2: mode-for-config tui"
  (define cfg (parse-cli-args '("--tui")))
  (check-equal? (mode-for-config cfg) 'tui))

(test-case "M3: mode-for-config json"
  (define cfg (parse-cli-args '("--json")))
  (check-equal? (mode-for-config cfg) 'json))

(test-case "M4: mode-for-config rpc"
  (define cfg (parse-cli-args '("--rpc")))
  (check-equal? (mode-for-config cfg) 'rpc))

(test-case "M5: mode-for-config gui"
  (define cfg (parse-cli-args '("--gui")))
  (check-equal? (mode-for-config cfg) 'gui))

(test-case "M6: mode-for-config print"
  (define cfg (parse-cli-args '("--print")))
  (check-equal? (mode-for-config cfg) 'print))

(test-case "M7: mode-for-config single (prompt positional)"
  (define cfg (parse-cli-args '("hello")))
  (check-equal? (mode-for-config cfg) 'single))

(test-case "M8: mode-for-config returns symbol for all modes"
  (for ([args '(() ("--tui") ("--json") ("--rpc") ("--gui") ("--print") ("hello"))])
    (define cfg (parse-cli-args args))
    (check-true (symbol? (mode-for-config cfg)))))

;; ============================================================
;; §4 scripts/run-tests.rkt — Sub-module boundary characterization
;; ============================================================

;; run-tests.rkt has (main) at module level — can't import directly.
;; Test sub-modules instead.

(test-case "RT1: parse.rkt sub-module exports test-file-result?"
  (check-not-false (dynamic-require "../scripts/run-tests/parse.rkt" 'test-file-result?)))

(test-case "RT2: parse.rkt sub-module exports test-file-result"
  (check-not-false (dynamic-require "../scripts/run-tests/parse.rkt" 'test-file-result)))

(test-case "RT3: classify.rkt sub-module exports base-dir"
  (check-not-false (dynamic-require "../scripts/run-tests/classify.rkt" 'base-dir)))

(test-case "RT4: reporting.rkt sub-module exports print-summary"
  (check-not-false (dynamic-require "../scripts/run-tests/reporting.rkt" 'print-summary)))

(test-case "RT5: inventory.rkt sub-module exports print-inventory"
  (check-not-false (dynamic-require "../scripts/run-tests/inventory.rkt" 'print-inventory)))

(test-case "RT6: gate-evidence.rkt sub-module exports record-gate-evidence!"
  (check-not-false (dynamic-require "../scripts/run-tests/gate-evidence.rkt" 'record-gate-evidence!)))

(test-case "RT7: run-tests cli.rkt sub-module is loadable"
  (check-not-false (dynamic-require "../scripts/run-tests/cli.rkt" #f)))
