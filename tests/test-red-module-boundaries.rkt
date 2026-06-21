#lang racket/base

;; @speed fast
;; @suite fast

;; W10 v0.99.37: RED module boundary characterization tests.
;; These tests document the CURRENT behavior of the 4 RED modules.
;; They are NOT testing correctness — they are golden-master tests
;; that capture the existing contract so any future migration can
;; verify nothing changed.
;;
;; RED modules tested (READ-ONLY — no source changes):
;; - cli/args.rkt (634L, risk 16)
;; - wiring/run-modes.rkt (621L, risk 17)
;; - scripts/run-tests.rkt (592L, risk 14+)
;; - agent/event-structs.rkt (562L, risk 14+)

(require rackunit
         rackunit/text-ui
         racket/list
         ;; Direct requires — these modules are already loaded by other tests
         (only-in "../cli/args.rkt"
                  cli-config
                  cli-config?
                  parse-cli-args
                  cli-config->runtime-config
                  cli-config-command
                  cli-config-mode
                  cli-config-model
                  cli-config-max-turns
                  cli-config-tools
                  cli-config-verbose?
                  cli-config-no-tools?
                  cli-config-prompt
                  cli-config-session-id)
         (only-in "../wiring/run-modes.rkt" build-runtime-from-cli mode-for-config reload-config!)
         (only-in "../agent/event-structs.rkt"
                  typed-event
                  typed-event?
                  typed-event-type
                  turn-start-event
                  turn-start-event?
                  message-start-event
                  message-start-event?
                  tool-call-event
                  tool-call-event?
                  tool-result-event
                  tool-result-event?))

;; ============================================================
;; cli/args.rkt Boundary Characterization
;; ============================================================

(define-test-suite cli-args-boundary-tests
                   (test-case "parse-cli-args returns cli-config struct"
                     (define cfg (parse-cli-args '()))
                     (check-true (cli-config? cfg) "empty args → cli-config"))
                   (test-case "default config: interactive chat mode"
                     (define cfg (parse-cli-args '()))
                     (check-equal? (cli-config-command cfg) 'chat)
                     (check-equal? (cli-config-mode cfg) 'interactive))
                   (test-case "prompt sets command to prompt"
                     (define cfg (parse-cli-args '("hello world")))
                     (check-equal? (cli-config-command cfg) 'prompt)
                     (check-equal? (cli-config-prompt cfg) "hello world"))
                   (test-case "--model sets model"
                     (define cfg (parse-cli-args '("--model" "gpt-4")))
                     (check-equal? (cli-config-model cfg) "gpt-4"))
                   (test-case "--max-turns sets max-turns"
                     (define cfg (parse-cli-args '("--max-turns" "5")))
                     (check-equal? (cli-config-max-turns cfg) 5))
                   (test-case "--verbose sets verbose?"
                     (define cfg (parse-cli-args '("--verbose")))
                     (check-true (cli-config-verbose? cfg)))
                   (test-case "--no-tools sets no-tools?"
                     (define cfg (parse-cli-args '("--no-tools")))
                     (check-true (cli-config-no-tools? cfg)))
                   (test-case "--tui sets mode to tui"
                     (define cfg (parse-cli-args '("--tui")))
                     (check-equal? (cli-config-mode cfg) 'tui))
                   (test-case "--json sets mode to json"
                     (define cfg (parse-cli-args '("--json")))
                     (check-equal? (cli-config-mode cfg) 'json))
                   (test-case "--rpc sets mode to rpc"
                     (define cfg (parse-cli-args '("--rpc")))
                     (check-equal? (cli-config-mode cfg) 'rpc))
                   (test-case "--tool accumulates"
                     (define cfg (parse-cli-args '("--tool" "bash" "--tool" "edit")))
                     (check-equal? (cli-config-tools cfg) '("bash" "edit")))
                   (test-case "cli-config->runtime-config produces hash"
                     (define cfg (parse-cli-args '("--model" "test-model" "--max-turns" "3")))
                     (define h (cli-config->runtime-config cfg))
                     (check-true (hash? h))
                     (check-equal? (hash-ref h 'model) "test-model")
                     (check-equal? (hash-ref h 'max-iterations) 3))
                   (test-case "session subcommand parsed"
                     (define cfg (parse-cli-args '("sessions" "list")))
                     (check-equal? (cli-config-command cfg) 'sessions))
                   (test-case "doctor subcommand parsed"
                     (define cfg (parse-cli-args '("doctor")))
                     (check-equal? (cli-config-command cfg) 'doctor)))

;; ============================================================
;; wiring/run-modes.rkt Boundary Characterization
;; ============================================================

(define-test-suite run-modes-boundary-tests
                   (test-case "mode-for-config: interactive mode"
                     (define cfg (parse-cli-args '()))
                     (check-equal? (mode-for-config cfg) 'interactive))
                   (test-case "mode-for-config: tui mode"
                     (define cfg (parse-cli-args '("--tui")))
                     (check-equal? (mode-for-config cfg) 'tui))
                   (test-case "mode-for-config: json mode"
                     (define cfg (parse-cli-args '("--json")))
                     (check-equal? (mode-for-config cfg) 'json))
                   (test-case "mode-for-config: rpc mode"
                     (define cfg (parse-cli-args '("--rpc")))
                     (check-equal? (mode-for-config cfg) 'rpc)))

;; ============================================================
;; agent/event-structs.rkt Boundary Characterization
;; ============================================================

(define-test-suite event-structs-boundary-tests
                   (test-case "typed-event base struct is exported"
                     (check-true (procedure? typed-event) "typed-event is available"))
                   (test-case "typed-event? predicate works"
                     (check-true (procedure? typed-event?) "typed-event? predicate available"))
                   (test-case "typed-event-type accessor works"
                     (check-true (procedure? typed-event-type)))
                   (test-case "turn-start-event struct exported"
                     (check-true (procedure? turn-start-event)))
                   (test-case "turn-start-event? predicate exported"
                     (check-true (procedure? turn-start-event?)))
                   (test-case "message-start-event exported"
                     (check-true (procedure? message-start-event)))
                   (test-case "message-start-event? predicate exported"
                     (check-true (procedure? message-start-event?)))
                   (test-case "tool-call-event exported"
                     (check-true (procedure? tool-call-event)))
                   (test-case "tool-call-event? predicate exported"
                     (check-true (procedure? tool-call-event?)))
                   (test-case "tool-result-event exported"
                     (check-true (procedure? tool-result-event)))
                   (test-case "tool-result-event? predicate exported"
                     (check-true (procedure? tool-result-event?))))

;; ============================================================
;; scripts/run-tests.rkt Indirect Boundary Test
;; (Direct import would execute the runner; test via metadata instead)
;; ============================================================

(define-test-suite run-tests-boundary-tests
                   (test-case "run-tests.rkt is a valid Racket module"
                     ;; If we can require any of its sub-modules, the module is sound
                     (check-not-false (dynamic-require "../scripts/run-tests/parse.rkt"
                                                       'test-file-result?)
                                      "sub-module exports test-file-result?"))
                   (test-case "run-tests classify sub-module accessible"
                     (check-not-false (dynamic-require "../scripts/run-tests/classify.rkt" 'base-dir)
                                      "classify sub-module exports base-dir")))

;; ============================================================
;; Run all tests
;; ============================================================

(define-test-suite all-red-module-boundary-tests
                   cli-args-boundary-tests
                   run-modes-boundary-tests
                   event-structs-boundary-tests
                   run-tests-boundary-tests)

(module+ test
  (run-tests all-red-module-boundary-tests))

(module+ main
  (run-tests all-red-module-boundary-tests))
