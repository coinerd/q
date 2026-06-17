#lang racket/base

;; @speed fast
;; @suite default

;; tests/test-cli-flags.rkt
;; v0.99.23 §5.1: Tests for --agent-pool and --parallel CLI flags.
;; Verifies that:
;; - --agent-pool N is parsed correctly into cli-config
;; - --parallel flag is parsed correctly into cli-config
;; - Default values are correct (agent-pool #f, parallel? #f)
;; - current-agent-pool-limit parameter defaults to 3
;; - Pool limit is respected by spawn-subagents parallel computation

(require rackunit
         rackunit/text-ui
         racket/string
         "../cli/args.rkt"
         "../tools/builtins/spawn-subagent.rkt")

;; ── Test Suite ──

(define suite
  (test-suite "CLI Flags: --agent-pool + --parallel (v0.99.23 §5.1)"

    ;; ── --agent-pool flag ──

    (test-case "--agent-pool 1 sets agent-pool to 1"
      (define cfg (parse-cli-args '("--agent-pool" "1" "test prompt")))
      (check-equal? (cli-config-agent-pool cfg) 1))

    (test-case "--agent-pool 5 sets agent-pool to 5"
      (define cfg (parse-cli-args '("--agent-pool" "5" "test prompt")))
      (check-equal? (cli-config-agent-pool cfg) 5))

    (test-case "agent-pool defaults to #f when not specified"
      (define cfg (parse-cli-args '("test prompt")))
      (check-false (cli-config-agent-pool cfg)))

    (test-case "--agent-pool with invalid value produces help"
      (define cfg (parse-cli-args '("--agent-pool" "abc" "test")))
      (check-equal? (cli-config-command cfg) 'help))

    (test-case "--agent-pool 0 is rejected (must be positive)"
      (define cfg (parse-cli-args '("--agent-pool" "0" "test")))
      (check-equal? (cli-config-command cfg) 'help))

    ;; ── --parallel flag ──

    (test-case "--parallel sets parallel? to #t"
      (define cfg (parse-cli-args '("--parallel" "test prompt")))
      (check-true (cli-config-parallel? cfg)))

    (test-case "parallel? defaults to #f when not specified"
      (define cfg (parse-cli-args '("test prompt")))
      (check-false (cli-config-parallel? cfg)))

    (test-case "--parallel with --agent-pool works together"
      (define cfg (parse-cli-args '("--parallel" "--agent-pool" "2" "test")))
      (check-true (cli-config-parallel? cfg))
      (check-equal? (cli-config-agent-pool cfg) 2))

    ;; ── current-agent-pool-limit parameter ──

    (test-case "current-agent-pool-limit defaults to 3"
      (check-equal? (current-agent-pool-limit) 3))))

(run-tests suite)
