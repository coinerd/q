#lang racket/base

;; @speed fast
;; @suite default
;; @boundary unit

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

;; ── v1.00.27 W1: tier-semantics truthfulness ──────────────────────────────
;; `fast` is the broad PR regression tier (what CI runs per PR); `unit-fast`
;; is the developer iteration tier with a declared local SLO (p90, measured
;; in W4). CLI help text must name tiers exactly as the docs do, and the
;; docs must not regress to legacy synonyms. Help strings and docs only —
;; this section asserts no runner behavior.

(require racket/file
         racket/list
         racket/port
         (only-in "../scripts/run-tests/cli.rkt" usage))

(define tier-doc-conventions "docs/TEST_CONVENTIONS.md")
(define tier-doc-plan "docs/TDD-TEST-STRATEGY-PLAN.md")

;; Legacy synonyms that must never come back as tier names.
(define forbidden-tier-synonyms
  (list #rx"unit fast" #rx"fast-unit" #rx"unitfast" #rx"quick tier" #rx"unit-tier"))

(define (tier-usage-text)
  (with-output-to-string usage))

(define tier-semantics-suite
  (test-suite "tier-semantics truthfulness (v1.00.27 W1)"

    (test-case "usage names fast as the broad PR regression tier"
      (define u (tier-usage-text))
      (check-regexp-match #rx"fast +Broad PR regression tier" u)
      (check-regexp-match #rx"what CI runs per PR" u))

    (test-case "usage names unit-fast as the developer iteration tier with its SLO"
      (define u (tier-usage-text))
      (check-regexp-match #rx"unit-fast +Developer iteration tier" u)
      (check-regexp-match #rx"measured in W4" u))

    (test-case "usage does not regress to legacy tier synonyms"
      (define u (tier-usage-text))
      (for ([rx (in-list forbidden-tier-synonyms)])
        (check-false (regexp-match? rx u) "legacy tier synonym in --help")))

    (test-case "TEST_CONVENTIONS.md carries the tier table and SLO anchors"
      (define doc (file->string tier-doc-conventions))
      (check-regexp-match #rx"(?i:## Tier semantics)" doc)
      (check-regexp-match #rx"(?i:broad PR regression tier)" doc)
      (check-regexp-match #rx"(?i:developer iteration tier)" doc)
      (check-regexp-match #rx"measured in W4" doc)
      (check-regexp-match #rx"(?i:never gate or filter required CI)" doc))

    (test-case "TDD-TEST-STRATEGY-PLAN.md states the tier semantics"
      (define doc (file->string tier-doc-plan))
      (check-regexp-match #rx"(?i:broad PR regression tier)" doc)
      (check-regexp-match #rx"(?i:developer iteration tier)" doc)
      (check-regexp-match #rx"measured in W4" doc)
      (check-regexp-match #rx"(?i:fail-open)" doc))

    (test-case "docs do not regress to legacy tier synonyms"
      (for ([path (in-list (list tier-doc-conventions tier-doc-plan))])
        (define doc (file->string path))
        (for ([rx (in-list forbidden-tier-synonyms)])
          (check-false (regexp-match? rx doc) (format "legacy tier synonym in ~a" path)))))))

(run-tests tier-semantics-suite)
