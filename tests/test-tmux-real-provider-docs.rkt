#lang racket/base

;; @speed fast
;; @suite default

(require rackunit
         racket/file
         racket/runtime-path
         racket/string)

(define-runtime-path runbook "../docs/reports/TMUX-TUI-EXPLORATION-SAFETY-RUNBOOK-v0.99.44.md")
(define-runtime-path audit "../docs/reports/AUDIT-v0.99.44-FINAL.md")

(define text (file->string runbook))
(define audit-text (file->string audit))

(test-case "runbook contains the executable authorization contract"
  (for ([required (in-list '("Q_TMUX_TUI_TESTS=1"
                             "Q_TMUX_TUI_REAL_PROVIDER=1"
                             "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM=I_UNDERSTAND_COSTS"
                             "Q_TMUX_TUI_REAL_PROVIDER_HOME"))])
    (check-true (string-contains? text required) required))
  (check-false (string-contains? text "Q_TMUX_REAL_PROVIDER_KEY"))
  (check-false (string-contains? text "Q_TMUX_REAL_PROVIDER=1")))

(test-case "runbook lists exact registered scenario tags and real command"
  (for ([tag (in-list
              '("memory" "gsd" "mas" "tools" "release-audit" "durable-memory" "resume" "compact"))])
    (check-true (string-contains? text (format "`~a`" tag)) tag))
  (check-true (string-contains? text
                                "racket scripts/tmux-tui-explore.rkt --mode real --filter tools"))
  (check-true (string-contains? text "--non-gating")))

(test-case "runbook documents actual helper and cleanup contracts"
  (check-true (string-contains? text "tmux-env-home"))
  (check-false (string-contains? text "`temp-home`"))
  (check-true (string-contains? text "config.rkt"))
  (check-true (string-contains? text "dynamic-wind"))
  (check-true (string-contains? text "kill-session"))
  (check-true (string-contains? text "kill-server"))
  (check-true (string-contains? text "copied-credential HOME")))

(test-case "runbook does not advertise nonexistent selectable suites"
  (check-false (string-contains? text "| tui-tmux |"))
  (check-false (string-contains? text "| tui-tmux-real |"))
  (check-true (string-contains? text "--suite tui")))

(test-case "historical audit includes explicit v0.99.50 erratum"
  (check-true (string-contains? audit-text "v0.99.50 Erratum"))
  (check-true (string-contains? audit-text "synthetic"))
  (check-true (string-contains? audit-text "Q_TMUX_TUI_REAL_PROVIDER_CONFIRM")))
