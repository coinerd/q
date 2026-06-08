#lang racket

;; @speed fast
;; @suite default

;; BOUNDARY: unit

;; tests/test-command-parity.rkt -- Command conformance parity fixture (R14)
;;
;; Asserts that commands registered via extensions are parseable
;; by the TUI command parser, and that core commands are present.

(require rackunit
         rackunit/text-ui
         (only-in "../tui/command-parse.rkt" make-command-table))

(define tui-table (make-command-table))
(define tui-names (sort (hash-keys tui-table) string<?))

;; TUI commands that should exist in the command table
(define expected-commands
  (list "/help"
        "/h"
        "/?" ;; help
        "/quit"
        "/q"
        "/exit" ;; quit
        "/clear"
        "/cls" ;; clear
        "/compact" ;; compact
        "/switch" ;; switch
        "/model"
        "/m" ;; model
        "/status"
        "/st"
        "/info" ;; status
        "/retry"
        "/r" ;; retry
        "/cancel"
        "/stop"
        "/interrupt" ;; interrupt
        "/fork" ;; fork
        "/history" ;; history
        "/branches" ;; branches
        "/leaves" ;; leaves
        "/children" ;; children
        "/tree" ;; tree
        "/sessions" ;; sessions
        "/reload" ;; reload
        "/name" ;; name
        "/activate"
        "/a" ;; activate
        "/login" ;; login (#5331)
        "/goal"
        "/g" ;; goal
        ))

(define parity-suite
  (test-suite "Command conformance parity"

    (test-case "expected TUI commands are parseable"
      (for ([cmd (in-list expected-commands)])
        (check hash-has-key?
               tui-table
               cmd
               (format "Expected command ~a not found in TUI command table" cmd))))

    (test-case "TUI command table has core commands"
      (for ([cmd '("/help" "/quit" "/clear" "/compact" "/switch" "/model")])
        (check hash-has-key? tui-table cmd (format "Core command ~a missing from TUI table" cmd))))
    (test-case "all TUI commands are in expected list -- no stale entries"
      (for ([cmd (in-list (sort (hash-keys tui-table) string<?))])
        (check member
               cmd
               expected-commands
               (format "TUI command ~a not in expected list -- stale or untested?" cmd))))))

(run-tests parity-suite)
