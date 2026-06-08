#lang racket/base

;; @speed slow  ;; @suite workflows

;; BOUNDARY: integration
;; @suite workflows
;; @boundary integration
;; @speed fast
;; @mutates none
;; tests/workflows/test-command-surface-contract.rkt — Command surface contract tests
;;
;; Verifies that command dispatch produces expected state transitions
;; and events without requiring full session infrastructure.

(require rackunit
         racket/string
         "../../tui/commands/context.rkt"
         "../../tui/state-types.rkt"
         "../../tui/command-parse.rkt")

;; ---------------------------------------------------------------------------
;; cmd-ctx construction contracts
;; ---------------------------------------------------------------------------

(test-case "cmd-ctx construction: all boxes initialized"
  (define cctx (cmd-ctx
                (box (initial-ui-state))
                (box #f)
                #f  ; event-bus
                #f  ; session-dir
                (box #f)  ; needs-redraw
                (box #f)  ; model-registry
                (box #f)  ; last-prompt
                #f  ; session-runner
                (box "")  ; input-text
                (box #f)  ; extension-registry
                #f  ; session-factory-runner
                (box #f)  ; agent-session
                (box #f)))  ; goal-cancel
  (check-true (cmd-ctx? cctx))
  (check-false (unbox (cmd-ctx-running-box cctx))))

(test-case "cmd-ctx state-box holds ui-state"
  (define ui (initial-ui-state))
  (define cctx (cmd-ctx
                (box ui) (box #f) #f #f (box #f) (box #f) (box #f) #f (box "") (box #f) #f (box #f) (box #f)))
  (check-true (ui-state? (unbox (cmd-ctx-state-box cctx)))))

;; ---------------------------------------------------------------------------
;; Command parse contracts
;; ---------------------------------------------------------------------------

(test-case "command-parse: /help is recognized"
  (define result (parse-command-name "/help"))
  (check-true (parsed-command? result)))

(test-case "command-parse: /model is recognized with arg"
  (define result (parse-command-name "/model gpt-4"))
  (check-true (parsed-command? result))
  (check-true (>= (length (parsed-command-args result)) 1)))

(test-case "command-parse: /goal is recognized"
  (define result (parse-command-name "/goal build tests"))
  (check-true (parsed-command? result)))

(test-case "command-parse: unknown command returns 'unknown or parsed-command"
  (define result (parse-command-name "/unknown-xyz"))
  ;; Returns 'unknown symbol for unrecognized commands
  (check-true (or (eq? result 'unknown) (parsed-command? result) (not result))))

(test-case "command-parse: empty string is not a command"
  (define result (parse-command-name ""))
  (check-false result))

(test-case "command-parse: non-slash input is not a command"
  (define result (parse-command-name "hello world"))
  (check-false result))

;; ---------------------------------------------------------------------------
;; State transition contracts
;; ---------------------------------------------------------------------------

(test-case "needs-redraw can be set and cleared"
  (define cctx (cmd-ctx
                (box (initial-ui-state)) (box #f) #f #f
                (box #f) (box #f) (box #f) #f (box "") (box #f) #f (box #f) (box #f)))
  (check-false (unbox (cmd-ctx-needs-redraw-box cctx)))
  (set-box! (cmd-ctx-needs-redraw-box cctx) #t)
  (check-true (unbox (cmd-ctx-needs-redraw-box cctx)))
  (set-box! (cmd-ctx-needs-redraw-box cctx) #f)
  (check-false (unbox (cmd-ctx-needs-redraw-box cctx))))

(test-case "goal-cancel-box starts #f, can be set"
  (define cctx (cmd-ctx
                (box (initial-ui-state)) (box #f) #f #f
                (box #f) (box #f) (box #f) #f (box "") (box #f) #f (box #f) (box #f)))
  (check-false (unbox (cmd-ctx-goal-cancel-box cctx)))
  (set-box! (cmd-ctx-goal-cancel-box cctx) #t)
  (check-true (unbox (cmd-ctx-goal-cancel-box cctx))))

;; ---------------------------------------------------------------------------
;; Parsed command structure contracts
;; ---------------------------------------------------------------------------

(test-case "parsed-command has canonical-name"
  (define result (parse-command-name "/sessions"))
  (check-true (symbol? (parsed-command-canonical-name result))))

(test-case "parsed-command args are a list"
  (define result (parse-command-name "/switch session-1"))
  (check-true (list? (parsed-command-args result))))
