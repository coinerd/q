#lang racket

;; BOUNDARY: integration

;; tests/test-execute-command-hook.rkt — Characterization tests for /go unknown command bug
;;
;; Bug report: .planning/BUG_REPORT-v0.54.x-GSD-GO-UNKNOWN-COMMAND.md
;;
;; T0.1: execute-command hook timeout should NOT produce generic "Unknown command"
;; T0.2: execute-command hook is now critical (returns 'block on timeout/error)
;;
;; POST-FIX behavior (W1 applied):
;; - T0.2/T0.2b/T0.2c expect 'block (critical hook)
;; - T0.1 still falls to unknown (W2 will fix TUI fallback)

(require rackunit
         rackunit/text-ui
         racket/string
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt"
         (only-in "../extensions/api.rkt" make-extension-registry register-extension! extension)
         (only-in "../tui/commands.rkt"
                  process-extension-command
                  cmd-ctx
                  cmd-ctx?
                  cmd-ctx-state-box
                  cmd-ctx-running-box
                  cmd-ctx-needs-redraw-box
                  cmd-ctx-input-text-box
                  cmd-ctx-extension-registry-box)
         (only-in "../tui/state.rkt" initial-ui-state ui-state-transcript)
         (only-in "../tui/state-types.rkt" transcript-entry-text))

;; ============================================================
;; Helpers
;; ============================================================

(define (make-ext-reg-with-execute-command handler)
  (define reg (make-extension-registry))
  (define ext (extension "test-gsd" "0.1" "1.0" (hasheq 'execute-command handler)))
  (register-extension! reg ext)
  reg)

(define (make-test-cctx #:ext-reg [ext-reg #f])
  (cmd-ctx (box (initial-ui-state))
           (box #t) ; running
           #f ; event-bus
           #f ; session-dir
           (box #f) ; needs-redraw
           #f ; model-registry-box
           (box #f) ; last-prompt-box
           #f ; session-runner
           (box "/go") ; input-text-box
           (box ext-reg) ; extension-registry-box
           #f ; session-factory-runner
           (box #f) ; agent-session-box
           (box #f))) ; goal-cancel-box

(define (get-transcript-texts cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (map transcript-entry-text (ui-state-transcript state)))

(define (transcript-contains? cctx substr)
  (ormap (lambda (t) (string-contains? t substr)) (get-transcript-texts cctx)))

;; ============================================================
;; T0.2: Hook semantics — execute-command is currently advisory
;; ============================================================

(define execute-command-hook-tests
  (test-suite "execute-command-hook characterization tests"
    (test-case "T0.2: execute-command hook timeout returns block (critical default)"
      ;; POST-FIX: execute-command IS in critical-hooks,
      ;; so timeout returns 'block (safety-first default).
      (parameterize ([current-hook-timeout-ms 1])
        (define (slow-handler payload)
          (sleep 0.1) ; way longer than 1ms
          (hook-amend (hasheq 'text "should not reach")))
        (define reg (make-ext-reg-with-execute-command slow-handler))
        (define result (dispatch-hooks 'execute-command (hasheq 'command "/go" 'input "/go") reg))
        ;; POST-FIX: critical returns 'block
        (check-equal? (hook-result-action result)
                      'block
                      "POST-FIX: execute-command timeout returns block (critical)")))

    (test-case "T0.2b: execute-command hook error returns block (critical default)"
      ;; POST-FIX: errors also default to block for critical hooks
      (define (failing-handler payload)
        (error "handler crash!"))
      (define reg (make-ext-reg-with-execute-command failing-handler))
      (define result (dispatch-hooks 'execute-command (hasheq 'command "/go" 'input "/go") reg))
      (check-equal? (hook-result-action result)
                    'block
                    "POST-FIX: execute-command error returns block (critical)"))

    (test-case "T0.2c: execute-command IS in critical-hooks"
      ;; POST-FIX: verify execute-command is now classified as critical
      (check-true (critical-hook? 'execute-command) "POST-FIX: execute-command is critical"))

    ;; ============================================================
    ;; T0.1: TUI integration — /go timeout produces unknown command
    ;; ============================================================

    (test-case "T0.1: /go with extension timeout falls to unknown command"
      ;; PRE-FIX characterization: when extension handler times out,
      ;; process-extension-command shows "Unknown command" because
      ;; the timed-out advisory hook returns 'pass (not 'amend).
      ;; After W1+W2 fixes, this should show a better message.
      (define (slow-handler payload)
        (sleep 0.1)
        (hook-amend (hasheq 'text "should not reach")))
      (define ext-reg (make-ext-reg-with-execute-command slow-handler))
      (define cctx (make-test-cctx #:ext-reg ext-reg))
      (define state (unbox (cmd-ctx-state-box cctx)))

      (parameterize ([current-hook-timeout-ms 1])
        (process-extension-command cctx state))

      ;; POST-FIX (W1+W2): hook returns 'block (critical), TUI shows specific message
      (check-true (transcript-contains? cctx "could not be dispatched")
                  "POST-FIX: /go timeout shows command-dispatch failure message")
      (check-false (transcript-contains? cctx "Unknown command")
                   "POST-FIX: /go timeout does NOT show generic unknown command"))

    (test-case "T0.1b: /go with no extension registry shows unknown command"
      ;; No extension registry → no handler → falls to unknown.
      ;; This is EXPECTED behavior (no GSD extension loaded).
      (define cctx (make-test-cctx #:ext-reg #f))
      (define state (unbox (cmd-ctx-state-box cctx)))
      (process-extension-command cctx state)
      (check-true (transcript-contains? cctx "Unknown command")
                  "no ext-reg: /go shows unknown (expected)"))))

(module+ main
  (run-tests execute-command-hook-tests))
