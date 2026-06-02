#lang racket

;; BOUNDARY: integration

;; tests/test-gsd-transaction-contract.rkt — Characterization tests for /go transaction contract mismatch
;;
;; Bug report: .planning/BUG_REPORT-v0.54.x-GSD-GO-TRANSACTION-CONTRACT-MISMATCH.md
;; Bug plan: .planning/BUG_PLAN-v0.54.x-GSD-GO-TRANSACTION-CONTRACT-MISMATCH.md
;;
;; T0.1: with-gsd-transaction now accepts multi-value thunks (W1 fixed)
;; T0.2: TUI block message renders /go correctly (W2 fixed)
;; T1.0: Error path arity crash — define-values expects 2, gets 1 gsd-err (BUG)

(require rackunit
         rackunit/text-ui
         racket/match
         racket/string
         "../extensions/gsd/core.rkt"
         "../extensions/gsd/command-types.rkt"
         "../extensions/hooks.rkt"
         "../util/hook-types.rkt"
         (only-in "../extensions/api.rkt" make-extension-registry register-extension! extension)
         (only-in "../tui/commands.rkt"
                  process-extension-command
                  cmd-ctx
                  cmd-ctx-state-box
                  cmd-ctx-needs-redraw-box
                  cmd-ctx-input-text-box
                  cmd-ctx-extension-registry-box)
         (only-in "../tui/state-types.rkt"
                  initial-ui-state
                  ui-state-transcript
                  transcript-entry-text))

;; ============================================================
;; Helpers
;; ============================================================

(define (get-transcript-texts cctx)
  (define state (unbox (cmd-ctx-state-box cctx)))
  (map transcript-entry-text (ui-state-transcript state)))

(define (transcript-contains? cctx substr)
  (ormap (lambda (t) (string-contains? t substr)) (get-transcript-texts cctx)))

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

(define (make-ext-reg-with-execute-command handler)
  (define reg (make-extension-registry))
  (define ext (extension "test-gsd" "0.1" "1.0" (hasheq 'execute-command handler)))
  (register-extension! reg ext)
  reg)

;; ============================================================
;; Characterization Tests
;; ============================================================

(define gsd-transaction-contract-tests
  (test-suite "gsd-transaction-contract characterization tests"

    ;; T0.1: with-gsd-transaction contract rejects multi-value thunks
    ;; This is the ROOT CAUSE of the /go failure.
    ;; The contract says (-> any/c any/c any/c any/c) — single return value.
    ;; But launch-wave-executor uses (define-values (exec waves) (with-gsd-transaction ...))
    ;; which requires the thunk to return 2 values via (values exec waves).
    (test-case "T0.1: with-gsd-transaction accepts multi-value thunk (POST-FIX)"
      ;; POST-FIX: with-gsd-transaction removed from contract-out so it
      ;; no longer constrains thunk return arity. Multi-value thunks work.
      (define-values (exec waves)
        (with-gsd-transaction "test-multi-value"
                              (lambda () (values 'executor '(0 1 2)))
                              (lambda (e snap) (void))))
      (check-equal? exec 'executor "first value returned")
      (check-equal? waves '(0 1 2) "second value returned"))

    (test-case "T0.1b: with-gsd-transaction accepts single-value thunk"
      ;; Single-value thunks should work fine.
      (define result
        (with-gsd-transaction "test-single-value" (lambda () 'ok) (lambda (e snap) 'rolled-back)))
      (check-equal? result 'ok "single-value thunk returns normally"))

    (test-case "T0.1c: with-gsd-transaction rollback works for single-value thunk errors"
      ;; Error in thunk triggers rollback.
      (define result
        (with-gsd-transaction "test-error" (lambda () (error "boom")) (lambda (e snap) 'rolled-back)))
      (check-false (gsd-success? result) "error thunk returns gsd-err result")
      (check-true (string-contains? (gsd-command-result-message result) "boom")
                  "error message propagated"))

    ;; T0.2: TUI block message renders /go (no double-slash)
    ;; parse-extension-command returns "/go" (with slash).
    ;; POST-FIX: format string uses "Command ~a ..." (no extra slash).
    (test-case "T0.2: TUI block message shows /go (no double slash)"
      ;; Create a handler that triggers 'block by timing out.
      (define (slow-handler payload)
        (sleep 0.1)
        (hook-amend (hasheq 'text "should not reach")))
      (define ext-reg (make-ext-reg-with-execute-command slow-handler))
      (define cctx (make-test-cctx #:ext-reg ext-reg))
      (define state (unbox (cmd-ctx-state-box cctx)))

      (parameterize ([current-hook-timeout-ms 1])
        (process-extension-command cctx state))

      ;; POST-FIX: message contains "/go" (single slash)
      (check-true (transcript-contains? cctx "/go") "POST-FIX: block message shows /go")
      ;; And must NOT contain double-slash
      (check-false (transcript-contains? cctx "//go") "POST-FIX: block message has no double slash"))

    (test-case "T0.2b: TUI block message includes useful guidance"
      ;; Even with the formatting bug, the message should contain guidance.
      (define (slow-handler payload)
        (sleep 0.1)
        (hook-amend (hasheq 'text "should not reach")))
      (define ext-reg (make-ext-reg-with-execute-command slow-handler))
      (define cctx (make-test-cctx #:ext-reg ext-reg))
      (define state (unbox (cmd-ctx-state-box cctx)))

      (parameterize ([current-hook-timeout-ms 1])
        (process-extension-command cctx state))

      (check-true (transcript-contains? cctx "could not be dispatched")
                  "block message includes dispatch-failure guidance"))

    ;; ============================================================
    ;; v0.54.10 — Error-path arity crash fix
    ;; ============================================================

    ;; T1.0: POST-FIX — Using define (not define-values) with list wrapping
    ;; avoids the arity crash. The caller pattern now matches
    ;; command-handlers.rkt: launch-wave-executor.
    (test-case "T1.0: define + list wrapping handles error path safely"
      ;; The thunk returns (list exec wis) on success.
      ;; On error, with-gsd-transaction returns single gsd-err.
      ;; Both work with (define result ...) — no arity crash.
      (define result
        (with-gsd-transaction "go" (lambda () (error "plan file missing")) (lambda (e snap) (void))))
      (check-false (gsd-success? result) "error returns gsd-err")
      (check-true (string-contains? (gsd-command-result-message result) "plan file missing")
                  "error message present"))

    ;; T1.0b: Success path — list wrapping works with define + match
    (test-case "T1.0b: Success path with list wrapping + define"
      (define result
        (with-gsd-transaction "go" (lambda () (list 'my-executor '(0 1 2))) (lambda (e snap) (void))))
      (check-false (gsd-command-result? result) "success returns list, not gsd-result")
      (match-define (list exec wis) result)
      (check-equal? exec 'my-executor "executor preserved")
      (check-equal? wis '(0 1 2) "wave indices preserved"))

    ;; T1.0c: Raw define-values STILL crashes on error (documenting the hazard)
    (test-case "T1.0c: Raw define-values on error path still arity-crashes"
      ;; This confirms why the caller was changed to use define + list.
      ;; with-gsd-transaction always returns single gsd-err on exception.
      (check-exn #rx"expected number of values not received"
                 (lambda ()
                   (define-values (executor wave-indices)
                     (with-gsd-transaction "go" (lambda () (error "boom")) (lambda (e snap) (void))))
                   (void executor wave-indices))
                 "Raw define-values crashes on transaction error path"))))

(module+ main
  (run-tests gsd-transaction-contract-tests))

(module+ test
  (run-tests gsd-transaction-contract-tests))
