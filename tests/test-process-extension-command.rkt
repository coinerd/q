#lang racket/base

;; BOUNDARY: integration

;; tests/test-process-extension-command.rkt -- Tests for process-extension-command (N-10)

(require rackunit
         "../tui/commands.rkt"
         "../tui/state-types.rkt")

;; ============================================================
;; N-10: process-extension-command basic interface tests
;; ============================================================

(define (make-test-cctx input-text)
  (define state-box (box (initial-ui-state)))
  (define input-box (box input-text))
  (define redraw-box (box #f))
  (cmd-ctx state-box ;; state-box
           (box #f) ;; running-box
           #f ;; event-bus
           #f ;; session-dir
           redraw-box ;; needs-redraw-box
           (box #f) ;; model-registry-box
           (box #f) ;; last-prompt-box
           (box #f) ;; session-runner
           input-box ;; input-text-box
           (box #f) ;; extension-registry-box (no extensions)
           (box #f) ;; session-factory-runner
           (box #f) ;; agent-session-box
           (box #f))) ;; goal-cancel-box

(test-case "process-extension-command is a procedure"
  (check-true (procedure? process-extension-command)))

(test-case "process-extension-command with no extensions returns 'continue"
  (define cctx (make-test-cctx ""))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

(test-case "process-extension-command with non-slash input returns 'continue"
  (define cctx (make-test-cctx "hello world"))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

(test-case "process-extension-command with slash but no extensions returns 'continue"
  (define cctx (make-test-cctx "/unknown-command arg1"))
  (define result (process-extension-command cctx (initial-ui-state)))
  (check-equal? result 'continue))

;; P1 regression: submit with no runner should produce visible error
;; Test the execute-extension-command indirectly by mocking the hook result.
;; When runner=#f and submit payload arrives, transcript must grow.
(test-case "execute-extension-command submit with no runner shows error"
  (define cctx (make-test-cctx "/plan test task"))
  (define initial-transcript (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
  ;; Simulate what process-extension-command does on amend with submit payload
  (execute-extension-command cctx
                             (unbox (cmd-ctx-state-box cctx))
                             (hasheq 'submit "plan the project" 'text "Planning: test task"))
  (define updated-transcript (ui-state-transcript (unbox (cmd-ctx-state-box cctx))))
  (check > (length updated-transcript) (length initial-transcript)
         "submit with no runner should add error to transcript"))
