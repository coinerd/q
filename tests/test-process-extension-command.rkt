#lang racket/base

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
           (box #f))) ;; session-factory-runner

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
