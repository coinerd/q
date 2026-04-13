#lang racket

;; test-tui-keybindings.rkt — Tests for tui/tui-keybindings.rkt
;;
;; Tests the tui-ctx struct, mark-dirty!, tui-ctx->cmd-ctx,
;; and process-slash-command. Does NOT test handle-key/handle-mouse
;; which require terminal (covered by test-tui-enter.rkt and test-tui-keys.rkt).

(require rackunit
         rackunit/text-ui
         "../tui/tui-keybindings.rkt"
         "../tui/state.rkt"
         (only-in "../tui/commands.rkt" cmd-ctx? cmd-ctx-state-box cmd-ctx-running-box))

(define test-tui-keybindings
  (test-suite "tui/tui-keybindings"

    ;; --------------------------------------------------
    ;; Test 1: make-tui-ctx creates a valid tui-ctx
    ;; --------------------------------------------------
    (test-case "make-tui-ctx returns tui-ctx struct"
      (define ctx (make-tui-ctx))
      (check-pred tui-ctx? ctx)
      (check-pred box? (tui-ctx-ui-state-box ctx))
      (check-pred box? (tui-ctx-input-state-box ctx))
      (check-pred box? (tui-ctx-running-box ctx))
      (check-pred box? (tui-ctx-needs-redraw-box ctx)))

    ;; --------------------------------------------------
    ;; Test 2: make-tui-ctx accepts keyword arguments
    ;; --------------------------------------------------
    (test-case "make-tui-ctx #:session-runner stores custom runner"
      (define called-with (box #f))
      (define runner (lambda (prompt) (set-box! called-with prompt)))
      (define ctx (make-tui-ctx #:session-runner runner))
      ((tui-ctx-session-runner ctx) "test")
      (check-equal? (unbox called-with) "test"))

    (test-case "make-tui-ctx #:model-registry stores registry"
      (define ctx (make-tui-ctx #:model-registry #f))
      (check-false (unbox (tui-ctx-model-registry-box ctx))))

    ;; --------------------------------------------------
    ;; Test 3: mark-dirty! sets needs-redraw flag
    ;; --------------------------------------------------
    (test-case "mark-dirty! sets needs-redraw to #t"
      (define ctx (make-tui-ctx))
      (set-box! (tui-ctx-needs-redraw-box ctx) #f)
      (mark-dirty! ctx)
      (check-true (unbox (tui-ctx-needs-redraw-box ctx))))

    (test-case "mark-dirty! when already dirty stays #t"
      (define ctx (make-tui-ctx))
      (mark-dirty! ctx)
      (mark-dirty! ctx)
      (check-true (unbox (tui-ctx-needs-redraw-box ctx))))

    ;; --------------------------------------------------
    ;; Test 4: tui-ctx->cmd-ctx converts correctly
    ;; --------------------------------------------------
    (test-case "tui-ctx->cmd-ctx returns cmd-ctx"
      (define ctx (make-tui-ctx))
      (define cctx (tui-ctx->cmd-ctx ctx))
      (check-pred cmd-ctx? cctx)
      ;; Same boxes shared
      (check-eq? (cmd-ctx-state-box cctx) (tui-ctx-ui-state-box ctx))
      (check-eq? (cmd-ctx-running-box cctx) (tui-ctx-running-box ctx)))

    ;; --------------------------------------------------
    ;; Test 5: process-slash-command dispatches correctly
    ;; --------------------------------------------------
    (test-case "process-slash-command 'quit sets running to #f"
      (define ctx (make-tui-ctx))
      (check-true (unbox (tui-ctx-running-box ctx)))
      (process-slash-command ctx 'quit)
      (check-false (unbox (tui-ctx-running-box ctx))))

    (test-case "process-slash-command 'clear clears transcript"
      (define ctx (make-tui-ctx))
      ;; Add a transcript entry
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (define entry (transcript-entry 'user "hello" (current-inexact-milliseconds) (hash)))
      (set-box! (tui-ctx-ui-state-box ctx) (add-transcript-entry state entry))
      (check-equal? (length (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx)))) 1)
      (process-slash-command ctx 'clear)
      (check-equal? (length (ui-state-transcript (unbox (tui-ctx-ui-state-box ctx)))) 0))

    ;; --------------------------------------------------
    ;; Test 6: tui-ctx struct is transparent
    ;; --------------------------------------------------
    (test-case "tui-ctx is transparent"
      (define ctx (make-tui-ctx))
      (check-true (tui-ctx? ctx)))))

(run-tests test-tui-keybindings)
