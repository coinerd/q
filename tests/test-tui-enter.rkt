#lang racket

;; test-tui-enter.rkt — Tests for BUG-26: TUI Enter key handling
;;
;; charterm maps Enter (byte 13) to 'return symbol, not #\return char.
;; These tests verify that handle-key correctly processes 'return.

(require rackunit
         rackunit/text-ui
         "../interfaces/tui.rkt"
         "../tui/input.rkt"
         "../tui/state.rkt")

;; Helper: create a tui-ctx with a collector for submitted text
(define (make-test-ctx)
  (define submitted (box '()))
  (define ctx (make-tui-ctx
               #:session-runner
               (lambda (prompt)
                 (set-box! submitted
                           (append (unbox submitted) (list prompt))))))
  ctx)

;; Helper: insert text into ctx input state
(define (type-text ctx text)
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (for ([ch (in-string text)])
    (set-box! (tui-ctx-input-state-box ctx)
              (input-insert-char (unbox (tui-ctx-input-state-box ctx)) ch))))

(define test-tui-enter
  (test-suite
   "TUI Enter Key (BUG-26)"

   ;; --------------------------------------------------
   ;; Test 1: 'return symbol submits typed text
   ;; --------------------------------------------------
   (test-case "handle-key 'return submits typed text"
     (define ctx (make-test-ctx))
     (type-text ctx "hello")
     (define result (handle-key ctx 'return))
     (check-equal? result '(submit "hello"))
     (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx))) ""))

   ;; --------------------------------------------------
   ;; Test 2: 'return on empty input returns 'continue
   ;; --------------------------------------------------
   (test-case "handle-key 'return on empty input continues"
     (define ctx (make-test-ctx))
     (define result (handle-key ctx 'return))
     (check-equal? result 'continue))

   ;; --------------------------------------------------
   ;; Test 3: 'return with slash command returns 'command
   ;; --------------------------------------------------
   (test-case "handle-key 'return with /quit returns command"
     (define ctx (make-test-ctx))
     (type-text ctx "/quit")
     (define result (handle-key ctx 'return))
     (check-equal? result '(command quit)))

   ;; --------------------------------------------------
   ;; Test 4: 'return with /help returns command
   ;; --------------------------------------------------
   (test-case "handle-key 'return with /help returns command"
     (define ctx (make-test-ctx))
     (type-text ctx "/help")
     (define result (handle-key ctx 'return))
     (check-equal? result '(command help)))

   ;; --------------------------------------------------
   ;; Test 5: 'return with /clear returns command
   ;; --------------------------------------------------
   (test-case "handle-key 'return with /clear returns command"
     (define ctx (make-test-ctx))
     (type-text ctx "/clear")
     (define result (handle-key ctx 'return))
     (check-equal? result '(command clear)))

   ;; --------------------------------------------------
   ;; Test 6: 'kp-return (numpad enter) also submits
   ;; --------------------------------------------------
   (test-case "handle-key 'kp-return submits typed text"
     (define ctx (make-test-ctx))
     (type-text ctx "numpad")
     (define result (handle-key ctx 'kp-return))
     (check-equal? result '(submit "numpad"))
     (check-equal? (input-state-buffer (unbox (tui-ctx-input-state-box ctx))) ""))

   ;; --------------------------------------------------
   ;; Test 7: 'return adds user message to transcript
   ;; --------------------------------------------------
   (test-case "handle-key 'return adds user entry to transcript"
     (define ctx (make-test-ctx))
     (type-text ctx "test message")
     (handle-key ctx 'return)
     (define state (unbox (tui-ctx-ui-state-box ctx)))
     (define transcript (ui-state-transcript state))
     (check-equal? (length transcript) 1)
     (define entry (car transcript))
     (check-equal? (transcript-entry-kind entry) 'user)
     (check-equal? (transcript-entry-text entry) "test message"))

   ;; --------------------------------------------------
   ;; Test 8: Multiple enters work correctly
   ;; --------------------------------------------------
   (test-case "handle-key multiple enters work"
     (define ctx (make-test-ctx))
     (type-text ctx "first")
     (handle-key ctx 'return)
     (type-text ctx "second")
     (define result (handle-key ctx 'return))
     (check-equal? result '(submit "second"))
     (define state (unbox (tui-ctx-ui-state-box ctx)))
     (check-equal? (length (ui-state-transcript state)) 2))

   ;; --------------------------------------------------
   ;; Test 9: 'return with whitespace-only input continues
   ;; --------------------------------------------------
   (test-case "handle-key 'return with whitespace-only continues"
     (define ctx (make-test-ctx))
     (type-text ctx "   ")
     (define result (handle-key ctx 'return))
     (check-equal? result 'continue))
   ))

(run-tests test-tui-enter)
