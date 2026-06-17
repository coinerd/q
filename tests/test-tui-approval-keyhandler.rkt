#lang racket

;; @speed fast  ;; @suite tui

;; tests/test-tui-approval-keyhandler.rkt
;; v0.99.25 W2 §5.3: Tests for the TUI approval overlay key handler.
;;
;; Tests verify:
;; - 'y' key approves and dismisses overlay
;; - 'n' key denies and dismisses overlay
;; - Escape cancels (denies) and dismisses overlay
;; - Unrecognized keys pass through when approval overlay is active
;; - Handler returns 'pass when no approval overlay is active
;; - Handler returns 'pass for other overlay types (tree-browser)

(require rackunit
         rackunit/text-ui
         racket/struct
         (only-in "../tui/tui-keybindings.rkt"
                  make-tui-ctx
                  tui-ctx?
                  tui-ctx-ui-state-box
                  handle-approval-overlay-key)
         "../tui/state-types.rkt"
         (only-in "../tui/approval-channel.rkt"
                  make-approval-channel
                  set-approval-channel!
                  clear-approval-channel!
                  current-approval-channel
                  approval-channel-ch
                  approval-put!))

;; ── Helpers ──

(define (make-test-ctx)
  (make-tui-ctx #:session-runner void #:event-bus #f #:session-queue #f))

(define (state-with-approval-overlay)
  (struct-copy ui-state
               (initial-ui-state)
               [active-overlay
                (overlay-state 'approval-prompt
                               '()
                               ""
                               'top-left
                               #f
                               #f
                               0
                               (hasheq 'capabilities '(shell-exec) 'task-preview "test"))]))

(define (state-with-tree-overlay)
  (struct-copy ui-state
               (initial-ui-state)
               [active-overlay (overlay-state 'tree-browser '() "" 'top-left #f #f 0 #f)]))

(define (state-without-overlay)
  (initial-ui-state))

;; Set up and tear down approval channel for each test
(define (with-approval-channel thunk)
  (dynamic-wind (lambda () (set-approval-channel! (make-approval-channel)))
                thunk
                (lambda () (clear-approval-channel!))))

(define (get-approval-result)
  (define ch (current-approval-channel))
  (and ch (sync/timeout 0.1 (approval-channel-ch ch))))

;; ── Test Suite ──

(define suite
  (test-suite "TUI Approval Overlay Key Handler (v0.99.25 W2)"

    ;; ── Approve ──

    (test-case "y key approves spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx #\y))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #t)
                               (check-false (ui-state-active-overlay
                                             (unbox (tui-ctx-ui-state-box ctx)))))))

    (test-case "Y key (uppercase) approves spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx #\Y))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #t))))

    (test-case "y symbol approves spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx 'y))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #t))))

    ;; ── Deny ──

    (test-case "n key denies spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx #\n))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #f)
                               (check-false (ui-state-active-overlay
                                             (unbox (tui-ctx-ui-state-box ctx)))))))

    (test-case "N key (uppercase) denies spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx #\N))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #f))))

    ;; ── Cancel (Escape) ──

    (test-case "Escape cancels (denies) spawn"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx 'escape))
                               (check-equal? result 'handled)
                               (check-equal? (get-approval-result) #f)
                               (check-false (ui-state-active-overlay
                                             (unbox (tui-ctx-ui-state-box ctx)))))))

    ;; ── Pass-through ──

    (test-case "unrecognized key passes through"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (define result (handle-approval-overlay-key ctx #\a))
                               (check-equal? result 'pass)
                               ;; Overlay should still be active
                               (define ov
                                 (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
                               (check-not-false ov)
                               (check-equal? (overlay-state-type ov) 'approval-prompt)
                               ;; No result on channel
                               (check-false (get-approval-result)))))

    (test-case "returns 'pass when no overlay is active"
      (define ctx (make-test-ctx))
      (set-box! (tui-ctx-ui-state-box ctx) (state-without-overlay))
      (define result (handle-approval-overlay-key ctx #\y))
      (check-equal? result 'pass))

    (test-case "returns 'pass for tree-browser overlay"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-tree-overlay))
                               (define result (handle-approval-overlay-key ctx #\y))
                               (check-equal? result 'pass)
                               ;; Tree overlay should still be active
                               (define ov
                                 (ui-state-active-overlay (unbox (tui-ctx-ui-state-box ctx))))
                               (check-not-false ov)
                               (check-equal? (overlay-state-type ov) 'tree-browser))))

    ;; ── Multiple keys don't double-fire ──

    (test-case "after approval, overlay is gone (no double-fire)"
      (with-approval-channel (lambda ()
                               (define ctx (make-test-ctx))
                               (set-box! (tui-ctx-ui-state-box ctx) (state-with-approval-overlay))
                               (handle-approval-overlay-key ctx #\y)
                               ;; Second keypress should pass through (overlay dismissed)
                               (define result2 (handle-approval-overlay-key ctx #\y))
                               (check-equal? result2 'pass))))))

(run-tests suite)
