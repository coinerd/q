#lang racket

;; q/tests/test-ui-action-adapters.rkt — Tests for TUI and GUI action adapters
;;
;; W1.3 (v0.94.1): Verify both adapters correctly apply ui-delta values
;; to their respective state types.

(require rackunit
         rackunit/text-ui
         "../tui/ui-action-adapter.rkt"
         "../gui/ui-action-adapter.rkt"
         "../ui-core/ui-delta.rkt"
         "../tui/state-types.rkt"
         (only-in "../tui/state.rkt"
                  initial-ui-state
                  ui-state-extension-widgets
                  ui-state-custom-header
                  ui-state-custom-footer)
         (only-in "../gui/gui-types.rkt" make-gui-state gui-state-status))

;; ── TUI Adapter Tests ──────────────────────────────────────

(define-test-suite
 test-tui-action-adapter
 (test-case "tui-apply-delta: set-header updates custom-header"
   (define state (initial-ui-state))
   (define delta (ui-delta DELTA-SET-HEADER '("line1" "line2")))
   (define result (tui-apply-delta delta state))
   (check-equal? (ui-state-custom-header result) '("line1" "line2")))
 (test-case "tui-apply-delta: clear-header sets custom-header to #f"
   (define state (struct-copy ui-state (initial-ui-state) [custom-header '("old")]))
   (define delta (ui-delta DELTA-CLEAR-HEADER #f))
   (define result (tui-apply-delta delta state))
   (check-false (ui-state-custom-header result)))
 (test-case "tui-apply-delta: set-footer updates custom-footer"
   (define state (initial-ui-state))
   (define delta (ui-delta DELTA-SET-FOOTER '("footer")))
   (define result (tui-apply-delta delta state))
   (check-equal? (ui-state-custom-footer result) '("footer")))
 (test-case "tui-apply-delta: clear-footer sets custom-footer to #f"
   (define state (struct-copy ui-state (initial-ui-state) [custom-footer '("old")]))
   (define delta (ui-delta DELTA-CLEAR-FOOTER #f))
   (define result (tui-apply-delta delta state))
   (check-false (ui-state-custom-footer result)))
 (test-case "tui-apply-delta: register-widget adds to extension-widgets"
   (define state (initial-ui-state))
   (define delta (ui-delta DELTA-REGISTER-WIDGET (list 'ext1 'main (hash 'content '("data")))))
   (define result (tui-apply-delta delta state))
   (define widgets (ui-state-extension-widgets result))
   (check-equal? (hash-ref widgets (cons 'ext1 'main) #f) '("data")))
 (test-case "tui-apply-delta: unregister-widget removes specific key"
   (define state0 (initial-ui-state))
   (define state1
     (struct-copy ui-state
                  state0
                  [extension-widgets (hash (cons 'ext1 'k1) "a" (cons 'ext1 'k2) "b")]))
   (define delta (ui-delta DELTA-UNREGISTER-WIDGET (cons 'ext1 'k1)))
   (define result (tui-apply-delta delta state1))
   (define widgets (ui-state-extension-widgets result))
   (check-false (hash-ref widgets (cons 'ext1 'k1) #f))
   (check-equal? (hash-ref widgets (cons 'ext1 'k2) #f) "b"))
 (test-case "tui-apply-delta: unregister-widget with #f key removes all for ext"
   (define state0 (initial-ui-state))
   (define state1
     (struct-copy ui-state
                  state0
                  [extension-widgets
                   (hash (cons 'ext1 'k1) "a" (cons 'ext1 'k2) "b" (cons 'ext2 'k1) "c")]))
   (define delta (ui-delta DELTA-UNREGISTER-WIDGET (cons 'ext1 #f)))
   (define result (tui-apply-delta delta state1))
   (define widgets (ui-state-extension-widgets result))
   (check-false (hash-ref widgets (cons 'ext1 'k1) #f))
   (check-false (hash-ref widgets (cons 'ext1 'k2) #f))
   (check-equal? (hash-ref widgets (cons 'ext2 'k1) #f) "c"))
 (test-case "tui-apply-delta: unknown delta type returns state unchanged"
   (define state (initial-ui-state))
   (define delta (ui-delta 'unknown-type 'payload))
   (define result (tui-apply-delta delta state))
   (check-eq? result state))
 (test-case "tui-apply-deltas: multiple deltas applied in sequence"
   (define state (initial-ui-state))
   (define deltas (list (ui-delta DELTA-SET-HEADER '("h")) (ui-delta DELTA-SET-FOOTER '("f"))))
   (define result (tui-apply-deltas deltas state))
   (check-equal? (ui-state-custom-header result) '("h"))
   (check-equal? (ui-state-custom-footer result) '("f")))
 (test-case "make-tui-action-handler: processes ui.* events"
   (define state-box (box (initial-ui-state)))
   (define notify-called (box 0))
   (define handler (make-tui-action-handler state-box))
   (handler (hash 'type "ui.header.set" 'lines '("test-header")))
   (check-equal? (ui-state-custom-header (unbox state-box)) '("test-header")))
 (test-case "make-tui-action-handler: ignores non-ui.* events"
   (define state-box (box (initial-ui-state)))
   (define handler (make-tui-action-handler state-box))
   (handler (hash 'type "user.input" 'text "hello"))
   ;; State should not have changed
   (check-false (ui-state-custom-header (unbox state-box)))))

;; ── GUI Adapter Tests ──────────────────────────────────────

(define-test-suite
 test-gui-action-adapter
 (test-case "gui-apply-delta: set-status updates status"
   (define state (make-gui-state))
   (define delta (ui-delta DELTA-SET-STATUS 'processing))
   (define result (gui-apply-delta delta state))
   (check-equal? (gui-state-status result) 'processing))
 (test-case "gui-apply-delta: unknown delta returns state unchanged"
   (define state (make-gui-state))
   (define delta (ui-delta 'unknown-type 'payload))
   (define result (gui-apply-delta delta state))
   (check-eq? result state))
 (test-case "gui-apply-deltas: multiple deltas applied"
   (define state (make-gui-state))
   (define deltas (list (ui-delta DELTA-SET-STATUS 'processing) (ui-delta DELTA-SET-STATUS 'idle)))
   (define result (gui-apply-deltas deltas state))
   (check-equal? (gui-state-status result) 'idle))
 (test-case "make-gui-action-handler: processes ui.status.set"
   (define state-box (box (make-gui-state)))
   (define handler (make-gui-action-handler state-box))
   (handler (hash 'type "ui.status.set" 'status 'processing))
   (check-equal? (gui-state-status (unbox state-box)) 'processing))
 (test-case "make-gui-action-handler: ignores non-ui.* events"
   (define state-box (box (make-gui-state)))
   (define handler (make-gui-action-handler state-box))
   (handler (hash 'type "user.input" 'text "hello"))
   (check-equal? (gui-state-status (unbox state-box)) 'idle))
 (test-case "gui-apply-delta: header/footer deltas are no-ops (skeleton)"
   ;; GUI doesn't have custom-header/custom-footer fields yet
   (define state (make-gui-state))
   (define result1 (gui-apply-delta (ui-delta DELTA-SET-HEADER '("h")) state))
   (define result2 (gui-apply-delta (ui-delta DELTA-SET-FOOTER '("f")) state))
   (check-eq? result1 state)
   (check-eq? result2 state))

 ;; ── T-4: Additional GUI adapter coverage ───

 (test-case "gui-apply-delta: set-theme is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-SET-THEME 'dark) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: set-layout is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-SET-LAYOUT 'wide) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: set-focus is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-SET-FOCUS 'input) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: register-widget is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-REGISTER-WIDGET '("ext" "key" ())) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: unregister-widget is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-UNREGISTER-WIDGET '("ext" . "key")) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: add-message is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-ADD-MESSAGE '("msg")) state))
   (check-eq? result state))

 (test-case "gui-apply-delta: update-message is no-op (skeleton)"
   (define state (make-gui-state))
   (define result (gui-apply-delta (ui-delta DELTA-UPDATE-MESSAGE '("id" "msg")) state))
   (check-eq? result state)))

(run-tests test-tui-action-adapter)
(run-tests test-gui-action-adapter)
