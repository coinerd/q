#lang racket

;; q/tests/test-ui-action-parity.rkt — Equivalence fixture tests
;;
;; W2.4 (v0.94.2): Verify that the same UI actions produce equivalent
;; observable effects in both TUI and GUI adapters through the shared reducer.

(require rackunit
         rackunit/text-ui
         "../ui-core/ui-delta.rkt"
         "../ui-core/ui-reducer.rkt"
         "../tui/ui-action-adapter.rkt"
         "../gui/ui-action-adapter.rkt"
         "../tui/state-types.rkt"
         (only-in "../tui/state.rkt"
                  initial-ui-state
                  ui-state-custom-header
                  ui-state-custom-footer
                  ui-state-extension-widgets)
         (only-in "../gui/gui-types.rkt" make-gui-state gui-state-status))

;; Helper: apply action to both backends and return results
(define (apply-both action-type payload)
  (define tui-result (apply-action-with tui-delta-handlers action-type payload (initial-ui-state)))
  (define gui-result (apply-action-with gui-delta-handlers action-type payload (make-gui-state)))
  (values tui-result gui-result))

;; Helper: apply action to TUI only
(define (tui-apply-action action-type payload)
  (apply-action-with tui-delta-handlers action-type payload (initial-ui-state)))

;; Helper: apply action to GUI only
(define (gui-apply-action action-type payload)
  (apply-action-with gui-delta-handlers action-type payload (make-gui-state)))

(define-test-suite
 test-ui-action-parity
 ;; ─── Reducer output stability ───
 (test-case "ui-action->deltas produces stable output for header.set"
   (define payload (hash 'lines '("hello")))
   (define d1 (ui-action->deltas "ui.header.set" payload))
   (define d2 (ui-action->deltas "ui.header.set" payload))
   (check-equal? d1 d2))
 (test-case "ui-action->deltas produces stable output for widget.register"
   (define payload (hash 'ext-name 'e 'key 'k 'descriptor (hash 'content "x")))
   (define d1 (ui-action->deltas "ui.widget.register" payload))
   (define d2 (ui-action->deltas "ui.widget.register" payload))
   (check-equal? d1 d2))
 ;; ─── TUI adapter effects ───
 (test-case "TUI: header.set → custom-header updated"
   (define result (tui-apply-action "ui.header.set" (hash 'lines '("h1" "h2"))))
   (check-equal? (ui-state-custom-header result) '("h1" "h2")))
 (test-case "TUI: header.clear → custom-header #f"
   (define s1 (tui-apply-action "ui.header.set" (hash 'lines '("x"))))
   (define s2 (apply-action-with tui-delta-handlers "ui.header.clear" (hash) s1))
   (check-false (ui-state-custom-header s2)))
 (test-case "TUI: footer.set → custom-footer updated"
   (define result (tui-apply-action "ui.footer.set" (hash 'lines '("f1"))))
   (check-equal? (ui-state-custom-footer result) '("f1")))
 (test-case "TUI: footer.clear → custom-footer #f"
   (define s1 (tui-apply-action "ui.footer.set" (hash 'lines '("x"))))
   (define s2 (apply-action-with tui-delta-handlers "ui.footer.clear" (hash) s1))
   (check-false (ui-state-custom-footer s2)))
 (test-case "TUI: widget.register → extension-widgets updated"
   (define result
     (tui-apply-action
      "ui.widget.register"
      (hash 'ext-name 'ext1 'key 'slot1 'descriptor (hash 'content '("widget-text")))))
   (define widgets (ui-state-extension-widgets result))
   (check-equal? (hash-ref widgets (cons 'ext1 'slot1) #f) '("widget-text")))
 (test-case "TUI: widget.unregister → extension-widgets entry removed"
   (define s1
     (tui-apply-action "ui.widget.register"
                       (hash 'ext-name 'ext1 'key 'slot1 'descriptor (hash 'content '("w")))))
   (define s2
     (apply-action-with tui-delta-handlers
                        "ui.widget.unregister"
                        (hash 'ext-name 'ext1 'key 'slot1)
                        s1))
   (check-false (hash-ref (ui-state-extension-widgets s2) (cons 'ext1 'slot1) #f)))
 ;; ─── GUI adapter effects ───
 (test-case "GUI: status.set → gui-state status updated"
   (define result (gui-apply-action "ui.status.set" (hash 'status 'processing)))
   (check-equal? (gui-state-status result) 'processing))
 (test-case "GUI: header.set → no-op (not yet implemented)"
   (define state (make-gui-state))
   (define result (gui-apply-action "ui.header.set" (hash 'lines '("h"))))
   ;; GUI doesn't have custom-header field yet — state unchanged
   (check-equal? result state))
 ;; ─── Parity: both adapters consume same delta set ───
 (test-case "both adapters handle same action without error"
   ;; Apply several actions to both backends — just verify no errors
   (for ([action-type (in-list '("ui.header.set" "ui.header.clear"
                                                 "ui.footer.set"
                                                 "ui.footer.clear"
                                                 "ui.status.set"
                                                 "ui.theme.change"
                                                 "ui.layout.breakpoint"
                                                 "ui.focus.request"))]
         [payload (in-list (list (hash 'lines '("h"))
                                 (hash)
                                 (hash 'lines '("f"))
                                 (hash)
                                 (hash 'status 'idle)
                                 (hash 'theme 'dark)
                                 (hash 'breakpoint 'wide)
                                 (hash 'component 'input)))])
     (apply-action-with tui-delta-handlers action-type payload (initial-ui-state))
     (apply-action-with gui-delta-handlers action-type payload (make-gui-state)))
   (check-true #t))
 ;; ─── Handler table identity ───
 (test-case "TUI handler table is a delta-handler-table"
   (check-true (delta-handler-table? tui-delta-handlers)))
 (test-case "GUI handler table is a delta-handler-table"
   (check-true (delta-handler-table? gui-delta-handlers)))
 ;; ─── Unknown action parity ───
 (test-case "both adapters return unchanged state for unknown action"
   (define tui-state (initial-ui-state))
   (define gui-state (make-gui-state))
   (define tui-result (apply-action-with tui-delta-handlers "ui.unknown" (hash) tui-state))
   (define gui-result (apply-action-with gui-delta-handlers "ui.unknown" (hash) gui-state))
   (check-eq? tui-result tui-state)
   (check-eq? gui-result gui-state)))

(run-tests test-ui-action-parity)
