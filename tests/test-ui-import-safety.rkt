#lang racket

;; q/tests/test-ui-import-safety.rkt — Characterization tests for
;; headless import safety of UI modules.
;;
;; W0.2: Verify that ui-core/ and gui descriptor modules can be imported
;; without requiring a display server (headless-safe).
;;
;; Static requires prove import safety at compile/load time.

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt"
         "../ui-core/dispatch.rkt"
         "../ui-core/observable-bridge.rkt"
         "../gui/gui-types.rkt"
         "../gui/state-sync.rkt"
         "../gui/slash-commands.rkt"
         "../extensions/ui-surface.rkt"
         "../tui/state-events.rkt")

;; hook-types is Typed Racket — use static require via quote
(require (only-in "../util/hook-types.rkt" hook-action-schemas))

(define-test-suite test-ui-import-safety
                   (test-case "ui-core/theme-protocol: default-theme works headless"
                     (define t (default-theme))
                     (check-true (ui-theme? t))
                     (check-not-false (theme-ref t 'background)))
                   (test-case "ui-core/layout-protocol: default-gui-layout works headless"
                     (define l (default-gui-layout))
                     (check-true (gui-layout? l)))
                   (test-case "ui-core/dispatch: dispatch-submit! works headless"
                     (dispatch-submit! (hash) "test")
                     (check-true #t))
                   (test-case "ui-core/observable-bridge: make-gui-state-bridge works headless"
                     (define b (make-gui-state-bridge (box (hash)) (hash)))
                     (check-true (gui-state-bridge? b)))
                   (test-case "gui/gui-types: make-gui-message works headless"
                     (define m (make-gui-message "user" "hello"))
                     (check-equal? (gui-message-role m) "user"))
                   (test-case "gui/state-sync: make-gui-event-subscriber works headless"
                     (define s (make-gui-event-subscriber (box (make-gui-state))))
                     (check-true (procedure? s)))
                   (test-case "gui/slash-commands: add-system-msg! works headless"
                     (define state-box (box (make-gui-state)))
                     (add-system-msg! "test" state-box (make-semaphore 1))
                     (check-equal? (length (gui-state-messages (unbox state-box))) 1))
                   (test-case "extensions/ui-surface: ui-set-header! works headless with registry"
                     ;; Just verify the function exists and the module loaded
                     (check-true (procedure? ui-set-header!)))
                   (test-case "util/hook-types: GUI hooks registered"
                     (check-not-false (hash-ref hook-action-schemas 'gui.window.opened #f))
                     (check-not-false (hash-ref hook-action-schemas 'gui.theme.changed #f))
                     (check-not-false (hash-ref hook-action-schemas 'gui.input.submit #f)))
                   (test-case "tui/state-events: apply-event-to-state works headless"
                     (check-true (procedure? apply-event-to-state))))

(run-tests test-ui-import-safety)
