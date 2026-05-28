#lang racket

;; q/tests/test-gui-smoke.rkt — Smoke tests for all GUI modules

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt"
         "../ui-core/observable-bridge.rkt"
         "../ui-core/dispatch.rkt"
         "../gui/app.rkt"
         "../gui/theme-manager.rkt"
         "../gui/extension-slots/widget-zone.rkt"
         "../gui/extension-slots/custom-renderer.rkt"
         "../gui/extension-slots/extension-bridge.rkt"
         "../gui/views/status.rkt"
         "../gui/views/input.rkt"
         "../gui/views/transcript.rkt"
         "../gui/views/message-entry.rkt"
         "../gui/views/overlay.rkt"
         "../gui/views/sidebar.rkt"
         "../gui/views/toolbar.rkt"
         "../gui/views/code-block.rkt")

(define-test-suite test-gui-smoke
  (test-case "theme-protocol: default-theme returns ui-theme"
    (define t (default-theme))
    (check-true (ui-theme? t))
    (check-not-false (theme-ref t 'background)))
  (test-case "layout-protocol: default-gui-layout works"
    (define layout (default-gui-layout))
    (check-true (gui-layout? layout)))
  (test-case "observable-bridge: make-gui-state-bridge works"
    (define b (make-gui-state-bridge (box (hash)) (hash)))
    (check-true (gui-state-bridge? b)))
  (test-case "dispatch: dispatch-submit! works"
    (dispatch-submit! (hash) "test input")
    (check-true #t))
  (test-case "app: make-gui-app creates app"
    (define b (make-gui-state-bridge (box (hash)) (hash)))
    (define a (make-gui-app b (default-theme) (default-gui-layout)))
    (check-true (gui-app? a)))
  (test-case "theme-manager: make-theme-manager works"
    (define mgr (make-theme-manager))
    (check-true (theme-manager? mgr)))
  (test-case "widget-zone: make-widget-zone works"
    (define z (make-widget-zone 'test))
    (check-true (widget-zone? z)))
  (test-case "custom-renderer: make-renderer-registry works"
    (define r (make-renderer-registry))
    (check-true (renderer-registry? r)))
  (test-case "extension-bridge: make-gui-extension-bridge works"
    (define b (make-gui-extension-bridge))
    (check-true (gui-extension-bridge? b)))
  (test-case "status: render-status-bar returns hash"
    (define result (render-status-bar (default-theme) #:model "test" #:status 'idle))
    (check-true (hash? result)))
  (test-case "input: render-input-area returns hash"
    (define result (render-input-area (default-theme) #:text "hello" #:cursor 2))
    (check-true (hash? result)))
  (test-case "transcript: render-transcript returns hash"
    (define result (render-transcript (default-theme) '() #:scroll-offset 0))
    (check-true (hash? result)))
  (test-case "overlay: render-alert returns hash"
    (define result (render-alert (default-theme) "Test"))
    (check-true (hash? result)))
  (test-case "sidebar: render-sidebar returns hash"
    (define result (render-sidebar (default-theme)))
    (check-true (hash? result)))
  (test-case "toolbar: render-toolbar returns hash"
    (define result (render-toolbar (default-theme)))
    (check-true (hash? result)))
  (test-case "code-block: render-code-block returns hash"
    (define result (render-code-block (default-theme) "(+ 1 2)"))
    (check-true (hash? result))))

(run-tests test-gui-smoke)
