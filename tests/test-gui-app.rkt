#lang racket

;; q/tests/test-gui-app.rkt — Tests for gui/app.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/observable-bridge.rkt"
         "../ui-core/theme-protocol.rkt"
         "../ui-core/layout-protocol.rkt"
         "../gui/app.rkt")

(define (make-test-bridge)
  (define state-box (box #f))
  (make-gui-state-bridge state-box #f))

(define-test-suite
 test-gui-app
 (test-case "make-gui-app creates app with defaults"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (check-equal? (app-ref app 'status-text) "Ready")
   (check-equal? (app-ref app 'input-text) "")
   (check-false (app-ref app 'model-name))
   (check-equal? (app-ref app 'focused-view) 'input)
   (check-equal? (app-ref app 'messages) '()))
 (test-case "app-ref returns #f for unknown key"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (check-false (app-ref app 'nonexistent)))
 (test-case "app-update returns new app with updated field"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app2 (app-update app 'status-text "Processing..."))
   (check-equal? (app-ref app2 'status-text) "Processing...")
   ;; Original unchanged
   (check-equal? (app-ref app 'status-text) "Ready"))
 (test-case "app-update with model-name"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app2 (app-update app 'model-name "gpt-4"))
   (check-equal? (app-ref app2 'model-name) "gpt-4"))
 (test-case "app-update with messages"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define msgs (list (hash 'role 'user 'text "hello")))
   (define app2 (app-update app 'messages msgs))
   (check-equal? (length (app-ref app2 'messages)) 1))
 (test-case "render-app returns 3 view descriptors"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define views (render-app app))
   (check-equal? (length views) 3)
   (check-equal? (hash-ref (list-ref views 0) 'view) 'status-bar)
   (check-equal? (hash-ref (list-ref views 1) 'view) 'transcript)
   (check-equal? (hash-ref (list-ref views 2) 'view) 'input-area))
 (test-case "render-app includes theme colors"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define views (render-app app))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'bg) "#6c7086")
   (check-equal? (hash-ref status-bar 'fg) "#cdd6f4"))
 (test-case "render-app reflects focus state"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define views (render-app app))
   (define input-view (list-ref views 2))
   (check-true (hash-ref input-view 'focused))
   (define transcript-view (list-ref views 1))
   (check-false (hash-ref transcript-view 'focused)))
 (test-case "app-update unknown key returns same app"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app2 (app-update app 'nonexistent 'value))
   (check-eq? app app2))
 ;; ── Status mapping tests (F2 fix verification) ──
 (test-case "render-app status reflects processing state"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-processing (app-update app 'status-text "Processing..."))
   (define views (render-app app-processing))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'processing))
 (test-case "render-app status reflects error state"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-error (app-update app 'status-text "Error: timeout"))
   (define views (render-app app-error))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'error))
 (test-case "render-app status defaults to idle for non-matching text"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-ready (app-update app 'status-text "Ready"))
   (define views (render-app app-ready))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'idle))
 ;; T5: #f status-text should map to 'idle
 (test-case "render-app status is idle when status-text is #f"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-no-status (app-update app 'status-text #f))
   (define views (render-app app-no-status))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'idle))
 ;; T6: "Streaming" and "Thinking" should map to 'processing
 (test-case "render-app status reflects streaming state"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-streaming (app-update app 'status-text "Streaming response..."))
   (define views (render-app app-streaming))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'processing))
 (test-case "render-app status reflects thinking state"
   (define app (make-gui-app (make-test-bridge) (default-theme) (default-gui-layout)))
   (define app-thinking (app-update app 'status-text "Thinking..."))
   (define views (render-app app-thinking))
   (define status-bar (list-ref views 0))
   (check-equal? (hash-ref status-bar 'status) 'processing)))

(run-tests test-gui-app)
