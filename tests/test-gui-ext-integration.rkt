#lang racket

;; @speed slow
;; @suite default

;; q/tests/test-gui-ext-integration.rkt — Integration test for extension slots

(require rackunit
         rackunit/text-ui
         "../gui/extension-slots/extension-bridge.rkt"
         "../gui/extension-slots/widget-zone.rkt"
         "../gui/extension-slots/custom-renderer.rkt")

(define-test-suite test-gui-ext-integration
                   (test-case "full extension lifecycle: load → register → render → unload"
                     (define bridge (make-gui-extension-bridge))
                     ;; Setup zones
                     (bridge-register-zone! bridge 'sidebar)
                     (bridge-register-zone! bridge 'toolbar)
                     ;; Load extension
                     (bridge-extension-loaded bridge "clock-ext" (hash 'name "Clock" 'version "1.0"))
                     ;; Register widget in sidebar
                     (define sidebar (bridge-get-zone bridge 'sidebar))
                     (zone-register-widget! sidebar (hash 'id 'clock-display 'text "12:00"))
                     ;; Register custom renderer
                     (define registry (bridge-get-renderer-registry bridge))
                     (register-renderer! registry 'time (lambda (d) (format "Time: ~a" d)))
                     ;; Verify
                     (check-equal? (length (bridge-list-extensions bridge)) 1)
                     (check-equal? (length (zone-render sidebar)) 1)
                     (check-equal? (render-with-registry registry 'time "12:00") "Time: 12:00")
                     ;; Unload
                     (bridge-extension-unloaded bridge "clock-ext")
                     (check-equal? (length (bridge-list-extensions bridge)) 0))
                   (test-case "multiple extensions share zones"
                     (define bridge (make-gui-extension-bridge))
                     (bridge-register-zone! bridge 'toolbar)
                     (bridge-extension-loaded bridge "ext-a" (hash 'name "A"))
                     (bridge-extension-loaded bridge "ext-b" (hash 'name "B"))
                     (define toolbar (bridge-get-zone bridge 'toolbar))
                     (zone-register-widget! toolbar (hash 'id 'btn-a 'text "A"))
                     (zone-register-widget! toolbar (hash 'id 'btn-b 'text "B"))
                     (check-equal? (length (zone-render toolbar)) 2)
                     ;; Unload one extension
                     (bridge-extension-unloaded bridge "ext-a")
                     ;; Widget still in zone (cleanup is separate concern)
                     (check-equal? (length (zone-render toolbar)) 2)))

(run-tests test-gui-ext-integration)
