#lang racket

;; q/tests/test-gui-extension-bridge.rkt — Tests for gui/extension-slots/extension-bridge.rkt

(require rackunit
         rackunit/text-ui
         "../gui/extension-slots/extension-bridge.rkt"
         "../gui/extension-slots/widget-zone.rkt"
         "../gui/extension-slots/custom-renderer.rkt")

(define-test-suite test-gui-extension-bridge
                   (test-case "make-gui-extension-bridge creates bridge"
                     (define b (make-gui-extension-bridge))
                     (check-true (gui-extension-bridge? b)))
                   (test-case "bridge-register-zone! creates zone"
                     (define b (make-gui-extension-bridge))
                     (bridge-register-zone! b 'sidebar)
                     (define z (bridge-get-zone b 'sidebar))
                     (check-not-false z)
                     (check-true (widget-zone? z)))
                   (test-case "bridge-get-zone returns #f for unregistered zone"
                     (define b (make-gui-extension-bridge))
                     (check-false (bridge-get-zone b 'nonexistent)))
                   (test-case "bridge-get-renderer-registry returns registry"
                     (define b (make-gui-extension-bridge))
                     (define r (bridge-get-renderer-registry b))
                     (check-true (renderer-registry? r)))
                   (test-case "bridge-extension-loaded registers extension"
                     (define b (make-gui-extension-bridge))
                     (bridge-extension-loaded b "my-ext" (hash 'name "My Extension" 'version "1.0"))
                     (check-equal? (bridge-list-extensions b) '("my-ext")))
                   (test-case "bridge-extension-unloaded removes extension"
                     (define b (make-gui-extension-bridge))
                     (bridge-extension-loaded b "ext1" (hash 'name "Ext1"))
                     (bridge-extension-loaded b "ext2" (hash 'name "Ext2"))
                     (bridge-extension-unloaded b "ext1")
                     (check-equal? (length (bridge-list-extensions b)) 1)
                     (check-not-false (member "ext2" (bridge-list-extensions b))))
                   (test-case "multiple zones coexist"
                     (define b (make-gui-extension-bridge))
                     (bridge-register-zone! b 'sidebar)
                     (bridge-register-zone! b 'toolbar)
                     (bridge-register-zone! b 'status)
                     (check-not-false (bridge-get-zone b 'sidebar))
                     (check-not-false (bridge-get-zone b 'toolbar))
                     (check-not-false (bridge-get-zone b 'status))))

(run-tests test-gui-extension-bridge)
