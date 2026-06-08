#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-toolbar.rkt — Tests for gui/views/toolbar.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/toolbar.rkt")

(define-test-suite test-gui-toolbar
                   (test-case "toolbar-button creates button hash"
                     (define btn (toolbar-button 'save "Save"))
                     (check-equal? (hash-ref btn 'type) 'button)
                     (check-equal? (hash-ref btn 'id) 'save)
                     (check-equal? (hash-ref btn 'label) "Save")
                     (check-equal? (hash-ref btn 'action) 'save))
                   (test-case "toolbar-button with custom action"
                     (define btn (toolbar-button 'delete "Del" #:action 'confirm-delete))
                     (check-equal? (hash-ref btn 'action) 'confirm-delete))
                   (test-case "toolbar-separator creates separator"
                     (define sep (toolbar-separator))
                     (check-equal? (hash-ref sep 'type) 'separator))
                   (test-case "render-toolbar produces view descriptor"
                     (define result (render-toolbar (default-theme)))
                     (check-equal? (hash-ref result 'view) 'toolbar)
                     (check-equal? (hash-ref result 'button-count) 0)
                     (check-equal? (hash-ref result 'position) 'top))
                   (test-case "render-toolbar with items"
                     (define items
                       (list (toolbar-button 'new "New")
                             (toolbar-separator)
                             (toolbar-button 'open "Open")
                             (toolbar-button 'save "Save")))
                     (define result (render-toolbar (default-theme) #:items items))
                     (check-equal? (hash-ref result 'button-count) 3)
                     (check-equal? (hash-ref result 'separator-count) 1))
                   (test-case "render-toolbar at bottom"
                     (define result (render-toolbar (default-theme) #:position 'bottom))
                     (check-equal? (hash-ref result 'position) 'bottom)))

(run-tests test-gui-toolbar)
