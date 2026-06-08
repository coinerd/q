#lang racket

;; @speed fast
;; @suite default

;; q/tests/test-gui-sidebar.rkt — Tests for gui/views/sidebar.rkt

(require rackunit
         rackunit/text-ui
         "../ui-core/theme-protocol.rkt"
         "../gui/views/sidebar.rkt")

(define-test-suite
 test-gui-sidebar
 (test-case "sidebar-item creates item hash"
   (define item (sidebar-item 'home "Home" 'icon-home))
   (check-equal? (hash-ref item 'id) 'home)
   (check-equal? (hash-ref item 'label) "Home")
   (check-equal? (hash-ref item 'icon) 'icon-home))
 (test-case "sidebar-item without icon"
   (define item (sidebar-item 'settings "Settings"))
   (check-false (hash-ref item 'icon)))
 (test-case "sidebar-section creates section"
   (define sec (sidebar-section 'nav "Navigation" (list (sidebar-item 'home "Home"))))
   (check-equal? (hash-ref sec 'id) 'nav)
   (check-equal? (length (hash-ref sec 'items)) 1))
 (test-case "render-sidebar produces view descriptor"
   (define result (render-sidebar (default-theme)))
   (check-equal? (hash-ref result 'view) 'sidebar)
   (check-equal? (hash-ref result 'width) 30)
   (check-false (hash-ref result 'collapsed))
   (check-equal? (hash-ref result 'section-count) 0))
 (test-case "render-sidebar with sections"
   (define sections
     (list (sidebar-section 'nav "Nav" (list (sidebar-item 'a "A") (sidebar-item 'b "B")))
           (sidebar-section 'tools "Tools" (list (sidebar-item 'c "C")))))
   (define result (render-sidebar (default-theme) #:sections sections))
   (check-equal? (hash-ref result 'section-count) 2)
   (check-equal? (hash-ref result 'item-count) 3))
 (test-case "render-sidebar collapsed reduces width"
   (define result (render-sidebar (default-theme) #:collapsed #t))
   (check-equal? (hash-ref result 'width) 2)
   (check-true (hash-ref result 'collapsed)))
 (test-case "render-sidebar with active section"
   (define result (render-sidebar (default-theme) #:active-section 'nav))
   (check-equal? (hash-ref result 'active-section) 'nav)))

(run-tests test-gui-sidebar)
