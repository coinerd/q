#lang racket

;; q/tests/test-widget-zone.rkt — Tests for gui/extension-slots/widget-zone.rkt

(require rackunit
         rackunit/text-ui
         "../gui/extension-slots/widget-zone.rkt")

(define-test-suite test-widget-zone
                   (test-case "make-widget-zone creates zone"
                     (define z (make-widget-zone 'sidebar))
                     (check-true (widget-zone? z))
                     (check-equal? (widget-zone-name z) 'sidebar))
                   (test-case "zone-register-widget! adds widget"
                     (define z (make-widget-zone 'sidebar))
                     (define w (hash 'id 'clock 'view 'clock-widget))
                     (zone-register-widget! z w)
                     (check-equal? (length (zone-render z)) 1))
                   (test-case "zone-register-widget! replaces by id"
                     (define z (make-widget-zone 'sidebar))
                     (zone-register-widget! z (hash 'id 'clock 'text "v1"))
                     (zone-register-widget! z (hash 'id 'clock 'text "v2"))
                     (check-equal? (length (zone-render z)) 1)
                     (check-equal? (hash-ref (car (zone-render z)) 'text) "v2"))
                   (test-case "zone-unregister-widget! removes widget"
                     (define z (make-widget-zone 'sidebar))
                     (zone-register-widget! z (hash 'id 'clock 'text "time"))
                     (zone-register-widget! z (hash 'id 'weather 'text "sunny"))
                     (zone-unregister-widget! z 'clock)
                     (check-equal? (length (zone-render z)) 1)
                     (check-equal? (hash-ref (car (zone-render z)) 'id) 'weather))
                   (test-case "zone-render returns all widgets"
                     (define z (make-widget-zone 'toolbar))
                     (zone-register-widget! z (hash 'id 'btn1))
                     (zone-register-widget! z (hash 'id 'btn2))
                     (zone-register-widget! z (hash 'id 'btn3))
                     (check-equal? (length (zone-render z)) 3))
                   (test-case "zone-find-widget returns matching widget"
                     (define z (make-widget-zone 'sidebar))
                     (zone-register-widget! z (hash 'id 'clock 'text "12:00"))
                     (define found (zone-find-widget z 'clock))
                     (check-not-false found)
                     (check-equal? (hash-ref found 'text) "12:00"))
                   (test-case "zone-find-widget returns #f for missing"
                     (define z (make-widget-zone 'sidebar))
                     (check-false (zone-find-widget z 'nonexistent))))

(run-tests test-widget-zone)
