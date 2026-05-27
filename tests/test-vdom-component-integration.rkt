#lang racket

;; tests/test-vdom-component-integration.rkt — Component input dispatch + focus tests
;;
;; Run: raco test tests/test-vdom-component-integration.rkt

(require rackunit
         rackunit/text-ui
         "../tui/component.rkt"
         "../tui/vdom-components.rkt"
         "../tui/state.rkt"
         "../tui/state-types.rkt")

;; Helper: check if a value is a bubble result
(define (bubble-result? v)
  (and (pair? v) (eq? (car v) 'bubble)))

(define test-vdom-comp-integration
  (test-suite
   "vdom component integration"

   (test-case "vdom components have correct suffixed IDs"
     (check-equal? (q-component-id (make-transcript-vdom-component)) 'transcript-vdom)
     (check-equal? (q-component-id (make-status-bar-vdom-component)) 'status-bar-vdom)
     (check-equal? (q-component-id (make-input-box-vdom-component)) 'input-box-vdom)
     (check-equal? (q-component-id (make-header-vdom-component)) 'header-vdom))

   (test-case "focusable-components returns list"
     (define comps (list (make-transcript-vdom-component)
                         (make-input-box-vdom-component)))
     (define focusable (focusable-components comps))
     (check-true (list? focusable)))

   (test-case "cycle-focus returns #f with no focusable components"
     (define comps (list (make-transcript-vdom-component)
                         (make-input-box-vdom-component)))
     (define focusable (focusable-components comps))
     (when (null? focusable)
       (check-false (cycle-focus comps 'transcript-vdom))))

   (test-case "component-handle-input bubbles without handler"
     (define comp (make-header-vdom-component))
     (define st (initial-ui-state))
     (define-values (st* result)
       (component-handle-input comp 'test-data st))
     (check-true (ui-state? st*))
     (check-true (bubble-result? result)))

   (test-case "overlay component wraps content"
     (define overlay (make-overlay-vdom-component
                      (make-transcript-vdom-component)
                      #:anchor (make-header-vdom-component)))
     (check-equal? (q-component-id overlay) 'overlay-vdom)
     (define st (initial-ui-state #:model-name "test"))
     (define vnodes (component-render overlay st 80))
     (check-true (list? vnodes)))

   (test-case "all vdom components render without error"
     (define st (initial-ui-state #:model-name "test"))
     (for ([comp (in-list (list (make-transcript-vdom-component)
                                (make-status-bar-vdom-component)
                                (make-input-box-vdom-component)
                                (make-header-vdom-component)))])
       (define result (component-render comp st 80))
       (check-true (list? result)
                   (format "~a renders" (q-component-id comp)))))))

(run-tests test-vdom-comp-integration)
