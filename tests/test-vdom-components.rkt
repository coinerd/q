#lang racket

;; tests/test-vdom-components.rkt — Tests for tui/vdom-components.rkt

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/vdom-components.rkt"
         "../tui/component.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/state.rkt")

;; ============================================================
;; Component construction
;; ============================================================

(test-case "transcript component is vdom"
  (define comp (make-transcript-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'transcript-vdom))

(test-case "status bar component is vdom"
  (define comp (make-status-bar-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'status-bar-vdom))

(test-case "input box component is vdom"
  (define comp (make-input-box-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'input-box-vdom))

(test-case "header component is vdom"
  (define comp (make-header-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'header-vdom))

;; ============================================================
;; Component rendering returns vnodes
;; ============================================================

(test-case "transcript component returns vnodes"
  (define comp (make-transcript-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result)))

(test-case "status bar component returns vnodes"
  (define comp (make-status-bar-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-true (vhbox? (car result))))

(test-case "header component returns vnodes"
  (define comp (make-header-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-true (vhbox? (car result))))

;; ============================================================
;; Overlay component
;; ============================================================

(test-case "overlay component wraps content over anchor"
  (define anchor (make-status-bar-vdom-component))
  (define content (make-q-component (lambda (st w) (list (vtext "Popup!" '(inverse)))) #:vdom? #t))
  (define overlay (make-overlay-vdom-component content #:anchor anchor #:col 5 #:row 0))
  (check-true (q-component-vdom? overlay))
  (define result (component-render overlay (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-true (voverlay? (car result))))

;; ============================================================
;; Frame composition
;; ============================================================

(test-case "frame component composes all zones"
  (define frame
    (make-vdom-frame-component (make-header-vdom-component)
                               (make-transcript-vdom-component)
                               (make-status-bar-vdom-component)
                               (make-input-box-vdom-component)))
  (check-true (q-component-vdom? frame))
  (define result (component-render frame (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-true (vvbox? (car result))))

(test-case "frame component with #f zones"
  (define frame (make-vdom-frame-component #f #f (make-status-bar-vdom-component) #f))
  (define result (component-render frame (initial-ui-state) 80))
  (check-true (andmap vnode? result)))

;; ============================================================
;; Full pipeline: component → vdom → layout → buffer
;; ============================================================

(test-case "status bar renders to cell buffer"
  (define comp (make-status-bar-vdom-component))
  (define vnodes (component-render comp (initial-ui-state) 80))
  (define buf (make-cell-buffer 80 3))
  (render-vdom-to-buffer! (vvbox vnodes) buf 80)
  (define row0 (cell-buffer-row-string buf 0))
  (check-true (> (string-length row0) 0)))

(test-case "frame renders complete layout to buffer"
  (define frame
    (make-vdom-frame-component (make-header-vdom-component)
                               (make-transcript-vdom-component)
                               (make-status-bar-vdom-component)
                               (make-input-box-vdom-component)))
  (define vnodes (component-render frame (initial-ui-state) 80))
  (define buf (make-cell-buffer 80 10))
  (render-vdom-to-buffer! (vvbox vnodes) buf 80)
  ;; Should have content in at least row 0
  (define row0 (cell-buffer-row-string buf 0))
  (check-true (> (string-length (string-trim row0)) 0)))
