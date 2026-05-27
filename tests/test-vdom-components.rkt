#lang racket

;; tests/test-vdom-components.rkt — Tests for tui/vdom-components.rkt

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/vdom-components.rkt"
         "../tui/component.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/state.rkt"
         "../tui/state-types.rkt"
         "../tui/render/message-layout.rkt")

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

(test-case "production status bar renders model name"
  (define st (initial-ui-state #:model-name "gpt-4o"))
  (define comp (make-status-bar-vdom-component))
  (define result (component-render comp st 80))
  (check-true (andmap vnode? result))
  (check-equal? (length result) 1))

(test-case "production status bar renders session label"
  (define st (initial-ui-state #:session-id "test-sess" #:model-name "gpt-4o"))
  (define comp (make-status-bar-vdom-component))
  (define result (component-render comp st 80))
  (check-true (andmap vnode? result))
  (define v (car result))
  (check-true (vhbox? v)))

(test-case "production status bar renders to cell buffer"
  (define st (initial-ui-state #:model-name "gpt-4o"))
  (define comp (make-status-bar-vdom-component))
  (define vnodes (component-render comp st 80))
  (define buf (make-cell-buffer 80 1))
  (render-vdom-to-buffer! (vvbox vnodes) buf 80)
  (define row0 (cell-buffer-row-string buf 0))
  (check-true (> (string-length (string-trim row0)) 0)))

(test-case "input box component is vdom"
  (define comp (make-input-box-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'input-box-vdom))

(test-case "production input box renders prompt"
  (define comp (make-input-box-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-equal? (length result) 1))

(test-case "production input box renders to cell buffer"
  (define comp (make-input-box-vdom-component))
  (define vnodes (component-render comp (initial-ui-state) 80))
  (define buf (make-cell-buffer 80 1))
  (render-vdom-to-buffer! (vvbox vnodes) buf 80)
  (define row0 (cell-buffer-row-string buf 0))
  ;; Should contain "q> " prompt
  (check-not-false (string-contains? row0 "q>")))

(test-case "header component is vdom"
  (define comp (make-header-vdom-component))
  (check-true (q-component-vdom? comp))
  (check-equal? (q-component-id comp) 'header-vdom))

(test-case "production header renders inverse-style header"
  (define comp (make-header-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-true (vhbox? (car result)))
  ;; The header should have inverse style
  (define children (vhbox-children (car result)))
  (check-not-false (member 'inverse (vtext-style (car children)))))

(test-case "production header renders to cell buffer"
  (define comp (make-header-vdom-component))
  (define vnodes (component-render comp (initial-ui-state) 80))
  (define buf (make-cell-buffer 80 1))
  (render-vdom-to-buffer! (vvbox vnodes) buf 80)
  (define row0 (cell-buffer-row-string buf 0))
  ;; Should contain "q" in the header
  (check-not-false (string-contains? row0 "q")))

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

;; ============================================================
;; Production TranscriptComponent tests
;; ============================================================

(test-case "production transcript renders entries as vnodes"
  (define st0 (initial-ui-state))
  (define st1 (add-transcript-entry st0 (transcript-entry 'user "Hello" 0 (hash) #f)))
  (define st2 (add-transcript-entry st1 (transcript-entry 'assistant "Hi there!" 0 (hash) #f)))
  (define comp (make-transcript-vdom-component))
  (define result (component-render comp st2 80))
  (check-true (andmap vnode? result))
  ;; Should have multiple rows (user + assistant entries)
  (check-true (> (length result) 0)))

(test-case "production transcript with empty state returns vnodes"
  (define comp (make-transcript-vdom-component))
  (define result (component-render comp (initial-ui-state) 80))
  (check-true (andmap vnode? result)))

(test-case "styled-line->vnode converts single segment"
  (define sl (styled-line (list (styled-segment "Hello" '(bold)))))
  (define v (styled-line->vnode sl))
  (check-true (vhbox? v))
  (define children (vhbox-children v))
  (check-equal? (length children) 1)
  (check-equal? (vtext-text (car children)) "Hello")
  (check-equal? (vtext-style (car children)) '(bold)))

(test-case "styled-line->vnode converts multi-segment line"
  (define sl (styled-line (list (styled-segment "Hello " '()) (styled-segment "World" '(bold)))))
  (define v (styled-line->vnode sl))
  (check-true (vhbox? v))
  (define children (vhbox-children v))
  (check-equal? (length children) 2)
  (check-equal? (vtext-text (car children)) "Hello ")
  (check-equal? (vtext-style (cadr children)) '(bold)))

(test-case "styled-line->vnode handles empty segments"
  (define sl (styled-line '()))
  (define v (styled-line->vnode sl))
  (check-true (vhbox? v)))

(test-case "styled-lines->vnodes converts list"
  (define lines
    (list (styled-line (list (styled-segment "Line 1" '())))
          (styled-line (list (styled-segment "Line 2" '(bold))))))
  (define vnodes (styled-lines->vnodes lines))
  (check-equal? (length vnodes) 2)
  (check-true (andmap vhbox? vnodes)))

(test-case "production transcript renders to cell buffer"
  (define st0 (initial-ui-state))
  (define st1 (add-transcript-entry st0 (transcript-entry 'user "Test message" 0 (hash) #f)))
  (define comp (make-transcript-vdom-component #:height 10))
  (define vnodes ((q-component-render-fn comp) st1 80))
  (define buf (make-cell-buffer 80 10))
  ;; Render only the first 10 vnodes (height-clipped)
  (for ([vn (in-list (take vnodes (min (length vnodes) 10)))]
        [i (in-naturals)])
    (render-vdom-to-buffer! vn buf 80 #:start-row i))
  ;; Should have content in at least one row
  (define row0 (cell-buffer-row-string buf 9))
  (check-true (> (string-length (string-trim row0)) 0)))
