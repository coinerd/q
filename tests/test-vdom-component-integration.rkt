#lang racket

;; tests/test-vdom-component-integration.rkt — Integration: vdom components alongside existing renderer

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/vdom-bridge.rkt"
         "../tui/vdom-components.rkt"
         "../tui/component.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/renderer.rkt"
         "../tui/state.rkt"
         "../tui/render/message-layout.rkt")

;; ============================================================
;; Dual-path: vdom and existing renderer produce similar output
;; ============================================================

(test-case "vdom status bar and existing renderer both render"
  ;; Existing renderer path
  (define comps (make-render-components))
  (define st (initial-ui-state))
  (define existing-lines (render-components-status comps st 80))
  (check-true (andmap styled-line? existing-lines))

  ;; Vdom path
  (define vdom-comp (make-status-bar-vdom-component))
  (define vdom-result (component-render vdom-comp st 80))
  (check-true (andmap vnode? vdom-result))

  ;; Both paths produce output
  (check-true (> (length existing-lines) 0))
  (check-true (> (length vdom-result) 0)))

(test-case "vdom frame produces complete screen layout"
  (define st (initial-ui-state))
  (define frame
    (make-vdom-frame-component (make-header-vdom-component)
                               (make-transcript-vdom-component)
                               (make-status-bar-vdom-component)
                               (make-input-box-vdom-component)))
  (define vnodes (component-render frame st 80))
  (check-true (andmap vnode? vnodes))

  ;; Layout to styled-lines
  (define lines (vdom-layout (car vnodes) 80))
  (check-true (> (length lines) 0))
  (check-true (andmap styled-line? lines))

  ;; Render to cell buffer
  (define buf (make-cell-buffer 80 24))
  (render-styled-lines-to-buffer! lines buf 80)
  ;; Verify non-empty content
  (check-true (> (string-length (string-trim (cell-buffer-row-string buf 0))) 0)))

;; ============================================================
;; Component caching works with vdom components
;; ============================================================

(test-case "vdom components cache by width"
  (define comp (make-header-vdom-component))
  (define st (initial-ui-state))

  ;; First render
  (define result1 (component-render comp st 80))
  (check-equal? (component-cached-width comp) 80)

  ;; Second render at same width — should be cached
  (define result2 (component-render comp st 80))
  (check-eq? result1 result2)

  ;; Different width — cache miss
  (define result3 (component-render comp st 40))
  (check-equal? (component-cached-width comp) 40)
  (check-not-eq? result1 result3))

;; ============================================================
;; Focus management with vdom components
;; ============================================================

(test-case "vdom components can be focusable"
  (define comps
    (list (make-q-component (lambda (s w) (list (vtext "A" '())))
                            #:id 'comp-a
                            #:wants-focus? #t
                            #:vdom? #t)
          (make-q-component (lambda (s w) (list (vtext "B" '()))) #:id 'comp-b #:vdom? #t)
          (make-q-component (lambda (s w) (list (vtext "C" '())))
                            #:id 'comp-c
                            #:wants-focus? #t
                            #:vdom? #t)))
  (define focusable (focusable-components comps))
  (check-equal? (length focusable) 2)
  (check-equal? (cycle-focus comps #f) 'comp-a)
  (check-equal? (cycle-focus comps 'comp-a) 'comp-c)
  (check-equal? (cycle-focus comps 'comp-c) 'comp-a))

;; ============================================================
;; Component compose with mixed vdom types
;; ============================================================

(test-case "component-compose with all vdom components"
  (define comps (list (make-header-vdom-component) (make-status-bar-vdom-component)))
  (define result (component-compose comps (initial-ui-state) 80))
  (check-true (andmap vnode? result))
  (check-equal? (length result) 2))
