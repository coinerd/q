#lang racket

;; tests/test-vdom-bridge.rkt — Integration test for vdom pipeline

(define (row-string buf row)
  (cell-buffer-row-string buf row))

(require rackunit
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/vdom-bridge.rkt"
         "../tui/cell-buffer.rkt"
         "../tui/render/message-layout.rkt")

;; ============================================================
;; styled-line ↔ vnode conversion
;; ============================================================

(test-case "styled-line->vnode roundtrip"
  (define line (styled-line (list (styled-segment "Hello" '(bold)) (styled-segment " World" '()))))
  (define v (styled-line->vnode line))
  (check-true (vnode? v))
  (check-true (vhbox? v))
  (check-equal? (length (vhbox-children v)) 2))

(test-case "styled-lines->vnode preserves line count"
  (define lines
    (list (styled-line (list (styled-segment "Line1" '())))
          (styled-line (list (styled-segment "Line2" '())))))
  (define v (styled-lines->vnode lines))
  (check-true (vvbox? v))
  (check-equal? (length (vvbox-children v)) 2))

;; ============================================================
;; Full pipeline: vnode → layout → render → cell-buffer
;; ============================================================

(test-case "full pipeline: header bar"
  (define header (vhbox (list (vtext " q " '(bold cyan)) (vfill* 10) (vtext "v0.60 " '(dim)))))
  (define buf (make-cell-buffer 40 5))
  (render-vdom-to-buffer! header buf 40)
  (define row0 (row-string buf 0))
  (check-true (string-contains? row0 "q"))
  (check-true (string-contains? row0 "v0.60")))

(test-case "full pipeline: multi-section frame"
  (define sections
    (list (vvbox (list (vhbox (list (vtext "Header" '(bold))))
                       (vtext "Body line 1" '())
                       (vtext "Body line 2" '())))
          (vhbox (list (vtext "Status: OK" '(green))))))
  (define buf (make-cell-buffer 40 10))
  (render-vdom-frame! buf sections 40)
  (check-true (string-contains? (row-string buf 0) "Header"))
  (check-true (string-contains? (row-string buf 1) "Body line 1"))
  (check-true (string-contains? (row-string buf 3) "Status")))

(test-case "full pipeline: overlay popup"
  (define popup
    (voverlay (vvbox (list (vhbox (list (vtext "┌" '(white))
                                        (vtext "Popup" '(inverse))
                                        (vtext "┐" '(white))))))
              (vtext "Background text here" '())
              2
              0))
  (define buf (make-cell-buffer 30 5))
  (render-vdom-to-buffer! popup buf 30)
  (define row0 (row-string buf 0))
  (check-true (string-contains? row0 "Popup")))

(test-case "vdom render with styled-line conversion"
  (define orig-lines
    (list (styled-line (list (styled-segment "Hello " '()) (styled-segment "World" '(bold))))))
  (define vnode (styled-lines->vnode orig-lines))
  (define buf (make-cell-buffer 20 3))
  (render-vdom-to-buffer! vnode buf 20)
  (check-true (string-contains? (row-string buf 0) "Hello"))
  (check-true (string-contains? (row-string buf 0) "World")))

;; ============================================================
;; use-vdom-render? parameter
;; ============================================================

(test-case "use-vdom-render? parameter default is #t"
  (check-true (use-vdom-render?)))

(test-case "use-vdom-render? can be set"
  (parameterize ([use-vdom-render? #f])
    (check-false (use-vdom-render?))))
