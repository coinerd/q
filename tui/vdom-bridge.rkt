#lang racket/base

;; q/tui/vdom-bridge.rkt — Bridge between vdom pipeline and render loop
;;
;; Provides a vdom-based frame render function that can be used
;; as an alternative to the existing renderer. The existing
;; renderer.rkt remains the default; this module enables the
;; vdom path for future GUI backend compatibility.
;;
;; Usage: Set (use-vdom-render? #t) to enable vdom rendering.

(require racket/contract
         "vdom.rkt"
         "vdom-layout.rkt"
         "vdom-render.rkt"
         "cell-buffer.rkt"
         "render/message-layout.rkt")

;; Parameter to switch between vdom and direct rendering.
;; Always #t now — legacy path removed in v0.61.3.
(define use-vdom-render? (make-parameter #t))

;; ============================================================
;; Frame rendering via vdom
;; ============================================================

;; Render a list of vnodes (one per section) to a cell buffer.
;; Each vnode produces styled-lines which are rendered sequentially.
;; Sections: header-lines, transcript-lines, status-line, input-lines.
(define (render-vdom-frame! buf sections width #:header-count [header-count 0])
  (define section-vnodes
    (for/list ([sec (in-list sections)])
      (if (vnode? sec)
          sec
          (vvbox '()))))
  (define start-row 0)
  (for ([vnode (in-list section-vnodes)])
    (define lines (vdom-layout vnode width))
    (render-styled-lines-to-buffer! lines buf width #:start-row start-row)
    (set! start-row (+ start-row (length lines)))))

;; ============================================================
;; Convenience: convert styled-line to vnode
;; ============================================================

(define (styled-line->vnode line)
  (define segs (styled-line-segments line))
  (vhbox (for/list ([seg (in-list segs)])
           (vtext (styled-segment-text seg) (styled-segment-style seg)))))

;; Convert list of styled-lines to a vvbox vnode
(define (styled-lines->vnode lines)
  (vvbox (map styled-line->vnode lines)))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide use-vdom-render?
         (contract-out [render-vdom-frame!
                        (->* (cell-buffer? (listof any/c) exact-nonnegative-integer?)
                             (#:header-count exact-nonnegative-integer?)
                             void?)]
                       [styled-line->vnode (-> styled-line? vnode?)]
                       [styled-lines->vnode (-> (listof styled-line?) vnode?)]))
