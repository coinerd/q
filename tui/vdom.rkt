#lang racket/base

;; q/tui/vdom.rkt — Virtual DOM data types
;;
;; Pure data structs representing terminal UI elements. No rendering logic.
;; The vdom layer sits between state/components and the cell-buffer renderer.
;;
;; vnode types:
;;   vtext   — styled text segment
;;   vhbox   — horizontal concatenation of children
;;   vvbox   — vertical stacking of children
;;   vfill   — horizontal padding/spacing
;;   voverlay — layered content with anchor positioning
;;
;; Layout engine (vdom-layout.rkt) converts vnodes → styled-line list.
;; Render engine (vdom-render.rkt) converts styled-lines → cell-buffer writes.

(require racket/contract
         racket/list)

;; ============================================================
;; Style type — same model as styled-segment
;; ============================================================

;; Styles are (listof symbol): 'bold 'italic 'inverse 'underline 'dim
;;   'red 'green 'yellow 'blue 'cyan 'magenta 'white

;; ============================================================
;; vnode types
;; ============================================================

;; Styled text node
(struct vtext
        (text ; string
         style ; (listof symbol) — styling attributes
         )
  #:transparent)

;; Horizontal box — children laid out left to right
(struct vhbox
        (children ; (listof vnode?)
         )
  #:transparent)

;; Vertical box — children laid out top to bottom
(struct vvbox
        (children ; (listof vnode?)
         )
  #:transparent)

;; Horizontal fill — padding to a given width
(struct vfill
        (width ; exact-nonnegative-integer?
         char ; char (default #\space)
         style ; (listof symbol)
         )
  #:transparent)

;; Overlay — one child rendered on top of another at an anchor point
(struct voverlay
        (content ; vnode?
         anchor ; vnode?
         col ; exact-nonnegative-integer? — x offset
         row ; exact-nonnegative-integer? — y offset
         )
  #:transparent)

;; ============================================================
;; Predicates
;; ============================================================

(define (vnode? v)
  (or (vtext? v) (vhbox? v) (vvbox? v) (vfill? v) (voverlay? v)))

;; ============================================================
;; Convenience constructors
;; ============================================================

(define (vtext* text . styles)
  (vtext text styles))

(define (vfill* width [char #\space])
  (vfill width char '()))

;; ============================================================
;; Tree operations
;; ============================================================

;; Count the total text length of a vnode tree
(define (vnode-text-length v)
  (cond
    [(vtext? v) (string-length (vtext-text v))]
    [(vhbox? v) (for/sum ([c (in-list (vhbox-children v))]) (vnode-text-length c))]
    [(vvbox? v)
     (define children (vvbox-children v))
     (if (null? children)
         0
         (apply max (map vnode-text-length children)))]
    [(vfill? v) (vfill-width v)]
    [(voverlay? v) (vnode-text-length (voverlay-anchor v))]
    [else 0]))

;; Count the height (lines) of a vnode tree
(define (vnode-height v)
  (cond
    [(vtext? v) 1]
    [(vhbox? v)
     (if (null? (vhbox-children v))
         0
         (apply max (map vnode-height (vhbox-children v))))]
    [(vvbox? v) (for/sum ([c (in-list (vvbox-children v))]) (vnode-height c))]
    [(vfill? v) 1]
    [(voverlay? v) (max (vnode-height (voverlay-content v)) (vnode-height (voverlay-anchor v)))]
    [else 0]))

;; Map over all vtext nodes in tree
(define (vnode-map-text f v)
  (cond
    [(vtext? v) (f v)]
    [(vhbox? v) (vhbox (map (lambda (c) (vnode-map-text f c)) (vhbox-children v)))]
    [(vvbox? v) (vvbox (map (lambda (c) (vnode-map-text f c)) (vvbox-children v)))]
    [(vfill? v) v]
    [(voverlay? v)
     (voverlay (vnode-map-text f (voverlay-content v))
               (vnode-map-text f (voverlay-anchor v))
               (voverlay-col v)
               (voverlay-row v))]
    [else v]))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (struct-out vtext)
         (struct-out vhbox)
         (struct-out vvbox)
         (struct-out vfill)
         (struct-out voverlay)
         vnode?
         vtext*
         vfill*
         vnode-text-length
         vnode-height
         vnode-map-text)
