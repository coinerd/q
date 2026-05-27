#lang racket/base

;; q/tui/vdom-layout.rkt — VDOM layout engine
;;
;; Converts vnode tree → styled-line list (same format as message-layout.rkt).
;; Handles:
;;   - vtext → single styled-segment
;;   - vhbox → horizontal concatenation (segments in one line)
;;   - vvbox → vertical stacking (multiple lines)
;;   - vfill → padding segments
;;   - voverlay → layered content at anchor position
;;
;; Width constraint: the caller specifies available width.
;; Lines wider than the width are truncated.

(require racket/contract
         racket/list
         racket/string
         "vdom.rkt"
         "render/message-layout.rkt")

;; ============================================================
;; Layout engine
;; ============================================================

;; Layout a vnode tree into styled-lines within given width.
;; Returns (listof styled-line?)
(define (vdom-layout v width)
  (cond
    [(vtext? v) (layout-vtext v width)]
    [(vhbox? v) (layout-vhbox v width)]
    [(vvbox? v) (layout-vvbox v width)]
    [(vfill? v) (layout-vfill v width)]
    [(voverlay? v) (layout-voverlay v width)]
    [else '()]))

;; vtext → single styled-line with one segment
(define (layout-vtext v width)
  (define text (vtext-text v))
  (define style (vtext-style v))
  (define truncated
    (if (> (string-length text) width)
        (substring text 0 width)
        text))
  (list (styled-line (list (styled-segment truncated style)))))

;; vhbox → concatenate children horizontally into one line
(define (layout-vhbox v width)
  (define children (vhbox-children v))
  (if (null? children)
      (list (styled-line '()))
      (let ([segments (layout-hbox-children children width 0)]) (list (styled-line segments)))))

;; Helper: lay out hbox children into a flat segment list
(define (layout-hbox-children children remaining-width consumed)
  (if (null? children)
      '()
      (let* ([child (car children)]
             [rest (cdr children)]
             [available (- remaining-width consumed)])
        (if (<= available 0)
            '()
            (let ()
              (define segs (child->segments child available))
              (define seg-width (segments-width segs))
              (append segs (layout-hbox-children rest remaining-width (+ consumed seg-width))))))))

;; Convert a leaf vnode to segments (for hbox concatenation)
(define (child->segments v width)
  (cond
    [(vtext? v)
     (define text (vtext-text v))
     (define truncated
       (if (> (string-length text) width)
           (substring text 0 width)
           text))
     (list (styled-segment truncated (vtext-style v)))]
    [(vfill? v)
     (define fill-w (min (vfill-width v) width))
     (list (styled-segment (make-string fill-w (vfill-char v)) (vfill-style v)))]
    [(vhbox? v) (layout-hbox-children (vhbox-children v) width 0)]
    [else
     ;; For complex children, flatten to text
     (define text (vnode->flat-text v))
     (define truncated
       (if (> (string-length text) width)
           (substring text 0 width)
           text))
     (list (styled-segment truncated '()))]))

;; Compute total width of segments
(define (segments-width segs)
  (apply + (map (lambda (s) (string-length (styled-segment-text s))) segs)))

;; Flatten vnode to plain text (fallback for complex nesting in hbox)
(define (vnode->flat-text v)
  (cond
    [(vtext? v) (vtext-text v)]
    [(vhbox? v) (apply string-append (map vnode->flat-text (vhbox-children v)))]
    [(vvbox? v) (string-join (map vnode->flat-text (vvbox-children v)) "\n")]
    [(vfill? v) (make-string (vfill-width v) (vfill-char v))]
    [(voverlay? v) (vnode->flat-text (voverlay-anchor v))]
    [else ""]))

;; vvbox → stack children vertically
(define (layout-vvbox v width)
  (apply append (map (lambda (child) (vdom-layout child width)) (vvbox-children v))))

;; vfill → single line of padding
(define (layout-vfill v width)
  (define fill-w (min (vfill-width v) width))
  (list (styled-line (list (styled-segment (make-string fill-w (vfill-char v)) (vfill-style v))))))

;; voverlay → render anchor, then overlay content at position
(define (layout-voverlay v width)
  (define anchor-lines (vdom-layout (voverlay-anchor v) width))
  (define content-lines (vdom-layout (voverlay-content v) width))
  (define offset-col (voverlay-col v))
  (define offset-row (voverlay-row v))
  ;; Overlay content lines onto anchor lines
  (for/list ([anchor-line (in-list anchor-lines)]
             [anchor-idx (in-naturals)])
    (define overlay-idx (- anchor-idx offset-row))
    (if (and (>= overlay-idx 0) (< overlay-idx (length content-lines)))
        ;; Merge overlay line into anchor line
        (merge-lines anchor-line (list-ref content-lines overlay-idx) offset-col)
        anchor-line)))

;; Merge an overlay line into an anchor line at given column
(define (merge-lines anchor overlay col)
  (define anchor-segs (styled-line-segments anchor))
  (define overlay-segs (styled-line-segments overlay))
  ;; Simple approach: prepend anchor up to col, then overlay, then rest of anchor
  ;; For now, concatenate with positioning
  (define anchor-text (segs->text anchor-segs))
  (define overlay-text (segs->text overlay-segs))
  (define result-text
    (let ([prefix (if (> (string-length anchor-text) col)
                      (substring anchor-text 0 col)
                      (string-append anchor-text
                                     (make-string (- col (string-length anchor-text)) #\space)))]
          [suffix (if (> (string-length anchor-text) (+ col (string-length overlay-text)))
                      (substring anchor-text (+ col (string-length overlay-text)))
                      "")])
      (string-append prefix overlay-text suffix)))
  ;; Preserve overlay styles
  (define result-segs
    (if (null? overlay-segs)
        (list (styled-segment result-text '()))
        (list (styled-segment result-text (styled-segment-style (car overlay-segs))))))
  (styled-line result-segs))

;; Extract plain text from segments
(define (segs->text segs)
  (apply string-append (map styled-segment-text segs)))

;; ============================================================
;; Contracts and exports
;; ============================================================

(provide (contract-out [vdom-layout (-> vnode? exact-nonnegative-integer? (listof styled-line?))])
         merge-lines
         child->segments
         segs->text
         vnode->flat-text)
