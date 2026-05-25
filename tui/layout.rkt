#lang racket/base

;; tui/layout.rkt — 5-region TUI layout computation (#5257, #5258)
;;
;; Defines the terminal layout as five stacked regions:
;;   1. Header      — fixed height (status bar)
;;   2. Transcript  — flexible height (scrollable message history)
;;   3. Widget bar  — fixed height (lifecycle widgets, below transcript)
;;   4. Input       — fixed height (command/prompt entry)
;;   5. Overlay     — floating, absolute-positioned (dialogs)
;;
;; Height allocation:
;;   - Header: 1 line
;;   - Input: 3 lines (input + border + spacing)
;;   - Widget bar: configurable via widget-bar-height parameter (default 3)
;;   - Transcript: remaining space = terminal-height - header - widget-bar - input
;;   - Overlay: not counted — drawn on top of other regions

(require racket/contract
         racket/list
         "state-ui.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Layout region struct
;; ═══════════════════════════════════════════════════════════════════

(struct layout-region
        (name ; symbol — region identifier
         y ; exact-nonnegative-integer — top row
         height ; exact-nonnegative-integer — number of lines
         width ; exact-nonnegative-integer — number of columns
         )
  #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Layout computation
;; ═══════════════════════════════════════════════════════════════════

;; Height constants
(define header-height 1)
(define input-height 3)

;; Compute the 5-region layout for a given terminal size.
;; Returns a hash of region-name -> layout-region.
(define (compute-layout term-height
                        term-width
                        #:widget-bar-h [widget-bar-h (widget-bar-height)]
                        #:has-widgets? [has-widgets? #f])
  (define effective-widget-h (if (and has-widgets? (> widget-bar-h 0)) widget-bar-h 0))
  (define fixed-height (+ header-height effective-widget-h input-height))
  (define transcript-height (max 0 (- term-height fixed-height)))
  (hasheq
   'header
   (layout-region 'header 0 header-height term-width)
   'transcript
   (layout-region 'transcript header-height transcript-height term-width)
   'widget-bar
   (layout-region 'widget-bar (+ header-height transcript-height) effective-widget-h term-width)
   'input
   (layout-region 'input
                  (+ header-height transcript-height effective-widget-h)
                  input-height
                  term-width)))

;; Get transcript region from layout
(define (layout-transcript layout)
  (hash-ref layout 'transcript))

;; Get widget-bar region from layout
(define (layout-widget-bar layout)
  (hash-ref layout 'widget-bar))

;; Get header region from layout
(define (layout-header layout)
  (hash-ref layout 'header))

;; Get input region from layout
(define (layout-input layout)
  (hash-ref layout 'input))

;; Clip lines to a region's height.
(define (clip-to-region lines region)
  (takef (append lines (make-list (layout-region-height region) ""))
         (lambda (_) #t)
         (layout-region-height region)))

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

(provide layout-region
         layout-region?
         layout-region-name
         layout-region-y
         layout-region-height
         layout-region-width
         header-height
         input-height
         (contract-out [compute-layout
                        (->* (exact-positive-integer? exact-positive-integer?)
                             (#:widget-bar-h exact-nonnegative-integer? #:has-widgets? boolean?)
                             hash?)]
                       [layout-transcript (-> hash? layout-region?)]
                       [layout-widget-bar (-> hash? layout-region?)]
                       [layout-header (-> hash? layout-region?)]
                       [layout-input (-> hash? layout-region?)]
                       [clip-to-region (-> (listof any/c) layout-region? (listof any/c))]))
