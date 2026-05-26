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
;; Canonical positional args: (term-height, term-width).
;; Use keyword args for extra safety. Minimum height clamped to 4.
(define (compute-layout term-height
                        term-width
                        #:widget-bar-h [widget-bar-h (widget-bar-height)]
                        #:has-widgets? [has-widgets? #f])
  (define height (max (+ header-height input-height) term-height))
  (define width (max 1 term-width))
  (define effective-widget-h (if (and has-widgets? (> widget-bar-h 0)) widget-bar-h 0))
  (define fixed-height (+ header-height effective-widget-h input-height))
  (define transcript-height (max 0 (- height fixed-height)))
  (hasheq
   'header
   (layout-region 'header 0 header-height width)
   'transcript
   (layout-region 'transcript header-height transcript-height width)
   'widget-bar
   (layout-region 'widget-bar (+ header-height transcript-height) effective-widget-h width)
   'input
   (layout-region 'input (+ header-height transcript-height effective-widget-h) input-height width)))

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

;; Backward-compatible accessors for modules/tests that still use the old
;; tui-layout API. The canonical representation is now a region hash.
(define (tui-layout? v)
  (and (hash? v) (hash-has-key? v 'header) (hash-has-key? v 'transcript) (hash-has-key? v 'input)))

(define (tui-layout-cols layout)
  (layout-region-width (layout-header layout)))

(define (tui-layout-rows layout)
  (define input-region (layout-input layout))
  (+ (layout-region-y input-region) (layout-region-height input-region)))

(define (tui-layout-header-row layout)
  ;; Old API returned #f when no header row was shown.
  ;; The new layout always has a header, but for backward compat return #f.
  #f)

(define (tui-layout-transcript-start-row layout)
  (layout-region-y (layout-transcript layout)))

(define (tui-layout-transcript-height layout)
  (layout-region-height (layout-transcript layout)))

(define (tui-layout-status-row layout)
  (layout-region-y (layout-input layout)))

(define (tui-layout-input-row layout)
  ;; The row where the user types — second line of the 3-line input region
  ;; (first line is border/prompt header, second is text entry, third is spacing).
  ;; This matches the renderer's (add1 (layout-region-y input-region)).
  (add1 (layout-region-y (layout-input layout))))

(define (compute-layout-with-widgets cols rows widget-line-count)
  (compute-layout rows
                  cols
                  #:widget-bar-h widget-line-count
                  #:has-widgets? (positive? widget-line-count)))

;; Convenience: accept old (cols rows) positional order.
;; Use keyword args for unambiguous calls.
(define (compute-layout-legacy cols
                               rows
                               #:widget-bar-h [widget-bar-h 0]
                               #:has-widgets? [has-widgets? #f])
  (compute-layout rows cols #:widget-bar-h widget-bar-h #:has-widgets? has-widgets?))

;; Clip lines to a region's height.
(define (clip-to-region lines region)
  (take (append lines (make-list (layout-region-height region) "")) (layout-region-height region)))

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
                       [clip-to-region (-> (listof any/c) layout-region? (listof any/c))])
         tui-layout?
         tui-layout-cols
         tui-layout-rows
         tui-layout-header-row
         tui-layout-transcript-start-row
         tui-layout-transcript-height
         tui-layout-status-row
         tui-layout-input-row
         compute-layout-with-widgets
         compute-layout-legacy)
