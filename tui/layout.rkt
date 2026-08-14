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

;; Multiline composer height bounds (W3, v0.99.96).
;; The composer text area is bounded: at least 1 text row, at most
;; `max-composer-text-rows` (configurable, default 6).  The total input
;; region height adds 2 chrome rows (border/prompt header + spacing) on
;; top of the text rows.
(define default-max-composer-text-rows 6)
(define min-composer-text-rows 1)
(define composer-chrome-rows 2)

;; Clamp a requested composer text-row count to [1, max-rows].
(define (clamp-composer-text-rows rows [max-rows default-max-composer-text-rows])
  (max min-composer-text-rows (min (max rows min-composer-text-rows)
                                   (max max-rows min-composer-text-rows))))

;; Compute the input region height from a composer text-row count.
;; Legacy single-line composer: 3 rows (border + 1 text + spacing).
(define (composer-input-height text-rows [max-rows default-max-composer-text-rows])
  (+ composer-chrome-rows (clamp-composer-text-rows text-rows max-rows)))

;; Compute the 5-region layout for a given terminal size.
;; Returns a hash of region-name -> layout-region.
;; Canonical positional args: (term-height, term-width).
;; Use keyword args for extra safety. Minimum height clamped to 4.
;; `#:composer-height` (W3): composer TEXT rows from the shared visual
;; layout.  When omitted, the legacy fixed 3-row input region is used.
;; Height is clamped to [1, #:max-composer-rows]; transcript takes the rest.
(define (compute-layout term-height
                        term-width
                        #:widget-bar-h [widget-bar-h (widget-bar-height)]
                        #:has-widgets? [has-widgets? #f]
                        #:composer-height [composer-text-rows #f]
                        #:max-composer-rows [max-composer-rows default-max-composer-text-rows])
  (define effective-input-height
    (if composer-text-rows
        (composer-input-height composer-text-rows max-composer-rows)
        input-height))
  (define height (max (+ header-height effective-input-height) term-height))
  (define width (max 1 term-width))
  (define effective-widget-h (if (and has-widgets? (> widget-bar-h 0)) widget-bar-h 0))
  (define fixed-height (+ header-height effective-widget-h effective-input-height))
  (define transcript-height (max 0 (- height fixed-height)))
  (hasheq
   'header
   (layout-region 'header 0 header-height width)
   'transcript
   (layout-region 'transcript header-height transcript-height width)
   'widget-bar
   (layout-region 'widget-bar (+ header-height transcript-height) effective-widget-h width)
   'input
   (layout-region 'input (+ header-height transcript-height effective-widget-h) effective-input-height width)))

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
         min-composer-text-rows
         default-max-composer-text-rows
         composer-chrome-rows
         clamp-composer-text-rows
         composer-input-height
         (contract-out [compute-layout
                        (->* (exact-positive-integer? exact-positive-integer?)
                             (#:widget-bar-h exact-nonnegative-integer?
                              #:has-widgets? boolean?
                              #:composer-height (or/c #f exact-positive-integer?)
                              #:max-composer-rows exact-positive-integer?)
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
         compute-layout-with-widgets)

;; ═══════════════════════════════════════════════════════════════════
;; Tests (W3: composer-height)
;; ═══════════════════════════════════════════════════════════════════

(module+ test
  (require rackunit)

  ;; Legacy: no composer-height -> fixed 3-row input region.
  (let ([l (compute-layout 24 80)])
    (check-equal? (layout-region-height (layout-input l)) 3)
    (check-equal? (layout-region-height (layout-transcript l)) 20))

  ;; Composer height clamped: below min -> 1 text row (+2 chrome = 3).
  (let ([l (compute-layout 24 80 #:composer-height 0)])
    (check-equal? (layout-region-height (layout-input l)) 3))

  ;; 4 text rows -> 6-row input region, transcript shrinks accordingly.
  (let ([l (compute-layout 24 80 #:composer-height 4)])
    (check-equal? (layout-region-height (layout-input l)) 6)
    (check-equal? (layout-region-height (layout-transcript l)) 17))

  ;; Above default max (6) -> clamped to 6 text rows = 8-row region.
  (let ([l (compute-layout 40 80 #:composer-height 20)])
    (check-equal? (layout-region-height (layout-input l)) 8))

  ;; Configurable maximum overrides the default.
  (let ([l (compute-layout 40 80 #:composer-height 10 #:max-composer-rows 10)])
    (check-equal? (layout-region-height (layout-input l)) 12))

  ;; Transcript consumes the remaining height even at max composer.
  (let ([l (compute-layout 40 80 #:composer-height 20)])
    (check-equal? (+ (layout-region-height (layout-transcript l))
                     (layout-region-height (layout-input l)))
                  39))

  (check-equal? (clamp-composer-text-rows 3) 3)
  (check-equal? (clamp-composer-text-rows 0) 1)
  (check-equal? (clamp-composer-text-rows 99) 6)
  (check-equal? (composer-input-height 1) 3)
  (check-equal? (composer-input-height 6) 8)
  (check-equal? (composer-input-height 9) 8))
