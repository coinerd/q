#lang racket/base

(require racket/keyword
         racket/list
         "render.rkt"
         "layout.rkt"
         "state.rkt"
         "input.rkt")
;;
;; Renders styled-lines to a tui-ubuf buffer.
;; No terminal I/O directly — all output goes through ubuf.
;;
;; tui-ubuf ubuf-putstring! signature:
;;   (ubuf-putstring! ub x y str
;;     #:fg [fg 7]  #:bg [bg 0]
;;     #:bold [bold #f]  #:underline [underline #f]
;;     #:italic [italic #f]  #:blink [blink #f])
;;
;; NOTE: tui-ubuf has NO #:inverse, #:dim, or #:reset keywords.
;;   inverse = swap fg/bg (fg=0, bg=7)
;;   dim     = fg=8 (dark gray)
;;   reset   = fg=7, bg=0, bold=#f (default values)

;; Main rendering function
(provide render-frame!

         ;; Style mapping (for testing)
         style->ubuf-kws

         ;; Ubuf operations (settable for testing)
         current-ubuf-clear
         current-ubuf-putstring)

;; ============================================================
;; Ubuf operations (parameterized for testability)
;; ============================================================

;; Parameter for ubuf-clear! function
(define current-ubuf-clear (make-parameter (lambda (ubuf) (void))))

;; Parameter for ubuf-putstring! function
;; Signature matches tui-ubuf: (ubuf x y str #:fg #:bg #:bold #:underline ...)
(define current-ubuf-putstring
  (make-parameter (lambda (ubuf
                           x
                           y
                           str
                           #:fg [fg 7]
                           #:bg [bg 0]
                           #:bold [bold #f]
                           #:underline [underline #f]
                           #:italic [italic #f]
                           #:blink [blink #f])
                    (void))))

;; ============================================================
;; Style → ubuf attribute conversion
;; ============================================================

;; Color name → ANSI color number
(define (color-name->number c)
  (case c
    [(black) 0]
    [(red) 1]
    [(green) 2]
    [(yellow) 3]
    [(blue) 4]
    [(magenta) 5]
    [(cyan) 6]
    [(white) 7]
    [(dim-gray dark-gray) 8]
    [else 7]))

;; Resolve a list of style symbols into ubuf keyword arguments.
;; Returns (values kw-list val-list) for keyword-apply.
;; IMPORTANT: keyword-apply requires keywords in SORTED (alphabetical) order.
;;
;; Style symbols from render.rkt:
;;   'bold 'inverse 'underline 'dim 'red 'green 'yellow 'cyan
(define (style->ubuf-kws styles)
  (define fg-color #f)
  (define bg-color #f)
  (define bold? #f)
  (define underline? #f)
  (define italic? #f)
  (define inverse? #f)
  (for ([s (in-list styles)])
    (case s
      [(bold) (set! bold? #t)]
      [(italic) (set! italic? #t)]
      [(inverse) (set! inverse? #t)]
      [(underline) (set! underline? #t)]
      [(dim)
       (when (not fg-color)
         (set! fg-color 8))]
      [(red) (set! fg-color 1)]
      [(green) (set! fg-color 2)]
      [(yellow) (set! fg-color 3)]
      [(blue) (set! fg-color 4)]
      [(magenta) (set! fg-color 5)]
      [(cyan) (set! fg-color 6)]
      [(white) (set! fg-color 7)]
      [else (void)]))
  ;; Collect kw/val pairs — only include non-defaults
  (define pairs '())
  ;; Inverse: swap fg/bg
  (when inverse?
    (set! pairs (cons (cons '#:fg 0) pairs))
    (set! pairs (cons (cons '#:bg (or fg-color 7)) pairs)))
  (when (and (not inverse?) fg-color)
    (set! pairs (cons (cons '#:fg fg-color) pairs)))
  (when bold?
    (set! pairs (cons (cons '#:bold #t) pairs)))
  (when underline?
    (set! pairs (cons (cons '#:underline #t) pairs)))
  (when italic?
    (set! pairs (cons (cons '#:italic #t) pairs)))
  ;; Sort by keyword (alphabetically) — keyword-apply requirement
  (define sorted (sort pairs keyword<? #:key car))
  (values (map car sorted) (map cdr sorted)))

;; ============================================================
;; Draw styled-line to ubuf at given row
;; ============================================================

;; Draw a styled-line to the ubuf at the specified row, starting at column 1.
;; The line is padded to width with spaces.
(define (draw-styled-line! ubuf line row width)
  (define ubuf-putstring! (current-ubuf-putstring))
  (define col 0)
  (define remaining-width width)
  ;; Draw each segment
  (for ([seg (in-list (styled-line-segments line))])
    #:break (<= remaining-width 0)
    (define text (styled-segment-text seg))
    (define styles (styled-segment-style seg))
    ;; Truncate if necessary
    (define visible-text
      (if (> (string-length text) remaining-width)
          (substring text 0 remaining-width)
          text))
    ;; Resolve styles to ubuf keywords
    (define-values (kws vals) (style->ubuf-kws styles))
    ;; Draw segment with styles
    (keyword-apply ubuf-putstring! kws vals (list ubuf col row visible-text))
    (set! col (+ col (string-length visible-text)))
    (set! remaining-width (- remaining-width (string-length visible-text))))
  ;; Pad rest with spaces
  (when (> remaining-width 0)
    (ubuf-putstring! ubuf col row (make-string remaining-width #\space))))

;; ============================================================
;; Frame rendering
;; ============================================================

;; Render a complete frame to the ubuf buffer.
;;
;; Parameters:
;;   ubuf       — ubuf buffer (mutable)
;;   ui-state   — current ui-state
;;   input-st   — current input-state
;;   layout     — tui-layout struct
;;
;; Side effect: Modifies ubuf contents.
;; Returns: (values cursor-col cursor-row ui-state frame-lines)
;;   frame-lines is (listof string) — plain text for each row, for diffing
(define (render-frame! ubuf ui-state input-st layout)
  (define cols (tui-layout-cols layout))
  (define rows (tui-layout-rows layout))
  (define header-row (tui-layout-header-row layout))
  (define transcript-start-row (tui-layout-transcript-start-row layout))
  (define transcript-height (tui-layout-transcript-height layout))
  (define status-row (tui-layout-status-row layout))
  (define input-row (tui-layout-input-row layout))

  ;; Get the ubuf operations
  (define ubuf-clear! (current-ubuf-clear))
  (define ubuf-putstring! (current-ubuf-putstring))

  ;; Layout rows are 0-based, matching ubuf's 0-based indexing.
  (define header-y header-row)
  (define trans-y transcript-start-row)
  (define status-y status-row)
  (define input-y input-row)

  ;; 1. Clear the buffer
  (ubuf-clear! ubuf)

  ;; 2. Draw header row (inverse = fg=0, bg=7)
  (define header-text (format " q ~a" (make-string (max 0 (- cols 3)) #\space)))
  (ubuf-putstring! ubuf 0 header-y header-text #:fg 0 #:bg 7)

  ;; Build frame-lines for diffing
  (define frame-vec (make-vector rows ""))
  (vector-set! frame-vec header-y header-text)

  ;; 3. Draw transcript entries (with render cache)
  (define-values (trans-lines-raw ui-state*) (render-transcript ui-state transcript-height cols))
  ;; Apply selection highlight if selection is active
  (define sel-anchor (ui-state-sel-anchor ui-state))
  (define sel-end (ui-state-sel-end ui-state))
  (define trans-lines
    (if (and sel-anchor sel-end)
        (apply-selection-highlight trans-lines-raw sel-anchor sel-end trans-y)
        trans-lines-raw))
  (define visible-lines
    (if (> (length trans-lines) transcript-height)
        (take-right trans-lines transcript-height)
        trans-lines))
  (define pad-count (- transcript-height (length visible-lines)))
  (for ([i (in-range pad-count)])
    (define blank (make-string cols #\space))
    (ubuf-putstring! ubuf 0 (+ trans-y i) blank)
    (vector-set! frame-vec (+ trans-y i) blank))
  (for ([line (in-list visible-lines)]
        [i (in-naturals)])
    (draw-styled-line! ubuf line (+ trans-y pad-count i) cols)
    (vector-set! frame-vec (+ trans-y pad-count i) (styled-line->text line)))

  ;; 4. Draw status bar (inverse = fg=0, bg=7)
  (define status-line (render-status-bar ui-state cols))
  (draw-styled-line! ubuf status-line status-y cols)
  (vector-set! frame-vec status-y (styled-line->text status-line))

  ;; 5. Draw input line
  (define input-line (render-input-line input-st cols))
  (draw-styled-line! ubuf input-line input-y cols)
  (vector-set! frame-vec input-y (styled-line->text input-line))

  ;; 6. Return cursor position (0-indexed for ANSI escape)
  (define-values (_visible-text _scroll-offset cursor-display-col)
    (input-visible-window input-st cols))
  (values cursor-display-col input-y ui-state* (vector->list frame-vec)))
