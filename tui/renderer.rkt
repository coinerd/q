#lang racket/base

;; tui/renderer.rkt — Terminal UI rendering engine
;;
;; Layer: TUI (interface layer)
;; Purpose: Renders styled-lines to a native cell buffer for terminal display.
;; Manages message layout, status line, diff rendering, and theme application.
;; Consumes runtime events via state-events.rkt and renders to virtual buffer.

(require racket/contract
         racket/keyword
         racket/list
         racket/logging
         racket/string
         "render.rkt"
         "layout.rkt"
         "state.rkt"
         "input.rkt"
         "char-width.rkt"
         "cell-buffer.rkt"
         "component.rkt"
         (only-in "theme.rkt" current-tui-theme theme-ref theme-color->sgr))
;;
;; Renders styled-lines to a native cell buffer.
;; No terminal I/O directly — all output goes through the buffer.
;;
;; cell-buffer-putstring! signature:
;;   (ubuf-putstring! ub x y str
;;     #:fg [fg 7]  #:bg [bg 0]
;;     #:bold [bold #f]  #:underline [underline #f]
;;     #:italic [italic #f]  #:blink [blink #f])
;;
;; NOTE: cell buffer has NO #:inverse, #:dim, or #:reset keywords.
;;   inverse = swap fg/bg (fg=0, bg=7)
;;   dim     = fg=8 (dark gray)
;;   reset   = fg=7, bg=0, bold=#f (default values)

;; Main rendering function
(provide (contract-out
          [render-header-line (-> exact-nonnegative-integer? styled-line?)]
          [render-overlay-lines
           (-> (or/c #f overlay-state?) exact-nonnegative-integer? (listof styled-line?))]
          ;; Component-based rendering
          ;; Style mapping (for testing)
          [style->ubuf-kws (-> (listof symbol?) (values (listof keyword?) list?))]
          ;; Ubuf operations (settable for testing)
          ;; Width safety net
          [current-assert-width (parameter/c boolean?)]
          [assert-line-width! (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          ;; Low-level draw (for testing)
          [draw-styled-line!
           (-> any/c styled-line? exact-nonnegative-integer? exact-nonnegative-integer? void?)]))

;; ============================================================
;; Ubuf operations (parameterized for testability)
;; ============================================================

;; DI parameters removed — draw-styled-line! takes cell-buffer directly.

;; ============================================================
;; Width safety net
;; ============================================================

;; When #t, assert-line-width! raises on overflow instead of truncating.
;; Set to #t in tests for immediate visibility of width bugs.
(define current-assert-width (make-parameter #f))

;; Validate that a rendered line does not exceed max-width.
;; - In production: log warning, truncate to max-width
;; - In test mode (current-assert-width = #t): raise exn:fail
(define (assert-line-width! line-text max-width)
  (define vw (string-visible-width line-text))
  (when (> vw max-width)
    (define msg
      (format "width overflow: ~a cols in ~a-col terminal (line: ~a)"
              vw
              max-width
              (if (> (string-length line-text) 60)
                  (string-append (substring line-text 0 60) "...")
                  line-text)))
    (if (current-assert-width)
        (raise (exn:fail msg (current-continuation-marks)))
        (log-warning msg)))
  vw)

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
    [(bright-black) 8]
    [(bright-red) 9]
    [(bright-green) 10]
    [(bright-yellow) 11]
    [(bright-blue) 12]
    [(bright-magenta) 13]
    [(bright-cyan) 14]
    [(bright-white) 15]
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
      ;; Bright colors use 8-bit: 8=bright-black, 9=bright-red, etc.
      [(bright-black) (set! fg-color 8)]
      [(bright-red) (set! fg-color 9)]
      [(bright-green) (set! fg-color 10)]
      [(bright-yellow) (set! fg-color 11)]
      [(bright-blue) (set! fg-color 12)]
      [(bright-magenta) (set! fg-color 13)]
      [(bright-cyan) (set! fg-color 14)]
      [(bright-white) (set! fg-color 15)]
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
(define (draw-styled-line! buf line row width)
  ;; buf is a cell-buffer? — uses cell-buffer-putstring! directly
  (define ubuf-putstring! cell-buffer-putstring!)
  (define col 0)
  (define remaining-width width)
  ;; Draw each segment
  (for ([seg (in-list (styled-line-segments line))])
    #:break (<= remaining-width 0)
    (define text (string-replace (styled-segment-text seg) "\n" " "))
    (define styles (styled-segment-style seg))
    ;; Truncate if necessary (use visible width for CJK support)
    (define visible-text
      (if (> (string-visible-width text) remaining-width)
          ;; Truncate by character until visible width fits
          (truncate-to-visible-width text remaining-width)
          text))
    ;; Resolve styles to ubuf keywords
    (define-values (kws vals) (style->ubuf-kws styles))
    ;; Draw segment with styles
    (keyword-apply ubuf-putstring! kws vals (list buf col row visible-text))
    (set! col (+ col (string-visible-width visible-text)))
    (set! remaining-width (- remaining-width (string-visible-width visible-text))))
  ;; Pad rest with spaces
  (when (> remaining-width 0)
    (ubuf-putstring! buf col row (make-string remaining-width #\space))))
;; Width overflow protection: draw-styled-line! truncates per-segment
;; and pads to exactly 'width'. Upstream wrap-styled-line handles word wrap.
;; The assert-line-width! function is available for external callers to
;; validate line widths before they reach this function.)

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
;; Render header line as a styled-line.
;; Returns a single styled-line with inverse style.
(define (render-header-line cols)
  (define header-text (format " q ~a" (make-string (max 0 (- cols 3)) #\space)))
  (styled-line (list (styled-segment header-text '(inverse)))))

;; Render overlay lines from overlay-state, clipped to transcript height.
;; Returns (listof styled-line?) ready for drawing.
(define (render-overlay-lines overlay transcript-height)
  (if (not overlay)
      '()
      (let* ([ov-content (overlay-state-content overlay)]
             [ov-lines (if (> (length ov-content) transcript-height)
                           (take-right ov-content transcript-height)
                           ov-content)])
        ov-lines)))

;; ============================================================
;; Component-based rendering (#641)
;; ============================================================

;; A render-components struct holds per-zone q-component instances.
;; Each zone (transcript, status, input) has its own component with
;; independent caching by width. This provides:
;; - Per-zone cache isolation (#642)
;; - Foundation for overlay composition (Wave 5)
;; - Architecture alignment with component.rkt
(struct render-components (trans-comp status-comp))

;; Create component instances for transcript and status zones.
;; Input line is NOT wrapped as a component because it changes every keystroke
;; and receives separate input-state not available through ui-state.
(define (make-render-components)
  ;; Transcript component
  (render-components (make-q-component (lambda (st w)
                                         (define-values (lines _st) (render-transcript st 1000 w))
                                         lines)
                                       #:id 'transcript)
                     ;; Status bar component
                     (make-q-component (lambda (st w) (list (render-status-bar st w)))
                                       #:id 'status-bar)))

;; Render transcript zone through component. Returns (values lines ui-state).
(define (render-components-transcript comps state height width)
  (component-invalidate! (render-components-trans-comp comps))
  (define-values (lines state*) (render-transcript state height width))
  (values lines state*))

;; Render status bar through component. Returns (listof styled-line).
(define (render-components-status comps state width)
  (component-render (render-components-status-comp comps) state width))

;; Invalidate all component caches.
(define (render-components-invalidate! comps)
  (component-invalidate! (render-components-trans-comp comps))
  (component-invalidate! (render-components-status-comp comps)))
