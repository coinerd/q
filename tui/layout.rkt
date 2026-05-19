#lang racket/base

;; tui/layout.rkt — Panel composition and screen geometry
;;
;; Pure functions that compute which panels go where on screen.
;; No terminal I/O.

;; Layout computation
(provide compute-layout
         compute-layout-with-widgets

         ;; Layout struct
         tui-layout
         tui-layout?
         tui-layout-cols
         tui-layout-rows
         tui-layout-header-row
         tui-layout-transcript-start-row
         tui-layout-transcript-height
         tui-layout-status-row
         tui-layout-input-row)

;; Screen layout: where each panel starts and how tall it is
(struct tui-layout
        (cols rows ; screen dimensions
              header-row ; row number for header (0-based)
              transcript-start-row ; row number for transcript start (0-based)
              transcript-height ; number of rows available for transcript
              status-row ; row number for status bar (0-based)
              input-row ; row number for input line (0-based)
              )
  #:transparent)

;; Compute layout given screen dimensions
;; Layout (0-based row numbers):
;;   Row 0..H-3:   Transcript (H-2 lines)
;;   Row H-2:      Status bar (1 line)
;;   Row H-1:      Input line (1 line)
(define (compute-layout cols rows)
  (compute-layout-with-widgets cols rows 0))

;; Compute layout accounting for widget container rows.
;; widget-rows-above: number of widget lines above input.
(define (compute-layout-with-widgets cols rows [widget-rows-above 0])
  (define min-height 3) ; Need at least 1 transcript + status + input
  (define actual-rows (max min-height rows))
  (define non-transcript (+ 2 widget-rows-above)) ; status + input (no header)
  (define transcript-h (max 1 (- actual-rows non-transcript)))
  (tui-layout cols
              actual-rows
              #f ; header-row: #f = no header (v0.28.14)
              0 ; transcript-start-row
              transcript-h ; transcript-height
              (+ transcript-h widget-rows-above) ; status-row
              (+ 1 transcript-h widget-rows-above) ; input-row
              ))
