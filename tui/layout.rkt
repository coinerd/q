#lang racket/base

;; tui/layout.rkt — Panel composition and screen geometry
;;
;; Pure functions that compute which panels go where on screen.
;; No terminal I/O.

;; Layout computation
(provide compute-layout

         ;; Layout struct
         (struct-out tui-layout))

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
;;   Row 0:        Header (1 line) — "q | session | model"
;;   Row 1..H-3:   Transcript (H-3 lines)
;;   Row H-2:      Status bar (1 line)
;;   Row H-1:      Input line (1 line)
(define (compute-layout cols rows)
  (define min-height 4) ; Need at least header + 1 transcript + status + input
  (define actual-rows (max min-height rows))
  (define transcript-h (max 1 (- actual-rows 3)))
  (tui-layout cols
              actual-rows
              0 ; header-row
              1 ; transcript-start-row
              transcript-h ; transcript-height
              (+ 1 transcript-h) ; status-row
              (+ 2 transcript-h))) ; input-row
