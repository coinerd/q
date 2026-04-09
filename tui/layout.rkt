#lang racket/base

;; tui/layout.rkt — Panel composition and screen geometry
;;
;; Pure functions that compute which panels go where on screen.
;; No terminal I/O.

(provide
 ;; Layout computation
 compute-layout

 ;; Layout struct
 (struct-out tui-layout))

;; Screen layout: where each panel starts and how tall it is
(struct tui-layout
  (cols rows                ; screen dimensions
   header-row               ; row number for header (1-based)
   transcript-start-row     ; row number for transcript start (1-based)
   transcript-height        ; number of rows available for transcript
   status-row               ; row number for status bar (1-based)
   input-row                ; row number for input line (1-based)
   )
  #:transparent)

;; Compute layout given screen dimensions
;; Layout:
;;   Row 1:        Header (1 line) — "q | session | model"
;;   Row 2..H-2:   Transcript (H-3 lines)
;;   Row H-1:      Status bar (1 line)
;;   Row H:        Input line (1 line)
(define (compute-layout cols rows)
  (define min-height 4)  ; Need at least header + 1 transcript + status + input
  (define actual-rows (max min-height rows))
  (define transcript-h (max 1 (- actual-rows 3)))
  (tui-layout cols actual-rows
              1                    ; header-row
              2                    ; transcript-start-row
              transcript-h         ; transcript-height
              (+ 2 transcript-h)   ; status-row
              (+ 3 transcript-h))) ; input-row
