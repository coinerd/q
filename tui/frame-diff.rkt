#lang racket/base

(provide (struct-out diff-cmd)
         diff-frames
         frame->string-lines)

;; A diff command: tells the terminal what to do
(struct diff-cmd
        (type ; 'write | 'clear-from | 'full
         row ; integer — 0-based row
         content ; string or #f — text to write (for 'write type)
         )
  #:transparent)

;; Convert a ubuf-style rendered frame into a list of strings (one per row)
;; This is a helper for testing. frame is a list of strings.
(define (frame->string-lines frame)
  frame)

;; Diff two frames and produce minimal update commands.
;; prev: (listof string) — previous frame lines (may be empty for first frame)
;; curr: (listof string) — current frame lines
;; Returns: (listof diff-cmd)
(define (diff-frames prev curr)
  (cond
    ;; No previous frame (first render or resize) — full redraw
    [(not prev) (list (diff-cmd 'full 0 #f))]
    [(null? prev) (list (diff-cmd 'full 0 #f))]
    [(not (= (length prev) (length curr))) (list (diff-cmd 'full 0 #f))]
    [else
     (define diffs
       (for/list ([p (in-list prev)]
                  [c (in-list curr)]
                  [i (in-naturals)]
                  #:when (not (string=? p c)))
         (diff-cmd 'write i c)))
     diffs]))
