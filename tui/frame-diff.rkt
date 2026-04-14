#lang racket/base

(require racket/list)

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
;;
;; Handles frames of different lengths incrementally:
;;   - Changed lines → 'write
;;   - New lines (curr longer) → 'write for appended lines
;;   - Removed lines (prev longer) → 'clear-from for surplus rows
(define (diff-frames prev curr)
  (cond
    ;; No previous frame (first render or resize) — full redraw
    [(not prev) (list (diff-cmd 'full 0 #f))]
    [(null? prev) (list (diff-cmd 'full 0 #f))]
    [else
     (define prev-len (length prev))
     (define curr-len (length curr))
     (cond
       ;; Same length — standard row-by-row diff
       [(= prev-len curr-len)
        (for/list ([p (in-list prev)]
                   [c (in-list curr)]
                   [i (in-naturals)]
                   #:when (not (string=? p c)))
          (diff-cmd 'write i c))]
       ;; Current is longer — diff common prefix, append new lines
       [(> curr-len prev-len)
        (define common-diffs
          (for/list ([p (in-list prev)]
                     [c (in-list curr)]
                     [i (in-naturals)]
                     #:when (not (string=? p c)))
            (diff-cmd 'write i c)))
        (define appended
          (for/list ([c (in-list (drop curr prev-len))]
                     [i (in-naturals prev-len)])
            (diff-cmd 'write i c)))
        (append common-diffs appended)]
       ;; Current is shorter — diff common prefix, clear surplus
       [else
        (define common-diffs
          (for/list ([p (in-list curr)]
                     [c (in-list curr)]
                     [i (in-naturals)]
                     #:when (not (string=? (list-ref prev i) c)))
            (diff-cmd 'write i c)))
        ;; Clear rows from curr-len to prev-len-1
        (define cleared
          (list (diff-cmd 'clear-from curr-len #f)))
        (append common-diffs cleared)])]))
