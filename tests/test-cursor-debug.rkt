#lang racket
;; @speed fast
;; @suite tui

(require "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/tui-render-loop.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/layout.rkt"
         "../tui/component.rkt")

(define (draw-cursor! ubuf col row)
  (define cursor-cell (cell-buffer-ref ubuf col row))
  (cell-buffer-set! ubuf
                    col
                    row
                    #:char (cell-char cursor-cell)
                    #:fg (cell-bg cursor-cell)
                    #:bg (cell-fg cursor-cell)
                    #:bold (cell-bold? cursor-cell)
                    #:underline (cell-underline? cursor-cell)
                    #:italic (cell-italic? cursor-cell)
                    #:blink (cell-blink? cursor-cell)))

(define (render-frame ubuf state inp layout prev)
  (define-values (c r st* fl) (render-frame-vdom! ubuf state inp layout))
  (draw-cursor! ubuf c r)
  (define out (open-output-string))
  (render-smart! prev ubuf out #:sync? #f)
  (values c r out))

(define layout1 (compute-layout 10 40))

;; Frame 1: "hello" cursor at end
(define inp1 (input-insert-string (initial-input-state) "hello"))
(define ubuf1 (make-cell-buffer 40 10))
(define-values (c1 r1 out1) (render-frame ubuf1 (initial-ui-state) inp1 layout1 #f))
(printf "Frame1 cursor col=~a~n" c1)
(printf "ANSI: ")
(for ([ch (in-string (get-output-string out1))])
  (if (< (char->integer ch) 32)
      (printf "\\x~x" (char->integer ch))
      (printf "~a" ch)))
(printf "~n")

;; Frame 2: move cursor left 2 (cursor at 'llo' start, col 6)
(define inp2 (struct-copy input-state (input-cursor-left (input-cursor-left inp1)) [cursor 3]))
(define ubuf2 (make-cell-buffer 40 10))
(define-values (c2 r2 out2) (render-frame ubuf2 (initial-ui-state) inp2 layout1 ubuf1))
(printf "Frame2 cursor col=~a~n" c2)
(printf "ANSI: ")
(for ([ch (in-string (get-output-string out2))])
  (if (< (char->integer ch) 32)
      (printf "\\x~x" (char->integer ch))
      (printf "~a" ch)))
(printf "~n")
