#lang racket/base

(require rackunit
         "../../tui/frame-diff.rkt")

(define empty-frame '())
(define frame-3 '("line0" "line1" "line2"))
(define frame-3-mod '("line0" "CHANGED" "line2"))
(define frame-3-all-mod '("a" "b" "c"))
(define frame-4 '("line0" "line1" "line2" "line3"))

;; Empty prev → full redraw
(check-equal? (diff-frames empty-frame frame-3)
              (list (diff-cmd 'full 0 #f))
              "empty prev yields full redraw")

;; Same frame → empty diffs
(check-equal? (diff-frames frame-3 frame-3) '() "identical frames yield no diffs")

;; One line changed → one write cmd
(check-equal? (diff-frames frame-3 frame-3-mod)
              (list (diff-cmd 'write 1 "CHANGED"))
              "single changed line yields one write cmd")

;; Multiple lines changed → multiple write cmds in order
(check-equal? (diff-frames frame-3 frame-3-all-mod)
              (list (diff-cmd 'write 0 "a") (diff-cmd 'write 1 "b") (diff-cmd 'write 2 "c"))
              "all lines changed yields write cmds for all rows")

;; Different length → full redraw
(check-equal? (diff-frames frame-3 frame-4)
              (list (diff-cmd 'full 0 #f))
              "different-length frames yield full redraw")

;; frame->string-lines is identity
(check-equal? (frame->string-lines frame-3) frame-3 "frame->string-lines returns the frame unchanged")

;; #f prev (first render) → full redraw (Issue #390)
(check-equal? (diff-frames #f frame-3)
              (list (diff-cmd 'full 0 #f))
              "#f prev yields full redraw (first render)")
(check-equal? (diff-frames #f empty-frame)
              (list (diff-cmd 'full 0 #f))
              "#f prev with empty current yields full redraw")
