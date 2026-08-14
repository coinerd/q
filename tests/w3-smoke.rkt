#lang racket/base
(require rackunit
         "../ui-core/composer-model.rkt"
         "../ui-core/composer-layout.rkt")

;; ---- model ----
(define st0 (make-composer-state))
(check-equal? (composer-state-buffer st0) "")
(check-equal? (composer-state-cursor st0) 0)
(check-true (composer-at-beginning? st0))
(check-true (composer-at-end? st0))
(check-true (composer-empty? st0))

;; insert / cursor
(define st1 (composer-insert-char st0 #\a))
(check-equal? (composer-state-buffer st1) "a")
(check-equal? (composer-state-cursor st1) 1)
(check-true (composer-at-end? st1))

(define st2 (composer-insert-char st1 #\b))
(check-equal? (composer-state-buffer st2) "ab")
(check-equal? (composer-state-cursor st2) 2)

;; backspace deletes grapheme
(check-equal? (composer-state-buffer (composer-backspace st2)) "a")
(check-equal? (composer-state-cursor (composer-backspace st2)) 1)
;; backspace at 0 = no-op
(check-equal? (composer-state-buffer (composer-backspace st0)) "")

;; delete forward
;; forward delete at end of buffer = no-op
(check-equal? (composer-state-buffer (composer-delete st1)) "a")

;; move left/right
(check-equal? (composer-state-cursor (composer-cursor-left st2)) 1)
(check-equal? (composer-state-cursor (composer-cursor-left (composer-cursor-left st2))) 0)
(check-equal? (composer-state-cursor (composer-cursor-right (composer-cursor-left st2))) 2)

;; home/end
(check-equal? (composer-state-cursor (composer-home st2)) 0)
(check-equal? (composer-state-cursor (composer-end (composer-home st2))) 2)

;; newline
(define st3 (composer-insert-newline (composer-insert-string st0 "ab" 'paste)))
(check-equal? (composer-state-buffer st3) "ab\n")
(check-equal? (composer-state-cursor st3) 3)
(define st4 (composer-insert-char st3 #\c))
(check-equal? (composer-state-buffer st4) "ab\nc")

;; selection
(define sel (composer-set-selection st2 0))
(check-true (composer-has-selection? sel))
(check-equal? (composer-selection-text sel) "ab")
(check-equal? (composer-state-buffer (composer-backspace sel)) "")
(check-false (composer-has-selection? (composer-backspace sel)))

;; undo/redo
(define un (composer-undo (composer-insert-char st0 #\x)))
(check-equal? (composer-state-buffer un) "")
(check-equal? (composer-state-buffer (composer-redo un)) "x")

(define killed (composer-kill-to-end (struct-copy composer-state st2 [cursor 0])))
(check-equal? (composer-state-buffer killed) "")
(check-equal? (composer-state-kill-ring killed) '("ab"))
(check-equal? (composer-state-buffer (composer-yank killed)) "ab")

;; undo grouping: same tag collapses
(define g1 (composer-insert-string st0 "a" 'type))
(define g2 (composer-insert-string g1 "b" 'type))
(check-equal? (length (composer-state-undo-stack g2)) 1)
(define g3 (composer-insert-string g2 "c" 'paste))
(check-equal? (length (composer-state-undo-stack g3)) 2)

;; replace buffer clamps cursor
(check-equal? (composer-state-buffer (composer-replace-buffer st2 "xyz")) "xyz")
(check-equal? (composer-state-cursor (composer-replace-buffer st2 "x")) 1)

;; graphemes
(check-true (>= (composer-grapheme-count "hello") 5))
(check-equal? (composer-prev-grapheme-start "ab" 2) 1)
(check-equal? (composer-next-grapheme-start "ab" 0) 1)

;; ---- layout (unit width fn: 1 char = 1 cell) ----
(define W composer-unit-display-width)

;; simple single line
(define l1 (compute-composer-layout "hello" 2 80 W))
(check-equal? (length (composer-layout-lines l1)) 1)
(check-equal? (composer-layout-cursor-row l1) 0)
(check-equal? (composer-layout-cursor-col l1) 2)

;; soft wrap at width 3
(define l2 (compute-composer-layout "abcdef" 4 3 W))
(check-equal? (map composer-visual-line-text (composer-layout-lines l2)) '("abc" "def"))
(check-equal? (map composer-visual-line-start (composer-layout-lines l2)) '(0 3))
(check-equal? (composer-layout-cursor-row l2) 1)
(check-equal? (composer-layout-cursor-col l2) 1)

;; hard newline
(define l3 (compute-composer-layout "ab\ncd" 3 80 W))
(check-equal? (map composer-visual-line-text (composer-layout-lines l3)) '("ab" "cd"))
(check-true (composer-visual-line-hard? (car (composer-layout-lines l3))))
(check-equal? (composer-layout-cursor-row l3) 1)
(check-equal? (composer-layout-cursor-col l3) 0)

;; empty buffer → one empty line, cursor 0,0
(define l4 (compute-composer-layout "" 0 80 W))
(check-equal? (length (composer-layout-lines l4)) 1)
(check-equal? (composer-layout-cursor-row l4) 0)
(check-equal? (composer-layout-cursor-col l4) 0)

;; trailing newline → extra empty line; cursor 1,0
(define l5 (compute-composer-layout "ab\n" 3 80 W))
(check-equal? (map composer-visual-line-text (composer-layout-lines l5)) '("ab" ""))
(check-equal? (composer-layout-cursor-row l5) 1)
(check-equal? (composer-layout-cursor-col l5) 0)

;; cursor beyond end clamps to last line
(define l6 (compute-composer-layout "ab" 99 80 W))
(check-equal? (composer-layout-cursor-row l6) 0)
(check-equal? (composer-layout-cursor-col l6) 2)

;; mixed wrap + newline
(define l7 (compute-composer-layout "abcde\nfghij" 8 4 W))
(check-equal? (map composer-visual-line-text (composer-layout-lines l7)) '("abcd" "e" "fghi" "j"))
(check-equal? (composer-layout-cursor-row l7) 2)
(check-equal? (composer-layout-cursor-col l7) 2)

;; wide char wrap: width fn says every char = 2 cells; width 6 → 3 chars/line
(define (wide2 s)
  (* 2 (string-length s)))
(define l8 (compute-composer-layout "abcd" 4 6 wide2))
(check-equal? (map composer-visual-line-text (composer-layout-lines l8)) '("abc" "d"))
(check-equal? (composer-layout-cursor-row l8) 1)
(check-equal? (composer-layout-cursor-col l8) 2)

;; vertical movement
(define vm-st (make-composer-state #:buffer "abcde\nfghij" #:cursor 8))
(check-equal? (composer-state-cursor (composer-move-up vm-st 4 W)) 5)
(check-equal? (composer-state-cursor (composer-move-down vm-st 4 W)) 11)

;; viewport
(check-equal? (composer-viewport-top 0 3 0) 0)
(check-equal? (composer-viewport-top 3 3 0) 1)
(check-equal? (composer-viewport-top 5 3 1) 3)
(check-equal? (composer-needed-rows l7) 4)

(define-values (above? below?) (composer-viewport-indicators 1 2 5))
(check-true above?)
(check-true below?)
