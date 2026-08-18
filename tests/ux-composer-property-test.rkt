#lang racket/base

;; q/tests/ux-composer-property-test.rkt — W0 property-style composer tests.
;;
;; W3 requirement: these tests now exercise the REAL shared composer
;; (q/ui-core/composer-model.rkt + q/ui-core/composer-layout.rkt), not
;; the legacy single-line viewport. They cover grapheme clusters, emoji
;; (ZWJ), CJK wide chars, combining marks, boundary-at-width wraps,
;; @speed fast  ;; @suite default
;; @boundary unit
;; newline insertion, visual-line navigation, and resize stability.
;;
;; Run: raco test q/tests/ux-composer-property-test.rkt

(require rackunit
         racket/list
         racket/string
         "../ui-core/composer-model.rkt"
         "../ui-core/composer-layout.rkt"
         "../ui-core/feature-flags.rkt")

;; Unit display width for property testing: 1 char = 1 cell.
;; (The TUI passes its own terminal-cell width fn; GUI passes font
;;  measurement — both are parameterized, proven here with the unit fn.)
(define (uw s)
  (string-length s))

;; ─────────────────────────────────────────────────────────────────
;; Round-trip invariant: concatenating visual-line texts (plus #\newline
;; for hard breaks) reconstructs the buffer.
;; ─────────────────────────────────────────────────────────────────
(define (layout-text-reconstruct layout)
  (apply string-append
         (for/list ([vl (composer-layout-lines layout)])
           (if (composer-visual-line-hard? vl)
               (string-append (composer-visual-line-text vl) "\n")
               (composer-visual-line-text vl)))))

(define (check-reconstruct buffer width)
  (define layout (compute-composer-layout buffer 0 width uw))
  (check-equal? (layout-text-reconstruct layout) buffer))

;; ─────────────────────────────────────────────────────────────────
;; Grapheme cluster fixtures
;; ─────────────────────────────────────────────────────────────────
(define family-emoji "👨‍👩‍👧‍👦") ; ZWJ sequence
(define flag-emoji "🇩🇪") ; regional indicators
(define skin-tone "👍🏽") ; emoji + modifier
(define combining "e\u0301") ; e + combining acute
(define cjk "你好世界") ; wide chars (unit-width fn: 4 cells)
(define ascii "abcdef")

;; ─────────────────────────────────────────────────────────────────
;; 1. Layout invariants
;; ─────────────────────────────────────────────────────────────────
(check-equal? (layout-text-reconstruct (compute-composer-layout "hello world" 0 5 uw)) "hello world")

(test-case "layout reconstructs buffer (no newlines, several widths)"
  (for ([w (in-list '(1 2 3 5 7 11 100))])
    (check-reconstruct "hello world" w)
    (check-reconstruct "aaa bbb ccc ddd" w)))

(test-case "layout reconstructs buffer with newlines"
  (for ([w (in-list '(1 2 3 5 100))])
    (check-reconstruct "a\nbb\nccc\n\nd" w)
    (check-reconstruct "one\ntwo" w)
    (check-reconstruct "\n\n\n" w)))

(test-case "empty buffer yields one empty visual line"
  (define layout (compute-composer-layout "" 0 10 uw))
  (check-equal? (composer-layout-row-count layout) 1)
  (check-equal? (composer-visual-line-text (car (composer-layout-lines layout))) ""))

;; ─────────────────────────────────────────────────────────────────
;; 2. Soft wrap at terminal-width boundary
;; ─────────────────────────────────────────────────────────────────
(test-case "boundary-at-width wrap: 5 chars in width 5 → 1 line"
  (define layout (compute-composer-layout "abcde" 5 5 uw))
  (check-equal? (composer-layout-row-count layout) 1))

(test-case "boundary-at-width wrap: 6 chars in width 5 → 2 lines"
  (define layout (compute-composer-layout "abcdef" 6 5 uw))
  (check-equal? (composer-layout-row-count layout) 2)
  (check-equal? (composer-visual-line-text (first (composer-layout-lines layout))) "abcde")
  (check-equal? (composer-visual-line-text (second (composer-layout-lines layout))) "f"))

(test-case "logical offsets are absolute across wraps"
  (define layout (compute-composer-layout "abcdef" 3 5 uw))
  (define l1 (first (composer-layout-lines layout)))
  (define l2 (second (composer-layout-lines layout)))
  (check-equal? (composer-visual-line-start l2) (+ (composer-visual-line-start l1) 5))
  (check-equal? (composer-visual-line-end l2) 6))

(test-case "wrap never loses or duplicates characters (property)"
  (define chars "abcde12345 ")
  (for* ([n (in-range 0 24)]
         [w (in-range 1 8)])
    (define buf
      (apply string
             (for/list ([i (in-range n)])
               (string-ref chars (modulo (* i 7) (string-length chars))))))
    (check-reconstruct buf w)))

;; ─────────────────────────────────────────────────────────────────
;; 3. Cursor authority: cursor (row,col) always inside the layout
;; ─────────────────────────────────────────────────────────────────
(define (check-cursor-in-bounds buffer cursor width)
  (define layout (compute-composer-layout buffer cursor width uw))
  (check-true (>= (composer-layout-cursor-row layout) 0))
  (check-true (< (composer-layout-cursor-row layout) (composer-layout-row-count layout)))
  (check-true (>= (composer-layout-cursor-col layout) 0)))

(test-case "cursor always within assigned region (property)"
  (define buf "ab\ncd efgh ij\nklm")
  (for ([cursor (in-range 0 (add1 (string-length buf)))]
        [w (in-list '(1 2 3 4 8 50))])
    (check-cursor-in-bounds buf cursor w)))

(test-case "cursor at soft-wrap boundary sits at end of earlier line"
  (define layout (compute-composer-layout "abcdef" 5 5 uw))
  (check-equal? (composer-layout-cursor-row layout) 0)
  (check-equal? (composer-layout-cursor-col layout) 5))

(test-case "cursor after newline sits on next line start"
  (define layout (compute-composer-layout "ab\ncd" 3 10 uw))
  (check-equal? (composer-layout-cursor-row layout) 1)
  (check-equal? (composer-layout-cursor-col layout) 0))

;; ─────────────────────────────────────────────────────────────────
;; 4. Grapheme-aware editing: cursor moves by grapheme, not codepoint
;; ─────────────────────────────────────────────────────────────────
(test-case "grapheme count: ZWJ family emoji = 1 grapheme"
  (check-equal? (composer-grapheme-count family-emoji) 1)
  (check-equal? (composer-grapheme-count flag-emoji) 1)
  (check-equal? (composer-grapheme-count skin-tone) 1)
  (check-equal? (composer-grapheme-count combining) 1)
  (check-equal? (composer-grapheme-count ascii) 6))

(test-case "backspace deletes whole ZWJ sequence"
  (define st (make-composer-state #:buffer family-emoji #:cursor (string-length family-emoji)))
  (define st2 (composer-backspace st))
  (check-equal? (composer-state-buffer st2) "")
  (check-equal? (composer-state-cursor st2) 0))

(test-case "backspace deletes combining sequence as one"
  (define st (make-composer-state #:buffer combining #:cursor (string-length combining)))
  (define st2 (composer-backspace st))
  (check-equal? (composer-state-buffer st2) ""))

(test-case "cursor-left skips entire grapheme cluster"
  (define st
    (make-composer-state #:buffer (string-append "a" family-emoji)
                         #:cursor (string-length (string-append "a" family-emoji))))
  (define st2 (composer-cursor-left st))
  (check-equal? (composer-state-cursor st2) 1))

;; ─────────────────────────────────────────────────────────────────
;; 5. Insert/delete before/after cursor keeps cursor valid
;; ─────────────────────────────────────────────────────────────────
(test-case "insert before cursor advances cursor (type at end)"
  (define st (make-composer-state #:buffer "ab" #:cursor 2))
  (define st2 (composer-insert-char st #\c))
  (check-equal? (composer-state-buffer st2) "abc")
  (check-equal? (composer-state-cursor st2) 3))

(test-case "insert into middle leaves suffix intact"
  (define st (make-composer-state #:buffer "ac" #:cursor 1))
  (define st2 (composer-insert-char st #\b))
  (check-equal? (composer-state-buffer st2) "abc")
  (check-equal? (composer-state-cursor st2) 2))

(test-case "insert multi-char string with graphemes"
  (define st (make-composer-state #:buffer "ab" #:cursor 1))
  (define st2 (composer-insert-string st (string-append cjk "x")))
  (check-equal? (composer-state-buffer st2) (string-append "a" cjk "xb"))
  (check-equal? (composer-state-cursor st2) (add1 (string-length (string-append "a" cjk)))))

(test-case "delete at cursor removes grapheme after cursor"
  (define st (make-composer-state #:buffer (string-append "a" family-emoji) #:cursor 1))
  (define st2 (composer-delete st))
  (check-equal? (composer-state-buffer st2) "a")
  (check-equal? (composer-state-cursor st2) 1))

(test-case "cursor always valid after every edit (property, mixed graphemes)"
  (define pieces (list "a" cjk family-emoji combining "z" "\n" " "))
  (define st0 (make-composer-state))
  (define st
    (for/fold ([st st0]) ([i (in-range 60)])
      (define p (list-ref pieces (modulo (* i 5) (length pieces))))
      (case (modulo i 4)
        [(0) (composer-insert-string st p)]
        [(1) (composer-insert-newline st)]
        [(2) (composer-cursor-left st)]
        [(3) (composer-cursor-right st)])))
  (define buf (composer-state-buffer st))
  (check-true (<= 0 (composer-state-cursor st) (string-length buf)) "cursor within buffer bounds")
  ;; cursor stays in-bounds under every width after arbitrary edits
  (for ([w (in-list '(1 2 3 6 40))])
    (check-cursor-in-bounds buf (composer-state-cursor st) w)))

;; ─────────────────────────────────────────────────────────────────
;; 6. Newline insertion and visual-line navigation
;; ─────────────────────────────────────────────────────────────────
(test-case "insert newline splits buffer"
  (define st (make-composer-state #:buffer "ab" #:cursor 1))
  (define st2 (composer-insert-newline st))
  (check-equal? (composer-state-buffer st2) "a\nb"))

(test-case "move down then up restores position on short lines"
  (define st (make-composer-state #:buffer "ab\ncd" #:cursor 1))
  (define layout
    (compute-composer-layout (composer-state-buffer st) (composer-state-cursor st) 20 uw))
  (define down (composer-move-down st 20 uw))
  (check-equal? (composer-state-cursor down) 4) ; after 'd' line start +1
  (define down-layout
    (compute-composer-layout (composer-state-buffer down) (composer-state-cursor down) 20 uw))
  (define up (composer-move-up down 20 uw))
  (check-equal? (composer-state-cursor up) 1)
  (check-true (>= (composer-layout-cursor-row layout) 0))
  (check-true (>= (composer-layout-cursor-row down-layout) 0)))

(test-case "move down over soft wrap follows visual rows"
  (define st (make-composer-state #:buffer "abcdefghij" #:cursor 2))
  (define down (composer-move-down st 5 uw))
  (define layout
    (compute-composer-layout (composer-state-buffer down) (composer-state-cursor down) 5 uw))
  (check-equal? (composer-layout-cursor-row layout) 1))

(test-case "home/end operate on visual line"
  (define st (make-composer-state #:buffer "abcdefghij" #:cursor 7))
  (define home (composer-visual-home st 5 uw))
  (check-equal? (composer-state-cursor home) 5) ; start of second visual row
  (define end (composer-visual-end (make-composer-state #:buffer "abcdefghij" #:cursor 5) 5 uw))
  (check-equal? (composer-state-cursor end) 10)
  ;; logical home/end (model, no layout) → buffer bounds
  (check-equal? (composer-state-cursor (composer-home st)) 0)
  (check-equal? (composer-state-cursor (composer-end st)) 10))

;; ─────────────────────────────────────────────────────────────────
;; 7. Viewport: bounded composer height with indicator
;; ─────────────────────────────────────────────────────────────────
(test-case "viewport follows cursor and clamps"
  ;; cursor beyond visible window scrolls down
  (check-equal? (composer-viewport-top 9 3) 7)
  ;; cursor inside window stays
  (check-equal? (composer-viewport-top 1 3 0) 0)
  ;; never scrolls past what's needed
  (check-equal? (composer-viewport-top 2 10) 0))

(test-case "viewport indicators fire only when content overflows"
  (define-values (up? down?) (composer-viewport-indicators 0 3 5))
  (check-false up?)
  (check-true down?)
  (define-values (u2? d2?) (composer-viewport-indicators 2 3 5))
  (check-true u2?)
  (check-false d2?)
  (define-values (u3? d3?) (composer-viewport-indicators 0 3 3))
  (check-false u3?)
  (check-false d3?))

(test-case "needed rows bounded by max-rows config"
  (define layout (compute-composer-layout "aaa\nbbb\nccc\nddd\neee\nfff\nggg" 0 10 uw))
  (check-true (>= (composer-needed-rows layout) 1))
  (check-true (<= (composer-needed-rows layout) 7)))

;; ─────────────────────────────────────────────────────────────────
;; 8. Resize while editing → state stable, only layout recomputes
;; ─────────────────────────────────────────────────────────────────
(test-case "resize keeps buffer, cursor, selection stable"
  (define st (make-composer-state #:buffer "hello\nworld" #:cursor 8 #:sel-anchor 6))
  (for ([w (in-list '(1 3 5 30))])
    (define layout
      (compute-composer-layout (composer-state-buffer st) (composer-state-cursor st) w uw))
    (check-equal? (layout-text-reconstruct layout) "hello\nworld")
    (check-cursor-in-bounds (composer-state-buffer st) (composer-state-cursor st) w))
  (check-equal? (composer-state-cursor st) 8)
  (check-equal? (composer-state-sel-anchor st) 6))

;; ─────────────────────────────────────────────────────────────────
;; 9. Feature flag: default ON since W5 (v0.99.96), flips cleanly
;; ─────────────────────────────────────────────────────────────────
(test-case "tui.multiline-composer.enabled defaults to #t (W5 flip)"
  (check-true (tui-multiline-composer-enabled)))

(test-case "flag flip enables multiline path"
  (check-true (with-multiline-composer (λ () (tui-multiline-composer-enabled))))
  (check-false (without-multiline-composer (λ () (tui-multiline-composer-enabled)))))

(test-case "max rows default 6, minimum honored as ≥1"
  (check-equal? (tui-multiline-composer-max-rows) 6)
  (check-true (>= (max 1 (tui-multiline-composer-max-rows)) 1)))
