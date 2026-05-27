#lang racket

;; tests/test-cell-diff-render.rkt — Tests for tui/cell-diff-render.rkt

(require rackunit
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt")

;; ============================================================
;; SGR generation
;; ============================================================

(test-case "cell->sgr produces reset+fg+bg for default cell"
  (define cell (vector #\space 7 0 #f #f #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr "38;5;7"))
  (check-true (string-contains? sgr "48;5;16")) ; bg=0 mapped to 16
  ;; Bold is \";1\" followed by \";\" or \"m\", not part of \";16\"
  (check-false (regexp-match? #rx";1[;m]" sgr)))

(test-case "cell->sgr includes bold"
  (define cell (vector #\A 7 0 #t #f #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr ";1")))

(test-case "cell->sgr includes underline"
  (define cell (vector #\A 7 0 #f #t #f #f))
  (define sgr (cell->sgr cell))
  (check-true (string-contains? sgr ";4")))

;; ============================================================
;; Render deltas to port (no sync — avoid terminal side effects)
;; ============================================================

(test-case "render-deltas-to-port! writes ANSI for changed cells"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 3 2 #:char #\X #:fg 31)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  ;; Render without terminal sync (avoid detect-sync-mode)
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain cursor positioning
  (check-true (string-contains? result "\x1b[3;4H"))
  ;; Should contain the character
  (check-true (string-contains? result "X")))

(test-case "render-deltas-to-port! multiple changes"
  (define a (make-cell-buffer 10 5))
  (define b (make-cell-buffer 10 5))
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (define deltas (diff-cell-buffers a b))
  (define out (open-output-string))
  (render-deltas-to-port! deltas b out #:sync? #f)
  (define result (get-output-string out))
  (check-true (string-contains? result "A"))
  (check-true (string-contains? result "B")))

;; ============================================================
;; Full buffer render
;; ============================================================

(test-case "render-buffer-to-port! writes all rows"
  (define buf (make-cell-buffer 5 3))
  (cell-buffer-putstring! buf 0 0 "Hello")
  (define out (open-output-string))
  (render-buffer-to-port! buf out #:sync? #f)
  (define result (get-output-string out))
  ;; Should contain the text
  (check-true (string-contains? result "Hello"))
  ;; Should start with home cursor
  (check-true (string-prefix? result "\x1b[H")))

;; ============================================================
;; Smart render threshold
;; ============================================================

(test-case "render-smart! uses full render when > 50% changed"
  (define a (make-cell-buffer 4 2)) ; 8 cells total
  (define b (make-cell-buffer 4 2))
  ;; Change 5 cells (62.5%) — should trigger full render
  (cell-buffer-set! b 0 0 #:char #\A)
  (cell-buffer-set! b 1 0 #:char #\B)
  (cell-buffer-set! b 2 0 #:char #\C)
  (cell-buffer-set! b 3 0 #:char #\D)
  (cell-buffer-set! b 0 1 #:char #\E)
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Full render starts with home cursor
  (check-true (string-prefix? result "\x1b[H")))

(test-case "render-smart! uses delta render when < 50% changed"
  (define a (make-cell-buffer 10 5)) ; 50 cells
  (define b (make-cell-buffer 10 5))
  ;; Change only 1 cell (2%) — should use delta
  (cell-buffer-set! b 5 3 #:char #\Z)
  (define out (open-output-string))
  (render-smart! a b out #:sync? #f)
  (define result (get-output-string out))
  ;; Delta render uses cursor positioning, not home
  (check-true (string-contains? result "\x1b[4;6H")))

(test-case "render-smart! full render for #f prev"
  (define buf (make-cell-buffer 5 3))
  (define out (open-output-string))
  (render-smart! #f buf out #:sync? #f)
  (define result (get-output-string out))
  (check-true (string-prefix? result "\x1b[H")))
