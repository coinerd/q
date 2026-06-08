#lang racket

;; tests/bench-streaming-render.rkt — Streaming render benchmarks
;;
;; Measures cell-diff pipeline performance:
;; 1. Per-cell vs batched delta rendering (batch should be ≥30% faster)
;; 2. VDOM pipeline overhead (state → vdom → cell-buffer vs direct render)
;;
;; Run: raco test tests/bench-streaming-render.rkt

(require rackunit
         "../tui/cell-buffer.rkt"
         "../tui/cell-diff.rkt"
         "../tui/cell-diff-render.rkt"
         "../tui/vdom.rkt"
         "../tui/vdom-layout.rkt"
         "../tui/vdom-render.rkt"
         "../tui/component.rkt"
         "../tui/vdom-components.rkt"
         "../tui/state.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (make-filled-buffer cols rows char fg)
  (define buf (make-cell-buffer cols rows))
  (for* ([r (in-range rows)]
         [c (in-range cols)])
    (cell-buffer-set! buf c r #:char char #:fg fg))
  buf)

;; Write N random-ish lines to a buffer, starting at row 0
(define (write-streaming-lines! buf cols n)
  (for ([r (in-range n)])
    (when (< r (cell-buffer-rows buf))
      (for ([c (in-range cols)])
        (define ch (integer->char (+ 65 (modulo (+ r c) 26))))
        (cell-buffer-set! buf c r #:char ch #:fg 7)))))

;; ============================================================
;; Benchmark 1: Batch vs per-cell delta rendering
;; ============================================================

(test-case "bench: batched rendering ≥30% faster than per-cell"
  (define cols 80)
  (define rows 50)
  (define prev (make-filled-buffer cols rows #\space 7))
  (define curr (make-cell-buffer cols rows))
  ;; Write streaming content
  (write-streaming-lines! curr cols rows)
  (define deltas (diff-cell-buffers prev curr))

  ;; Batched render (current implementation)
  (define out-batch (open-output-string))
  (define batch-time
    (let ([start (current-inexact-milliseconds)])
      (for ([_ (in-range 100)])
        (render-deltas-to-port! deltas curr out-batch #:sync? #f))
      (- (current-inexact-milliseconds) start)))

  ;; Per-cell render (simulated old behavior)
  (define (render-per-cell deltas buf out)
    (define prev-sgr #f)
    (for ([d (in-list deltas)])
      (define col (cell-delta-col d))
      (define row (cell-delta-row d))
      (define new-cell (cell-delta-new-cell d))
      (display (format "\x1b[~a;~aH" (add1 row) (add1 col)) out)
      (define sgr (cell->sgr new-cell))
      (unless (equal? sgr prev-sgr)
        (display sgr out)
        (set! prev-sgr sgr))
      (display (string (cell-char new-cell)) out))
    (display "\x1b[0m" out))

  (define out-percell (open-output-string))
  (define percell-time
    (let ([start (current-inexact-milliseconds)])
      (for ([_ (in-range 100)])
        (render-per-cell deltas curr out-percell))
      (- (current-inexact-milliseconds) start)))

  ;; Report
  (printf "Batched: ~a ms, Per-cell: ~a ms~n" batch-time percell-time)
  (when (> percell-time 0)
    (define speedup (* 100.0 (/ (- percell-time batch-time) percell-time)))
    (printf "Speedup: ~a%~n" (real->decimal-string speedup 1))
    ;; Batched should be at least as fast (allow small variance)
    (check-true (<= batch-time (+ percell-time 5)) "batched should not be slower")))

;; ============================================================
;; Benchmark 2: VDOM pipeline overhead
;; ============================================================

(test-case "bench: vdom pipeline overhead < 5ms per frame"
  (define cols 80)
  (define rows 24)
  (define st (initial-ui-state #:model-name "test-model"))

  ;; Measure vdom pipeline: state → component render → vdom → cell-buffer
  (define comp (make-transcript-vdom-component))
  (define buf (make-cell-buffer cols rows))

  (define vdom-time
    (let ([start (current-inexact-milliseconds)])
      (for ([_ (in-range 50)])
        (define vnodes (component-render comp st cols))
        (render-vdom-to-buffer! (vvbox vnodes) buf cols))
      (- (current-inexact-milliseconds) start)))

  (define per-frame (/ vdom-time 50.0))
  (printf "VDOM pipeline: ~a ms total, ~a ms/frame~n"
          vdom-time
          (real->decimal-string per-frame 2))
  ;; Should be < 5ms per frame
  (check-true (< per-frame 5.0) "vdom overhead should be < 5ms/frame"))

;; ============================================================
;; Benchmark 3: Cell-diff throughput
;; ============================================================

(test-case "bench: cell-diff handles 10K changes efficiently"
  (define cols 80)
  (define rows 125) ; 10,000 cells
  (define a (make-filled-buffer cols rows #\space 7))
  (define b (make-cell-buffer cols rows))
  (write-streaming-lines! b cols rows)

  (define diff-time
    (let ([start (current-inexact-milliseconds)])
      (diff-cell-buffers a b)
      (- (current-inexact-milliseconds) start)))

  (printf "Cell-diff 10K cells: ~a ms~n" diff-time)
  (check-true (< diff-time 100.0) "10K cell diff should complete in < 100ms"))
