#lang racket/base

;; q/tui/render/diff-render.rkt — selection highlighting and frame diff integration
;;
;; Pure functions for selection highlighting and diff rendering.

(require racket/list
         "../state.rkt"
         "../input.rkt"
         "message-layout.rkt")

(provide style-invert
         apply-selection-highlight
         highlight-line-range
         render-transcript
         render-branch-list
         render-leaf-nodes
         render-children-list)

;; Invert a style (for selection highlighting).
(define (style-invert style)
  (if (member 'inverse style)
      (remove 'inverse style)
      (cons 'inverse style)))

;; Apply selection highlight to styled lines.
(define (apply-selection-highlight lines sel-anchor sel-end trans-start [pad-count 0])
  (if (not (and sel-anchor sel-end))
      lines
      (let-values ([(start-col start-row end-col end-row)
                    (normalize-selection-range sel-anchor sel-end)])
        (for/list ([line (in-list lines)]
                   [row (in-naturals (+ trans-start pad-count))])
          (cond
            [(and (>= row start-row) (< row end-row))
             ;; Full line highlight
             (highlight-line-range line 0 #f)]
            [(and (= row start-row) (= row end-row))
             ;; Partial highlight on same row
             (highlight-line-range line start-col end-col)]
            [(= row start-row)
             ;; Start of multi-row highlight
             (highlight-line-range line start-col #f)]
            [(= row end-row)
             ;; End of multi-row highlight
             (highlight-line-range line 0 end-col)]
            [else line])))))

;; Highlight a column range within a styled line.
(define (highlight-line-range sl col-start col-end)
  (define segs (styled-line-segments sl))
  (define new-segs
    (for/fold ([acc '()])
              ([seg (in-list segs)])
      (define text (styled-segment-text seg))
      (define style (styled-segment-style seg))
      (define inverted (cons (styled-segment text (style-invert style)) '()))
      (append acc inverted)))
  (styled-line new-segs))

;; Render the transcript.
(define (render-transcript state transcript-height [width 200])
  (define entries (ui-state-transcript state))
  (define scroll (ui-state-scroll-offset state))
  (define streaming-text (ui-state-streaming-text state))
  (define all-entries
    (if streaming-text
        (append entries (list (transcript-entry 'assistant streaming-text (current-inexact-milliseconds) (hash))))
        entries))
  (define styled-lines
    (apply append (map (lambda (e) (format-entry e width)) all-entries)))
  ;; Apply scroll offset
  (define start (min scroll (max 0 (- (length styled-lines) transcript-height))))
  (define visible (take (drop styled-lines start) (min transcript-height (max 0 (- (length styled-lines) start)))))
  visible)

;; Render a branch list.
(define (render-branch-list branches [width 200])
  (for/list ([b (in-list branches)])
    (styled-line (list (styled-segment (format "  ~a" b) '())))))

;; Render leaf nodes.
(define (render-leaf-nodes branches [width 200])
  (for/list ([b (in-list branches)])
    (styled-line (list (styled-segment (format "  ○ ~a" b) '())))))

;; Render children list.
(define (render-children-list parent-id children [width 200])
  (for/list ([c (in-list children)])
    (styled-line (list (styled-segment (format "  ├─ ~a" c) '())))))
