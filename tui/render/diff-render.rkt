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
      (let-values ([(start-col start-row end-col end-row) (normalize-selection-range sel-anchor
                                                                                     sel-end)])
        (for/list ([line (in-list lines)]
                   [row (in-naturals (+ trans-start pad-count))])
          (cond
            ;; Full line highlight
            [(and (>= row start-row) (< row end-row)) (highlight-line-range line 0 #f)]
            ;; Partial highlight on same row
            [(and (= row start-row) (= row end-row)) (highlight-line-range line start-col end-col)]
            ;; Start of multi-row highlight
            [(= row start-row) (highlight-line-range line start-col #f)]
            ;; End of multi-row highlight
            [(= row end-row) (highlight-line-range line 0 end-col)]
            [else line])))))

;; Highlight a column range within a styled line.
(define (highlight-line-range sl col-start col-end)
  (define segs (styled-line-segments sl))
  (define-values (new-segs _final-col)
    (for/fold ([acc '()]
               [col 0])
              ([seg (in-list segs)])
      (define text (styled-segment-text seg))
      (define style (styled-segment-style seg))
      (define seg-len (string-length text))
      (define seg-start col)
      (define seg-end (+ col seg-len))
      (define inv-style (style-invert style))
      (cond
        ;; Segment fully outside range (after range end OR before range start)
        [(or (and col-end (>= seg-start col-end)) (and col-start (>= col-start seg-end)))
         (values (append acc (list seg)) (+ col seg-len))]
        ;; Segment fully inside range
        [(and (>= seg-start (or col-start 0)) (or (not col-end) (<= seg-end col-end)))
         (values (append acc (list (styled-segment text inv-style))) (+ col seg-len))]
        ;; Partial overlap
        [else
         (define s (max 0 (- (or col-start 0) seg-start)))
         (define e
           (min seg-len
                (if col-end
                    (- col-end seg-start)
                    seg-len)))
         (define before
           (if (> s 0)
               (substring text 0 s)
               ""))
         (define mid (substring text s (max s e)))
         (define after
           (if (< e seg-len)
               (substring text e)
               ""))
         (define parts
           (append (if (string=? before "")
                       '()
                       (list (styled-segment before style)))
                   (list (styled-segment mid inv-style))
                   (if (string=? after "")
                       '()
                       (list (styled-segment after style)))))
         (values (append acc parts) (+ col seg-len))])))
  (styled-line new-segs))

;; Render the transcript.
(define (render-transcript state transcript-height [width 200])
  (define entries (ui-state-transcript state))
  (define scroll (ui-state-scroll-offset state))
  (define streaming-text (ui-state-streaming-text state))
  (define streaming-thinking (ui-state-streaming-thinking state))
  (define chronological-entries (reverse entries))
  ;; v0.28.19: Show thinking entry during reasoning phase (when no content yet)
  (define all-entries
    (let* ([with-thinking (if (and streaming-thinking (not streaming-text))
                              (append chronological-entries
                                      (list (transcript-entry 'thinking
                                                              streaming-thinking
                                                              (current-inexact-milliseconds)
                                                              (hash)
                                                              #f)))
                              chronological-entries)]
           [with-text (if streaming-text
                          (append with-thinking
                                  (list (transcript-entry 'assistant
                                                          streaming-text
                                                          (current-inexact-milliseconds)
                                                          (hash)
                                                          #f)))
                          with-thinking)])
      with-text))
  (define styled-lines (apply append (map (lambda (e) (format-entry e width)) all-entries)))
  ;; Apply scroll offset — scroll=0 means show bottom (newest), positive = scrolled up
  (define total-lines (length styled-lines))
  (define max-start (max 0 (- total-lines transcript-height)))
  (define start (max 0 (- max-start scroll)))
  (define visible
    (take (drop styled-lines start) (min transcript-height (max 0 (- total-lines start)))))
  (values visible state))

;; Render a branch list.
(define (render-branch-list branches [width 200])
  (define header
    (styled-line (list (styled-segment (format "Branches (~a)" (length branches)) '(bold)))))
  (define lines
    (for/list ([b (in-list branches)])
      (define active (and (branch-info? b) (branch-info-active? b)))
      (define id
        (if (branch-info? b)
            (branch-info-id b)
            (format "~a" b)))
      (styled-line (list (styled-segment (format "  ~a ~a" (if active "\u2192" "\u25CB") id) '())))))
  (cons header lines))

;; Render leaf nodes.
(define (render-leaf-nodes branches [width 200])
  ;; Filter to only leaf nodes
  (define leaves (filter (lambda (b) (and (branch-info? b) (branch-info-leaf? b))) branches))
  (define header
    (styled-line (list (styled-segment (format "Leaf Nodes (~a)" (length leaves)) '(bold)))))
  (if (null? leaves)
      (list header)
      (cons header
            (for/list ([b (in-list leaves)])
              (define id (branch-info-id b))
              (styled-line (list (styled-segment (format "  \u25CB ~a" id) '())))))))

;; Render children list.
(define (render-children-list parent-id children [width 200])
  (define header
    (styled-line (list (styled-segment (format "Children of ~a (~a)" parent-id (length children))
                                       '(bold)))))
  (if (null? children)
      (list header (styled-line (list (styled-segment "  (no children)" '(dim)))))
      (cons header
            (for/list ([c (in-list children)])
              (styled-line (list (styled-segment (format "  ├─ ~a" c) '())))))))
