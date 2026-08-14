#lang racket/base

;; q/ui-core/composer-layout.rkt — Shared width-aware visual layout engine
;;
;; STABILITY: internal
;;
;; W3 (v0.99.96): The single authoritative visual layout calculation for
;; the multiline composer.  Given buffer text, a cursor (logical index),
;; an available display width and a *parameterized* display-width
;; function (TUI: terminal cell width; GUI: font measurement), it
;; computes:
;;
;;   1. The visual lines (substrings + logical offsets), soft-wrapped.
;;   2. The authoritative cursor cell (row, column).
;;
;; Renderers MUST paint exactly the visual lines produced here and MUST
;; use the SAME layout result for the software cursor and any hidden
;; hardware/IME cursor.  Recomputing cursor coordinates independently is
;; forbidden.
;;
;; PURITY CONTRACT:
;;   - No cell-buffer writes, no GUI drawing, no I/O.
;;   - Depends only on racket/base, racket/contract, racket/list,
;;     and the pure grapheme helpers in composer-model.rkt.

(require racket/contract
         racket/list
         "composer-model.rkt")

;; ═══════════════════════════════════════════════════════════════════
;; Data types
;; ═══════════════════════════════════════════════════════════════════

;; One visual (wrapped) line.
;;   text  — the substring to paint (never contains #\newline)
;;   start — logical offset of `text` start within the buffer
;;   end   — logical offset just past `text` end (== start + len text);
;;           for a hard line, `end` is the offset of the #\newline.
;;   hard? — #t when this line is terminated by a newline character
(struct composer-visual-line (text start end hard?) #:transparent)

;; Full layout result.
;;   lines        — (listof composer-visual-line), at least one
;;   cursor-row   — visual row of the cursor (0-based, into `lines`)
;;   cursor-col   — display column of the cursor within that row
;;   row-count    — (length lines)
;;   width        — the display width the layout was computed for
(struct composer-layout (lines cursor-row cursor-col row-count width) #:transparent)

;; ═══════════════════════════════════════════════════════════════════
;; Soft-wrap algorithm
;; ═══════════════════════════════════════════════════════════════════

;; Wrap one newline-free logical line into visual segments.
;; Returns list of (cons text-slice end-offset-relative-to-slice).
(define (wrap-segment text base-offset display-width width)
  (define w (max 1 width))
  (define len (string-length text))
  (let loop ([i 0] ; current scan position in text
             [seg-start 0] ; start of current visual segment
             [col 0] ; accumulated display column of segment
             [acc '()]) ; reversed list of (cons slice end-rel)
    (cond
      [(>= i len)
       (define slice (substring text seg-start len))
       (reverse (cons (list slice seg-start len) acc))]
      [else
       (define span (composer-grapheme-span-at text i))
       (define sp (min (max 1 span) (- (string-length text) i)))
       (define g (substring text i (+ i sp)))
       (define gw (display-width g))
       (cond
         ;; Grapheme fits (or segment empty → always place, avoids
         ;; infinite loops for wide chars in narrow columns).
         [(or (<= (+ col gw) w) (= col 0)) (loop (+ i sp) seg-start (+ col gw) acc)]
         [else
          (define slice (substring text seg-start i))
          (loop i i 0 (cons (list slice seg-start i) acc))])])))

;; Hard-line: split buffer on #\newline, wrap each, mark hard?.
(define (compute-composer-layout buffer cursor width display-width)
  (define w (max 1 width))
  (define logical-lines
    (let loop ([i 0]
               [start 0]
               [acc '()])
      (cond
        ;; trailing (possibly empty) final line
        [(>= i (string-length buffer)) (reverse (cons (cons start (string-length buffer)) acc))]
        [(char=? (string-ref buffer i) #\newline) (loop (add1 i) (add1 i) (cons (cons start i) acc))]
        [else (loop (add1 i) start acc)])))
  ;; Build all visual lines with absolute logical offsets.
  (define-values (lines _)
    (for/fold ([acc '()]
               [prev-end 0])
              ([logical (in-list logical-lines)])
      (define start (car logical))
      (define end (cdr logical)) ; exclusive; for hard lines == newline pos
      (define hard? (< end (string-length buffer)))
      (define text (substring buffer start end))
      (define segs (wrap-segment text start display-width w))
      (define hard-tag hard?)
      (define new-lines
        (for/list ([seg (in-list segs)]
                   [idx (in-naturals)])
          (define last? (= idx (sub1 (length segs))))
          (composer-visual-line (list-ref seg 0)
                                (+ start (list-ref seg 1))
                                (+ start (list-ref seg 2))
                                (and last? hard-tag))))
      (values (append acc new-lines) end)))
  (define all-lines
    (if (null? lines)
        (list (composer-visual-line "" 0 0 #f))
        lines))
  (define-values (crow ccol) (cursor->row/col all-lines cursor display-width))
  (composer-layout all-lines crow ccol (length all-lines) w))

;; ═══════════════════════════════════════════════════════════════════
;; Cursor mapping (authoritative)
;; ═══════════════════════════════════════════════════════════════════

;; Map a logical cursor index to (row, display-column).
;; A cursor at a soft-wrap boundary belongs to the END of the earlier
;; line; a cursor on a #\newline belongs to that line; a cursor after a
;; #\newline belongs to the next line's start.
(define (cursor->row/col lines cursor display-width)
  (let loop ([rest lines]
             [idx 0]
             [prev-line #f])
    (cond
      [(null? rest)
       ;; past the end: clamp to last line
       (define last (or prev-line (car lines)))
       (values (max 0 (sub1 (length lines))) (display-width (composer-visual-line-text last)))]
      [else
       (define line (car rest))
       (define start (composer-visual-line-start line))
       (define end (composer-visual-line-end line))
       (cond
         [(and (>= cursor start) (<= cursor end))
          ;; A cursor at a line's end boundary stays on THIS line
          ;; (col = line width).  A cursor just after a #\newline is
          ;; the next line's start and matches the next iteration.
          (values idx
                  (display-width (substring (composer-visual-line-text line)
                                            0
                                            (min (- cursor start)
                                                 (string-length (composer-visual-line-text line))))))]
         [else (loop (cdr rest) (add1 idx) line)])])))

;; Hard-line boundary handled explicitly: cursor == end on a hard line
;; means "on the newline" → same row, col = line width (handled above);
;; cursor == end+1 → next row (falls through to next line since
;; next.start == end+1).

;; ═══════════════════════════════════════════════════════════════════
;; Column ↔ logical offset (within one visual line)
;; ═══════════════════════════════════════════════════════════════════

;; Display column → logical offset, walking graphemes.
(define (visual-col->offset text col display-width)
  (let loop ([i 0]
             [c 0])
    (cond
      [(>= i (string-length text)) i]
      [(>= c col) i]
      [else
       (define span (composer-grapheme-span-at text i))
       (define sp (min (max 1 span) (- (string-length text) i)))
       (define g (substring text i (+ i sp)))
       (define gw (display-width g))
       (if (> (+ c gw) col)
           i
           (loop (+ i sp) (+ c gw)))])))

;; ═══════════════════════════════════════════════════════════════════
;; Vertical movement (needs the wrap → lives here)
;; ═══════════════════════════════════════════════════════════════════

;; Preferred-column-aware move up/down.  Returns the new composer-state
;; (cursor moved, preferred-col set from the pre-move column).
(define (composer-move-vertical st layout delta display-width)
  (define lines (composer-layout-lines layout))
  (define row (composer-layout-cursor-row layout))
  (define col (composer-layout-cursor-col layout))
  (define target-row (+ row delta))
  (cond
    [(or (< target-row 0) (>= target-row (length lines))) st]
    [else
     (define target (list-ref lines target-row))
     (define target-text (composer-visual-line-text target))
     (define target-col (min (or (composer-state-preferred-col st) col) (display-width target-text)))
     (define offset
       (+ (composer-visual-line-start target)
          (visual-col->offset target-text target-col display-width)))
     (struct-copy composer-state
                  st
                  [cursor offset]
                  [sel-anchor #f]
                  [preferred-col (or (composer-state-preferred-col st) col)])]))

(define (composer-move-up st width display-width)
  (composer-move-vertical
   st
   (compute-composer-layout (composer-state-buffer st) (composer-state-cursor st) width display-width)
   -1
   display-width))

(define (composer-move-down st width display-width)
  (composer-move-vertical
   st
   (compute-composer-layout (composer-state-buffer st) (composer-state-cursor st) width display-width)
   +1
   display-width))

;; VISUAL home/end — jump to the start/end of the current visual row
;; (soft-wrap aware).  Logical home/end (buffer bounds) live in
;; composer-model.rkt as composer-home / composer-end.
(define (composer-visual-home st width display-width)
  (define layout
    (compute-composer-layout (composer-state-buffer st)
                             (composer-state-cursor st)
                             width
                             display-width))
  (define target (list-ref (composer-layout-lines layout) (composer-layout-cursor-row layout)))
  (composer-set-cursor st (composer-visual-line-start target)))

(define (composer-visual-end st width display-width)
  (define layout
    (compute-composer-layout (composer-state-buffer st)
                             (composer-state-cursor st)
                             width
                             display-width))
  (define lines (composer-layout-lines layout))
  (define row (composer-layout-cursor-row layout))
  ;; A cursor sitting exactly at a soft-wrap boundary is ambiguous: the
  ;; layout assigns it to the end of the earlier row, but End should
  ;; advance to the end of the following row (pressing End twice walks
  ;; through the wrapped rows of a logical line).  Hard-line ends are
  ;; unambiguous and stay.
  (define next-row
    (and (< (add1 row) (length lines))
         (let* ([cur (list-ref lines row)]
                [nxt (list-ref lines (add1 row))])
           (and (not (composer-visual-line-hard? cur))
                (= (composer-state-cursor st) (composer-visual-line-end cur))
                (> (composer-visual-line-end nxt) (composer-visual-line-end cur))
                (add1 row)))))
  (define target (list-ref lines (or next-row row)))
  (composer-set-cursor st (composer-visual-line-end target)))

;; ═══════════════════════════════════════════════════════════════════
;; Viewport
;; ═══════════════════════════════════════════════════════════════════

;; Scroll-viewport-follows-cursor.  Given cursor row, viewport height
;; and the previous top row, return the new top row keeping the cursor
;; visible with minimal scrolling.
(define (composer-viewport-top cursor-row height [current-top 0])
  (define h (max 1 height))
  (define top
    (cond
      [(< cursor-row current-top) cursor-row]
      [(>= cursor-row (+ current-top h)) (- (add1 cursor-row) h)]
      [else current-top]))
  (max 0 top))

;; Which scroll indicators should be painted: (values above? below?)
(define (composer-viewport-indicators top height total)
  (values (> top 0) (< (+ top (max 1 height)) total)))

;; Number of visual rows the composer needs (>= 1).
(define (composer-needed-rows layout)
  (max 1 (composer-layout-row-count layout)))

;; ═══════════════════════════════════════════════════════════════════
;; Simple display-width adapters
;; ═══════════════════════════════════════════════════════════════════

;; Codepoint-count width (every char = 1 cell).  Useful for tests and
;; as a GUI fallback.
(define (composer-unit-display-width s)
  (string-length s))

;; ═══════════════════════════════════════════════════════════════════
;; Provides
;; ═══════════════════════════════════════════════════════════════════

(provide composer-visual-line
         composer-visual-line?
         composer-layout
         composer-layout?
         (contract-out
          [composer-visual-line-text (-> composer-visual-line? string?)]
          [composer-visual-line-start (-> composer-visual-line? exact-nonnegative-integer?)]
          [composer-visual-line-end (-> composer-visual-line? exact-nonnegative-integer?)]
          [composer-visual-line-hard? (-> composer-visual-line? boolean?)]
          [composer-layout-lines (-> composer-layout? (listof composer-visual-line?))]
          [composer-layout-cursor-row (-> composer-layout? exact-nonnegative-integer?)]
          [composer-layout-cursor-col (-> composer-layout? exact-nonnegative-integer?)]
          [composer-layout-row-count (-> composer-layout? exact-nonnegative-integer?)]
          [composer-layout-width (-> composer-layout? exact-nonnegative-integer?)]
          [compute-composer-layout
           (-> string?
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               composer-layout?)]
          [composer-move-up
           (-> composer-state?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               composer-state?)]
          [composer-move-down
           (-> composer-state?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               composer-state?)]
          [composer-visual-home
           (-> composer-state?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               composer-state?)]
          [composer-visual-end
           (-> composer-state?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               composer-state?)]
          [composer-viewport-top
           (->* (exact-nonnegative-integer? exact-nonnegative-integer?)
                (exact-nonnegative-integer?)
                exact-nonnegative-integer?)]
          [composer-viewport-indicators
           (-> exact-nonnegative-integer?
               exact-nonnegative-integer?
               exact-nonnegative-integer?
               (values boolean? boolean?))]
          [composer-needed-rows (-> composer-layout? exact-nonnegative-integer?)]
          [composer-unit-display-width (-> string? exact-nonnegative-integer?)]
          [visual-col->offset
           (-> string?
               exact-nonnegative-integer?
               (-> string? exact-nonnegative-integer?)
               exact-nonnegative-integer?)]))
