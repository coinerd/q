#lang racket/base

;; q/ui-core/composer-model.rkt — shared pure multiline composer state.
;;
;; W3 (v0.99.96): the single authoritative SEMANTIC text-editor state for
;; both the TUI and the GUI composer.  Pure data + pure transitions.
;;
;; Contains NO terminal cells, NO `text%`, NO clipboard calls, NO layout:
;; visual lines and cursor (row,col) live in q/ui-core/composer-layout.rkt.
;;
;; Cursor/anchor are STRING indices (logical), always kept on grapheme
;; cluster boundaries by the movement/edit operations.

(require racket/contract)

(provide composer-home composer-end
         composer-state?
         composer-state
         composer-state-buffer
         composer-state-cursor
         composer-state-sel-anchor
         composer-state-preferred-col
         composer-state-undo-group
         composer-state-history-intent
         composer-state-viewport-intent
         composer-state-undo-stack
         composer-state-redo-stack
         composer-state-kill-ring
         composer-undo
         composer-redo
         composer-kill-to-end
         composer-yank
         composer-replace-buffer
         composer-set-selection
         composer-at-beginning?
         composer-at-end?
         composer-empty?
         (contract-out
          [make-composer-state
           (->* () (#:buffer string?
                    #:cursor exact-nonnegative-integer?
                    #:sel-anchor (or/c #f exact-nonnegative-integer?)
                    #:preferred-col (or/c #f exact-nonnegative-integer?)
                    #:undo-group (or/c #f symbol?)
                    #:history-intent (or/c #f 'up 'down)
                    #:viewport-intent (or/c #f 'follow-cursor 'top 'bottom))
                composer-state?)]
          ;; Grapheme segmentation (dependency-free)
          [composer-grapheme-count (-> string? exact-nonnegative-integer?)]
          [composer-grapheme-start? (-> string? exact-nonnegative-integer? boolean?)]
          [composer-next-grapheme-start (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [composer-prev-grapheme-start (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          [composer-grapheme-span-at (-> string? exact-nonnegative-integer? exact-nonnegative-integer?)]
          ;; Selection helpers
          [composer-selection-range (-> composer-state? (values exact-nonnegative-integer?
                                                               exact-nonnegative-integer?))]
          [composer-selection-text (-> composer-state? string?)]
          [composer-has-selection? (-> composer-state? boolean?)]
          [composer-select-all (-> composer-state? composer-state?)]
          [composer-clear-selection (-> composer-state? composer-state?)]
          ;; Editing (grapheme-aware)
          [composer-insert-char (-> composer-state? char? composer-state?)]
          [composer-insert-string
           (->* (composer-state? string?) ((or/c #f symbol?)) composer-state?)]
          [composer-insert-newline (-> composer-state? composer-state?)]
          [composer-backspace (-> composer-state? composer-state?)]
          [composer-delete (-> composer-state? composer-state?)]
          ;; Cursor movement (logical)
          [composer-cursor-left (-> composer-state? composer-state?)]
          [composer-cursor-right (-> composer-state? composer-state?)]
          [composer-cursor-home (-> composer-state? composer-state?)]
          [composer-cursor-end (-> composer-state? composer-state?)]
          [composer-set-cursor (-> composer-state? exact-nonnegative-integer? composer-state?)]
          [composer-clear (-> composer-state? composer-state?)]
          [composer-load-text (-> composer-state? string? composer-state?)]
          ;; History intent
          [composer-set-history-intent (-> composer-state? (or/c #f 'up 'down) composer-state?)]))

;; ═══════════════════════════════════════════════════════════════════
;; State
;; ═══════════════════════════════════════════════════════════════════

(struct composer-state
  (buffer           ; string
   cursor           ; string index, on a grapheme boundary
   sel-anchor       ; #f | string index
   preferred-col    ; #f | cell column remembered across vertical moves
   undo-group       ; #f | symbol? (coalescing token)
   history-intent   ; #f | 'up | 'down
   viewport-intent  ; #f | 'follow-cursor | 'top | 'bottom
   undo-stack       ; (listof (list buffer cursor anchor tag)) — newest first
   redo-stack       ; (listof (list buffer cursor anchor tag)) — newest first
   kill-ring)       ; (listof string?) — newest first
  #:transparent)

(define (make-composer-state
         #:buffer [buffer ""]
         #:cursor [cursor 0]
         #:sel-anchor [sel-anchor #f]
         #:preferred-col [preferred-col #f]
         #:undo-group [undo-group #f]
         #:history-intent [history-intent #f]
         #:viewport-intent [viewport-intent #f])
  (define b (if (string? buffer) buffer ""))
  (composer-state b
                  (max 0 (min cursor (string-length b)))
                  (and sel-anchor (max 0 (min sel-anchor (string-length b))))
                  preferred-col
                  undo-group
                  history-intent
                  viewport-intent
                  '()
                  '()
                  '()))

;; ═══════════════════════════════════════════════════════════════════
;; Grapheme cluster segmentation (dependency-free)
;;
;; Covers the cases that matter for terminal composition: combining
;; marks (Mn/Mc/Me), ZWJ sequences, variation selectors, emoji
;; modifiers (skin tones), and regional-indicator pairs (flags).
;; ═══════════════════════════════════════════════════════════════════

(define (mark? c)
  (member (char-general-category c) '(mn mc me)))

(define (zwj? c) (char=? c #\U200D))
(define (variation-selector? c)
  (or (char<=? #\UFE00 c #\UFE0F)
      (char<=? #\UE0100 c #\UE01EF)))
(define (emoji-modifier? c)
  (char<=? #\U1F3FB c #\U1F3FF))
(define (regional-indicator? c)
  (char<=? #\U1F1E6 c #\U1F1FF))
(define (continue-char? c)
  (or (mark? c) (variation-selector? c) (emoji-modifier? c)))

;; Boundary i (0..n) is a cluster start unless the char at i continues
;; the cluster to its left (mark / selector / modifier / ZWJ itself /
;; ZWJ's right-hand member / second regional indicator of a pair).
(define (boundary-start? s i)
  (define n (string-length s))
  (if (>= i n)
      #t
      (let ([ci (string-ref s i)])
        (not (or (mark? ci)
                 (variation-selector? ci)
                 (emoji-modifier? ci)
                 (zwj? ci)
                 (and (> i 0) (zwj? (string-ref s (sub1 i))))
                 (and (regional-indicator? ci) (> i 0)
                      (regional-indicator? (string-ref s (sub1 i)))))))))

;; Cluster start at or before pos (pos snapped down to a boundary).
(define (cluster-start s pos)
  (let loop ([i (min pos (string-length s))])
    (if (or (<= i 0) (boundary-start? s i)) i (loop (sub1 i)))))

;; End (exclusive) of the cluster starting at start.
(define (cluster-end s start)
  (define n (string-length s))
  (let loop ([i (add1 start)] [after-zwj? #f])
    (cond [(>= i n) i]
          [(zwj? (string-ref s i))
           (loop (add1 i) #t)]
          [after-zwj?
           (loop (add1 i) #f)]
          [(continue-char? (string-ref s i))
           (loop (add1 i) #f)]
          ;; second regional indicator completes a flag cluster
          [(and (regional-indicator? (string-ref s start))
                (regional-indicator? (string-ref s i))
                (= i (add1 start)))
           (loop (add1 i) #f)]
          [else i])))

(define (composer-grapheme-start? s pos)
  (and (<= 0 pos (string-length s))
       (= pos (cluster-start s pos))))

(define (composer-next-grapheme-start s pos)
  (define n (string-length s))
  (define p (max 0 (min pos n)))
  (if (>= p n) n (cluster-end s (cluster-start s p))))

(define (composer-prev-grapheme-start s pos)
  (define p (max 0 (min pos (string-length s))))
  (cluster-start s (cluster-start s (max 0 (sub1 p)))))

(define (composer-grapheme-span-at s pos)
  (if (>= pos (string-length s))
      0
      ;; Span LENGTH of the cluster containing pos — NOT the absolute
      ;; next-boundary offset.  (composer-next-grapheme-start returns
      ;; an absolute index; subtracting the cluster start yields the
      ;; length the wrap algorithm needs.)
      (let ([start (cluster-start s pos)])
        (max 1 (- (cluster-end s start) start)))))

(define (composer-grapheme-count s)
  (define n (string-length s))
  (let loop ([i 0] [acc 0])
    (if (>= i n) acc (loop (cluster-end s i) (add1 acc)))))

;; ═══════════════════════════════════════════════════════════════════
;; Selection
;; ═══════════════════════════════════════════════════════════════════

(define (composer-has-selection? st)
  (and (composer-state-sel-anchor st)
       (not (= (composer-state-sel-anchor st) (composer-state-cursor st)))))

(define (composer-selection-range st)
  (define a (or (composer-state-sel-anchor st) (composer-state-cursor st)))
  (define c (composer-state-cursor st))
  (if (<= a c) (values a c) (values c a)))

(define (composer-selection-text st)
  (if (composer-has-selection? st)
      (let-values ([(a b) (composer-selection-range st)])
        (substring (composer-state-buffer st) a b))
      ""))

(define (composer-select-all st)
  (struct-copy composer-state st
               [cursor (string-length (composer-state-buffer st))]
               [sel-anchor 0]))

(define (composer-clear-selection st)
  (struct-copy composer-state st [sel-anchor #f]))

;; Delete the active selection, if any; returns the state unchanged
;; when there is nothing selected.
(define (delete-selection st)
  (if (composer-has-selection? st)
      (let-values ([(a b) (composer-selection-range st)])
        (struct-copy composer-state st
                     [buffer (string-append (substring (composer-state-buffer st) 0 a)
                                            (substring (composer-state-buffer st) b))]
                     [cursor a]
                     [sel-anchor #f]))
      st))

;; ═══════════════════════════════════════════════════════════════════
;; Editing (grapheme-aware)
;; ═══════════════════════════════════════════════════════════════════

;; ═══════════════════════════════════════════════════════════════════
(define (push-undo st tag)
  (define entry (list (composer-state-buffer st)
                      (composer-state-cursor st)
                      (composer-state-sel-anchor st)
                      tag))
  (define stack (composer-state-undo-stack st))
  (if (and (pair? stack) tag (eq? (list-ref (car stack) 3) tag))
      stack
      (cons entry stack)))

;; Commit an edit: record undo (grouped by `tag`) and clear redo.
(define (commit st tag edited)
  (struct-copy composer-state edited
               [undo-stack (push-undo st tag)]
               [redo-stack '()]))

(define (insert-at st str tag)
  ;; Snap to a cluster boundary so we never split a grapheme.
  (define buf (composer-state-buffer st))
  (define pos (cluster-start buf (composer-state-cursor st)))
  (commit st tag
          (struct-copy composer-state st
                       [buffer (string-append (substring buf 0 pos) str
                                              (substring buf pos))]
                       [cursor (+ pos (string-length str))]
                       [sel-anchor #f]
                       [preferred-col #f])))

(define (composer-insert-char st c) (insert-at st (string c) 'type))
(define (composer-insert-string st s [tag #f])
  (if (= (string-length s) 0) st (insert-at st s tag)))
(define (composer-insert-newline st) (insert-at st "\n" 'type))

(define (composer-backspace st)
  (cond
    [(composer-has-selection? st) (commit st #f (delete-selection st))]
    [(= (composer-state-cursor st) 0) st]
    [else
     (define buf (composer-state-buffer st))
     (define cur (composer-state-cursor st))
     (define prev (composer-prev-grapheme-start buf cur))
     (commit st #f
             (struct-copy composer-state st
                          [buffer (string-append (substring buf 0 prev)
                                                 (substring buf cur))]
                          [cursor prev]
                          [preferred-col #f]))]))

(define (composer-delete st)
  (cond
    [(composer-has-selection? st) (commit st #f (delete-selection st))]
    [else
     (define buf (composer-state-buffer st))
     (define cur (composer-state-cursor st))
     (define n (string-length buf))
     (if (>= cur n) st
         (let* ([start (cluster-start buf cur)]
                [end (cluster-end buf start)])
           (commit st #f
                   (struct-copy composer-state st
                                [buffer (string-append
                                         (substring buf 0 start)
                                         (substring buf end))]))))]))

(define (composer-undo st)
  (define stack (composer-state-undo-stack st))
  (if (null? stack) st
      (let* ([e (car stack)]
             [redo-entry (list (composer-state-buffer st)
                               (composer-state-cursor st)
                               (composer-state-sel-anchor st)
                               #f)])
        (struct-copy composer-state st
                     [buffer (list-ref e 0)]
                     [cursor (list-ref e 1)]
                     [sel-anchor (list-ref e 2)]
                     [undo-stack (cdr stack)]
                     [redo-stack (cons redo-entry
                                       (composer-state-redo-stack st))]))))

(define (composer-redo st)
  (define stack (composer-state-redo-stack st))
  (if (null? stack) st
      (let* ([e (car stack)]
             [undo-entry (list (composer-state-buffer st)
                               (composer-state-cursor st)
                               (composer-state-sel-anchor st)
                               #f)])
        (struct-copy composer-state st
                     [buffer (list-ref e 0)]
                     [cursor (list-ref e 1)]
                     [sel-anchor (list-ref e 2)]
                     [redo-stack (cdr stack)]
                     [undo-stack (cons undo-entry
                                       (composer-state-undo-stack st))]))))

(define (composer-kill-to-end st)
  (define buf (composer-state-buffer st))
  (define cur (composer-state-cursor st))
  (define killed (substring buf cur))
  (if (= (string-length killed) 0)
      st
      (commit st #f
              (struct-copy composer-state st
                           [buffer (substring buf 0 cur)]
                           [kill-ring (cons killed
                                            (composer-state-kill-ring st))]))))

(define (composer-yank st)
  (define ring (composer-state-kill-ring st))
  (if (null? ring) st (insert-at st (car ring) 'paste)))

(define (composer-cursor-left st)
  (define buf (composer-state-buffer st))
  (define cur (composer-state-cursor st))
  (if (= cur 0) st
      (struct-copy composer-state st
                   [cursor (composer-prev-grapheme-start buf cur)]
                   [preferred-col #f])))

(define (composer-cursor-right st)
  (define buf (composer-state-buffer st))
  (define cur (composer-state-cursor st))
  (if (>= cur (string-length buf)) st
      (struct-copy composer-state st
                   [cursor (composer-next-grapheme-start buf cur)]
                   [preferred-col #f])))

(define (composer-home st) (composer-cursor-home st))
(define (composer-end st) (composer-cursor-end st))

(define (composer-at-beginning? st)
  (= (composer-state-cursor st) 0))

(define (composer-at-end? st)
  (= (composer-state-cursor st)
     (string-length (composer-state-buffer st))))

(define (composer-empty? st)
  (= 0 (string-length (composer-state-buffer st))))

(define (composer-cursor-home st)
  (struct-copy composer-state st [cursor 0] [preferred-col #f]))

(define (composer-cursor-end st)
  (struct-copy composer-state st
               [cursor (string-length (composer-state-buffer st))]
               [preferred-col #f]))

(define (composer-set-cursor st pos)
  (define buf (composer-state-buffer st))
  (define p (max 0 (min pos (string-length buf))))
  (struct-copy composer-state st [cursor p]))

(define (composer-set-selection st anchor)
  (define b (string-length (composer-state-buffer st)))
  (struct-copy composer-state st
               [sel-anchor (max 0 (min anchor b))]))

(define (composer-replace-buffer st text)
  (define b (if (string? text) text ""))
  (commit st #f
          (struct-copy composer-state st
                       [buffer b]
                       [cursor (min (composer-state-cursor st)
                                    (string-length b))]
                       [sel-anchor #f]
                       [preferred-col #f])))

(define (composer-clear st)
  (struct-copy composer-state st
               [buffer ""]
               [cursor 0]
               [sel-anchor #f]
               [preferred-col #f]
               [viewport-intent 'follow-cursor]))

(define (composer-load-text st text)
  (struct-copy composer-state st
               [buffer (if (string? text) text "")]
               [cursor 0]
               [sel-anchor #f]
               [preferred-col #f]
               [viewport-intent 'follow-cursor]))

(define (composer-set-history-intent st intent)
  (struct-copy composer-state st [history-intent intent]))

;; Tests
;; ═══════════════════════════════════════════════════════════════════

(module+ test
  (require rackunit)
  (define family "👨‍👩‍👧‍👦")
  (define flag "🇩🇪")
  (define skin "👍🏽")
  (define combi "é")

  (check-equal? (composer-grapheme-count family) 1)
  (check-equal? (composer-grapheme-count flag) 1)
  (check-equal? (composer-grapheme-count skin) 1)
  (check-equal? (composer-grapheme-count combi) 1)
  (check-equal? (composer-grapheme-count "abcdef") 6)
  (check-equal? (composer-grapheme-count "") 0)
  (check-equal? (composer-grapheme-count "a\nb") 3)

  (define fam-len (string-length family))
  (check-equal? (composer-prev-grapheme-start family fam-len) 0)
  (check-equal? (composer-next-grapheme-start family 0) fam-len)

  ;; Editing
  (define st1 (composer-insert-char (make-composer-state #:buffer "ab" #:cursor 2) #\c))
  (check-equal? (composer-state-buffer st1) "abc")
  (check-equal? (composer-state-cursor st1) 3)

  (define st2 (composer-insert-newline (make-composer-state #:buffer "ab" #:cursor 1)))
  (check-equal? (composer-state-buffer st2) "a\nb")

  (check-equal?
   (composer-state-buffer
    (composer-backspace (make-composer-state #:buffer family #:cursor fam-len)))
   "")
  (check-equal?
   (composer-state-buffer
    (composer-delete (make-composer-state
                      #:buffer (string-append "a" family) #:cursor 1)))
   "a")

  ;; Selection
  (define sel (make-composer-state #:buffer "abcdef" #:cursor 2 #:sel-anchor 5))
  (check-true (composer-has-selection? sel))
  (check-equal? (composer-selection-text sel) "cde")
  (check-equal? (composer-state-buffer (composer-backspace sel)) "abf")

  ;; Movement
  (check-equal?
   (composer-state-cursor
    (composer-cursor-left (make-composer-state
                           #:buffer (string-append "a" family)
                           #:cursor (add1 fam-len))))
   1)
  (check-equal? (composer-state-cursor (composer-cursor-home sel)) 0)
  (check-equal? (composer-state-cursor (composer-cursor-end sel)) 6)

  ;; Purity: originals untouched
  (check-equal? (composer-state-buffer sel) "abcdef"))
