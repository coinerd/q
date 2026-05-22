#lang racket/base

;; q/tui/input/state-types.rkt — input state struct definitions and constants
;;
;; Pure data definitions. No logic.

(require "../../util/error-helpers.rkt")
(require racket/contract
         racket/string
         racket/list
         "../char-width.rkt"
         "../terminal.rkt")

(provide input-state
         input-state?
         input-state-buffer
         input-state-cursor
         input-state-history
         input-state-history-idx
         input-state-saved-text
         input-state-scroll-offset
         input-state-undo-stack
         input-state-redo-stack
         input-state-kill-ring
         mouse-event
         mouse-event?
         mouse-event-type
         mouse-event-button
         mouse-event-x
         mouse-event-y

         ;; Constants
         MAX-UNDO-STACK
         MAX-KILL-RING
         INPUT-PROMPT-WIDTH
         CURSOR_MARKER

         (contract-out
          ;; Constructor
          [initial-input-state (-> any/c)]
          ;; Internal helpers (shared by editing-ops, history-ops)
          [push-undo (-> any/c any/c any/c)]
          [push-kill (-> any/c string? any/c)]
          [strip-for-undo (-> any/c any/c)]
          ;; Mouse event support
          [parse-mouse-event (-> any/c any/c)]
          [decode-mouse-x10 (-> exact-integer? exact-integer? exact-integer? any/c)]
          [decode-mouse-tui-term (-> any/c any/c)]
          [normalize-selection-range
           (-> any/c any/c (values exact-integer? exact-integer? exact-integer? exact-integer?))]
          ;; IME cursor markers
          [cursor-marker-string (-> string?)]
          [strip-cursor-markers (-> string? string?)]
          [has-cursor-markers? (-> string? boolean?)]
          [insert-cursor-marker (-> string? exact-nonnegative-integer? string?)]
          ;; Horizontal scroll
          [input-visible-window
           (-> any/c
               exact-positive-integer?
               (values string? exact-nonnegative-integer? exact-nonnegative-integer?))]))

;; Maximum undo stack depth
(define MAX-UNDO-STACK 100)

;; Maximum kill ring entries
(define MAX-KILL-RING 10)

;; Visible input width helper — prompt takes 3 columns (">> ")
(define INPUT-PROMPT-WIDTH 3)

;; Input state
(struct input-state
        (buffer ; string — current input text
         cursor ; integer — cursor position (0 = before first char)
         history ; (listof string) — submitted inputs, newest last
         history-idx ; integer or #f — current position in history (#f = not browsing)
         saved-text ; string or #f — text saved when entering history browsing
         scroll-offset ; integer — horizontal scroll offset for long inputs
         undo-stack ; (listof input-state) — prior states for undo (max MAX-UNDO-STACK)
         redo-stack ; (listof input-state) — undone states for redo
         kill-ring ; (listof string) — killed text ring (max MAX-KILL-RING)
         )
  #:transparent)

(define (initial-input-state)
  (input-state "" 0 '() #f #f 0 '() '() '()))

;; Push old-st onto undo stack of new-st, clear redo stack.
;; Returns new-st with updated undo/redo stacks.
(define (push-undo old-st new-st)
  (define stack (cons (strip-for-undo old-st) (input-state-undo-stack new-st)))
  (struct-copy input-state
               new-st
               [undo-stack
                (if (> (length stack) MAX-UNDO-STACK)
                    (take stack MAX-UNDO-STACK)
                    stack)]
               [redo-stack '()]))

;; Strip history/undo/redo/kill for undo snapshots (avoid unbounded nesting).
(define (strip-for-undo st)
  (struct-copy input-state
               st
               [history '()]
               [history-idx #f]
               [saved-text #f]
               [undo-stack '()]
               [redo-stack '()]
               [kill-ring '()]))

;; Push text onto kill ring
(define (push-kill st text)
  (if (string=? text "")
      st
      (let ([ring (cons text (input-state-kill-ring st))])
        (struct-copy input-state
                     st
                     [kill-ring
                      (if (> (length ring) MAX-KILL-RING)
                          (take ring MAX-KILL-RING)
                          ring)]))))

;; ============================================================
;; Mouse event support
;; ============================================================

;; Mouse event types
(struct mouse-event
        (type ; symbol: 'mouse-click | 'mouse-scroll-up | 'mouse-scroll-down
         button ; integer: button number (0=left, 1=middle, 2=right) for clicks, 0 otherwise
         x ; integer: column (0-based)
         y ; integer: row (0-based)
         )
  #:transparent)

;; Parse a mouse event from raw bytes.
;; X10 mouse protocol: \x1b[M followed by 3 bytes (cb, cx, cy)
(define (parse-mouse-event bytes-or-list)
  (define lst
    (cond
      [(bytes? bytes-or-list) (bytes->list bytes-or-list)]
      [(list? bytes-or-list) bytes-or-list]
      [else #f]))
  (if (or (not lst) (< (length lst) 6))
      #f
      (let ([b0 (list-ref lst 0)]
            [b1 (list-ref lst 1)]
            [b2 (list-ref lst 2)]
            [cb (list-ref lst 3)]
            [cx (list-ref lst 4)]
            [cy (list-ref lst 5)])
        (if (not (and (= b0 #x1b) (= b1 #x5b) (= b2 #x4d)))
            #f
            (let* ([button-code (- cb 32)]
                   [button (bitwise-and button-code #x03)]
                   [modifier (bitwise-and button-code #x1c)]
                   [x (- cx 33)]
                   [y (- cy 33)])
              (cond
                [(= (bitwise-and button-code 64) 64)
                 (mouse-event (if (= button 0) 'mouse-scroll-up 'mouse-scroll-down)
                              0
                              (max 0 x)
                              (max 0 y))]
                [(and (<= button 2)
                      (= (bitwise-and button-code 32) 0)
                      (= (bitwise-and button-code 64) 0))
                 (mouse-event 'mouse-click button (max 0 x) (max 0 y))]
                [else #f]))))))

;; Decode an X10-encoded mouse event from raw cb/cx/cy bytes.
(define (decode-mouse-x10 cb cx cy)
  (define button-code (- cb 32))
  (define button (bitwise-and button-code #x03))
  (define motion? (= (bitwise-and button-code 32) 32))
  (define scroll? (= (bitwise-and button-code 64) 64))
  (define x (- cx 33))
  (define y (- cy 33))
  (cond
    [scroll? (list 'mouse (if (= button 0) 'scroll-up 'scroll-down) x y)]
    [(= button 3) (list 'mouse 'release x y)]
    [(and motion? (<= button 2)) (list 'mouse 'drag x y)]
    [(and (<= button 2) (not motion?)) (list 'mouse 'click button x y)]
    [else #f]))

;; Decode a tui-term tmousemsg struct into q's internal mouse event format.
(define (decode-mouse-tui-term msg)
  (with-safe-fallback #f
                      (define kind (tmousemsg-kind msg))
                      (define x (tmousemsg-pos-x msg))
                      (define y (tmousemsg-pos-y msg))
                      (define left (tmousemsg-left? msg))
                      (case kind
                        [(wheel-up) (list 'mouse 'scroll-up x y)]
                        [(wheel-down) (list 'mouse 'scroll-down x y)]
                        [(press)
                         (define button
                           (cond
                             [(tmousemsg-left? msg) 0]
                             [(tmousemsg-middle? msg) 1]
                             [(tmousemsg-right? msg) 2]
                             [else 0]))
                         (list 'mouse 'click button x y)]
                        [(release) (list 'mouse 'release x y)]
                        [(move)
                         (if (or left (tmousemsg-right? msg) (tmousemsg-middle? msg))
                             (list 'mouse 'drag x y)
                             #f)]
                        [(leave) #f]
                        [else #f])))

;; Normalize selection so start <= end (row-major order)
(define (normalize-selection-range anchor end)
  (if (or (< (cdr anchor) (cdr end)) (and (= (cdr anchor) (cdr end)) (<= (car anchor) (car end))))
      (values (car anchor) (cdr anchor) (car end) (cdr end))
      (values (car end) (cdr end) (car anchor) (cdr anchor))))

;; ============================================================
;; IME cursor markers
;; ============================================================

(define CURSOR_MARKER #\u200B)

(define (cursor-marker-string)
  (string CURSOR_MARKER))

(define (strip-cursor-markers str)
  (string-replace str (cursor-marker-string) ""))

(define (has-cursor-markers? str)
  (string-contains? str (cursor-marker-string)))

(define (insert-cursor-marker str pos)
  (define safe-pos (min pos (string-length str)))
  (string-append (substring str 0 safe-pos) (cursor-marker-string) (substring str safe-pos)))

;; ============================================================
;; Horizontal scroll
;; ============================================================

;; Compute the visible window of the buffer given a terminal width.
(define (input-visible-window st cols)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define offset (input-state-scroll-offset st))
  (define max-visible (max 1 (- cols INPUT-PROMPT-WIDTH)))
  (define cursor-offset-width
    (if (or (>= offset (string-length buf)) (< cur offset))
        0
        (string-visible-width (substring buf offset cur))))
  (define new-offset
    (cond
      [(< cur offset) (find-offset-for-cursor buf cur max-visible)]
      [(> cursor-offset-width max-visible) (find-offset-for-cursor buf cur max-visible)]
      [else offset]))
  (define clamped-offset (max 0 (min new-offset (string-length buf))))
  (define end-pos (find-end-pos buf clamped-offset max-visible))
  (define visible-text (substring buf clamped-offset end-pos))
  (define cursor-display-col
    (+ INPUT-PROMPT-WIDTH
       (if (>= clamped-offset cur)
           0
           (string-visible-width (substring buf clamped-offset cur)))))
  (values visible-text clamped-offset cursor-display-col))

(define (find-offset-for-cursor buf cursor max-visible)
  (define target-width (max 1 (- max-visible 1)))
  (let loop ([i (min cursor (string-length buf))]
             [col 0])
    (cond
      [(<= i 0) 0]
      [else
       (define c (string-ref buf (sub1 i)))
       (define w (char-width c))
       (define new-col (+ col w))
       (if (> new-col target-width)
           i
           (loop (sub1 i) new-col))])))

(define (find-end-pos buf start-offset max-visible)
  (let loop ([i start-offset]
             [col 0])
    (cond
      [(>= i (string-length buf)) (string-length buf)]
      [else
       (define c (string-ref buf i))
       (define w (char-width c))
       (define new-col (+ col w))
       (if (> new-col max-visible)
           i
           (loop (add1 i) new-col))])))
