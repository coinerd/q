#lang racket/base

;; q/tui/input.rkt — Input buffer, cursor movement, history, slash commands
;;
;; Pure functions only. No terminal I/O. No side effects.
;; The input-state struct is immutable.

(require racket/string
         racket/list
         "char-width.rkt")

;; Structs
(provide (struct-out input-state)
         (struct-out mouse-event)

         ;; Constructors
         initial-input-state

         ;; Editing (all pure, return new input-state)
         input-insert-char
         input-insert-newline
         input-backspace
         input-delete
         input-cursor-left
         input-cursor-right
         input-home
         input-end
         input-clear

         ;; History
         input-history-push
         input-history-up
         input-history-down

         ;; Submission
         input-submit
         input-current-text
         input-at-beginning?
         input-at-end?
         input-empty?

         ;; Slash commands
         input-slash-command
         parse-tui-slash-command

         ;; Horizontal scroll
         input-visible-window
         INPUT-PROMPT-WIDTH

         ;; Mouse events
         parse-mouse-event

         ;; Selection helpers
         normalize-selection-range

         ;; X10 mouse decoding
         decode-mouse-x10)

;; Input state
(struct input-state
        (buffer ; string — current input text
         cursor ; integer — cursor position (0 = before first char)
         history ; (listof string) — submitted inputs, newest last
         history-idx ; integer or #f — current position in history (#f = not browsing)
         saved-text ; string or #f — text saved when entering history browsing
         scroll-offset ; integer — horizontal scroll offset for long inputs
         )
  #:transparent)

(define (initial-input-state)
  (input-state "" 0 '() #f #f 0))

;; Editing operations
(define (input-insert-char st ch)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define new-buf (string-append (substring buf 0 cur) (string ch) (substring buf cur)))
  (struct-copy input-state st [buffer new-buf] [cursor (+ cur 1)]))

(define (input-backspace st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (let ([new-buf (string-append (substring buf 0 (- cur 1)) (substring buf cur))])
        (struct-copy input-state st [buffer new-buf] [cursor (- cur 1)]))))

;; Visible input width helper — prompt takes 4 columns ("q> ")
(define INPUT-PROMPT-WIDTH 3)

;; Compute the visible window of the buffer given a terminal width.
;; Returns (values visible-text scroll-offset cursor-display-col)
;; scroll-offset is the character index of the first visible character.
;; Uses visible column width (CJK-aware) instead of string-length.
(define (input-visible-window st cols)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define offset (input-state-scroll-offset st))
  (define max-visible (max 1 (- cols INPUT-PROMPT-WIDTH)))
  ;; Compute visible width from offset to cursor
  (define cursor-offset-width
    (if (or (>= offset (string-length buf))
            (< cur offset))
        0
        (string-visible-width (substring buf offset cur))))
  ;; Compute total visible width from offset to end
  (define buf-remaining-width
    (if (>= offset (string-length buf))
        0
        (string-visible-width (substring buf offset))))
  ;; Adjust offset so cursor is visible
  (define new-offset
    (cond
      ;; Cursor is before current offset
      [(< cur offset)
       ;; Walk backward from offset to find a new offset
       ;; that places cursor at or near the left edge
       (find-offset-for-cursor buf cur max-visible)]
      ;; Cursor's visible width from offset exceeds max-visible
      [(> cursor-offset-width max-visible)
       (find-offset-for-cursor buf cur max-visible)]
      [else offset]))
  (define clamped-offset (max 0 (min new-offset (string-length buf))))
  ;; Find end position: advance from clamped-offset until visible width > max-visible
  (define end-pos (find-end-pos buf clamped-offset max-visible))
  (define visible-text (substring buf clamped-offset end-pos))
  (define cursor-display-col
    (+ INPUT-PROMPT-WIDTH
       (if (>= clamped-offset cur)
           0
           (string-visible-width (substring buf clamped-offset cur)))))
  (values visible-text clamped-offset cursor-display-col))

;; Find a character offset into `buf` such that the visible width from
;; that offset to `cursor` fits within `max-visible` columns.
;; The cursor is placed as far right as possible (one column from the edge).
(define (find-offset-for-cursor buf cursor max-visible)
  ;; Target: visible width from offset to cursor <= max-visible - 1
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

;; Find end position: advance from start-offset until visible width
;; would exceed max-visible columns.
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

;; Insert a literal newline at cursor position (for multi-line input)
(define (input-insert-newline st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (define new-buf (string-append (substring buf 0 cur) "\n" (substring buf cur)))
  (struct-copy input-state st [buffer new-buf] [cursor (add1 cur)] [history-idx #f]))

(define (input-delete st)
  (define buf (input-state-buffer st))
  (define cur (input-state-cursor st))
  (if (>= cur (string-length buf))
      st
      (struct-copy input-state
                   st
                   [buffer (string-append (substring buf 0 cur) (substring buf (+ cur 1)))]
                   [cursor cur])))

(define (input-cursor-left st)
  (define cur (input-state-cursor st))
  (if (zero? cur)
      st
      (struct-copy input-state st [cursor (- cur 1)])))

(define (input-cursor-right st)
  (define cur (input-state-cursor st))
  (define len (string-length (input-state-buffer st)))
  (if (>= cur len)
      st
      (struct-copy input-state st [cursor (+ cur 1)])))

(define (input-home st)
  (struct-copy input-state st [cursor 0]))

(define (input-end st)
  (struct-copy input-state st [cursor (string-length (input-state-buffer st))]))

(define (input-clear st)
  (struct-copy input-state st [buffer ""] [cursor 0]))

;; History
(define (input-history-push st text)
  ;; Add text to history if non-empty and different from last entry
  (if (or (string=? text "")
          (and (not (null? (input-state-history st)))
               (string=? text (last (input-state-history st)))))
      st
      (struct-copy input-state
                   st
                   [history (append (input-state-history st) (list text))]
                   [history-idx #f]
                   [saved-text #f])))

(define (input-history-up st)
  ;; Move to older entry in history
  (define hist (input-state-history st))
  (if (null? hist)
      st
      (let* ([current-idx (input-state-history-idx st)]
             [new-idx (if (not current-idx)
                          (- (length hist) 1) ; start at newest
                          (- current-idx 1))])
        (if (< new-idx 0)
            st ; already at oldest
            (let ([saved (or (input-state-saved-text st) (input-state-buffer st))])
              (struct-copy input-state
                           st
                           [history-idx new-idx]
                           [buffer (list-ref hist new-idx)]
                           [cursor (string-length (list-ref hist new-idx))]
                           [saved-text saved]))))))

(define (input-history-down st)
  ;; Move to newer entry in history
  (define hist (input-state-history st))
  (define current-idx (input-state-history-idx st))
  (if (not current-idx)
      st
      (let ([new-idx (+ current-idx 1)])
        (if (>= new-idx (length hist))
            ;; Exit history browsing — restore saved text
            (let ([saved (or (input-state-saved-text st) "")])
              (struct-copy input-state
                           st
                           [history-idx #f]
                           [buffer saved]
                           [cursor (string-length saved)]
                           [saved-text #f]))
            ;; Move to next entry
            (struct-copy input-state
                         st
                         [history-idx new-idx]
                         [buffer (list-ref hist new-idx)]
                         [cursor (string-length (list-ref hist new-idx))])))))

;; Submission
(define (input-submit st)
  ;; Returns (values text new-state) where new-state has buffer cleared
  ;; and text pushed to history. Returns #f for empty input.
  (define text (string-trim (input-state-buffer st)))
  (if (string=? text "")
      (values #f st)
      (let ([pushed (input-history-push st text)])
        (values text (struct-copy input-state pushed [buffer ""] [cursor 0])))))

(define (input-current-text st)
  (input-state-buffer st))

(define (input-at-beginning? st)
  (zero? (input-state-cursor st)))

(define (input-at-end? st)
  (= (input-state-cursor st) (string-length (input-state-buffer st))))

(define (input-empty? st)
  (string=? (input-state-buffer st) ""))

;; ============================================================
;; Mouse event support
;; ============================================================

;; Mouse event types: 'mouse-click, 'mouse-scroll-up, 'mouse-scroll-down
(struct mouse-event
        (type ; symbol: 'mouse-click | 'mouse-scroll-up | 'mouse-scroll-down
         button ; integer: button number (0=left, 1=middle, 2=right) for clicks, 0 otherwise
         x ; integer: column (0-based)
         y ; integer: row (0-based)
         )
  #:transparent)

;; Parse a mouse event from raw bytes.
;; X10 mouse protocol: \x1b[M followed by 3 bytes (cb, cx, cy)
;;   cb = button code (32 + button for press)
;;   cx = column + 33 (1-based)
;;   cy = row + 33 (1-based)
;; Returns mouse-event or #f if not a valid mouse sequence.
;;
;; bytes can be:
;;   - a bytes object of length >= 6 (ESC [ M cb cx cy ...)
;;   - a list of integers
(define (parse-mouse-event bytes-or-list)
  (define lst
    (cond
      [(bytes? bytes-or-list) (bytes->list bytes-or-list)]
      [(list? bytes-or-list) bytes-or-list]
      [else #f]))
  ;; Need at least ESC [ M cb cx cy = 6 bytes
  (if (or (not lst) (< (length lst) 6))
      #f
      (let ([b0 (list-ref lst 0)]
            [b1 (list-ref lst 1)]
            [b2 (list-ref lst 2)]
            [cb (list-ref lst 3)]
            [cx (list-ref lst 4)]
            [cy (list-ref lst 5)])
        ;; Check for ESC [ M prefix
        (if (not (and (= b0 #x1b) (= b1 #x5b) (= b2 #x4d)))
            #f
            (let* ([button-code (- cb 32)]
                   [button (bitwise-and button-code #x03)]
                   [modifier (bitwise-and button-code #x1c)]
                   [x (- cx 33)] ; convert to 0-based
                   [y (- cy 33)])
              (cond
                ;; Scroll wheel: button-code has bit 6 set (64)
                [(= (bitwise-and button-code 64) 64)
                 (mouse-event (if (= button 0) 'mouse-scroll-up 'mouse-scroll-down)
                              0
                              (max 0 x)
                              (max 0 y))]
                ;; Button press/release (no motion, no modifier check needed)
                [(and (<= button 2)
                      ;; Only button press (bit 5 clear) — not motion or release
                      (= (bitwise-and button-code 32) 0)
                      (= (bitwise-and button-code 64) 0))
                 (mouse-event 'mouse-click button (max 0 x) (max 0 y))]
                [else #f]))))))

;; ============================================================
;; Selection helpers
;; ============================================================

;; Normalize selection so start <= end (row-major order)
;; Returns (values start-col start-row end-col end-row)
(define (normalize-selection-range anchor end)
  (if (or (< (cdr anchor) (cdr end)) (and (= (cdr anchor) (cdr end)) (<= (car anchor) (car end))))
      (values (car anchor) (cdr anchor) (car end) (cdr end))
      (values (car end) (cdr end) (car anchor) (cdr anchor))))

;; ============================================================
;; X10 mouse decoding
;; ============================================================

;; Decode an X10-encoded mouse event from raw cb/cx/cy bytes.
;; Extracted for testability.
;; Returns: (list 'mouse type ...) or #f
(define (decode-mouse-x10 cb cx cy)
  (define button-code (- cb 32))
  (define button (bitwise-and button-code #x03))
  (define motion? (= (bitwise-and button-code 32) 32))
  (define scroll? (= (bitwise-and button-code 64) 64))
  (define x (- cx 33))
  (define y (- cy 33))
  (cond
    ;; Scroll wheel
    [scroll? (list 'mouse (if (= button 0) 'scroll-up 'scroll-down) x y)]
    ;; Release: button=3 in X10 mode 1002.
    ;; In mode 1002, release has button bits = 3, motion bit = 0.
    [(= button 3) (list 'mouse 'release x y)]
    ;; Drag: motion bit set, button still held
    [(and motion? (<= button 2)) (list 'mouse 'drag x y)]
    ;; Click (press): no motion, no scroll
    [(and (<= button 2) (not motion?)) (list 'mouse 'click button x y)]
    [else #f]))

;; Slash commands
(define (input-slash-command text)
  ;; Returns #t if text starts with /
  (and (> (string-length text) 0) (char=? (string-ref text 0) #\/)))

;; Parse slash command from input text
;; Returns: symbol | (list symbol arg...) | #f
;; Simple commands: 'help | 'clear | 'compact | 'interrupt | 'quit | 'branches | 'leaves | 'unknown | #f
;; Commands with args: '(switch id) | '(children id)
(define (parse-tui-slash-command text)
  (define trimmed (string-trim text))
  (cond
    [(string=? trimmed "") #f]
    [(not (char=? (string-ref trimmed 0) #\/)) #f]
    [else
     ;; Split command and arguments
     (define parts (string-split trimmed))
     (define cmd (car parts))
     (define args (cdr parts))
     (cond
       [(member cmd '("/help" "/h" "/?")) 'help]
       [(member cmd '("/clear" "/cls")) 'clear]
       [(member cmd '("/compact")) 'compact]
       [(member cmd '("/interrupt" "/stop" "/cancel")) 'interrupt]
       [(member cmd '("/quit" "/exit" "/q")) 'quit]
       [(member cmd '("/branches")) 'branches]
       [(member cmd '("/leaves")) 'leaves]
       [(member cmd '("/switch"))
        (if (null? args)
            '(switch-error "Usage: /switch <branch-id>")
            `(switch ,(car args)))]
       [(member cmd '("/children"))
        (if (null? args)
            '(children-error "Usage: /children <node-id>")
            `(children ,(car args)))]
       [(member cmd '("/model"))
        (if (null? args)
            'model
            `(model ,(car args)))]
       [(member cmd '("/history")) 'history]
       [(member cmd '("/fork"))
        (if (null? args)
            'history ; /fork with no arg shows history as fallback
            `(fork ,(car args)))]
       [else 'unknown])]))
