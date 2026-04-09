#lang racket

;; interfaces/tui.rkt — TUI interface entry point
;;
;; Wires together:
;;   - Runtime event subscription → state updates
;;   - Terminal key input → input state updates
;;   - State → rendering → terminal drawing
;;   - Slash command dispatch
;;
;; This module is the ONLY TUI module that knows about both
;; the runtime and the terminal.

(require racket/async-channel
         racket/bytes
         racket/string
         "../tui/terminal.rkt"
         "../tui/state.rkt"
         "../tui/input.rkt"
         "../tui/render.rkt"
         (prefix-in renderer: "../tui/renderer.rkt")
         "../tui/layout.rkt"
         "../agent/types.rkt"
         "../agent/event-bus.rkt"
         "../runtime/agent-session.rkt"
         "../runtime/session-index.rkt"
         "../tui/scrollback.rkt")

;; ============================================================
;; SGR bg=0 → default bg post-processor
;; ============================================================
;; tui-ubuf stores bg=0 (ANSI black) in cells.
;; vt-output emits SGR parameter 40 for bg=0, which can appear in
;; compound sequences like \e[37;40m or \e[37;40;1m.
;; We post-process the output to replace parameter 40 with 49
;; (terminal default background), matching pi's behavior.

;; Replace SGR parameter "40" (bg=black) with "49" (default bg)
;; in all SGR escape sequences within a string.
;; Handles compound sequences: \e[37;40m → \e[37;49m
;; Skips extended color parameters: \e[38;5;40m stays unchanged
(define sgr-pattern
  (regexp (format "~a\\[([0-9;]*)m" (integer->char 27))))

;; Parse SGR parameters and replace bg=black (40) with default (49).
;; Skips extended color sequences (38;5;N, 48;5;N, 38;2;R;G;B, 48;2;R;G;B).
(define (replace-bg-black-params params)
  (define parts (string-split params ";"))
  (define result (list))
  (define i 0)
  (define n (length parts))
  (let loop ()
    (when (< i n)
      (define p (list-ref parts i))
      (cond
        ;; Extended fg: 38;5;N or 38;2;R;G;B — skip all
        [(and (equal? p "38") (< (+ i 1) n))
         (define next (list-ref parts (+ i 1)))
         (cond
           [(and (equal? next "5") (< (+ i 2) n))
            ;; 38;5;N — consume 3 params
            (set! result (append result (list p next (list-ref parts (+ i 2)))))
            (set! i (+ i 3))]
           [(and (equal? next "2") (>= (- n i) 5))
            ;; 38;2;R;G;B — consume 5 params
            (set! result (append result (take (drop parts i) 5)))
            (set! i (+ i 5))]
           [else
            (set! result (append result (list p)))
            (set! i (+ i 1))])]
        ;; Extended bg: 48;5;N or 48;2;R;G;B — skip all
        [(and (equal? p "48") (< (+ i 1) n))
         (define next (list-ref parts (+ i 1)))
         (cond
           [(and (equal? next "5") (< (+ i 2) n))
            ;; 48;5;N — consume 3 params
            (set! result (append result (list p next (list-ref parts (+ i 2)))))
            (set! i (+ i 3))]
           [(and (equal? next "2") (>= (- n i) 5))
            ;; 48;2;R;G;B — consume 5 params
            (set! result (append result (take (drop parts i) 5)))
            (set! i (+ i 5))]
           [else
            (set! result (append result (list p)))
            (set! i (+ i 1))])]
        ;; bg=black → default
        [(equal? p "40")
         (set! result (append result (list "49")))
         (set! i (+ i 1))]
        [else
         (set! result (append result (list p)))
         (set! i (+ i 1))]))
    (when (< i n) (loop)))
  (string-join result ";"))

(define (fix-sgr-bg-black str)
  (regexp-replace* sgr-pattern str
    (lambda (whole-match params)
      (if (or (not params) (equal? params ""))
          whole-match
          (string-append (string (integer->char 27)) "["
                         (replace-bg-black-params params) "m")))))

;; Render ubuf to terminal with bg=0 replaced by terminal default bg.
;; Captures display-ubuf! output, post-processes SGR sequences,
;; then writes the fixed output to the real terminal.
(define (render-ubuf-to-terminal! ubuf)
  (define out (open-output-bytes))
  (display-ubuf! ubuf out #:only-dirty #f #:linear #f)
  (define bs (get-output-bytes out))
  (define str (bytes->string/utf-8 bs))
  (display (fix-sgr-bg-black str) (current-output-port)))

;; Import tui-ubuf for output buffering (dynamically with fallback)
(define tui-ubuf-available?
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (collection-path "tui")
    #t))

(define make-ubuf-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'make-ubuf))))

(define ubuf-clear!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'ubuf-clear!))))

(define ubuf-putstring!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf 'ubuf-putstring!))))

(define display-ubuf!-fn
  (and tui-ubuf-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/ubuf/output 'display-ubuf!))))

;; Fallback stubs for when tui-ubuf is not available
(define (stub-make-ubuf cols rows) (make-hash `((cols . ,cols) (rows . ,rows))))
(define (stub-ubuf-clear! ubuf) (void))
(define (stub-ubuf-putstring! ubuf col row str . attrs) (void))
(define (stub-display-ubuf! ubuf port . opts) (void))

(define make-ubuf (or make-ubuf-fn stub-make-ubuf))
(define ubuf-clear! (or ubuf-clear!-fn stub-ubuf-clear!))
(define ubuf-putstring! (or ubuf-putstring!-fn stub-ubuf-putstring!))
(define display-ubuf! (or display-ubuf!-fn stub-display-ubuf!))

(provide
 ;; Main entry
 run-tui
 run-tui-with-runtime

 ;; For testing
 make-tui-ctx
 fix-sgr-bg-black
 render-ubuf-to-terminal!
 tui-ctx?
 tui-ctx-ui-state-box
 tui-ctx-input-state-box
 tui-ctx-event-bus
 tui-ctx-session-runner
 tui-ctx-running-box
 tui-ctx-event-ch
 tui-ctx-needs-redraw-box
 tui-ctx-term-box
 handle-key
 handle-mouse
 decode-mouse-x10
 selection-text
 styled-line->text
 process-slash-command
 render-frame!
 draw-frame
 mark-dirty!
 tui-ctx-resize-ubuf!
 copy-text!
 copy-selection!
 current-clipboard-mode
 clipboard-backend-available?)

;; ============================================================
;; TUI context
;; ============================================================

;; Holds mutable references for the running TUI
(struct tui-ctx
  (ui-state-box      ; (boxof ui-state)
   input-state-box   ; (boxof input-state)
   event-bus         ; event-bus? or #f
   session-runner    ; (string -> void) — called with user prompts
   running-box       ; (boxof boolean) — set to #f to exit
   event-ch          ; async-channel — serializes runtime events into main loop (unbounded buffer)
   session-dir       ; (or/c path-string? #f) — session directory for index loading
   needs-redraw-box  ; (boxof boolean) — #t when state changed and frame needs redraw
   term-box          ; (boxof any) — terminal instance for tui-term
   ubuf-box          ; (boxof any) — ubuf buffer for output
   )
  #:transparent)

(define (make-tui-ctx #:event-bus [bus #f]
                      #:session-runner [runner (lambda (prompt) (void))]
                      #:session-dir [sess-dir #f])
  (tui-ctx (box (initial-ui-state))
           (box (initial-input-state))
           bus
           runner
           (box #t)
           (make-async-channel)
           sess-dir
           (box #t)     ; needs-redraw: #t for first frame
           (box #f)     ; term-box - set when terminal opened
           (box #f)))   ; ubuf-box - set when buffer created

;; ============================================================
;; Terminal/ubuf lifecycle helpers
;; ============================================================

;; Initialize terminal and ubuf for a context
(define (tui-ctx-init-terminal! ctx)
  (define term (tui-term-open))
  (set-box! (tui-ctx-term-box ctx) term)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-ubuf cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf)
  ;; Configure renderer to use real ubuf operations
  (renderer:current-ubuf-clear ubuf-clear!)
  (renderer:current-ubuf-putstring ubuf-putstring!)
  ;; Enable mouse tracking for scroll wheel support
  (enable-mouse-tracking)
  term)

;; Resize ubuf when terminal size changes
(define (tui-ctx-resize-ubuf! ctx)
  (define-values (cols rows) (tui-screen-size))
  (define ubuf (make-ubuf cols rows))
  (set-box! (tui-ctx-ubuf-box ctx) ubuf))

;; Get current ubuf from context
(define (tui-ctx-ubuf ctx)
  (unbox (tui-ctx-ubuf-box ctx)))

;; Get current term from context
(define (tui-ctx-term ctx)
  (unbox (tui-ctx-term-box ctx)))

;; ============================================================
;; Frame rendering (ubuf-based)
;; ============================================================

;; Render the complete frame to the terminal using ubuf.
;; Hides cursor during redraw to prevent flicker, shows after.
;; Clears needs-redraw flag after drawing.
;;
;; This is the NEW ubuf-based implementation. The old draw-frame
;; is kept as a deprecated alias for backward compatibility.
(define (render-frame! ctx)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define ubuf (tui-ctx-ubuf ctx))
  (define term (tui-ctx-term ctx))
  
  (define-values (cols rows) (tui-screen-size))
  (define layout (compute-layout cols rows))
  
  ;; Render to ubuf (returns cursor position)
  (define-values (cursor-col cursor-row)
    (renderer:render-frame! ubuf state inp layout))
  
  ;; Display ubuf to terminal
  ;; Use only-dirty=#f for full redraw (all cells written) but NOT linear mode
  ;; — linear mode outputs \r\n between rows which scrolls the terminal,
  ;;   causing a ghost cursor one row below the input line.
  ;; — With only-dirty=#f + linear=#f, all cells are output using cursor
  ;;   positioning (\e[row;colH), which avoids scroll artifacts.
  (tui-cursor-hide)
  (render-ubuf-to-terminal! ubuf)
  
  ;; Position cursor at input location (renderer returns 0-indexed, ANSI is 1-indexed)
  (tui-cursor (+ cursor-col 1) (+ cursor-row 1))
  (tui-cursor-show)
  (tui-flush)
  
  (set-box! (tui-ctx-needs-redraw-box ctx) #f))

;; Deprecated: Old draw-frame is now an alias for render-frame!
;; Use render-frame! for new code.
(define (draw-frame ctx)
  (render-frame! ctx))

;; ============================================================
;; Message adapter (ONLY place with terminal details)
;; ============================================================

;; Message types returned by next-message:
;;   - (list 'key keycode)     — key press (char or symbol)
;;   - (list 'resize cols rows) — terminal resized
;;   - (list 'redraw)          — redraw requested
;;   - #f                      — no message (timeout)

;; Read the next message from the terminal.
;; This is the ONLY place that touches terminal input details.
;; All other code (update, handle-key) works with the abstract message.
(define (next-message ctx #:timeout [timeout 0.05])
  (define msg (tui-read-key #:timeout timeout))
  (cond
    ;; No input available
    [(not msg) #f]
    ;; Key message
    [(tkeymsg? msg)
     (define keycode (tui-keycode msg))
     (if keycode
         (list 'key keycode)
         #f)]
    ;; Resize message
    [(tsizemsg? msg)
     (list 'resize (tsizemsg-cols msg) (tsizemsg-rows msg))]
    ;; Mouse message — decode from X10 protocol bytes
    ;; In button-event mode (1002), we get press, drag (motion), and release.
    ;; X10 encoding: button-code = cb - 32
    ;;   bits 0-1: button (0=left, 1=middle, 2=right, 3=release/no button)
    ;;   bit 2: shift
    ;;   bit 3: meta
    ;;   bit 4: control
    ;;   bit 5: motion (1=mouse moved while button held)
    ;;   bit 6: scroll wheel (64)
    [(tmousemsg? msg)
     (decode-mouse-x10 (tmousemsg-cb msg) (tmousemsg-cx msg) (tmousemsg-cy msg))]
    ;; Command message (redraw, etc.)
    [(tcmdmsg? msg)
     (case (tcmdmsg-cmd msg)
       [(redraw) (list 'redraw)]
       [else #f])]
    ;; Unknown message type
    [else #f]))

;; ============================================================
;; Key handling
;; ============================================================

;; Mark that the frame needs redraw.
(define (mark-dirty! ctx)
  (set-box! (tui-ctx-needs-redraw-box ctx) #t))

;; Handle a single key event.
;; Returns: 'continue | 'quit | (list 'submit string) | (list 'command symbol)
(define (handle-key ctx keycode)
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  ;; Any key that reaches here may change state — mark for redraw
  (mark-dirty! ctx)

  (cond
    ;; Regular character input
    [(char? keycode)
     (case keycode
       [(#\return #\newline)
        ;; Submit input
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (cond
          [(not text) 'continue]
          [(input-slash-command text)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown))]
          [else
           ;; Add user message to transcript
           (define user-entry (transcript-entry 'user text (current-inexact-milliseconds) (hash)))
           (set-box! (tui-ctx-ui-state-box ctx)
                     (add-transcript-entry state user-entry))
           (list 'submit text)])]
       [(#\backspace #\rubout)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-backspace inp))
        'continue]
       [(#\u001b)
        'continue]
       [else
        ;; Regular printable character
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-insert-char inp keycode))
        'continue])]

    ;; Symbol keys (arrows, function keys, etc.)
    [(symbol? keycode)
     (case keycode
       ;; Enter key — charterm sends 'return symbol, not #\return char
       [(return kp-return enter kp-enter)
        ;; Submit input (same logic as #\return/#\newline in char branch)
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (cond
          [(not text) 'continue]
          [(input-slash-command text)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown))]
          [else
           ;; Add user message to transcript
           (define user-entry (transcript-entry 'user text (current-inexact-milliseconds) (hash)))
           (set-box! (tui-ctx-ui-state-box ctx)
                     (add-transcript-entry state user-entry))
           (list 'submit text)])]
       [(left kp-left)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-cursor-left inp))
        'continue]
       [(right kp-right)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-cursor-right inp))
        'continue]
       [(up kp-up)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-history-up inp))
        'continue]
       [(down kp-down)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-history-down inp))
        'continue]
       [(backspace)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-backspace inp))
        'continue]
       [(delete kp-delete)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-delete inp))
        'continue]
       [(home kp-home)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-home inp))
        'continue]
       [(end kp-end)
        (set-box! (tui-ctx-input-state-box ctx)
                  (input-end inp))
        'continue]
       [(escape)
        'continue]
       [(page-up pgup kp-pgup)
        ;; Page up: scroll by one viewport height (page)
        (define-values (_cols rows) (tui-screen-size))
        (define layout (compute-layout _cols rows))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (scroll-up state (max 1 (tui-layout-transcript-height layout))))
        'continue]
       [(page-down pgdn kp-pgdn)
        ;; Page down: scroll by one viewport height (page)
        (define-values (_cols2 rows2) (tui-screen-size))
        (define layout2 (compute-layout _cols2 rows2))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (scroll-down state (max 1 (tui-layout-transcript-height layout2))))
        'continue]
       [(ctrl-c)
        ;; Ctrl-C: copy selection to clipboard
        (when (has-selection? state)
          (define text (selection-text ctx state))
          (when (and text (not (string=? text "")))
            (copy-text! text)))
        'continue]
       [else 'continue])]

    [else 'continue]))

;; Handle a mouse event.
;; msg-data is (list type x y) or (list type button x y)
;; Returns 'continue.
(define (handle-mouse ctx msg-data)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define mouse-type (car msg-data))
  (mark-dirty! ctx)
  (case mouse-type
    [(scroll-up)
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-up state 3))
     'continue]
    [(scroll-down)
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-down state 3))
     'continue]
    [(click)
     ;; Start selection: set anchor at click position
     (define button (cadr msg-data))
     (define x (caddr msg-data))
     (define y (cadddr msg-data))
     (when (= button 0)  ;; left click only
       (set-box! (tui-ctx-ui-state-box ctx)
                 (set-selection-anchor state x y)))
     'continue]
    [(drag)
     ;; Update selection end during drag
     (define x (cadr msg-data))
     (define y (caddr msg-data))
     (when (has-selection? state)
       (set-box! (tui-ctx-ui-state-box ctx)
                 (set-selection-end state x y)))
     'continue]
    [(release)
     ;; Copy selection to clipboard (platform tool + OSC 52 fallback).
     ;; Do NOT call set-selection-end here — the drag handler already
     ;; tracks sel-end correctly, and right-click releases would
     ;; corrupt the selection if we moved the endpoint.
     (when (has-selection? state)
       (define text (selection-text ctx state))
       (when (and text (not (string=? text "")))
         (copy-text! text)))
     'continue]
    [else 'continue]))

;; Extract plain text from the current selection.
;; Uses rendered lines to map screen coordinates to text.
(define (selection-text ctx state)
  (define anchor (ui-state-sel-anchor state))
  (define end (ui-state-sel-end state))
  (and anchor end
       (let ()
         ;; Normalize so start <= end
         (define-values (start-col start-row end-col end-row)
           (normalize-selection-range anchor end))
         ;; Get rendered lines for the transcript area
         (define-values (cols rows) (tui-screen-size))
         (define layout (compute-layout cols rows))
         (define trans-y (tui-layout-transcript-start-row layout))
         (define trans-height (tui-layout-transcript-height layout))
         (define all-lines (render-transcript state trans-height cols))
         ;; Map screen rows to line indices (screen row → rendered line index)
         ;; Mouse y is 0-based: row 0 = header, row 1 = first transcript line.
         ;; trans-y is 1-based: value 2 means transcript starts at screen row 1 (0-based).
         ;; So: line-index = screen-row - (trans-y - 1)
         (define start-idx (max 0 (- start-row (sub1 trans-y))))
         (define end-idx (min (sub1 (length all-lines)) (- end-row (sub1 trans-y))))
         (if (> start-idx end-idx)
             ""
             (string-join
              (for/list ([i (in-range start-idx (add1 end-idx))])
                (define line (list-ref all-lines i))
                (define text (styled-line->text line))
                (cond
                  [(= i start-idx end-idx)
                   ;; Single line: extract column range
                   (substring text (min start-col (string-length text))
                              (min (add1 end-col) (string-length text)))]
                  [(= i start-idx)
                   ;; First line: from start-col to end
                   (substring text (min start-col (string-length text)))]
                  [(= i end-idx)
                   ;; Last line: from 0 to end-col
                   (substring text 0 (min (add1 end-col) (string-length text)))]
                  [else text]))
              "\n")))))

;; Extract plain text from a styled-line
(define (styled-line->text sl)
  (apply string-append (map styled-segment-text (styled-line-segments sl))))

;; ============================================================
;; Slash command processing
;; ============================================================

;; Process a slash command. Returns 'continue | 'quit
;; cmd can be: symbol | (list symbol args...)
(define (process-slash-command ctx cmd)
  (mark-dirty! ctx) ; defensive: slash commands always change state
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  ;; Handle structured commands (lists)
  (cond
    [(list? cmd)
     (case (car cmd)
       [(switch) (handle-switch-command ctx (cadr cmd))]
       [(children) (handle-children-command ctx (cadr cmd))]
       [(switch-error children-error)
        (define entry (transcript-entry 'error (cadr cmd) 0 (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry state entry))
        'continue]
       [else 'continue])]
    ;; Handle simple symbol commands
    [else
     (case cmd
       [(help)
        (define help-text "Commands: /help /clear /compact /interrupt /quit /branches /leaves /children /switch")
        (define entry (transcript-entry 'system help-text 0 (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry state entry))
        'continue]
       [(clear)
        (set-box! (tui-ctx-ui-state-box ctx)
                  (struct-copy ui-state state [transcript '()]))
        'continue]
       [(compact)
        ;; Compact: add status message and notify runtime
        (define entry (transcript-entry 'system "[compact requested]" 0 (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry state entry))
        (when (tui-ctx-event-bus ctx)
          (publish! (tui-ctx-event-bus ctx)
                    (make-event "compact.requested"
                                (exact-truncate (/ (current-inexact-milliseconds) 1000))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        'continue]
       [(interrupt)
        ;; Interrupt: notify runtime
        (when (tui-ctx-event-bus ctx)
          (publish! (tui-ctx-event-bus ctx)
                    (make-event "interrupt.requested"
                                (exact-truncate (/ (current-inexact-milliseconds) 1000))
                                (or (ui-state-session-id state) "")
                                #f
                                (hash))))
        (define entry (transcript-entry 'system "[interrupt requested]" (current-inexact-milliseconds) (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry state entry))
        'continue]
       [(branches) (handle-branches-command ctx)]
       [(leaves) (handle-leaves-command ctx)]
       [(quit)
        (set-box! (tui-ctx-running-box ctx) #f)
        'quit]
       [(unknown)
        (define entry (transcript-entry 'error "Unknown command. Type /help for commands." 0 (hash)))
        (set-box! (tui-ctx-ui-state-box ctx)
                  (add-transcript-entry state entry))
        'continue]
       [else 'continue])]))

;; ============================================================
;; Branch inspection command handlers
;; ============================================================

;; Build branch-info structs from session index
(define (build-branch-info-list idx)
  (if (not idx)
      '()
      (let ([leaves (map message-id (leaf-nodes idx))]
            [entries (vector->list (session-index-entry-order idx))])
        (for/list ([msg (in-list entries)])
          (branch-info
           (message-id msg)
           (message-parent-id msg)
           (message-role msg)
           (member (message-id msg) leaves)
           #f)))))  ; active? will be set separately

;; Mark active branch in the list
(define (mark-active-branch branches active-id)
  (for/list ([b (in-list branches)])
    (struct-copy branch-info b [active? (equal? (branch-info-id b) active-id)])))

;; Get session index from current session (if available)
(define (get-session-index ctx)
  (define dir (tui-ctx-session-dir ctx))
  (if dir
      (with-handlers ([exn:fail? (lambda (e) #f)])
        (load-index dir))
      #f))

;; Handle /branches command
(define (handle-branches-command ctx)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define idx (get-session-index ctx))
  (define branches (build-branch-info-list idx))
  (define active-id (or (ui-state-current-branch state)
                        (and (not (null? branches))
                             (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render branch list as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-branch-list branches-with-active cols))
  (define new-state
    (for/fold ([s state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (tui-ctx-ui-state-box ctx)
            (set-visible-branches new-state branches-with-active))
  'continue)

;; Handle /leaves command
(define (handle-leaves-command ctx)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define idx (get-session-index ctx))
  (define branches (build-branch-info-list idx))
  (define active-id (or (ui-state-current-branch state)
                        (and (not (null? branches))
                             (branch-info-id (last branches)))))
  (define branches-with-active (mark-active-branch branches active-id))
  ;; Render leaf nodes as transcript entries
  (define-values (cols rows) (tui-screen-size))
  (define lines (render-leaf-nodes branches-with-active cols))
  (define new-state
    (for/fold ([s state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (tui-ctx-ui-state-box ctx) new-state)
  'continue)

;; Handle /switch <id> command
(define (handle-switch-command ctx branch-id)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define idx (get-session-index ctx))
  (define entry (if (and idx (lookup-entry idx branch-id))
                    (transcript-entry 'system
                                      (format "[switched to branch: ~a]" branch-id)
                                      0
                                      (hash 'branch-id branch-id))
                    (transcript-entry 'error
                                      (format "Branch not found: ~a" branch-id)
                                      0
                                      (hash))))
  (define new-state (add-transcript-entry state entry))
  (when (and idx (lookup-entry idx branch-id))
    (set-box! (tui-ctx-ui-state-box ctx)
              (set-current-branch new-state branch-id)))
  'continue)

;; Handle /children <id> command
(define (handle-children-command ctx node-id)
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  (define idx (get-session-index ctx))
  (define-values (lines new-state)
    (if (not idx)
        (values (list (styled-line (list (styled-segment "  No session index available" '(dim)))))
                state)
        (let ([children-msgs (children-of idx node-id)])
          (if (null? children-msgs)
              (values (list (styled-line (list (styled-segment (format "  Node ~a has no children" node-id) '(dim)))))
                      state)
              (let*-values ([(cols rows) (tui-screen-size)])
                (let* ([children-info
                        (for/list ([msg (in-list children-msgs)])
                          (branch-info
                           (message-id msg)
                           node-id
                           (message-role msg)
                           (null? (children-of idx (message-id msg)))
                           #f))]
                       [rendered (render-children-list node-id children-info cols)])
                  (values rendered state)))))))
  ;; Add lines to transcript
  (define final-state
    (for/fold ([s new-state])
              ([line (in-list lines)])
      (add-transcript-entry s (transcript-entry 'system
                                                  (styled-line->text line)
                                                  0
                                                  (hash)))))
  (set-box! (tui-ctx-ui-state-box ctx) final-state)
  'continue)

;; ============================================================
;; Runtime event subscription
;; ============================================================

(define (subscribe-runtime-events! ctx)
  (define bus (tui-ctx-event-bus ctx))
  (define ch (tui-ctx-event-ch ctx))
  (when bus
    (subscribe! bus
      (lambda (evt)
        ;; async-channel-put is non-blocking (unbounded buffer)
        (async-channel-put ch evt)))))

;; ============================================================
;; Main TUI loop
;; ============================================================

(define (tui-main-loop ctx)
  (let loop ()
    ;; Drain all pending events from the channel (non-blocking)
    (drain-events! ctx)
    
    ;; Poll for terminal resize (fallback for stub path where SIGWINCH
    ;; doesn't produce tsizemsg events). When the real tui-term library
    ;; is available, resize events also arrive via tsizemsg in next-message,
    ;; so this polling is a safety net that covers both paths.
    ;; Note: do NOT call tui-screen-size-cache-reset! here — it clears
    ;; screen-size-last-cols/rows to #f, which makes changed? return #t
    ;; on the next call, creating an infinite redraw loop.
    (when (tui-screen-size-changed?)
      (tui-ctx-resize-ubuf! ctx)
      (mark-dirty! ctx))
    
    ;; Draw the frame only when state changed
    (when (unbox (tui-ctx-needs-redraw-box ctx))
      (render-frame! ctx))
    
    (when (unbox (tui-ctx-running-box ctx))
      ;; Get next message from terminal (adapter pattern)
      ;; This is the ONLY place that touches terminal input details
      (define msg (next-message ctx #:timeout 0.05))
      
      (when msg
        (case (car msg)
          ;; Key press: dispatch to handler
          [(key)
           (define keycode (cadr msg))
           (define result (handle-key ctx keycode))
           (cond
             [(eq? result 'quit)
              (set-box! (tui-ctx-running-box ctx) #f)]
             [(and (list? result) (eq? (car result) 'submit))
              (define text (cadr result))
              ;; Submit to runtime (non-blocking)
              (define runner (tui-ctx-session-runner ctx))
              (thread (lambda () (runner text)))]
             [(and (list? result) (eq? (car result) 'command))
              (define cmd (cadr result))
              (process-slash-command ctx cmd)]
             [else (void)])
           ] ;; (mark-dirty is called inside handle-key)
          
          ;; Resize event: resize ubuf and mark dirty
          [(resize)
           (define cols (cadr msg))
           (define rows (caddr msg))
           ;; Resize the ubuf to match new terminal size
           (tui-ctx-resize-ubuf! ctx)
           ;; Reset screen size cache so next query gets new values
           (tui-screen-size-cache-reset!)
           (mark-dirty! ctx)]
          
          ;; Redraw command: mark dirty
          [(redraw)
           (mark-dirty! ctx)]
          
          ;; Mouse event: dispatch to handler
          [(mouse)
           (handle-mouse ctx (cdr msg))]
          
          ;; Unknown message type - ignore
          [else (void)]))
      
      (when (unbox (tui-ctx-running-box ctx))
        (loop)))))

;; Drain all pending events from the channel and apply them to ui-state.
;; Uses sync/timeout for non-blocking drain.
;; Sets needs-redraw when events are processed.
(define (drain-events! ctx)
  (define ch (tui-ctx-event-ch ctx))
  (let loop ()
    (define evt (sync/timeout 0 ch))
    (when evt
      (define state (unbox (tui-ctx-ui-state-box ctx)))
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (log-warning "TUI: error processing event: ~a" (exn-message e)))])
        (set-box! (tui-ctx-ui-state-box ctx)
                  (apply-event-to-state state evt)))
      (set-box! (tui-ctx-needs-redraw-box ctx) #t)
      (loop))))

;; ============================================================
;; Public entry points
;; ============================================================

;; Run TUI with a runtime config hash and cli-config
(define (run-tui-with-runtime rt-config cli-cfg)
  ;; rt-config is a hash from build-runtime-from-cli
  ;; cli-cfg is a cli-config struct
  (define bus (hash-ref rt-config 'event-bus #f))
  (define sess (make-agent-session rt-config))

  (define ctx (make-tui-ctx
               #:event-bus bus
               #:session-runner
               (lambda (prompt)
                 (run-prompt! sess prompt))))

  ;; Subscribe to events
  (subscribe-runtime-events! ctx)

  ;; Determine scrollback file path from session dir
  (define sess-dir (or (hash-ref rt-config 'session-dir #f)
                       (hash-ref rt-config 'store-dir #f)))
  (define scrollback-path (and sess-dir
                                (build-path sess-dir "scrollback.jsonl")))

  ;; Set initial session info, loading scrollback if available
  (define base-state
    (initial-ui-state
     #:session-id (session-id sess)
     #:model-name (hash-ref rt-config 'model-name #f)))
  (define init-state
    (if scrollback-path
        (let ([loaded (load-scrollback scrollback-path)])
          (if (null? loaded)
              base-state
              (struct-copy ui-state base-state [transcript loaded])))
        base-state))
  (set-box! (tui-ctx-ui-state-box ctx) init-state)

  ;; Initialize terminal, run main loop, cleanup on exit
  (tui-ctx-init-terminal! ctx)
  (with-handlers ([exn:break? (lambda (e) (void))]
                  [exn:fail? (lambda (e)
                               ;; Try cleanup before re-raising
                               (with-handlers ([exn:fail? (lambda (_) (void))])
                                 (tui-term-close (unbox (tui-ctx-term-box ctx))))
                               (raise e))])
    (tui-main-loop ctx))
  ;; Cleanup terminal BEFORE printing Goodbye
  ;; (so newline works correctly and we're on normal screen)
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  ;; Save scrollback
  (when scrollback-path
    (with-handlers ([exn:fail? (lambda (e) (void))])
      (let* ([state (unbox (tui-ctx-ui-state-box ctx))]
             [transcript (ui-state-transcript state)])
        (when (not (null? transcript))
          (save-scrollback transcript scrollback-path)))))
  (displayln "Goodbye."))

;; Simple TUI run (for testing without full runtime)
(define (run-tui #:session-runner [session-runner (lambda (prompt) (void))]
                 #:event-bus [bus #f])
  (define ctx (make-tui-ctx
               #:event-bus bus
               #:session-runner session-runner))
  (tui-ctx-init-terminal! ctx)
  (with-handlers ([exn:break? (lambda (e) (void))]
                  [exn:fail? (lambda (e)
                               (with-handlers ([exn:fail? (lambda (_) (void))])
                                 (tui-term-close (unbox (tui-ctx-term-box ctx))))
                               (raise e))])
    (tui-main-loop ctx))
  (with-handlers ([exn:fail? (lambda (e) (void))])
    (disable-mouse-tracking)
    (tui-term-close (unbox (tui-ctx-term-box ctx))))
  (displayln "Goodbye."))
