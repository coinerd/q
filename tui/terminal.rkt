#lang racket

(require "clipboard.rkt")

;; tui/terminal.rkt — Thin terminal I/O adapter using tui-term
;;
;; This module replaces the charterm-based adapter with tui-term.
;; It provides the same API as the original terminal.rkt so that
;; the rest of the TUI continues to work without changes.
;;
;; tui-term handles UTF-8 internally, so we don't need the UTF-8
;; accumulator logic that was required for charterm.
;;
;; NOTE: tui-term is a planned library. Currently, this module provides
;; the API contract and stubs. When tui-term is available, the stubs
;; can be replaced with actual implementations.

;; Stub structures for tui-term messages (used when library not available)
;; Stub message structs (fallback when tui-term unavailable)
;; Now using vectors for simplicity since tui-term is broken on Racket 8.10

;; Dynamic require tui-term if available
(define tui-term-available?
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (collection-path "tui")
    #t))

;; Dynamic requires (will be #f if not available)
(define make-tty-term-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'make-tty-term))))

(define term-alternate-screen-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-alternate-screen))))

(define term-normal-screen-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-normal-screen))))

(define term-hide-cursor-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-hide-cursor))))

(define term-show-cursor-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-show-cursor))))

(define term-close-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-close))))

(define current-term-size-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'current-term-size))))

(define read-msg-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'read-msg))))

(define byte-ready-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'byte-ready?))))

;; Dynamic require for message struct predicates and accessors
(define tkeymsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tkeymsg?))))

(define tkeymsg-key-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tkeymsg-key))))

(define tsizemsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg?))))

(define tsizemsg-cols-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg-cols))))

(define tsizemsg-rows-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg-rows))))

(define tcmdmsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tcmdmsg?))))

(define tcmdmsg-cmd-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tcmdmsg-cmd))))

(provide
 ;; Lifecycle
 tui-term-open
 tui-term-close
 with-tui-terminal

 ;; Query
 tui-screen-size
 tui-screen-size-cache-reset!
 tui-screen-size-changed?

 ;; Drawing (compatibility - delegated to current term)
 tui-clear-screen
 tui-cursor
 tui-display
 tui-newline
 tui-clear-line-right
 tui-flush

 ;; Cursor visibility
 tui-cursor-hide
 tui-cursor-show

 ;; Styles (compatibility)
 tui-normal
 tui-bold
 tui-inverse
 tui-underline
 tui-dim
 tui-fg

 ;; Input
 tui-read-key
 tui-byte-ready?

 ;; Mouse tracking
 enable-mouse-tracking
 disable-mouse-tracking

 ;; Clipboard
 clipboard-copy
 osc-52-copy ;; internal, exported for testing
 detect-clipboard-tool ;; exported for testing
 clipboard-copy-via-tool ;; exported for testing
 copy-text!
 copy-selection!
 clipboard-backend-available?
 current-clipboard-mode

 ;; Mouse events
 tmousemsg?
 tmousemsg-cb
 tmousemsg-cx
 tmousemsg-cy

 ;; Key helpers
 tui-key-char?
 tui-key-char
 tui-key-symbol
 tui-keycode

 ;; Message predicates and accessors (adapter pattern)
 tkeymsg?
 tkeymsg-key
 tsizemsg?
 tsizemsg-cols
 tsizemsg-rows
 tcmdmsg?
 tcmdmsg-cmd

 ;; UTF-8 support (compatibility stubs - tui-term handles UTF-8 internally)
 utf8-high-byte?
 utf8-lead-byte-count
 utf8-continuation-byte?
 reassemble-utf8-chars
 utf8-accumulate-char
 utf8-accumulator-reset!
 utf8-accumulator-length

 ;; CSI fragment detection (compatibility stubs - tui-term handles internally)
 get-csi-fragment-state
 csi-fragment-state-set!
 csi-fragment-reset!
 filter-csi-fragment
 csi-final-byte?)

;; ============================================================
;; tui-term stubs (when library not available)
;; ============================================================

(define (stub-make-tty-term #:tty [tty #f])
  ;; Put tty into raw mode so we can read individual keypresses.
  ;; Use system() so stty operates on the inherited terminal fd.
  ;; subprocess() doesn't inherit the controlling terminal properly.
  (with-handlers ([exn:fail? void])
    (system "stty raw -echo -icanon < /dev/tty 2>/dev/null"))
  (gensym 'stub-term))

(define (stub-term-alternate-screen term)
  (display "\x1b[?1049h")
  (flush-output))

(define (stub-term-normal-screen term)
  (display "\x1b[?1049l")
  (flush-output))

(define (stub-term-hide-cursor term)
  (display "\x1b[?25l")
  (flush-output))

(define (stub-term-show-cursor term)
  (display "\x1b[?25h")
  (flush-output))

(define (stub-term-close term)
  ;; Restore tty to sane mode
  (with-handlers ([exn:fail? void])
    (system "stty sane < /dev/tty 2>/dev/null")))

(define (stub-current-term-size)
  ;; Try environment variables first (set by shell in most terminals)
  (define env-cols (and (getenv "COLUMNS")
                        (string->number (getenv "COLUMNS"))))
  (define env-rows (and (getenv "LINES")
                        (string->number (getenv "LINES"))))
  (if (and env-cols env-rows (> env-cols 0) (> env-rows 0))
      (values env-cols env-rows)
      ;; Fallback: try stty size
      ;; Uses dynamic-wind to guarantee port cleanup and prevent fd leaks.
      ;; Previously leaked 3 fds per call → fd exhaustion after ~700 calls →
      ;; subprocess failed → hardcoded 80x24 fallback.
      (let ([result-cols 80] [result-rows 24])
        (define-values (sp stty-out stty-in stty-err)
          (with-handlers ([exn:fail? (lambda (e) (values #f #f #f #f))])
            (subprocess #f #f #f "/bin/sh" "-c" "stty size < /dev/tty 2>/dev/null")))
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (when stty-in (close-output-port stty-in))
            (when stty-out
              (let* ([out-str (port->string stty-out)]
                     [parts (string-split out-str)])
                (when (= (length parts) 2)
                  (set! result-cols (string->number (second parts)))
                  (set! result-rows (string->number (first parts)))))))
          (lambda ()
            (when stty-out (close-input-port stty-out))
            (when stty-err (close-input-port stty-err))
            (when sp (sync/timeout 1.0 sp))))
        (values result-cols result-rows))))

;; Real stdin-based input when tui-term is unavailable.
;; Reads raw bytes from stdin, decodes ANSI escape sequences,
;; and returns tkeymsg/tsizemsg structs (as vectors).
(define (real-stdin-read-msg #:timeout [timeout 0.20])
  (define in (current-input-port))
  (define result (sync/timeout timeout in))
  (if (not result)
      #f  ;; timeout, no input
      (let ([b (read-byte in)])
        (cond
          [(eof-object? b) #f]
          [(= b 27)  ;; ESC — could be escape sequence
           (sync/timeout 0.01 in)  ;; wait briefly for rest of sequence
           (if (char-ready? in)
               (let ([b2 (read-byte in)])
                 (cond
                   [(eof-object? b2) (make-tkeymsg-raw 'escape)]
                   [(= b2 91) (decode-csi-sequence in)]  ;; ESC[
                   [else (make-tkeymsg-raw (integer->char b2))]))  ;; Alt+key
               (make-tkeymsg-raw 'escape))]
          [(= b 13) (make-tkeymsg-raw 'return)]
          [(= b 10) (make-tkeymsg-raw 'return)]
          [(= b 127) (make-tkeymsg-raw 'backspace)]
          [(= b 8) (make-tkeymsg-raw 'backspace)]
          [(= b 3) (make-tkeymsg-raw 'ctrl-c)]  ;; Ctrl-C → copy selection
          ;; BUG-41: UTF-8 multi-byte sequence handling
          ;; Lead bytes 0xC0+ start a multi-byte sequence.
          ;; We use the existing utf8-accumulate-char accumulator
          ;; to collect continuation bytes and decode.
          [(>= b 192)
           (utf8-accumulator-reset!)
           (define lead-char (integer->char b))
           (define total-bytes (utf8-lead-byte-count b))
           (if (= total-bytes 1)
               ;; Shouldn't happen for >= 192, but defensive
               (make-tkeymsg-raw lead-char)
               ;; Start accumulation, read remaining bytes
               (let _loop ()
                 (define decoded (utf8-accumulate-char lead-char))
                 (cond
                   [decoded (make-tkeymsg-raw decoded)]
                   ;; Need more continuation bytes
                   [else
                    (sync/timeout 0.05 in)
                    (if (char-ready? in)
                        (let ([cb (read-byte in)])
                          (if (and (byte? cb) (utf8-continuation-byte? cb))
                              (begin
                                (set! lead-char (integer->char cb))
                                (_loop))
                              ;; Unexpected byte — reset and return what we have
                              (begin
                                (utf8-accumulator-reset!)
                                #f)))
                        ;; Timeout waiting for continuation — reset
                        (begin
                          (utf8-accumulator-reset!)
                          #f))])))]
          [(>= b 128) #f]  ;; Stray continuation byte (0x80-0xBF) — skip
          [(>= b 32) (utf8-accumulator-reset!) (make-tkeymsg-raw (integer->char b))]
          [else #f]))))

;; Decode a CSI sequence (ESC [ already consumed)
(define (decode-csi-sequence in)
  (sync/timeout 0.01 in)  ;; wait briefly for rest of sequence
  (define b (read-byte in))
  (cond
    [(eof-object? b) (make-tkeymsg-raw 'escape)]
    [(= b 65) (make-tkeymsg-raw 'up)]          ;; ESC[A
    [(= b 66) (make-tkeymsg-raw 'down)]        ;; ESC[B
    [(= b 67) (make-tkeymsg-raw 'right)]       ;; ESC[C
    [(= b 68) (make-tkeymsg-raw 'left)]        ;; ESC[D
    [(= b 72) (make-tkeymsg-raw 'home)]        ;; ESC[H
    [(= b 70) (make-tkeymsg-raw 'end)]         ;; ESC[F
    [(= b 80) (make-tkeymsg-raw 'f1)]          ;; ESC[P
    [(= b 81) (make-tkeymsg-raw 'f2)]          ;; ESC[Q
    [(= b 82) (make-tkeymsg-raw 'f3)]          ;; ESC[R
    [(= b 83) (make-tkeymsg-raw 'f4)]          ;; ESC[S
    ;; ESC[1~ = home, ESC[4~ = end, ESC[5~ = pgup, ESC[6~ = pgdn
    [(= b 49) (decode-csi-tilled in 'home)]   ;; ESC[1
    [(= b 52) (decode-csi-tilled in 'end)]    ;; ESC[4
    [(= b 53) (decode-csi-tilled in 'page-up)] ;; ESC[5
    [(= b 54) (decode-csi-tilled in 'page-down)] ;; ESC[6
    [(= b 50) (decode-csi-tilled in 'insert)] ;; ESC[2
    [(= b 51) (decode-csi-tilled in 'delete)] ;; ESC[3
    [(= b 77)  ;; ESC[M — X10 mouse event
     (sync/timeout 0.01 in)
     (define cb (read-byte in))
     (define cx (read-byte in))
     (define cy (read-byte in))
     (if (and (byte? cb) (byte? cx) (byte? cy))
         (make-tmousemsg-raw cb cx cy)
         (make-tkeymsg-raw 'escape))]
    [else (make-tkeymsg-raw 'escape)]))

;; Decode ESC[N~ style sequences (N already consumed)
(define (decode-csi-tilled in default-key)
  (sync/timeout 0.01 in)
  (define b2 (read-byte in))
  (cond
    [(and (byte? b2) (= b2 126)) (make-tkeymsg-raw default-key)]  ;; ~
    [(and (byte? b2) (= b2 59))  ;; ; — modifier like ESC[1;5A
     (sync/timeout 0.01 in)
     (read-byte in)  ;; skip modifier number
     (sync/timeout 0.01 in)
     (define b4 (read-byte in))
     (cond [(and (byte? b4) (= b4 65)) (make-tkeymsg-raw 'up)]
           [(and (byte? b4) (= b4 66)) (make-tkeymsg-raw 'down)]
           [(and (byte? b4) (= b4 67)) (make-tkeymsg-raw 'right)]
           [(and (byte? b4) (= b4 68)) (make-tkeymsg-raw 'left)]
           [(and (byte? b4) (= b4 72)) (make-tkeymsg-raw 'home)]
           [(and (byte? b4) (= b4 70)) (make-tkeymsg-raw 'end)]
           [else (make-tkeymsg-raw 'escape)])]
    [else (make-tkeymsg-raw 'escape)]))

;; Raw message constructors (compatible with tkeymsg?/tsizemsg? predicates)
;; When tui-term is unavailable, we use simple vectors as message structs
(define (make-tkeymsg-raw key)
  (vector 'tkeymsg key))

(define (make-tsizemsg-raw cols rows)
  (vector 'tsizemsg cols rows))

(define (make-tmousemsg-raw cb cx cy)
  (vector 'tmousemsg cb cx cy))

(define (tmousemsg? msg)
  (and (vector? msg) (eq? (vector-ref msg 0) 'tmousemsg)))

(define (tmousemsg-cb msg) (vector-ref msg 1))
(define (tmousemsg-cx msg) (vector-ref msg 2))
(define (tmousemsg-cy msg) (vector-ref msg 3))

(define (stub-byte-ready?)
  (char-ready? (current-input-port)))

;; ============================================================
;; Select actual or stub implementations
;; ============================================================

(define make-tty-term
  (or make-tty-term-fn stub-make-tty-term))

(define term-alternate-screen
  (or term-alternate-screen-fn stub-term-alternate-screen))

(define term-normal-screen
  (or term-normal-screen-fn stub-term-normal-screen))

(define term-hide-cursor
  (or term-hide-cursor-fn stub-term-hide-cursor))

(define term-show-cursor
  (or term-show-cursor-fn stub-term-show-cursor))

(define term-close
  (or term-close-fn stub-term-close))

(define current-term-size
  (or current-term-size-fn stub-current-term-size))

(define read-msg
  (or read-msg-fn real-stdin-read-msg))

(define byte-ready?
  (or byte-ready-fn stub-byte-ready?))

;; Message struct bindings — dispatch to real tui-term or vector fallback
(define (tkeymsg? msg)
  (or (and tkeymsg?-fn (tkeymsg?-fn msg))
      (and (vector? msg) (eq? (vector-ref msg 0) 'tkeymsg))))
(define (tkeymsg-key msg)
  (if (and tkeymsg-key-fn (tkeymsg?-fn msg))
      (tkeymsg-key-fn msg)
      (vector-ref msg 1)))
(define (tsizemsg? msg)
  (or (and tsizemsg?-fn (tsizemsg?-fn msg))
      (and (vector? msg) (eq? (vector-ref msg 0) 'tsizemsg))))
(define (tsizemsg-cols msg)
  (if (and tsizemsg-cols-fn (tsizemsg?-fn msg))
      (tsizemsg-cols-fn msg)
      (vector-ref msg 1)))
(define (tsizemsg-rows msg)
  (if (and tsizemsg-rows-fn (tsizemsg?-fn msg))
      (tsizemsg-rows-fn msg)
      (vector-ref msg 2)))
(define (tcmdmsg? msg)
  (or (and tcmdmsg?-fn (tcmdmsg?-fn msg))
      (and (vector? msg) (eq? (vector-ref msg 0) 'tcmdmsg))))
(define (tcmdmsg-cmd msg)
  (if (and tcmdmsg-cmd-fn (tcmdmsg?-fn msg))
      (tcmdmsg-cmd-fn msg)
      (vector-ref msg 1)))

;; ============================================================
;; Lifecycle
;; ============================================================

;; Open terminal for TUI use.
;; Opens tty, enables alternate screen, hides cursor.
(define (tui-term-open #:tty [tty #f])
  (define term (make-tty-term #:tty tty))
  (term-alternate-screen term)
  (term-hide-cursor term)
  term)

;; Close terminal and restore state.
;; Shows cursor, restores normal screen, closes tty.
(define (tui-term-close term)
  (term-show-cursor term)
  (term-normal-screen term)
  (term-close term))

;; Macro for safe terminal usage with cleanup.
(define-syntax-rule (with-tui-terminal body ...)
  (let ([term (make-tty-term)])
    (dynamic-wind
      (lambda () (term-alternate-screen term) (term-hide-cursor term))
      (lambda () body ...)
      (lambda ()
        (with-handlers ([exn:fail? (lambda (e) (void))])
          (term-show-cursor term)
          (term-normal-screen term))))))

;; ============================================================
;; Screen size (cached)
;; ============================================================

;; Cache screen size to avoid flooding the terminal with queries.
(define screen-size-cache-cols #f)
(define screen-size-cache-rows #f)
(define screen-size-cache-time 0)
(define screen-size-cache-ttl 0.1)  ;; re-query every 100ms for responsive resize
(define screen-size-last-cols #f)  ;; for resize detection
(define screen-size-last-rows #f)

(define (tui-screen-size-cache-reset!)
  (set! screen-size-cache-cols #f)
  (set! screen-size-cache-rows #f)
  (set! screen-size-cache-time 0)
  (set! screen-size-last-cols #f)
  (set! screen-size-last-rows #f))

;; Query screen size → (values cols rows)
;; Caches result for screen-size-cache-ttl seconds.
(define (tui-screen-size)
  (define now (current-inexact-milliseconds))
  (if (and screen-size-cache-cols
           screen-size-cache-rows
           (< (- now screen-size-cache-time) (* screen-size-cache-ttl 1000.0)))
      (values screen-size-cache-cols screen-size-cache-rows)
      (let-values ([(cols rows) (current-term-size)])
        (set! screen-size-cache-cols cols)
        (set! screen-size-cache-rows rows)
        (set! screen-size-cache-time now)
        (values cols rows))))

;; Check if the screen size has changed since the last call to this function.
;; Returns #t on first call or when cols/rows differ from last known values.
;; Used to trigger redraw on terminal resize during idle.
(define (tui-screen-size-changed?)
  (define-values (cols rows) (tui-screen-size))
  (cond
    [(not screen-size-last-cols)
     ;; First call
     (set! screen-size-last-cols cols)
     (set! screen-size-last-rows rows)
     #t]
    [(and (= cols screen-size-last-cols) (= rows screen-size-last-rows))
     #f]
    [else
     (set! screen-size-last-cols cols)
     (set! screen-size-last-rows rows)
     #t]))

;; ============================================================
;; Drawing primitives
;; ============================================================

;; These use direct ANSI escape sequences for compatibility.
;; When tui-term is fully available, these can delegate to term-* functions.

(define (tui-cursor-hide)
  (display "\x1b[?25l")
  (flush-output))

(define (tui-cursor-show)
  (display "\x1b[?25h")
  (flush-output))

(define (tui-clear-screen)
  (display "\x1b[2J")
  (display "\x1b[H")
  (flush-output))

(define (tui-cursor col row)
  (display (format "\x1b[~a;~aH" row col))
  (flush-output))

(define (tui-display text)
  (display text)
  (flush-output))

(define (tui-newline)
  (newline)
  (flush-output))

(define (tui-clear-line-right)
  (display "\x1b[0K")
  (flush-output))

(define (tui-flush)
  (flush-output))

;; ============================================================
;; Styles (using ANSI escape sequences)
;; ============================================================

(define (tui-normal)
  (display "\x1b[0m"))

(define (tui-bold)
  (display "\x1b[1m"))

(define (tui-inverse)
  (display "\x1b[7m"))

(define (tui-underline)
  (display "\x1b[4m"))

(define (tui-dim)
  (display "\x1b[2m"))

(define (tui-fg color)
  (define code
    (case color
      [(black) 30] [(red) 31] [(green) 32] [(yellow) 33]
      [(blue) 34] [(magenta) 35] [(cyan) 36] [(white) 37]
      [else 37]))
  (display (format "\x1b[~am" code)))

;; ============================================================
;; Input (message-based)
;; ============================================================

;; Read one key event from tui-term. Returns keyinfo or #f.
;; tui-term uses read-msg which returns tkeymsg, tsizemsg, etc.
(define (tui-read-key #:timeout [timeout 0.20])
  (read-msg #:timeout timeout))

;; Keycode mapping from tui-term symbols to our symbols
(define (map-tui-key tui-key)
  (case tui-key
    [(up) 'up]
    [(down) 'down]
    [(left) 'left]
    [(right) 'right]
    [(home) 'home]
    [(end) 'end]
    [(pgup) 'page-up]
    [(pgdn) 'page-down]
    [(return) 'return]
    [(backspace) 'backspace]
    [(delete) 'delete]
    [(escape) 'escape]
    [else tui-key]))

;; Extract keycode from keyinfo message.
;; Returns:
;;   char? or symbol? — the decoded key
;;   #f — not a key message, skip
(define (tui-keycode msg)
  (cond
    [(tkeymsg? msg)
     (define key (tkeymsg-key msg))
     (if (symbol? key)
         (map-tui-key key)
         key)]
    [else #f]))

;; Check if a byte is ready (input available)
(define (tui-byte-ready?)
  (byte-ready?))

;; ============================================================
;; Mouse tracking — X10 protocol
;; ============================================================

(define (enable-mouse-tracking)
  ;; Button-event tracking (mode 1002): reports press, drag, and release.
  ;; This is needed for text selection via click-drag.
  ;; We do NOT use SGR-Pixel (1006) — only X10 format is decoded.
  (display "\x1b[?1000h")  ;; basic tracking as fallback
  (display "\x1b[?1002h")  ;; button-event tracking (press + drag + release)
  (flush-output))

(define (disable-mouse-tracking)
  (display "\x1b[?1002l")
  (display "\x1b[?1000l")
  (flush-output))

;; Clipboard functions are now in clipboard.rkt
;; Re-exported for backward compatibility:
;;   detect-clipboard-tool, clipboard-copy-via-tool, osc-52-copy
;; clipboard-copy is replaced by copy-text! from clipboard.rkt

;; Backward-compatible wrapper: clipboard-copy calls copy-text!
(define (clipboard-copy text)
  (copy-text! text))

;; ============================================================
;; Key helpers (pure)
;; ============================================================

(define (tui-key-char? keycode)
  (char? keycode))

(define (tui-key-char keycode)
  keycode)

(define (tui-key-symbol keycode)
  keycode)

;; ============================================================
;; UTF-8 support (compatibility stubs)
;; ============================================================
;; These functions were used with charterm which required manual
;; UTF-8 handling. tui-term handles UTF-8 internally, so these
;; are provided as stubs for backward compatibility.

;; Check if a char represents a byte > 127 (UTF-8 lead or continuation)
(define (utf8-high-byte? ch)
  (and (char? ch)
       (> (char->integer ch) 127)))

;; Determine the number of bytes in a UTF-8 sequence from the lead byte value.
;; Returns 1 for ASCII/continuation, 2/3/4 for valid lead bytes.
(define (utf8-lead-byte-count b)
  (cond
    [(<= b 127) 1]                          ; ASCII
    [(<= 128 b 191) 1]                      ; continuation byte (0x80-0xBF)
    [(<= 192 b 223) 2]                      ; 2-byte sequence (0xC0-0xDF)
    [(<= 224 b 239) 3]                      ; 3-byte sequence (0xE0-0xEF)
    [(<= 240 b 247) 4]                      ; 4-byte sequence (0xF0-0xF7)
    [else 1]))

;; Check if a byte value is a UTF-8 continuation byte (10xxxxxx = 0x80-0xBF)
(define (utf8-continuation-byte? b)
  (<= 128 b 191))

;; Reassemble a list of chars (each representing a raw byte) into
;; a single Unicode character using UTF-8 decoding.
(define (reassemble-utf8-chars chars)
  (define bytes-list (map char->integer chars))
  (define raw-bytes (apply bytes bytes-list))
  (with-handlers ([exn:fail? (lambda (e) #\?)])
    (define str (bytes->string/utf-8 raw-bytes))
    (if (> (string-length str) 0)
        (string-ref str 0)
        (integer->char 0))))

;; UTF-8 accumulator state (stub - no longer needed with tui-term)
(define utf8-accumulator (list))

;; Reset the accumulator
(define (utf8-accumulator-reset!)
  (set! utf8-accumulator (list)))

;; Get current accumulator length (for testing)
(define (utf8-accumulator-length)
  (length utf8-accumulator))

;; Feed a char to the accumulator.
;; Returns:
;;   - #f if the sequence is incomplete (need more bytes)
;;   - char? if the sequence is complete and decoded
(define (utf8-accumulate-char ch)
  (set! utf8-accumulator (append utf8-accumulator (list ch)))
  (define lead (car utf8-accumulator))
  (define total (utf8-lead-byte-count (char->integer lead)))
  (define n (length utf8-accumulator))
  (cond
    [(>= n total)
     ;; Complete sequence — decode
     (define decoded (reassemble-utf8-chars utf8-accumulator))
     (set! utf8-accumulator (list))
     decoded]
    [else
     ;; Incomplete — need more bytes
     #f]))

;; ============================================================
;; CSI fragment detection (compatibility stubs)
;; ============================================================
;; These were used with charterm which leaked partial escape sequences.
;; tui-term handles escape sequences internally, so these are stubs.

(define csi-fragment-state 'idle)

(define (csi-fragment-state-set! s)
  (set! csi-fragment-state s))

(define (csi-fragment-reset!)
  (set! csi-fragment-state 'idle))

;; Check if a char is a CSI final byte (0x40-0x7E = @ through ~)
(define (csi-final-byte? ch)
  (define b (char->integer ch))
  (and (>= b #x40) (<= b #x7E)))

;; Get current CSI fragment state (for testing)
(define (get-csi-fragment-state)
  csi-fragment-state)

;; Process a keycode through the CSI fragment detector.
;; Returns:
;;   #f — this keycode is a fragment, should be suppressed
;;   kc — pass through to caller unchanged
(define (filter-csi-fragment kc)
  ;; With tui-term, escape sequences are handled internally.
  ;; This function is a no-op that passes through all keys.
  kc)
