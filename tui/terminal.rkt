#lang racket/base

;; tui/terminal.rkt — Thin terminal I/O facade
;;
;; This module is the terminal abstraction layer for the TUI. It dynamically
;; loads the `tui-term` package when available and falls back to pure-stub
;; implementations in headless environments (CI, containers, Emacs batch)
;; where terminal control is unavailable.
;;
;; Architecture: This facade re-exports the full public API from
;;   - terminal-bridge.rkt  (raw terminal I/O, screen-size queries)
;;   - terminal-input.rkt   (key reading, escape-sequence parsing)
;; and adds drawing primitives, style helpers, screen-size caching,
;; lifecycle management, and key helpers on top.
;;
;; The stub pattern: when tui-term is not installed, terminal-bridge.rkt
;; provides no-op stubs so the TUI module tree compiles and loads without
;; error. Runtime callers should check `tui-term-available?` before using
;; drawing functions.
;;
;; The public API (provide) is identical to the original monolithic module
;; before the refactor into bridge + input + facade.

(require racket/port
         racket/string
         "clipboard.rkt"
         "terminal-bridge.rkt"
         "terminal-input.rkt")

;; Lifecycle
(provide tui-term-open
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

         ;; Bracketed paste (DEC 2004)
         enable-bracketed-paste
         disable-bracketed-paste

         ;; Synchronized output (DEC mode 2026)
         terminal-sync-available?
         terminal-sync-begin!
         terminal-sync-end!
         detect-sync-mode-support!

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
         utf8-accumulator-length)

;; ============================================================
;; Input selection (bridge between terminal-bridge and terminal-input)
;; ============================================================

(define read-msg (or read-msg-fn real-stdin-read-msg))

(define byte-ready? (or byte-ready-fn stub-byte-ready?))

;; ============================================================
;; Lifecycle
;; ============================================================

;; Open terminal for TUI use.
;; Opens tty, enables alternate screen, hides cursor.
(define (tui-term-open #:tty [tty #f])
  (define term (make-tty-term #:tty tty))
  (term-alternate-screen term)
  (term-hide-cursor term)
  (enable-bracketed-paste)
  term)

;; Close terminal and restore state.
;; Shows cursor, restores normal screen, closes tty.
(define (tui-term-close term)
  (disable-bracketed-paste)
  (term-show-cursor term)
  (term-normal-screen term)
  (term-close term))

;; Macro for safe terminal usage with cleanup.
(define-syntax-rule (with-tui-terminal body ...)
  (let ([term (make-tty-term)])
    (dynamic-wind (lambda ()
                    (term-alternate-screen term)
                    (term-hide-cursor term)
                    (enable-bracketed-paste))
                  (lambda ()
                    body ...)
                  (lambda ()
                    (with-handlers ([exn:fail? (lambda (e) (void))])
                      (disable-bracketed-paste)
                      (term-show-cursor term)
                      (term-normal-screen term))))))

;; ============================================================
;; Screen size (cached)
;; ============================================================

;; Cache screen size to avoid flooding the terminal with queries.
(define screen-size-cache-cols #f)
(define screen-size-cache-rows #f)
(define screen-size-cache-time 0)
(define screen-size-cache-ttl 0.1) ;; re-query every 100ms for responsive resize
(define screen-size-last-cols #f) ;; for resize detection
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
    [(and (= cols screen-size-last-cols) (= rows screen-size-last-rows)) #f]
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
      [(black) 30]
      [(red) 31]
      [(green) 32]
      [(yellow) 33]
      [(blue) 34]
      [(magenta) 35]
      [(cyan) 36]
      [(white) 37]
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
;; Synchronized Output (DEC mode 2026)
;; ============================================================

;; Feature detection: check if the terminal likely supports
;; DEC private mode 2026 (Synchronized Output).
;; Modern terminals (kitty, wezterm, alacritty ≥0.13, foot, etc.)
;; support this. Unknown/older terminals get a graceful no-op.

(define sync-mode-supported #f)

(define (terminal-sync-available?)
  sync-mode-supported)

;; Probe terminal for sync mode support.
;; Checks TERM_PROGRAM and TERM for known supporters.
;; Called once during init. Gracefully defaults to #f.
(define (detect-sync-mode-support!)
  (define term-program (getenv "TERM_PROGRAM"))
  (define term (getenv "TERM"))
  (set! sync-mode-supported
        (or (and term-program
                 (member (string-downcase term-program)
                         '("wezterm" "kitty" "hyper" "alacritty" "ghostty" " rio" " contour")))
            (and term (regexp-match? #rx"^(foot|kitty|wezterm|ghostty)" term)))))

;; Begin synchronized output bracket.
;; All output between begin and end is batched by the terminal
;; and applied atomically, preventing torn frames.
(define (terminal-sync-begin!)
  (when sync-mode-supported
    (display "\x1b[?2026h")))

;; End synchronized output bracket.
;; Flushes the batched output atomically.
(define (terminal-sync-end!)
  (when sync-mode-supported
    (display "\x1b[?2026l")
    (flush-output)))

;; ============================================================
;; Mouse tracking — X10 protocol
;; ============================================================

(define (enable-mouse-tracking)
  ;; Button-event tracking (mode 1002): reports press, drag, and release.
  ;; This is needed for text selection via click-drag.
  ;; We do NOT use SGR-Pixel (1006) — only X10 format is decoded.
  (display "\x1b[?1000h") ;; basic tracking as fallback
  (display "\x1b[?1002h") ;; button-event tracking (press + drag + release)
  (flush-output))

(define (disable-mouse-tracking)
  (display "\x1b[?1002l")
  (display "\x1b[?1000l")
  (flush-output))

;; Bracketed paste mode (DEC 2004)
;; When enabled, terminal wraps pasted text in ESC[200~...ESC[201~
(define (enable-bracketed-paste)
  (display "\x1b[?2004h")
  (flush-output))

(define (disable-bracketed-paste)
  (display "\x1b[?2004l")
  (flush-output))

;; ============================================================
;; Clipboard backward-compatible wrapper
;; ============================================================

;; Clipboard functions are in clipboard.rkt.
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
