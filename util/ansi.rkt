#lang racket

;; ANSI escape sequence utilities for CLI color output.
;; Auto-detects TTY and respects NO_COLOR convention.

(require racket/string
         racket/format
         ffi/unsafe)

(provide color-enabled?
         ansi
         ansi-reset
         styled
         styled-prompt
         ;; Style constants for direct use
         ANSI-RESET
         ANSI-BOLD
         ANSI-DIM
         ANSI-ITALIC
         ANSI-UNDERLINE
         ANSI-INVERSE
         ANSI-RED
         ANSI-GREEN
         ANSI-YELLOW
         ANSI-BLUE
         ANSI-MAGENTA
         ANSI-CYAN
         ANSI-WHITE
         ANSI-GRAY)

;; ============================================================
;; ANSI escape constants
;; ============================================================

(define ANSI-RESET    "\x1b[0m")
(define ANSI-BOLD     "\x1b[1m")
(define ANSI-DIM      "\x1b[2m")
(define ANSI-ITALIC   "\x1b[3m")
(define ANSI-UNDERLINE "\x1b[4m")
(define ANSI-INVERSE  "\x1b[7m")
(define ANSI-RED      "\x1b[31m")
(define ANSI-GREEN    "\x1b[32m")
(define ANSI-YELLOW   "\x1b[33m")
(define ANSI-BLUE     "\x1b[34m")
(define ANSI-MAGENTA  "\x1b[35m")
(define ANSI-CYAN     "\x1b[36m")
(define ANSI-WHITE    "\x1b[37m")
(define ANSI-GRAY     "\x1b[90m")

;; ============================================================
;; TTY detection
;; ============================================================

;; Cache the result — terminal state doesn't change mid-run
(define ffi-isatty
  (get-ffi-obj "isatty" #f (_fun _int -> _int)
                (lambda () (lambda (fd) 0))))

(define (tty?)
  "Check if stdout (fd 1) is connected to a terminal."
  (= (ffi-isatty 1) 1))

(define (no-color?)
  "Check if NO_COLOR is set or TERM=dumb."
  (or (getenv "NO_COLOR")
      (let ([term (getenv "TERM")])
        (and term (string=? term "dumb")))))

(define (color-enabled?)
  "True when stdout is a TTY and NO_COLOR is not set."
  (and (tty?) (not (no-color?))))

;; ============================================================
;; Style application
;; ============================================================

(define (style->ansi sym)
  "Map a style symbol to its ANSI escape code."
  (case sym
    [(bold)      ANSI-BOLD]
    [(dim)       ANSI-DIM]
    [(italic)    ANSI-ITALIC]
    [(underline) ANSI-UNDERLINE]
    [(inverse)   ANSI-INVERSE]
    [(red)       ANSI-RED]
    [(green)     ANSI-GREEN]
    [(yellow)    ANSI-YELLOW]
    [(blue)      ANSI-BLUE]
    [(magenta)   ANSI-MAGENTA]
    [(cyan)      ANSI-CYAN]
    [(white)     ANSI-WHITE]
    [(gray)      ANSI-GRAY]
    [else        ""]))

(define (ansi codes text)
  "Wrap text with ANSI escape codes. Returns plain text if color is disabled."
  (if (color-enabled?)
      (string-append codes text ANSI-RESET)
      text))

(define (ansi-reset)
  "Return the ANSI reset code, or empty string if color disabled."
  (if (color-enabled?) ANSI-RESET ""))

(define (styled text style-syms)
  "Apply a list of style symbols to text. No-op when color is disabled."
  (if (color-enabled?)
      (let ([codes (string-join (map style->ansi style-syms) "")])
        (string-append codes text ANSI-RESET))
      text))

(define (styled-prompt prompt-text)
  "Style the q> prompt: bold cyan."
  (styled prompt-text '(bold cyan)))
