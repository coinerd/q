#lang racket/base

;; tui/image.rkt — terminal image protocol support (#1156)
;;
;; Provides:
;;   detect-image-protocol  — detect terminal image support from env vars
;;   kitty-image-escape     — generate Kitty graphics protocol escape sequences
;;   iterm2-image-escape    — generate iTerm2 inline image escape sequences
;;   render-image-escape    — dispatch to the right protocol automatically
;;   image-placeholder      — generate text placeholder for unsupported terminals
;;   string-chunks          — helper to split strings into fixed-size chunks

(require racket/string
         racket/format)

;; ============================================================
;; Protocol detection
;; ============================================================

;; detect-image-protocol : -> (or/c 'kitty 'iterm2 'wezterm 'ghostty #f)
;; Checks environment variables to determine which terminal image protocol
;; is available. Returns #f if no known protocol is detected.
(define (detect-image-protocol)
  (cond
    [(getenv "KITTY_WINDOW_ID") 'kitty]
    [(getenv "ITERM_SESSION_ID") 'iterm2]
    [(string-contains? (or (getenv "TERM") "") "xterm-kitty") 'kitty]
    [(string=? (or (getenv "TERM_PROGRAM") "") "WezTerm") 'wezterm]
    [(string=? (or (getenv "TERM_PROGRAM") "") "ghostty") 'ghostty]
    [else #f]))

;; ============================================================
;; Kitty image protocol
;; ============================================================

;; kitty-image-escape : string? #:width (or/c integer? #f) #:height (or/c integer? #f)
;;                     #:row (or/c integer? #f) #:col (or/c integer? #f)
;;                     -> string?
;; Generates a Kitty graphics protocol escape sequence for transmitting
;; a base64-encoded image. Splits payload into 4096-byte chunks per
;; the Kitty protocol specification.
(define (kitty-image-escape base64-data #:width [w #f] #:height [h #f] #:row [row #f] #:col [col #f])
  (define placement
    (format "a=T,f=100~a~a~a~a"
            (if w
                (format ",w=~a" w)
                "")
            (if h
                (format ",h=~a" h)
                "")
            (if row
                (format ",r=~a" row)
                "")
            (if col
                (format ",c=~a" col)
                "")))
  (define payload (format "~a;~a" placement base64-data))
  ;; Split into chunks of 4096 bytes for transmission
  (define chunk-size 4096)
  (define chunks (string-chunks payload chunk-size))
  (define total (length chunks))
  (string-append* (for/list ([chunk (in-list chunks)]
                             [i (in-naturals)])
                    (define m (if (= i (sub1 total)) 0 1))
                    (format "\x1b_G~a\x1b\\" chunk))))

;; ============================================================
;; iTerm2 image protocol
;; ============================================================

;; iterm2-image-escape : string? #:width (or/c integer? #f) #:height (or/c integer? #f)
;;                       -> string?
;; Generates an iTerm2 inline image escape sequence for transmitting
;; a base64-encoded image.
(define (iterm2-image-escape base64-data #:width [w #f] #:height [h #f])
  (format "\x1b]1337;File=inline=1~a~a:\x07"
          (if w
              (format ";width=~apx" w)
              "")
          base64-data))

;; ============================================================
;; Unified dispatch
;; ============================================================

;; render-image-escape : string? #:width (or/c integer? #f) #:height (or/c integer? #f)
;;                       #:protocol (or/c 'kitty 'iterm2 'wezterm 'ghostty #f)
;;                       -> (or/c string? #f)
;; Generates the appropriate escape sequence for the given protocol.
;; Returns #f if no protocol is specified or available.
(define (render-image-escape base64-data
                             #:width [w #f]
                             #:height [h #f]
                             #:protocol [protocol (detect-image-protocol)])
  (case protocol
    [(kitty) (kitty-image-escape base64-data #:width w #:height h)]
    [(iterm2) (iterm2-image-escape base64-data #:width w #:height h)]
    [(wezterm ghostty) (iterm2-image-escape base64-data #:width w #:height h)]
    [else #f]))

;; ============================================================
;; Placeholder for unsupported terminals
;; ============================================================

;; image-placeholder : string? integer? -> string?
;; Generates a text placeholder box with alt-text for terminals that
;; don't support image rendering.
(define (image-placeholder alt-text width)
  (define inner
    (if (> (string-length alt-text) (- width 4))
        (string-append (substring alt-text 0 (max 0 (- width 7))) "...")
        alt-text))
  (define border (make-string (string-length inner) #\─))
  (format "┌─~a─┐\n│ ~a │\n└─~a─┘" border inner border))

;; ============================================================
;; Utility
;; ============================================================

;; string-chunks : string? integer? -> (listof string?)
;; Splits a string into chunks of the given size.
(define (string-chunks str size)
  (define len (string-length str))
  (let loop ([i 0])
    (if (>= i len)
        '()
        (cons (substring str i (min (+ i size) len)) (loop (+ i size))))))

(provide detect-image-protocol
         kitty-image-escape
         iterm2-image-escape
         render-image-escape
         image-placeholder
         string-chunks)
