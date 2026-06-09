#lang racket/base

;; q/tui/input/kitty-protocol.rkt — Kitty keyboard protocol support
;;
;; Detection, enabling/disabling, and key codepoint mapping for the
;; Kitty keyboard protocol. Extracted from terminal-input.rkt (v0.96.16, AX1-1).
;; STABILITY: internal

(require racket/match)

(provide kitty-mode-enable!
         kitty-mode-disable!
         modify-other-keys-enable!
         modify-other-keys-disable!
         detect-kitty-support!
         parse-kitty-csi-u
         parse-modify-other-keys
         kitty-codepoint->key
         decode-sgr-mouse)

;; ============================================================
;; Kitty keyboard protocol (Issue #410)
;; ============================================================
;; Kitty keyboard protocol provides unambiguous key encoding.
;; Enable: ESC[=1u  Disable: ESC[=0u
;; Sequences: ESC[<codepoint>;<modifiers>u
;; Modifier bitmask: 1=shift, 2=alt, 4=ctrl, 8=super

(define (kitty-mode-enable! supported?)
  (when supported?
    (display "\x1b[=1u")
    (flush-output)))

(define (kitty-mode-disable! supported?)
  (when supported?
    (display "\x1b[=0u")
    (flush-output)))

;; modifyOtherKeys mode 4 (xterm-compatible)
(define (modify-other-keys-enable!)
  (display "\x1b[>4;2m")
  (flush-output))

(define (modify-other-keys-disable!)
  (display "\x1b[>4;0m")
  (flush-output))

;; Detect Kitty support from environment
(define (detect-kitty-support!)
  (define term-program (getenv "TERM_PROGRAM"))
  (define term (getenv "TERM"))
  (or (and term-program (member (string-downcase term-program) '("kitty" "ghostty")))
      (and term (regexp-match? #rx"^(kitty|ghostty)" term))))

;; Parse a Kitty CSI-u sequence: ESC[<codepoint>;<modifiers>u
(define (parse-kitty-csi-u codepoint modifiers)
  (define base-key (kitty-codepoint->key codepoint))
  (define mods (bitmask->modifiers modifiers))
  (list base-key mods))

;; Parse modifyOtherKeys sequence: ESC[27;<modifiers>;<keycode>~
(define (parse-modify-other-keys modifiers keycode)
  (define base-key
    (match keycode
      [13 'return]
      [27 'escape]
      [127 'backspace]
      [(? (lambda (v) (<= 32 v 126))) (integer->char keycode)]
      [_ #f]))
  (if base-key
      (list base-key (bitmask->modifiers modifiers))
      #f))

;; Convert modifier bitmask to list of modifier symbols
(define (bitmask->modifiers mask)
  (define mods '())
  (when (bitwise-bit-set? mask 0)
    (set! mods (cons 'shift mods)))
  (when (bitwise-bit-set? mask 1)
    (set! mods (cons 'alt mods)))
  (when (bitwise-bit-set? mask 2)
    (set! mods (cons 'ctrl mods)))
  (when (bitwise-bit-set? mask 3)
    (set! mods (cons 'super mods)))
  (reverse mods))

;; Map Kitty key codepoints to key symbols
(define (kitty-codepoint->key cp)
  (match cp
    [(? (lambda (v) (and (>= v 32) (<= v 126)))) (integer->char cp)]
    [57344 'escape]
    [57345 'enter]
    [57346 'tab]
    [57347 'backspace]
    [57348 'insert]
    [57349 'delete]
    [57350 'left]
    [57351 'right]
    [57352 'up]
    [57353 'down]
    [57354 'page-up]
    [57355 'page-down]
    [57356 'home]
    [57357 'end]
    [57358 'caps-lock]
    [57359 'scroll-lock]
    [57360 'num-lock]
    [57361 'print-screen]
    [57362 'pause]
    [57363 'menu]
    [(? (lambda (v) (and (>= v 57376) (<= v 57387)))) (string->symbol (format "f~a" (- cp 57375)))]
    [(? (lambda (v) (and (>= v 57388) (<= v 57399)))) (string->symbol (format "f~a" (- cp 57375)))]
    [_ 'unknown]))

;; ============================================================
;; SGR mouse decoding (mode 1006)
;; ============================================================

;; Decode an SGR-encoded mouse event from a port.
;; buffered-read-byte is passed as callback to avoid circular imports.
;; Returns: (vector 'tmousemsg cb cx cy) or (vector 'tkeymsg 'escape)
(define (decode-sgr-mouse in buffered-read-byte)
  (define (read-sgr-param acc)
    (define b (buffered-read-byte in 0.01))
    (match b
      [#f (values (if (null? acc) #f (string->number (list->string (reverse acc)))) #f)]
      [(? (lambda (v) (and (>= v 48) (<= v 57)))) (read-sgr-param (cons (integer->char b) acc))]
      [59 (values (if (null? acc) 0 (string->number (list->string (reverse acc)))) 'cont)]
      [_ (values (if (null? acc) #f (string->number (list->string (reverse acc)))) b)]))
  (define-values (sgr-button rest1) (read-sgr-param '()))
  (define (make-escape) (vector 'tkeymsg 'escape))
  (match (list sgr-button rest1)
    [(list #f _) (make-escape)]
    [(list _ (not 'cont)) (make-escape)]
    [_
     (define-values (sgr-x rest2) (read-sgr-param '()))
     (match (list sgr-x rest2)
       [(list #f _) (make-escape)]
       [(list _ (not 'cont)) (make-escape)]
       [_
        (define-values (sgr-y final-byte) (read-sgr-param '()))
        (match (list sgr-y final-byte)
          [(list #f _) (make-escape)]
          [(list _ #f) (make-escape)]
          [_
           (define release? (= final-byte 109))
           (define cb (if release? 35 (+ 32 sgr-button)))
           (define cx (+ sgr-x 32))
           (define cy (+ sgr-y 32))
           (vector 'tmousemsg cb cx cy)])])]))
