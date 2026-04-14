#lang racket/base

;; q/tui/terminal-input.rkt — ANSI key decoding, stdin reading, and UTF-8 support
;;
;; Handles raw stdin input reading, ANSI escape sequence parsing,
;; key/mouse event generation, and UTF-8 accumulator state.
;; These are the "real stdin" fallbacks used when tui-term is unavailable.
;;
;; Extracted from terminal.rkt for separation of concerns.

(require racket/port
         racket/string)

;; Raw stdin reading (used by facade for input selection)
(provide real-stdin-read-msg
         stub-byte-ready?
         buffered-read-byte
         input-buffer-reset!
         input-buffer-length

         ;; Kitty keyboard protocol (Issue #410)
         kitty-mode-supported?
         kitty-mode-enable!
         kitty-mode-disable!
         modify-other-keys-enable!
         modify-other-keys-disable!
         parse-kitty-csi-u
         parse-modify-other-keys
         kitty-codepoint->key
         detect-kitty-support!

         ;; Bracketed paste support
         bracketed-paste-start-seq
         bracketed-paste-end-seq
         bracketed-paste-begin-pattern?
         bracketed-paste-end-pattern?
         paste-buffer-reset!
         paste-buffer-add!
         paste-buffer-get
         in-paste?
         set-in-paste!
         make-paste-event
         paste-event?

         ;; Raw message constructors (vectors, compatible with tui-term message API)
         make-tkeymsg-raw
         make-tsizemsg-raw
         make-tmousemsg-raw

         ;; Mouse event predicates and accessors
         tmousemsg?
         tmousemsg-cb
         tmousemsg-cx
         tmousemsg-cy

         ;; UTF-8 support (compatibility stubs — tui-term handles UTF-8 internally)
         utf8-high-byte?
         utf8-lead-byte-count
         utf8-continuation-byte?
         reassemble-utf8-chars
         utf8-accumulate-char
         utf8-accumulator-reset!
         utf8-accumulator-length)

;; ============================================================
;; UTF-8 support (compatibility stubs)
;; ============================================================
;; These functions were used with charterm which required manual
;; UTF-8 handling. tui-term handles UTF-8 internally, so these
;; are provided as stubs for backward compatibility.

;; Check if a char represents a byte > 127 (UTF-8 lead or continuation)
(define (utf8-high-byte? ch)
  (and (char? ch) (> (char->integer ch) 127)))

;; Determine the number of bytes in a UTF-8 sequence from the lead byte value.
;; Returns 1 for ASCII/continuation, 2/3/4 for valid lead bytes.
(define (utf8-lead-byte-count b)
  (cond
    [(<= b 127) 1] ; ASCII
    [(<= 128 b 191) 1] ; continuation byte (0x80-0xBF)
    [(<= 192 b 223) 2] ; 2-byte sequence (0xC0-0xDF)
    [(<= 224 b 239) 3] ; 3-byte sequence (0xE0-0xEF)
    [(<= 240 b 247) 4] ; 4-byte sequence (0xF0-0xF7)
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

;; UTF-8 accumulator state
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
    ;; Incomplete — need more bytes
    [else #f]))

;; ============================================================
;; Raw message constructors (compatible with tkeymsg?/tsizemsg? predicates)
;; ============================================================
;; When tui-term is unavailable, we use simple vectors as message structs.

(define (make-tkeymsg-raw key)
  (vector 'tkeymsg key))

(define (make-tsizemsg-raw cols rows)
  (vector 'tsizemsg cols rows))

(define (make-tmousemsg-raw cb cx cy)
  (vector 'tmousemsg cb cx cy))

;; ============================================================
;; Bracketed paste support (DEC 2004 mode)
;; ============================================================
;; When enabled, the terminal wraps pasted text in:
;;   ESC[200~ ... pasted text ... ESC[201~
;; This allows the TUI to insert pasted text as a single undo entry.

(define bracketed-paste-start-seq "\x1b[200~")
(define bracketed-paste-end-seq "\x1b[201~")

;; Paste buffer state
(define paste-buffer-str "")
(define in-paste-state #f)

(define (in-paste?)
  in-paste-state)
(define (set-in-paste! v)
  (set! in-paste-state v))
(define (paste-buffer-reset!)
  (set! paste-buffer-str ""))
(define (paste-buffer-add! s)
  (set! paste-buffer-str (string-append paste-buffer-str s)))
(define (paste-buffer-get)
  paste-buffer-str)

;; Make a paste event
(define (make-paste-event text)
  (vector 'tpaste text))

(define (paste-event? msg)
  (and (vector? msg) (eq? (vector-ref msg 0) 'tpaste)))

;; Check if a CSI sequence matches bracketed paste start (200~)
(define (bracketed-paste-begin-pattern? CSI-params final-byte)
  (and (string=? CSI-params "200") (char=? final-byte #\~)))

;; Check if a CSI sequence matches bracketed paste end (201~)
(define (bracketed-paste-end-pattern? CSI-params final-byte)
  (and (string=? CSI-params "201") (char=? final-byte #\~)))

;; ============================================================
;; Mouse event predicates and accessors
;; ============================================================

(define (tmousemsg? msg)
  (and (vector? msg) (eq? (vector-ref msg 0) 'tmousemsg)))

(define (tmousemsg-cb msg)
  (vector-ref msg 1))
(define (tmousemsg-cx msg)
  (vector-ref msg 2))
(define (tmousemsg-cy msg)
  (vector-ref msg 3))

;; ============================================================
;; ANSI escape sequence decoding
;; ============================================================

;; Decode a CSI sequence (ESC [ already consumed)
(define (decode-csi-sequence in)
  (define b (buffered-read-byte in 0.01))
  (cond
    [(not b) (make-tkeymsg-raw 'escape)]
    [(= b 65) (make-tkeymsg-raw 'up)] ;; ESC[A
    [(= b 66) (make-tkeymsg-raw 'down)] ;; ESC[B
    [(= b 67) (make-tkeymsg-raw 'right)] ;; ESC[C
    [(= b 68) (make-tkeymsg-raw 'left)] ;; ESC[D
    [(= b 72) (make-tkeymsg-raw 'home)] ;; ESC[H
    [(= b 70) (make-tkeymsg-raw 'end)] ;; ESC[F
    [(= b 80) (make-tkeymsg-raw 'f1)] ;; ESC[P
    [(= b 81) (make-tkeymsg-raw 'f2)] ;; ESC[Q
    [(= b 82) (make-tkeymsg-raw 'f3)] ;; ESC[R
    [(= b 83) (make-tkeymsg-raw 'f4)] ;; ESC[S
    [(= b 49) (decode-csi-tilled in 'home)]
    [(= b 52) (decode-csi-tilled in 'end)]
    [(= b 53) (decode-csi-tilled in 'page-up)]
    [(= b 54) (decode-csi-tilled in 'page-down)]
    [(= b 50) (decode-csi-tilled in 'insert)]
    [(= b 51) (decode-csi-tilled in 'delete)]
    [(= b 77) ;; ESC[M — X10 mouse event
     (define cb (buffered-read-byte in 0.01))
     (define cx (buffered-read-byte in 0.01))
     (define cy (buffered-read-byte in 0.01))
     (if (and cb cx cy)
         (make-tmousemsg-raw cb cx cy)
         (make-tkeymsg-raw 'escape))]
    [else (make-tkeymsg-raw 'escape)]))

;; Decode ESC[N~ style sequences (N already consumed)
(define (decode-csi-tilled in default-key)
  (define b2 (buffered-read-byte in 0.01))
  (cond
    [(and (byte? b2) (= b2 126)) (make-tkeymsg-raw default-key)] ;; ~
    [(and (byte? b2) (= b2 59)) ;; ; — modifier like ESC[1;5A
     (define _mod (buffered-read-byte in 0.01)) ;; skip modifier number
     (define b4 (buffered-read-byte in 0.01))
     (cond
       [(and (byte? b4) (= b4 65)) (make-tkeymsg-raw 'up)]
       [(and (byte? b4) (= b4 66)) (make-tkeymsg-raw 'down)]
       [(and (byte? b4) (= b4 67)) (make-tkeymsg-raw 'right)]
       [(and (byte? b4) (= b4 68)) (make-tkeymsg-raw 'left)]
       [(and (byte? b4) (= b4 72)) (make-tkeymsg-raw 'home)]
       [(and (byte? b4) (= b4 70)) (make-tkeymsg-raw 'end)]
       [else (make-tkeymsg-raw 'escape)])]
    [else (make-tkeymsg-raw 'escape)]))

;; ============================================================
;; Kitty keyboard protocol (Issue #410)
;; ============================================================

;; Kitty keyboard protocol provides unambiguous key encoding.
;; Enable: ESC[=1u  Disable: ESC[=0u
;; Sequences: ESC[<codepoint>;<modifiers>u
;; Modifier bitmask: 1=shift, 2=alt, 4=ctrl, 8=super

(define kitty-supported #f)

(define (kitty-mode-supported?)
  kitty-supported)

(define (kitty-mode-enable!)
  (when kitty-supported
    (display "\x1b[=1u")
    (flush-output)))

(define (kitty-mode-disable!)
  (when kitty-supported
    (display "\x1b[=0u")
    (flush-output)))

;; modifyOtherKeys mode 4 (xterm-compatible)
;; Enable: ESC[>4;2m  Disable: ESC[>4;0m
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
  (set! kitty-supported
        (or (and term-program
                 (member (string-downcase term-program)
                         '("kitty" "ghostty")))
            (and term (regexp-match? #rx"^(kitty|ghostty)" term)))))

;; Parse a Kitty CSI-u sequence: ESC[<codepoint>;<modifiers>u
;; Returns (list 'key key-symbol modifiers) or #f
(define (parse-kitty-csi-u codepoint modifiers)
  (define base-key (kitty-codepoint->key codepoint))
  (define mods (bitmask->modifiers modifiers))
  (list base-key mods))

;; Parse modifyOtherKeys sequence: ESC[27;<modifiers>;<keycode>~
;; Returns (list 'key key-symbol modifiers) or #f
(define (parse-modify-other-keys modifiers keycode)
  (define base-key
    (cond
      [(= keycode 13) 'return]
      [(= keycode 27) 'escape]
      [(= keycode 127) 'backspace]
      [(<= 32 keycode 126) (integer->char keycode)]
      [else #f]))
  (if base-key
      (list base-key (bitmask->modifiers modifiers))
      #f))

;; Convert modifier bitmask to list of modifier symbols
;; Bit 1=shift, 2=alt, 4=ctrl, 8=super
(define (bitmask->modifiers mask)
  (define mods '())
  (when (bitwise-bit-set? mask 0) (set! mods (cons 'shift mods)))
  (when (bitwise-bit-set? mask 1) (set! mods (cons 'alt mods)))
  (when (bitwise-bit-set? mask 2) (set! mods (cons 'ctrl mods)))
  (when (bitwise-bit-set? mask 3) (set! mods (cons 'super mods)))
  (reverse mods))

;; Map Kitty key codepoints to key symbols
;; Special keys have fixed codepoints per the Kitty protocol
(define (kitty-codepoint->key cp)
  (cond
    ;; Printable ASCII (32-126)
    [(and (>= cp 32) (<= cp 126)) (integer->char cp)]
    ;; Special keys (Kitty-defined codepoints)
    [(= cp 57344) 'escape]
    [(= cp 57345) 'enter]
    [(= cp 57346) 'tab]
    [(= cp 57347) 'backspace]
    [(= cp 57348) 'insert]
    [(= cp 57349) 'delete]
    [(= cp 57350) 'left]
    [(= cp 57351) 'right]
    [(= cp 57352) 'up]
    [(= cp 57353) 'down]
    [(= cp 57354) 'page-up]
    [(= cp 57355) 'page-down]
    [(= cp 57356) 'home]
    [(= cp 57357) 'end]
    [(= cp 57358) 'caps-lock]
    [(= cp 57359) 'scroll-lock]
    [(= cp 57360) 'num-lock]
    [(= cp 57361) 'print-screen]
    [(= cp 57362) 'pause]
    [(= cp 57363) 'menu]
    ;; F1-F12: 57376-57387
    [(and (>= cp 57376) (<= cp 57387))
     (string->symbol (format "f~a" (- cp 57375)))]
    ;; F13-F24: 57388-57399
    [(and (>= cp 57388) (<= cp 57399))
     (string->symbol (format "f~a" (- cp 57375)))]
    ;; Unknown
    [else 'unknown]))

;; ============================================================
;; Input byte buffer (Issue #409)
;; ============================================================

;; Byte buffer for efficient stdin reading.
;; read-bytes-avail! fills the buffer; we consume from it.
(define input-buffer (make-bytes 256))
(define input-buffer-data #f) ;; #f or (cons start-pos end-pos)

(define (input-buffer-reset!)
  (set! input-buffer-data #f))

(define (input-buffer-length)
  (if input-buffer-data
      (- (cdr input-buffer-data) (car input-buffer-data))
      0))

;; Read one byte, using the buffer first.
;; Returns byte? or #f on timeout.
(define (buffered-read-byte in timeout)
  (cond
    ;; Data available in buffer
    [(and input-buffer-data
          (< (car input-buffer-data) (cdr input-buffer-data)))
     (define b (bytes-ref input-buffer (car input-buffer-data)))
     (set! input-buffer-data
           (cons (add1 (car input-buffer-data)) (cdr input-buffer-data)))
     b]
    ;; Buffer exhausted or empty — refill
    [else
     (define ready (sync/timeout timeout in))
     (if (not ready)
         #f ;; timeout
         (let ([n (read-bytes-avail! input-buffer in)])
           (cond
             [(eof-object? n) #f]
             [(and (integer? n) (> n 0))
              (set! input-buffer-data (cons 0 n))
              (buffered-read-byte in 0)] ;; recursive call to consume
             [else #f])))]))

;; ============================================================
;; Raw stdin reading (when tui-term is unavailable)
;; ============================================================

;; Real stdin-based input when tui-term is unavailable.
;; Uses buffered reading (Issue #409) for efficient stdin parsing.
;; Reads raw bytes from stdin, decodes ANSI escape sequences,
;; and returns tkeymsg/tsizemsg structs (as vectors).
(define (real-stdin-read-msg #:timeout [timeout 0.20])
  (define in (current-input-port))
  (define b (buffered-read-byte in timeout))
  (cond
    [(not b) #f]
    [(= b 27)
     (define b2 (buffered-read-byte in 0.01))
     (cond
       [(not b2) (make-tkeymsg-raw 'escape)]
       [(= b2 91) (decode-csi-sequence in)]
       [else (make-tkeymsg-raw (integer->char b2))])]
    [(= b 13) (make-tkeymsg-raw 'return)]
    [(= b 10) (make-tkeymsg-raw 'return)]
    [(= b 127) (make-tkeymsg-raw 'backspace)]
    [(= b 8) (make-tkeymsg-raw 'backspace)]
    [(= b 3) (make-tkeymsg-raw 'ctrl-c)]
    [(>= b 192)
     (utf8-accumulator-reset!)
     (define lead-char (integer->char b))
     (define total-bytes (utf8-lead-byte-count b))
     (if (= total-bytes 1)
         (make-tkeymsg-raw lead-char)
         (let _loop ()
           (define decoded (utf8-accumulate-char lead-char))
           (cond
             [decoded (make-tkeymsg-raw decoded)]
             [else
              (define cb (buffered-read-byte in 0.05))
              (if (and cb (utf8-continuation-byte? cb))
                  (begin
                    (set! lead-char (integer->char cb))
                    (_loop))
                  (begin
                    (utf8-accumulator-reset!)
                    #f))])))]
    [(>= b 128) #f]
    [(>= b 32)
     (utf8-accumulator-reset!)
     (make-tkeymsg-raw (integer->char b))]
    [else #f]))


(define (stub-byte-ready?)
  (char-ready? (current-input-port)))
