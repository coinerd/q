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
  (sync/timeout 0.01 in) ;; wait briefly for rest of sequence
  (define b (read-byte in))
  (cond
    [(eof-object? b) (make-tkeymsg-raw 'escape)]
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
    ;; ESC[1~ = home, ESC[4~ = end, ESC[5~ = pgup, ESC[6~ = pgdn
    [(= b 49) (decode-csi-tilled in 'home)] ;; ESC[1
    [(= b 52) (decode-csi-tilled in 'end)] ;; ESC[4
    [(= b 53) (decode-csi-tilled in 'page-up)] ;; ESC[5
    [(= b 54) (decode-csi-tilled in 'page-down)] ;; ESC[6
    [(= b 50) (decode-csi-tilled in 'insert)] ;; ESC[2
    [(= b 51) (decode-csi-tilled in 'delete)] ;; ESC[3
    [(= b 77) ;; ESC[M — X10 mouse event
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
    [(and (byte? b2) (= b2 126)) (make-tkeymsg-raw default-key)] ;; ~
    [(and (byte? b2) (= b2 59)) ;; ; — modifier like ESC[1;5A
     (sync/timeout 0.01 in)
     (read-byte in) ;; skip modifier number
     (sync/timeout 0.01 in)
     (define b4 (read-byte in))
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
;; Raw stdin reading (when tui-term is unavailable)
;; ============================================================

;; Real stdin-based input when tui-term is unavailable.
;; Reads raw bytes from stdin, decodes ANSI escape sequences,
;; and returns tkeymsg/tsizemsg structs (as vectors).
(define (real-stdin-read-msg #:timeout [timeout 0.20])
  (define in (current-input-port))
  (define result (sync/timeout timeout in))
  (if (not result)
      #f ;; timeout, no input
      (let ([b (read-byte in)])
        (cond
          [(eof-object? b) #f]
          [(= b 27) ;; ESC — could be escape sequence
           (sync/timeout 0.01 in) ;; wait briefly for rest of sequence
           (if (char-ready? in)
               (let ([b2 (read-byte in)])
                 (cond
                   [(eof-object? b2) (make-tkeymsg-raw 'escape)]
                   [(= b2 91) (decode-csi-sequence in)] ;; ESC[
                   [else (make-tkeymsg-raw (integer->char b2))])) ;; Alt+key
               (make-tkeymsg-raw 'escape))]
          [(= b 13) (make-tkeymsg-raw 'return)]
          [(= b 10) (make-tkeymsg-raw 'return)]
          [(= b 127) (make-tkeymsg-raw 'backspace)]
          [(= b 8) (make-tkeymsg-raw 'backspace)]
          [(= b 3) (make-tkeymsg-raw 'ctrl-c)] ;; Ctrl-C → copy selection
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
          [(>= b 128) #f] ;; Stray continuation byte (0x80-0xBF) — skip
          [(>= b 32)
           (utf8-accumulator-reset!)
           (make-tkeymsg-raw (integer->char b))]
          [else #f]))))

(define (stub-byte-ready?)
  (char-ready? (current-input-port)))
