#lang racket/base

;; q/tui/terminal-input.rkt — ANSI key decoding, stdin reading, and UTF-8 support
;;
;; Handles raw stdin input reading, ANSI escape sequence parsing,
;; key/mouse event generation, and UTF-8 accumulator state.
;; Native terminal input handling (raw mode, ANSI decoding).
;;

(require racket/match
         racket/port
         racket/string
         (only-in "input/kitty-protocol.rkt"
                  [kitty-mode-enable! impl:kitty-mode-enable!]
                  [kitty-mode-disable! impl:kitty-mode-disable!]
                  modify-other-keys-enable!
                  modify-other-keys-disable!
                  [detect-kitty-support! impl:detect-kitty-support!]
                  parse-kitty-csi-u
                  parse-modify-other-keys
                  kitty-codepoint->key
                  decode-sgr-mouse))
(require racket/contract)

;; ============================================================
;; Decoder state encapsulation (v0.51.3 §7)
;; ============================================================
;; All mutable state is captured in a single struct instance,
;; held in a parameter. Factory creates isolated instances.
(struct decoder-state
        ([utf8-acc #:mutable] ; (listof char) — cons list, reversed for push
         [paste-buf #:mutable] ; string — accumulated paste text
         [in-paste #:mutable] ; boolean — inside bracketed paste?
         [input-buf #:mutable] ; bytes — read buffer
         [input-buf-data #:mutable] ; (or/c #f (cons nat nat)) — buffer window
         [kitty-supp #:mutable]) ; boolean — kitty keyboard protocol detected
  #:transparent)

(define (make-terminal-input-decoder #:buffer-size [buffer-size 256])
  (decoder-state (list) "" #f (make-bytes buffer-size) #f #f))

;; Current decoder instance (default)
(define current-decoder (make-parameter (make-terminal-input-decoder)))

;; Accessor shortcuts via current parameter
(define (ds)
  (current-decoder))

;; Raw stdin reading (used by facade for input selection)
;; Decoder factory and parameter (v0.51.3)
(provide current-decoder
         decoder-state?
         decoder-state-utf8-acc
         decoder-state-paste-buf
         decoder-state-in-paste
         (contract-out [make-terminal-input-decoder (->* () () any/c)])
         decoder-state-input-buf
         decoder-state-input-buf-data

         real-stdin-read-msg
         decode-sgr-mouse
         default-byte-ready?
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

         ;; Raw message constructors (vector-based message API)
         make-tkeymsg-raw
         make-tsizemsg-raw
         make-tmousemsg-raw

         ;; Mouse event predicates and accessors
         tmousemsg?
         tmousemsg-cb
         tmousemsg-cx
         tmousemsg-cy

         ;; UTF-8 support (native stubs for UTF-8 byte handling)
         utf8-high-byte?
         utf8-lead-byte-count
         utf8-continuation-byte?
         reassemble-utf8-chars
         utf8-accumulate-char
         utf8-accumulator-reset!
         utf8-accumulator-length)

;; ============================================================
;; UTF-8 support functions
;; ============================================================
;; These functions handle UTF-8 decoding for terminal input.

;; Check if a char represents a byte > 127 (UTF-8 lead or continuation)
(define (utf8-high-byte? ch)
  (and (char? ch) (> (char->integer ch) 127)))

;; Determine the number of bytes in a UTF-8 sequence from the lead byte value.
;; Returns 1 for ASCII/continuation, 2/3/4 for valid lead bytes.
(define (utf8-lead-byte-count b)
  (match b
    [(? (lambda (v) (<= v 127))) 1]
    [(? (lambda (v) (<= 128 v 191))) 1]
    [(? (lambda (v) (<= 192 v 223))) 2]
    [(? (lambda (v) (<= 224 v 239))) 3]
    [(? (lambda (v) (<= 240 v 247))) 4]
    [_ 1]))

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

;; UTF-8 accumulator — now uses decoder-state struct (v0.51.3)

;; Reset the accumulator
(define (utf8-accumulator-reset!)
  (set-decoder-state-utf8-acc! (ds) (list)))

;; Get current accumulator length (for testing)
(define (utf8-accumulator-length)
  (length (decoder-state-utf8-acc (ds))))

;; Feed a char to the accumulator.
;; Returns:
;;   - #f if the sequence is incomplete (need more bytes)
;;   - char? if the sequence is complete and decoded
(define (utf8-accumulate-char ch)
  (define acc (decoder-state-utf8-acc (ds)))
  (set-decoder-state-utf8-acc! (ds) (cons ch acc))
  (define n (length (decoder-state-utf8-acc (ds))))
  (define lead-byte (char->integer (list-ref (decoder-state-utf8-acc (ds)) (- n 1))))
  (define expected (utf8-lead-byte-count lead-byte))
  (match (>= n expected)
    [#t
     (define decoded (reassemble-utf8-chars (reverse (decoder-state-utf8-acc (ds)))))
     (set-decoder-state-utf8-acc! (ds) (list))
     decoded]
    [_ #f]))

;; ============================================================
;; Raw message constructors (compatible with tkeymsg?/tsizemsg? predicates)
;; ============================================================
;; Vector-based message structs for terminal input events.

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

(define paste-buffer-max-size 1048576)

(define (in-paste?)
  (decoder-state-in-paste (ds)))
(define (set-in-paste! v)
  (set-decoder-state-in-paste! (ds) v))
(define (paste-buffer-reset!)
  (set-decoder-state-paste-buf! (ds) ""))
(define (paste-buffer-add! s)
  (define buf (decoder-state-paste-buf (ds)))
  (when (<= (+ (string-length buf) (string-length s)) paste-buffer-max-size)
    (set-decoder-state-paste-buf! (ds) (string-append buf s))))
(define (paste-buffer-get)
  (decoder-state-paste-buf (ds)))

(define (make-paste-event text)
  (vector 'tpaste text))

(define (paste-event? msg)
  (and (vector? msg) (eq? (vector-ref msg 0) 'tpaste)))

(define (bracketed-paste-begin-pattern? CSI-params final-byte)
  (and (string=? CSI-params "200") (char=? final-byte #\~)))

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
  (match b
    [#f (make-tkeymsg-raw 'escape)]
    [65 (make-tkeymsg-raw 'up)]
    [66 (make-tkeymsg-raw 'down)]
    [67 (make-tkeymsg-raw 'right)]
    [68 (make-tkeymsg-raw 'left)]
    [72 (make-tkeymsg-raw 'home)]
    [70 (make-tkeymsg-raw 'end)]
    [80 (make-tkeymsg-raw 'f1)]
    [81 (make-tkeymsg-raw 'f2)]
    [82 (make-tkeymsg-raw 'f3)]
    [83 (make-tkeymsg-raw 'f4)]
    [49 (decode-csi-tilled in 49)]
    [52 (decode-csi-tilled in 52)]
    [53 (decode-csi-tilled in 53)]
    [54 (decode-csi-tilled in 54)]
    [50 (decode-csi-tilled in 50)]
    [51 (decode-csi-tilled in 51)]
    ;; ESC[< — SGR mouse event (mode 1006)
    [60 (decode-sgr-mouse in buffered-read-byte)]
    [77 ;; ESC[M — X10 mouse event
     (define cb (buffered-read-byte in 0.01))
     (define cx (buffered-read-byte in 0.01))
     (define cy (buffered-read-byte in 0.01))
     (if (and cb cx cy)
         (make-tmousemsg-raw cb cx cy)
         (make-tkeymsg-raw 'escape))]
    [_ (make-tkeymsg-raw 'escape)]))

;; Decode ESC[N~ style sequences (first digit of N already consumed as `first-digit`)
;; Accumulates all remaining digit bytes to support multi-digit params like 200, 201.
(define (decode-csi-tilled in first-digit)
  ;; Accumulate digit bytes into a param string
  (define (digit-byte? b)
    (and (byte? b) (>= b 48) (<= b 57)))
  (define (read-digits acc)
    (define b (buffered-read-byte in 0.01))
    (match b
      [(? digit-byte?) (read-digits (cons (integer->char b) acc))]
      [_ (values (list->string (reverse acc)) b)]))
  (define-values (param-str final-byte) (read-digits (list (integer->char first-digit))))
  (define final-ch (and (byte? final-byte) (integer->char final-byte)))
  (match (list final-ch (char=? final-ch #\~))
    ;; Final ~ — standard CSI N ~ sequence
    [(list _ #t)
     (match param-str
       ["200"
        (set-in-paste! #t)
        (paste-buffer-reset!)
        (read-paste-until-end in)]
       ["201"
        (set-in-paste! #f)
        (define text (paste-buffer-get))
        (paste-buffer-reset!)
        (make-paste-event text)]
       ;; Standard key codes
       [_
        (define key (csi-num->key param-str))
        (if key
            (make-tkeymsg-raw key)
            (make-tkeymsg-raw 'escape))])]
    ;; ; — modifier like ESC[1;5A
    [(list _ _)
     #:when (and final-ch (char=? final-ch #\;))
     (define mod-byte (buffered-read-byte in 0.01)) ;; modifier number
     (define b4 (buffered-read-byte in 0.01))
     (define base-key (csi-num->key param-str))
     (define base-key-or-dir
       (match b4
         [65 'up]
         [66 'down]
         [67 'right]
         [68 'left]
         [72 'home]
         [70 'end]
         [_ base-key]))
     (match (list base-key-or-dir (and (byte? mod-byte) (> mod-byte 49)))
       [(list #f _) (make-tkeymsg-raw 'escape)]
       [(list _ #f) (make-tkeymsg-raw base-key-or-dir)]
       [(list _ #t)
        (define mod-sym
          (match mod-byte
            [50 'shift]
            [51 'alt]
            [52 'S-A]
            [53 'ctrl]
            [54 'S-C]
            [55 'A-C]
            [56 'S-A-C]
            [_ #f]))
        (if mod-sym
            (make-tkeymsg-raw (string->symbol (format "~a-~a" mod-sym base-key-or-dir)))
            (make-tkeymsg-raw base-key-or-dir))])]
    [else (make-tkeymsg-raw 'escape)]))

;; Map CSI tilde parameter string to key symbol.
;; Standard keys: 1=home, 2=insert, 3=delete, 4=end, 5=page-up, 6=page-down
(define (csi-num->key param-str)
  (match param-str
    ["1" 'home]
    ["2" 'insert]
    ["3" 'delete]
    ["4" 'end]
    ["5" 'page-up]
    ["6" 'page-down]
    [_ #f]))

;; Decode pasted bytes to string, replacing invalid UTF-8 with U+FFFD.
(define (decode-paste-bytes bs)
  (with-handlers ([exn:fail? (lambda (e)
                               ;; Fallback: decode byte-by-byte, replacing invalid sequences
                               (apply string
                                      (for/list ([b (in-bytes bs)])
                                        (if (< b 128)
                                            (integer->char b)
                                            #\UFFFD))))])
    (bytes->string/utf-8 bs)))

;; Read pasted content until ESC[201~ is received.
(define (read-paste-until-end in)
  (define end-seq (bytes->list #"\x1b[201~"))
  (define end-len (length end-seq))
  (define (match-end? pending)
    (and (= (length pending) end-len) (equal? pending end-seq)))
  (define (loop pending byte-acc)
    (define b (buffered-read-byte in 0.1))
    (match b
      [#f
       ;; Timeout — flush whatever we have
       (set-in-paste! #f)
       (define text (decode-paste-bytes (apply bytes (reverse byte-acc))))
       (paste-buffer-reset!)
       (make-paste-event text)]
      [_
       (define new-pending (append pending (list b)))
       (match (match-end? new-pending)
         [#t
          ;; Found end sequence — emit paste event
          (set-in-paste! #f)
          (define text (decode-paste-bytes (apply bytes (reverse byte-acc))))
          (paste-buffer-reset!)
          (make-paste-event text)]
         [_
          (match (>= (length new-pending) end-len)
            ;; Not a match — oldest byte is data, keep checking
            [#t (loop (cdr new-pending) (cons (car new-pending) byte-acc))]
            [_ (loop new-pending byte-acc)])])]))
  (loop '() '()))

;; ============================================================
;; Kitty keyboard protocol — thin wrappers (AX1-1 extraction)

(define (kitty-mode-supported?)
  (decoder-state-kitty-supp (ds)))

(define (kitty-mode-enable!)
  (impl:kitty-mode-enable! (decoder-state-kitty-supp (ds))))

(define (kitty-mode-disable!)
  (impl:kitty-mode-disable! (decoder-state-kitty-supp (ds))))

(define (detect-kitty-support!)
  (set-decoder-state-kitty-supp! (ds) (impl:detect-kitty-support!)))

;; Input byte buffer (Issue #409)
;; ============================================================

;; Byte buffer for efficient stdin reading.
;; read-bytes-avail! fills the buffer; we consume from it.
;; Input buffer — now uses decoder-state struct (v0.51.3)

(define (input-buffer-reset!)
  (set-decoder-state-input-buf-data! (ds) #f))

(define (input-buffer-length)
  (define d (decoder-state-input-buf-data (ds)))
  (if d
      (- (cdr d) (car d))
      0))

;; Read one byte, using the buffer first.
;; Returns byte? or #f on timeout.
(define (buffered-read-byte in timeout)
  (define d (decoder-state-input-buf-data (ds)))
  (define buf (decoder-state-input-buf (ds)))
  (match (and d (< (car d) (cdr d)))
    [#t
     (define b (bytes-ref buf (car d)))
     (set-decoder-state-input-buf-data! (ds) (cons (add1 (car d)) (cdr d)))
     b]
    [_ ;; Buffer exhausted or empty — refill
     (define ready (sync/timeout timeout in))
     (if (not ready)
         #f ;; timeout
         (let ([n (read-bytes-avail! buf in)])
           (match n
             [(? eof-object?)
              ;; EOF-ready ports can otherwise return immediately forever and
              ;; make an idle TUI loop busy-spin in noninteractive contexts.
              (when (and (number? timeout) (> timeout 0))
                (sleep timeout))
              #f]
             [(? (lambda (v) (and (integer? v) (> v 0))))
              (set-decoder-state-input-buf-data! (ds) (cons 0 n))
              (buffered-read-byte in 0)]
             [else #f])))]))

;; ============================================================
;; Raw stdin reading
;; ============================================================

(define (real-stdin-read-msg #:timeout [timeout 0.20])
  (define in (current-input-port))
  (define b (buffered-read-byte in timeout))
  (match b
    [#f #f]
    [27
     (define b2 (buffered-read-byte in 0.01))
     (match b2
       [#f (make-tkeymsg-raw 'escape)]
       [91 (decode-csi-sequence in)]
       [_ (make-tkeymsg-raw (integer->char b2))])]
    [13 (make-tkeymsg-raw 'return)]
    [10 (make-tkeymsg-raw 'return)]
    [127 (make-tkeymsg-raw 'backspace)]
    [8 (make-tkeymsg-raw 'backspace)]
    [3 (make-tkeymsg-raw 'ctrl-c)]
    [(? (lambda (v) (>= v 192)))
     (utf8-accumulator-reset!)
     (define lead-char (integer->char b))
     (define total-bytes (utf8-lead-byte-count b))
     (if (= total-bytes 1)
         (make-tkeymsg-raw lead-char)
         (let _loop ()
           (define decoded (utf8-accumulate-char lead-char))
           (match decoded
             [(? values) (make-tkeymsg-raw decoded)]
             [_
              (define cb (buffered-read-byte in 0.05))
              (if (and cb (utf8-continuation-byte? cb))
                  (begin
                    (set! lead-char (integer->char cb))
                    (_loop))
                  (begin
                    (utf8-accumulator-reset!)
                    #f))])))]
    [(? (lambda (v) (>= v 128))) #f]
    [(? (lambda (v) (>= v 32)))
     (utf8-accumulator-reset!)
     (make-tkeymsg-raw (integer->char b))]
    [_ #f]))

(define (default-byte-ready?)
  (char-ready? (current-input-port)))
