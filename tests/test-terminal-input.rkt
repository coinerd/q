#lang racket

;; tests/test-terminal-input.rkt — Tests for tui/terminal-input.rkt
;;
;; Tests pure-logic functions: UTF-8 helpers, message constructors,
;; mouse event predicates, and ANSI decoding from string ports.

(require rackunit
         racket/port
         "../tui/terminal-input.rkt")

;; ============================================================
;; UTF-8 support functions
;; ============================================================

(test-case "utf8-high-byte? returns #t for chars > 127"
  (check-true (utf8-high-byte? (integer->char 200)))
  (check-true (utf8-high-byte? (integer->char 128))))

(test-case "utf8-high-byte? returns #f for ASCII chars"
  (check-false (utf8-high-byte? #\A))
  (check-false (utf8-high-byte? (integer->char 127))))

(test-case "utf8-lead-byte-count for ASCII"
  (check-equal? (utf8-lead-byte-count 65) 1))

(test-case "utf8-lead-byte-count for 2-byte sequence (0xC0-0xDF)"
  (check-equal? (utf8-lead-byte-count 192) 2)
  (check-equal? (utf8-lead-byte-count 223) 2))

(test-case "utf8-lead-byte-count for 3-byte sequence (0xE0-0xEF)"
  (check-equal? (utf8-lead-byte-count 224) 3)
  (check-equal? (utf8-lead-byte-count 239) 3))

(test-case "utf8-lead-byte-count for 4-byte sequence (0xF0-0xF7)"
  (check-equal? (utf8-lead-byte-count 240) 4)
  (check-equal? (utf8-lead-byte-count 247) 4))

(test-case "utf8-lead-byte-count for continuation bytes (0x80-0xBF)"
  (check-equal? (utf8-lead-byte-count 128) 1)
  (check-equal? (utf8-lead-byte-count 191) 1))

(test-case "utf8-continuation-byte? returns #t for 0x80-0xBF"
  (check-true (utf8-continuation-byte? 128))
  (check-true (utf8-continuation-byte? 191))
  (check-true (utf8-continuation-byte? 160)))

(test-case "utf8-continuation-byte? returns #f outside range"
  (check-false (utf8-continuation-byte? 127))
  (check-false (utf8-continuation-byte? 192)))

;; ============================================================
;; reassemble-utf8-chars
;; ============================================================

(test-case "reassemble-utf8-chars decodes 2-byte sequence (ä = 0xC3 0xA4)"
  (define chars (list (integer->char #xC3) (integer->char #xA4)))
  (check-equal? (reassemble-utf8-chars chars) #\ä))

(test-case "reassemble-utf8-chars decodes ASCII"
  (define chars (list #\A))
  (check-equal? (reassemble-utf8-chars chars) #\A))

(test-case "reassemble-utf8-chars decodes 3-byte sequence (€ = 0xE2 0x82 0xAC)"
  (define chars (list (integer->char #xE2) (integer->char #x82) (integer->char #xAC)))
  (check-equal? (reassemble-utf8-chars chars) #\€))

;; ============================================================
;; UTF-8 accumulator
;; ============================================================

(test-case "utf8-accumulator-reset! clears state"
  (utf8-accumulate-char (integer->char #xC3))
  (utf8-accumulator-reset!)
  (check-equal? (utf8-accumulator-length) 0))

(test-case "utf8-accumulate-char returns #f for incomplete sequence"
  (utf8-accumulator-reset!)
  (define result (utf8-accumulate-char (integer->char #xC3)))
  (check-false result)
  (check-equal? (utf8-accumulator-length) 1))

(test-case "utf8-accumulate-char returns char when complete"
  (utf8-accumulator-reset!)
  (utf8-accumulate-char (integer->char #xC3))
  (define result (utf8-accumulate-char (integer->char #xA4)))
  (check-equal? result #\ä)
  (check-equal? (utf8-accumulator-length) 0))

(test-case "utf8-accumulate-char handles ASCII (1 byte)"
  (utf8-accumulator-reset!)
  (define result (utf8-accumulate-char #\Z))
  (check-equal? result #\Z))

;; ============================================================
;; Raw message constructors
;; ============================================================

(test-case "make-tkeymsg-raw creates proper vector"
  (define msg (make-tkeymsg-raw 'enter))
  (check-true (vector? msg))
  (check-equal? (vector-ref msg 0) 'tkeymsg)
  (check-equal? (vector-ref msg 1) 'enter))

(test-case "make-tsizemsg-raw creates proper vector"
  (define msg (make-tsizemsg-raw 100 30))
  (check-true (vector? msg))
  (check-equal? (vector-ref msg 0) 'tsizemsg)
  (check-equal? (vector-ref msg 1) 100)
  (check-equal? (vector-ref msg 2) 30))

(test-case "make-tmousemsg-raw creates proper vector"
  (define msg (make-tmousemsg-raw 32 10 20))
  (check-true (vector? msg))
  (check-equal? (vector-ref msg 0) 'tmousemsg)
  (check-equal? (vector-ref msg 1) 32)
  (check-equal? (vector-ref msg 2) 10)
  (check-equal? (vector-ref msg 3) 20))

;; ============================================================
;; Mouse event predicates and accessors
;; ============================================================

(test-case "tmousemsg? recognizes mouse message"
  (check-true (tmousemsg? (vector 'tmousemsg 32 5 10))))

(test-case "tmousemsg? rejects non-mouse message"
  (check-false (tmousemsg? (vector 'tkeymsg 'x)))
  (check-false (tmousemsg? "not a vector")))

(test-case "tmousemsg accessors extract correct values"
  (define msg (vector 'tmousemsg 35 15 25))
  (check-equal? (tmousemsg-cb msg) 35)
  (check-equal? (tmousemsg-cx msg) 15)
  (check-equal? (tmousemsg-cy msg) 25))

;; ============================================================
;; ANSI escape sequence decoding via real-stdin-read-msg
;; ============================================================

(test-case "real-stdin-read-msg returns #f on empty input (timeout)"
  (define in (open-input-string ""))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.01))
    (check-false result)))

(test-case "real-stdin-read-msg decodes plain ASCII character"
  (define in (open-input-string "a"))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) #\a)))

(test-case "real-stdin-read-msg decodes return (CR)"
  (define in (open-input-string "\r"))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'return)))

(test-case "real-stdin-read-msg decodes return (LF)"
  (define in (open-input-string "\n"))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'return)))

(test-case "real-stdin-read-msg decodes backspace"
  (define in (open-input-bytes (bytes 127)))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'backspace)))

(test-case "real-stdin-read-msg decodes ctrl-c"
  (define in (open-input-bytes (bytes 3)))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'ctrl-c)))

(test-case "real-stdin-read-msg decodes ESC as escape (no follow-up)"
  ;; ESC with no follow-up bytes → escape key
  (define in (open-input-bytes (bytes 27)))
  (parameterize ([current-input-port in])
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'escape)))

(test-case "stub-byte-ready? works on string port"
  (define in (open-input-string ""))
  (parameterize ([current-input-port in])
    ;; char-ready? on an empty string port returns #t (EOF is ready)
    ;; This just checks the function doesn't crash
    (check-true (boolean? (stub-byte-ready?)))))

;; ============================================================
;; CSI modifier handling (#422)
;; ============================================================

(test-case "real-stdin-read-msg decodes Ctrl+Up (ESC[1;5A)"
  ;; CSI 1;5 A = Ctrl+Up arrow
  (define in (open-input-bytes (bytes 27 91 49 59 53 65)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'ctrl-up)))

(test-case "real-stdin-read-msg decodes Ctrl+Down (ESC[1;5B)"
  (define in (open-input-bytes (bytes 27 91 49 59 53 66)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'ctrl-down)))

(test-case "real-stdin-read-msg decodes Ctrl+Right (ESC[1;5C)"
  (define in (open-input-bytes (bytes 27 91 49 59 53 67)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'ctrl-right)))

(test-case "real-stdin-read-msg decodes Ctrl+Left (ESC[1;5D)"
  (define in (open-input-bytes (bytes 27 91 49 59 53 68)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'ctrl-left)))

(test-case "real-stdin-read-msg decodes Shift+Up (ESC[1;2A)"
  (define in (open-input-bytes (bytes 27 91 49 59 50 65)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'shift-up)))

(test-case "real-stdin-read-msg decodes Alt+Up (ESC[1;3A)"
  (define in (open-input-bytes (bytes 27 91 49 59 51 65)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'alt-up)))

(test-case "plain arrow key without modifier still works (ESC[A)"
  (define in (open-input-bytes (bytes 27 91 65)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'up)))

;; ============================================================
;; Paste buffer cap (#428)
;; ============================================================

(test-case "paste-buffer-add! respects 1MB cap"
  (paste-buffer-reset!)
  (set-in-paste! #t)
  ;; Add small amount
  (paste-buffer-add! "hello")
  (check-equal? (paste-buffer-get) "hello")
  ;; Adding to existing buffer works
  (paste-buffer-add! " world")
  (check-equal? (paste-buffer-get) "hello world")
  (set-in-paste! #f)
  (paste-buffer-reset!))

;; ============================================================
;; Bracketed paste CSI decoding (#493)
;; ============================================================

(test-case "real-stdin-read-msg decodes bracketed paste (ESC[200~textESC[201~)"
  ;; Simulate: ESC[200~helloESC[201~
  (define paste-bytes (bytes-append #"\x1b[200~hello\x1b[201~"))
  (define in (open-input-bytes paste-bytes))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (paste-buffer-reset!)
    (set-in-paste! #f)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (paste-event? result) "should be a paste event")
    (when (paste-event? result)
      (check-equal? (vector-ref result 1) "hello" "paste content"))
    (check-false (in-paste?) "should have exited paste mode")
    (paste-buffer-reset!)))

(test-case "real-stdin-read-msg decodes bracketed paste with multi-byte UTF-8"
  ;; Simulate: ESC[200~日本語ESC[201~
  (define paste-bytes
    (bytes-append #"\x1b[200~" (string->bytes/utf-8 "\u65e5\u672c\u8a9e") #"\x1b[201~"))
  (define in (open-input-bytes paste-bytes))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (paste-buffer-reset!)
    (set-in-paste! #f)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (paste-event? result) "should be a paste event")
    (when (paste-event? result)
      (check-equal? (vector-ref result 1) "\u65e5\u672c\u8a9e" "paste content"))
    (paste-buffer-reset!)))

(test-case "CSI tilde multi-digit: ESC[5~ is page-up"
  (define in (open-input-bytes (bytes 27 91 53 126)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg)
    (check-equal? (vector-ref result 1) 'page-up)))

;; ============================================================
;; SGR mouse decoding (mode 1006) — v0.10.7
;; ============================================================

(test-case "SGR mouse: left click press (ESC[<0;5;3M)"
  ;; SGR: ESC[<0;5;3M  button=0 (left), x=5, y=3, M=press
  ;; X10 equivalent: cb=32+0=32, cx=5+32=37, cy=3+32=35
  (define in (open-input-bytes (bytes 27 91 60 48 59 53 59 51 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 32 "cb = 32 (left press)")
      (check-equal? (tmousemsg-cx result) 37 "cx = 5+32")
      (check-equal? (tmousemsg-cy result) 35 "cy = 3+32"))))

(test-case "SGR mouse: left click release (ESC[<0;5;3m)"
  ;; SGR: ESC[<0;5;3m  button=0, x=5, y=3, m=release
  ;; X10 release: cb=35 (button=3), cx=37, cy=35
  (define in (open-input-bytes (bytes 27 91 60 48 59 53 59 51 109)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 35 "cb = 35 (release)")
      (check-equal? (tmousemsg-cx result) 37)
      (check-equal? (tmousemsg-cy result) 35))))

(test-case "SGR mouse: scroll up (ESC[<64;10;5M)"
  ;; SGR: ESC[<64;10;5M  button=64 (wheel-up), x=10, y=5
  ;; X10: cb=32+64=96, cx=10+32=42, cy=5+32=37
  (define in (open-input-bytes (bytes 27 91 60 54 52 59 49 48 59 53 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 96 "cb = 96 (scroll up)")
      (check-equal? (tmousemsg-cx result) 42)
      (check-equal? (tmousemsg-cy result) 37))))

(test-case "SGR mouse: scroll down (ESC[<65;10;5M)"
  ;; SGR: ESC[<65;10;5M  button=65 (wheel-down), x=10, y=5
  ;; X10: cb=32+65=97, cx=42, cy=37
  (define in (open-input-bytes (bytes 27 91 60 54 53 59 49 48 59 53 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 97 "cb = 97 (scroll down)")
      (check-equal? (tmousemsg-cx result) 42)
      (check-equal? (tmousemsg-cy result) 37))))

(test-case "SGR mouse: drag (ESC[<32;8;4M)"
  ;; SGR: ESC[<32;8;4M  button=32+0 (motion+left), x=8, y=4
  ;; X10: cb=32+32=64, cx=40, cy=36
  (define in (open-input-bytes (bytes 27 91 60 51 50 59 56 59 52 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 64 "cb = 64 (drag)")
      (check-equal? (tmousemsg-cx result) 40)
      (check-equal? (tmousemsg-cy result) 36))))

(test-case "SGR mouse: high coordinates (ESC[<0;300;200M)"
  ;; SGR: ESC[<0;300;200M  button=0, x=300, y=200
  ;; X10: cb=32, cx=332, cy=232
  (define sgr-bytes (bytes-append #"\x1b[<0;300;200M"))
  (define in (open-input-bytes sgr-bytes))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 32)
      (check-equal? (tmousemsg-cx result) 332)
      (check-equal? (tmousemsg-cy result) 232))))

(test-case "SGR mouse: incomplete sequence returns escape"
  ;; ESC[<0;5  with no terminator — should fall back to escape
  (define in (open-input-bytes (bytes 27 91 60 48 59 53)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (vector? result))
    (check-equal? (vector-ref result 0) 'tkeymsg "incomplete SGR returns key msg")
    (check-equal? (vector-ref result 1) 'escape "incomplete SGR = escape")))

(test-case "X10 mouse still works (ESC[M cb cx cy)"
  ;; Ensure existing X10 path is not broken
  ;; ESC[M  cb=32(left), cx=38(col 5), cy=36(row 3)
  (define in (open-input-bytes (bytes 27 91 77 32 38 36)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 32)
      (check-equal? (tmousemsg-cx result) 38)
      (check-equal? (tmousemsg-cy result) 36))))

(test-case "SGR mouse: right click press (ESC[<2;10;5M)"
  ;; SGR: ESC[<2;10;5M  button=2 (right), x=10, y=5
  ;; X10: cb=32+2=34, cx=42, cy=37
  (define in (open-input-bytes (bytes 27 91 60 50 59 49 48 59 53 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 34 "cb = 34 (right press)")
      (check-equal? (tmousemsg-cx result) 42)
      (check-equal? (tmousemsg-cy result) 37))))

(test-case "SGR mouse: middle click press (ESC[<1;10;5M)"
  ;; SGR: ESC[<1;10;5M  button=1 (middle), x=10, y=5
  ;; X10: cb=32+1=33, cx=42, cy=37
  (define in (open-input-bytes (bytes 27 91 60 49 59 49 48 59 53 77)))
  (parameterize ([current-input-port in])
    (input-buffer-reset!)
    (define result (real-stdin-read-msg #:timeout 0.1))
    (check-true (tmousemsg? result) "should be a mouse message")
    (when (tmousemsg? result)
      (check-equal? (tmousemsg-cb result) 33 "cb = 33 (middle press)")
      (check-equal? (tmousemsg-cx result) 42)
      (check-equal? (tmousemsg-cy result) 37))))
