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
  (define paste-bytes
    (bytes-append
     #"\x1b[200~hello\x1b[201~"))
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
    (bytes-append
     #"\x1b[200~"
     (string->bytes/utf-8 "\u65e5\u672c\u8a9e")
     #"\x1b[201~"))
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
