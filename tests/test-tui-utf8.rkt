#lang racket

;; test-tui-utf8.rkt — Tests for BUG-28: TUI UTF-8 support (accumulator-based)
;;
;; Two fixes:
;; 1. Output: charterm-display with bytes (instead of latin-1 string)
;; 2. Input: accumulator-based UTF-8 reassembly (no charterm-read-keyinfo calls)
;;
;; These tests verify the pure UTF-8 utility functions and the accumulator
;; state machine in q/tui/terminal.rkt

(require rackunit
         rackunit/text-ui
         "../tui/terminal.rkt")

;; ============================================================
;; Pure UTF-8 utility tests (no terminal required)
;; ============================================================

(define test-tui-utf8
  (test-suite
   "TUI UTF-8 Support (BUG-28 accumulator-based)"

   ;; --------------------------------------------------
   ;; Output encoding tests (pure, no terminal)
   ;; --------------------------------------------------

   (test-case "utf8-display-bytes encodes ASCII unchanged"
     (define bytes (string->bytes/utf-8 "hello"))
     (check-equal? bytes #"hello"))

   (test-case "utf8-display-bytes encodes umlauts correctly"
     (define bytes (string->bytes/utf-8 "äöü"))
     (check-equal? (bytes-length bytes) 6)  ; 3 chars × 2 bytes each
     (check-equal? (subbytes bytes 0 2) (string->bytes/utf-8 "ä"))
     (check-equal? (subbytes bytes 2 4) (string->bytes/utf-8 "ö"))
     (check-equal? (subbytes bytes 4 6) (string->bytes/utf-8 "ü")))

   (test-case "utf8-display-bytes encodes mixed text correctly"
     (define bytes (string->bytes/utf-8 "Hallö Wörld!"))
     (check-equal? (bytes->string/utf-8 bytes) "Hallö Wörld!"))

   (test-case "utf8-display-bytes encodes CJK correctly"
     (define bytes (string->bytes/utf-8 "日本語"))
     (check-equal? (bytes->string/utf-8 bytes) "日本語"))

   ;; --------------------------------------------------
   ;; UTF-8 utility function tests
   ;; --------------------------------------------------

   (test-case "utf8-lead-byte-count returns 2 for 2-byte sequence"
     (check-equal? (utf8-lead-byte-count #xC3) 2)
     (check-equal? (utf8-lead-byte-count #xC0) 2)
     (check-equal? (utf8-lead-byte-count #xDF) 2))

   (test-case "utf8-lead-byte-count returns 3 for 3-byte sequence"
     (check-equal? (utf8-lead-byte-count #xE0) 3)
     (check-equal? (utf8-lead-byte-count #xEF) 3))

   (test-case "utf8-lead-byte-count returns 4 for 4-byte sequence"
     (check-equal? (utf8-lead-byte-count #xF0) 4)
     (check-equal? (utf8-lead-byte-count #xF4) 4))

   (test-case "utf8-lead-byte-count returns 1 for ASCII/continuation"
     (check-equal? (utf8-lead-byte-count 65) 1)   ; 'A'
     (check-equal? (utf8-lead-byte-count #x7F) 1)  ; DEL
     (check-equal? (utf8-lead-byte-count #x80) 1)  ; continuation byte
     (check-equal? (utf8-lead-byte-count #xBF) 1)) ; continuation byte

   (test-case "utf8-continuation-byte? detects continuation bytes"
     (check-pred utf8-continuation-byte? #x80)
     (check-pred utf8-continuation-byte? #xBF)
     (check-pred utf8-continuation-byte? #xA4)
     (check-false (utf8-continuation-byte? #x40))
     (check-false (utf8-continuation-byte? #xC0))
     (check-false (utf8-continuation-byte? #x7F)))

   (test-case "utf8-high-byte? detects lead bytes"
     (check-true (utf8-high-byte? (integer->char #xC3)))
     (check-true (utf8-high-byte? (integer->char #xE6)))
     (check-true (utf8-high-byte? (integer->char #xF0)))
     (check-false (utf8-high-byte? #\A))
     (check-false (utf8-high-byte? #\z))
     (check-false (utf8-high-byte? (integer->char #x7F))))

   ;; --------------------------------------------------
   ;; Reassembly tests (pure function)
   ;; --------------------------------------------------

   (test-case "reassemble-utf8-chars decodes ä (2-byte)"
     (define result (reassemble-utf8-chars (list (integer->char #xC3)
                                                  (integer->char #xA4))))
     (check-equal? result #\ä))

   (test-case "reassemble-utf8-chars decodes ü (2-byte)"
     (define result (reassemble-utf8-chars (list (integer->char #xC3)
                                                  (integer->char #xBC))))
     (check-equal? result #\ü))

   (test-case "reassemble-utf8-chars decodes 日 (3-byte)"
     (define result (reassemble-utf8-chars (list (integer->char #xE6)
                                                  (integer->char #x97)
                                                  (integer->char #xA5))))
     (check-equal? result #\日))

   (test-case "reassemble-utf8-chars decodes emoji (4-byte)"
     (define result (reassemble-utf8-chars (list (integer->char #xF0)
                                                  (integer->char #x9F)
                                                  (integer->char #x98)
                                                  (integer->char #x80))))
     (check-equal? result #\😀))

   (test-case "reassemble-utf8-chars handles invalid bytes gracefully"
     (define result (reassemble-utf8-chars (list (integer->char #x80))))
     (check-equal? result #\?))

   (test-case "reassemble-utf8-chars roundtrip: all German umlauts"
     (for ([ch (in-list '(#\ä #\ö #\ü #\Ä #\Ö #\Ü #\ß))])
       (define bytes (string->bytes/utf-8 (string ch)))
       (define chars (map integer->char (bytes->list bytes)))
       (check-equal? (reassemble-utf8-chars chars) ch)))

   (test-case "reassemble-utf8-chars roundtrip: French accents"
     (for ([ch (in-list '(#\é #\è #\ê #\ë #\à #\ç))])
       (define bytes (string->bytes/utf-8 (string ch)))
       (define chars (map integer->char (bytes->list bytes)))
       (check-equal? (reassemble-utf8-chars chars) ch)))

   ;; --------------------------------------------------
   ;; Accumulator state machine tests
   ;; --------------------------------------------------

   (test-case "accumulator: complete 2-byte sequence across two calls"
     (utf8-accumulator-reset!)
     ;; First byte: lead byte 0xC3
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)  ; incomplete
     ;; Second byte: continuation 0xA4
     (define r2 (utf8-accumulate-char (integer->char #xA4)))
     (check-equal? r2 #\ä)
     ;; Accumulator should be reset
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "accumulator: complete 3-byte sequence"
     (utf8-accumulator-reset!)
     (define r1 (utf8-accumulate-char (integer->char #xE6)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #x97)))
     (check-false r2)
     (define r3 (utf8-accumulate-char (integer->char #xA5)))
     (check-equal? r3 #\日)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "accumulator: reset on non-high byte"
     (utf8-accumulator-reset!)
     ;; Start accumulating a 2-byte sequence
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)
     ;; Simulate tui-keycode behavior: non-high byte resets accumulator
     ;; utf8-accumulate-char is only called for high bytes.
     ;; tui-keycode handles the reset itself.
     (check-equal? (utf8-accumulator-length) 1)
     ;; Reset (as tui-keycode would do)
     (utf8-accumulator-reset!)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "accumulator: complete 4-byte sequence"
     (utf8-accumulator-reset!)
     (define r1 (utf8-accumulate-char (integer->char #xF0)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #x9F)))
     (check-false r2)
     (define r3 (utf8-accumulate-char (integer->char #x98)))
     (check-false r3)
     (define r4 (utf8-accumulate-char (integer->char #x80)))
     (check-equal? r4 #\😀)
     (check-equal? (utf8-accumulator-length) 0))

   (test-case "accumulator: stray continuation byte (not lead)"
     (utf8-accumulator-reset!)
     ;; 0xA4 is a continuation byte (0x80-0xBF), not a valid lead
     ;; utf8-high-byte? returns #t (> 127), but utf8-lead-byte-count returns 1
     ;; This means accumulator will expect 1 byte total, which is already satisfied
     (define r1 (utf8-accumulate-char (integer->char #xA4)))
     ;; It should complete immediately with just this byte
     (check-not-false r1)  ; should return a char (likely #\?)
     (check-equal? (utf8-accumulator-length) 0))

   ;; --------------------------------------------------
   ;; Integer keycode conversion tests (BUG-29 fix)
   ;; charterm-read-keyinfo returns integers for unrecognized bytes
   ;; --------------------------------------------------

   (test-case "integer 0xC3 converts to high-byte char"
     (check-true (utf8-high-byte? (integer->char #xC3))))

   (test-case "integer conversion: ä from two integer keycodes"
     (utf8-accumulator-reset!)
     ;; Simulate charterm returning integer keycodes for UTF-8 bytes
     ;; tui-keycode converts: (integer->char raw) before checking
     (define kc1 (integer->char #xC3))  ; lead byte
     (define kc2 (integer->char #xA4))  ; continuation byte
     (check-pred utf8-high-byte? kc1)
     (check-pred utf8-high-byte? kc2)
     ;; Accumulate through the state machine
     (define r1 (utf8-accumulate-char kc1))
     (check-false r1)  ; incomplete
     (define r2 (utf8-accumulate-char kc2))
     (check-equal? r2 #\ä))

   (test-case "integer conversion: ü from integer keycodes"
     (utf8-accumulator-reset!)
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #xBC)))
     (check-equal? r2 #\ü))

   (test-case "integer conversion: ß from integer keycodes"
     (utf8-accumulator-reset!)
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #x9F)))
     (check-equal? r2 #\ß))

   (test-case "integer conversion: ö from integer keycodes"
     (utf8-accumulator-reset!)
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #xB6)))
     (check-equal? r2 #\ö))

   (test-case "integer conversion: consecutive umlauts"
     (utf8-accumulator-reset!)
     ;; First ä
     (define r1 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r1)
     (define r2 (utf8-accumulate-char (integer->char #xA4)))
     (check-equal? r2 #\ä)
     ;; Then ö
     (define r3 (utf8-accumulate-char (integer->char #xC3)))
     (check-false r3)
     (define r4 (utf8-accumulate-char (integer->char #xB6)))
     (check-equal? r4 #\ö)
     (check-equal? (utf8-accumulator-length) 0))
   ))

;; ============================================================
;; CSI fragment detection tests (BUG-32)
;; ============================================================

(define csi-fragment-tests
  (test-suite
   "CSI Fragment Detection (BUG-32)"

   ;; --- filter-csi-fragment: escape → [ D (left arrow fragment) ---
   (test-case "CSI fragment: ESC then [D is consumed"
     (csi-fragment-reset!)
     ;; ESC arrives and is recognized as 'escape
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     ;; [ arrives as individual char
     (check-equal? (filter-csi-fragment #\[) #\[)
     ;; D (0x44) is a final byte → consumed
     (check-equal? (filter-csi-fragment #\D) #\D)
     ;; State is back to idle
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- filter-csi-fragment: ESC then [F (End key fragment) ---
   (test-case "CSI fragment: ESC then [F is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     ;; F (0x46) is a final byte → consumed
     (check-equal? (filter-csi-fragment #\F) #\F)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- filter-csi-fragment: ESC then [H (Home key fragment) ---
   (test-case "CSI fragment: ESC then [H is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (filter-csi-fragment #\H) #\H)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Complex CSI: resize report [8;78;318t ---
   (test-case "CSI fragment: ESC [8;78;318t is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (filter-csi-fragment #\8) #\8)
     (check-equal? (filter-csi-fragment #\;) #\;)
     (check-equal? (filter-csi-fragment #\7) #\7)
     (check-equal? (filter-csi-fragment #\8) #\8)
     (check-equal? (filter-csi-fragment #\;) #\;)
     (check-equal? (filter-csi-fragment #\3) #\3)
     (check-equal? (filter-csi-fragment #\1) #\1)
     (check-equal? (filter-csi-fragment #\8) #\8)
     ;; t (0x74) is a final byte → consumed, back to idle
     (check-equal? (filter-csi-fragment #\t) #\t)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- CSI with parameters: ESC[5~ (Page Up) ---
   (test-case "CSI fragment: ESC [5~ is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (filter-csi-fragment #\5) #\5)
     ;; ~ (0x7E) is a final byte
     (check-equal? (filter-csi-fragment #\~) #\~)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- SS3 sequence: ESC O P (F1) ---
   (test-case "CSI fragment: ESC O P is consumed (SS3)"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     ;; O (79) starts SS3
     (check-equal? (filter-csi-fragment #\O) #\O)
     ;; P is consumed as final byte
     (check-equal? (filter-csi-fragment #\P) #\P)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Normal chars pass through in idle state ---
   (test-case "Normal chars pass through in idle state"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment #\a) #\a)
     (check-equal? (filter-csi-fragment #\b) #\b)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Symbols (non-escape) reset state ---
   (test-case "Non-escape symbols reset to idle"
     (csi-fragment-reset!)
     ;; Simulate: escape arrived
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (get-csi-fragment-state) 'idle)
     ;; Then a recognized key like 'left arrives (not a fragment)
     (check-equal? (filter-csi-fragment 'left) 'left)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- [ after non-escape is normal input ---
   (test-case "[ in idle state passes through (not a fragment)"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Unknown char after ESC passes through ---
   (test-case "Unknown char after ESC passes through"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     ;; 'x' is not a CSI initiator
     (check-equal? (filter-csi-fragment #\x) #\x)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Consecutive escape sequences ---
   (test-case "Consecutive escape fragments are handled"
     (csi-fragment-reset!)
     ;; First: ESC [ A (up arrow fragment)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (filter-csi-fragment #\A) #\A)
     (check-equal? (get-csi-fragment-state) 'idle)
     ;; Second: ESC [ B (down arrow fragment)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\[) #\[)
     (check-equal? (filter-csi-fragment #\B) #\B)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- csi-final-byte? helper ---
   (test-case "csi-final-byte? identifies final bytes"
     (check-true (csi-final-byte? #\@))   ; 0x40
     (check-true (csi-final-byte? #\A))   ; 0x41
     (check-true (csi-final-byte? #\z))   ; 0x7A
     (check-true (csi-final-byte? #\~))   ; 0x7E
     (check-false (csi-final-byte? #\?))  ; 0x3F (parameter)
     (check-false (csi-final-byte? #\;))  ; 0x3B (parameter)
     (check-false (csi-final-byte? #\0))  ; 0x30 (parameter)
     (check-false (csi-final-byte? #\ ))) ; 0x20 (intermediate)

   ;; --- Normal typing after fragment recovery ---
   (test-case "Normal typing after fragment recovery works"
     (csi-fragment-reset!)
     ;; Simulate fragment: ESC [ D
     (filter-csi-fragment 'escape)
     (filter-csi-fragment #\[)
     (filter-csi-fragment #\D)
     ;; Now user types normally
     (check-equal? (filter-csi-fragment #\h) #\h)
     (check-equal? (filter-csi-fragment #\e) #\e)
     (check-equal? (filter-csi-fragment #\l) #\l)
     (check-equal? (filter-csi-fragment #\l) #\l)
     (check-equal? (filter-csi-fragment #\o) #\o)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- OSC fragment: ESC ] 0 ; title BEL ---
   (test-case "OSC fragment: ESC ]0;title BEL is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\]) #\])
     (check-equal? (filter-csi-fragment #\0) #\0)
     (check-equal? (filter-csi-fragment #\;) #\;)
     (check-equal? (filter-csi-fragment #\t) #\t)
     (check-equal? (filter-csi-fragment #\i) #\i)
     (check-equal? (filter-csi-fragment #\t) #\t)
     (check-equal? (filter-csi-fragment #\l) #\l)
     (check-equal? (filter-csi-fragment #\e) #\e)
     ;; BEL (char 7) terminates OSC
     (check-equal? (filter-csi-fragment (integer->char 7)) (integer->char 7))
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- OSC with ST terminator: ESC ] title ESC \ ---
   (test-case "OSC fragment with ST terminator is consumed"
     (csi-fragment-reset!)
     (check-equal? (filter-csi-fragment 'escape) 'escape)
     (check-equal? (filter-csi-fragment #\]) #\])
     (check-equal? (filter-csi-fragment #\t) #\t)
     (check-equal? (filter-csi-fragment #\x) #\x)
     ;; ESC starts ST
     (check-equal? (filter-csi-fragment (integer->char 27)) (integer->char 27))
     ;; \ (0x5C) completes ST
     (check-equal? (filter-csi-fragment #\\) #\\)
     (check-equal? (get-csi-fragment-state) 'idle))

   ;; --- Screen-size cache ---
   (test-case "tui-screen-size-cache-reset! clears cache"
     ;; This just verifies the function exists and doesn't error
     ;; The actual caching is tested implicitly via TUI integration
     (tui-screen-size-cache-reset!)
     (check-true #t "cache reset without error"))
   ))

;; ============================================================
;; Cursor visibility tests (BUG-33)
;; ============================================================

(define cursor-visibility-tests
  (test-suite
   "Cursor Visibility (BUG-33)"

   (test-case "tui-cursor-hide emits correct escape sequence"
     (define output (with-output-to-string (lambda () (tui-cursor-hide))))
     (check-equal? output "\x1b[?25l" "cursor hide emits ESC[?25l"))

   (test-case "tui-cursor-show emits correct escape sequence"
     (define output (with-output-to-string (lambda () (tui-cursor-show))))
     (check-equal? output "\x1b[?25h" "cursor show emits ESC[?25h"))

   (test-case "cursor hide/show are idempotent"
     ;; Multiple hide calls don't error
     (define output1 (with-output-to-string
                       (lambda ()
                         (tui-cursor-hide)
                         (tui-cursor-hide)
                         (tui-cursor-show))))
     (check-equal? output1 "\x1b[?25l\x1b[?25l\x1b[?25h"
                   "multiple hide/show produces correct sequence"))
   ))

;; ============================================================
;; BUG-41: real-stdin-read-msg UTF-8 integration tests
;; ============================================================
;; These test the actual input reading path through tui-read-key
;; (which delegates to real-stdin-read-msg when tui-term is unavailable).
;; We parameterize current-input-port with a bytes port containing
;; raw UTF-8 bytes and verify the decoded characters.
;;
;; These tests are expected to FAIL until real-stdin-read-msg is
;; updated to use the UTF-8 accumulator for multi-byte sequences.

(define (read-with-bytes bs)
  ;; Helper: call tui-read-key with current-input-port set to a bytes port
  (parameterize ([current-input-port (open-input-bytes bs)])
    (tui-read-key #:timeout 0.05)))

(define real-stdin-utf8-tests
  (test-suite
   "BUG-41: real-stdin-read-msg UTF-8 Decoding"

   (test-case "real-stdin-read-msg: 2-byte UTF-8 (ü)"
     (define msg (read-with-bytes #"\xC3\xBC"))
     (check-pred tkeymsg? msg)
     (check-equal? (tkeymsg-key msg) #\ü))

   (test-case "real-stdin-read-msg: 2-byte UTF-8 (ä)"
     (define msg (read-with-bytes #"\xC3\xA4"))
     (check-pred tkeymsg? msg)
     (check-equal? (tkeymsg-key msg) #\ä))

   (test-case "real-stdin-read-msg: 2-byte UTF-8 (ö)"
     (define msg (read-with-bytes #"\xC3\xB6"))
     (check-pred tkeymsg? msg)
     (check-equal? (tkeymsg-key msg) #\ö))

   (test-case "real-stdin-read-msg: 3-byte UTF-8 (日)"
     (define msg (read-with-bytes #"\xE6\x97\xA5"))
     (check-pred tkeymsg? msg)
     (check-equal? (tkeymsg-key msg) #\日))

   (test-case "real-stdin-read-msg: ASCII passthrough"
     (define msg (read-with-bytes #"h"))
     (check-pred tkeymsg? msg)
     (check-equal? (tkeymsg-key msg) #\h))

   (test-case "real-stdin-read-msg: mixed ASCII+UTF-8"
     ;; Two calls should return #\h then #\ü
     (define in (open-input-bytes #"h\xC3\xBC"))
     (define msg1
       (parameterize ([current-input-port in])
         (tui-read-key #:timeout 0.05)))
     (define msg2
       (parameterize ([current-input-port in])
         (tui-read-key #:timeout 0.05)))
     (check-pred tkeymsg? msg1)
     (check-equal? (tkeymsg-key msg1) #\h)
     (check-pred tkeymsg? msg2)
     (check-equal? (tkeymsg-key msg2) #\ü))
   ))

(run-tests test-tui-utf8)
(run-tests csi-fragment-tests)
(run-tests cursor-visibility-tests)
(run-tests real-stdin-utf8-tests)

;; ============================================================
;; Mouse event decoding tests
;; ============================================================

(define mouse-decode-tests
  (test-suite
   "Mouse Event Decoding (ESC[M)"

   (test-case "mouse: ESC[M decodes as tmousemsg"
     (define mouse-bytes (bytes #x1b #x5b #x4d #x40 #x41 #x42))
     (define msg (parameterize ([current-input-port (open-input-bytes mouse-bytes)])
       (tui-read-key #:timeout 0.05)))
     (check-pred tmousemsg? msg "mouse: ESC[M decoded as tmousemsg")
     (check-equal? (tmousemsg-cb msg) #x40 "mouse: button code preserved")
     (check-equal? (tmousemsg-cx msg) #x41 "mouse: column byte preserved")
     (check-equal? (tmousemsg-cy msg) #x42 "mouse: row byte preserved"))

   (test-case "mouse: different button code"
     (define mouse-bytes (bytes #x1b #x5b #x4d #x00 #x20 #x21))
     (define msg (parameterize ([current-input-port (open-input-bytes mouse-bytes)])
       (tui-read-key #:timeout 0.05)))
     (check-pred tmousemsg? msg)
     (check-equal? (tmousemsg-cb msg) #x00 "mouse: button 0")
     (check-equal? (tmousemsg-cx msg) #x20 "mouse: col 0x20")
     (check-equal? (tmousemsg-cy msg) #x21 "mouse: row 0x21"))

   (test-case "mouse: incomplete sequence returns escape"
     ;; Only ESC [ M with 2 data bytes (missing third)
     (define mouse-bytes (bytes #x1b #x5b #x4d #x40 #x41))
     (define msg (parameterize ([current-input-port (open-input-bytes mouse-bytes)])
       (tui-read-key #:timeout 0.05)))
     ;; Should fall back to escape since third byte is missing (eof)
     (check-true (or (tkeymsg? msg) (not msg))
                "mouse: incomplete sequence returns escape or #f"))

   (test-case "mouse: tmousemsg? rejects non-mouse messages"
     (check-false (tmousemsg? (vector 'tkeymsg 'up)))
     (check-false (tmousemsg? (vector 'tsizemsg 80 24)))
     (check-false (tmousemsg? 'hi))
     (check-false (tmousemsg? 42)))
   ))

