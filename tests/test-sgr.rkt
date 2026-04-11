#lang racket

;; tests/test-sgr.rkt — tests for tui/sgr.rkt
;;
;; Tests SGR post-processing that replaces bg=black (40) with
;; default bg (49) in ANSI escape sequences.

(require rackunit
         "../tui/sgr.rkt")

;; ============================================================
;; 1. No-op cases — strings without SGR sequences
;; ============================================================

(test-case
 "plain text unchanged"
 (check-equal? (fix-sgr-bg-black "Hello, world!") "Hello, world!"))

(test-case
 "empty string unchanged"
 (check-equal? (fix-sgr-bg-black "") ""))

;; ============================================================
;; 2. Simple SGR replacement: bg=black → default
;; ============================================================

(test-case
 "single bg=black SGR replaced"
 (check-equal?
  (fix-sgr-bg-black (format "\e[40mtext"))
  (format "\e[49mtext")))

(test-case
 "compound SGR: fg white + bg black"
 (check-equal?
  (fix-sgr-bg-black (format "\e[37;40mtext"))
  (format "\e[37;49mtext")))

(test-case
 "reset does not interfere"
 (check-equal?
  (fix-sgr-bg-black (format "\e[0;40mtext"))
  (format "\e[0;49mtext")))

;; ============================================================
;; 3. Multiple SGR sequences in one string
;; ============================================================

(test-case
 "two separate bg=black sequences both replaced"
 (check-equal?
  (fix-sgr-bg-black (format "\e[40mhello\e[0m \e[40mworld\e[0m"))
  (format "\e[49mhello\e[0m \e[49mworld\e[0m")))

(test-case
 "bg=black and fg-only sequence: only bg changed"
 (check-equal?
  (fix-sgr-bg-black (format "\e[32mgreen\e[40mblackbg"))
  (format "\e[32mgreen\e[49mblackbg")))

;; ============================================================
;; 4. Extended color sequences preserved
;; ============================================================

(test-case
 "256-color fg with param 40 unchanged"
 ;; \e[38;5;40m is fg=256-color-index-40, NOT bg=black
 (check-equal?
  (fix-sgr-bg-black (format "\e[38;5;40mtext"))
  (format "\e[38;5;40mtext")))

(test-case
 "256-color bg with param 40 unchanged"
 ;; \e[48;5;40m is bg=256-color-index-40, NOT bg=black
 (check-equal?
  (fix-sgr-bg-black (format "\e[48;5;40mtext"))
  (format "\e[48;5;40mtext")))

(test-case
 "truecolor fg preserved, bg=black still replaced"
 ;; \e[38;2;100;200;50;40m — 38;2;R;G;B is truecolor fg, then 40 is bg=black
 ;; After processing: 38;2;100;200;50 preserved, 40→49
 (check-equal?
  (fix-sgr-bg-black (format "\e[38;2;100;200;50;40mtext"))
  (format "\e[38;2;100;200;50;49mtext")))

(test-case
 "truecolor bg sequence preserved"
 ;; \e[48;2;100;200;50m is truecolor bg — param 40 doesn't appear
 (check-equal?
  (fix-sgr-bg-black (format "\e[48;2;100;200;50mtext"))
  (format "\e[48;2;100;200;50mtext")))

;; ============================================================
;; 5. Edge cases
;; ============================================================

(test-case
 "bare ESC[m (no params) unchanged"
 (check-equal?
  (fix-sgr-bg-black (format "\e[mtext"))
  (format "\e[mtext")))

(test-case
 "only fg color, no bg"
 (check-equal?
  (fix-sgr-bg-black (format "\e[31mred text"))
  (format "\e[31mred text")))

(test-case
 "bg=bright black (100-107 range) not affected"
 ;; SGR 100 is bright black bg, not 40
 (check-equal?
  (fix-sgr-bg-black (format "\e[100mtext"))
  (format "\e[100mtext")))
