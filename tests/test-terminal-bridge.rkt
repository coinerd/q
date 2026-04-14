#lang racket

;; tests/test-terminal-bridge.rkt — Tests for tui/terminal-bridge.rkt
;;
;; Tests the stub fallback functions and message predicates/accessors
;; from the terminal bridge module.

(require rackunit
         racket/port
         "../tui/terminal-bridge.rkt")

;; ============================================================
;; tui-term-available? predicate
;; ============================================================

(test-case "tui-term-available? is a boolean"
  (check-true (boolean? tui-term-available?)))

;; ============================================================
;; make-tty-term returns a value (stub or real)
;; ============================================================

(test-case "make-tty-term returns a value"
  ;; In CI without a real TTY, this returns a gensym (stub)
  (check-not-exn (lambda () (make-tty-term))))

;; ============================================================
;; Stub ANSI escape sequence functions (no crash)
;; ============================================================

(test-case "term-alternate-screen outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-alternate-screen 'stub))
  (check-equal? (get-output-string out) "\x1b[?1049h"))

(test-case "term-normal-screen outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-normal-screen 'stub))
  (check-equal? (get-output-string out) "\x1b[?1049l"))

(test-case "term-hide-cursor outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-hide-cursor 'stub))
  (check-equal? (get-output-string out) "\x1b[?25l"))

(test-case "term-show-cursor outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-show-cursor 'stub))
  (check-equal? (get-output-string out) "\x1b[?25h"))

;; ============================================================
;; current-term-size returns two values
;; ============================================================

(test-case "current-term-size returns two positive integers"
  (define-values (cols rows) (current-term-size))
  (check-true (and (integer? cols) (positive? cols)))
  (check-true (and (integer? rows) (positive? rows))))

;; ============================================================
;; Message predicates and accessors (vector fallback path)
;; ============================================================

(test-case "tkeymsg? recognizes vector tkeymsg"
  (define msg (vector 'tkeymsg 'enter))
  (check-true (tkeymsg? msg)))

(test-case "tkeymsg? rejects non-tkeymsg"
  (check-false (tkeymsg? "hello"))
  (check-false (tkeymsg? (vector 'other 42)))
  (check-false (tkeymsg? 123)))

(test-case "tkeymsg-key extracts key from vector"
  (define msg (vector 'tkeymsg 'up))
  (check-equal? (tkeymsg-key msg) 'up))

(test-case "tsizemsg? recognizes vector tsizemsg"
  (define msg (vector 'tsizemsg 80 24))
  (check-true (tsizemsg? msg)))

(test-case "tsizemsg? rejects non-tsizemsg"
  (check-false (tsizemsg? (vector 'tkeymsg 'x))))

(test-case "tsizemsg-cols and tsizemsg-rows extract dimensions"
  (define msg (vector 'tsizemsg 120 40))
  (check-equal? (tsizemsg-cols msg) 120)
  (check-equal? (tsizemsg-rows msg) 40))

(test-case "tcmdmsg? recognizes vector tcmdmsg"
  (define msg (vector 'tcmdmsg "exit"))
  (check-true (tcmdmsg? msg)))

(test-case "tcmdmsg? rejects non-tcmdmsg"
  (check-false (tcmdmsg? (vector 'tkeymsg 'x))))

(test-case "tcmdmsg-cmd extracts command string"
  (define msg (vector 'tcmdmsg "resize"))
  (check-equal? (tcmdmsg-cmd msg) "resize"))

;; ============================================================
;; Dynamic require results (may be #f in test env)
;; ============================================================

(test-case "read-msg-fn is boolean or procedure or #f"
  (check-true (or (procedure? read-msg-fn) (not read-msg-fn))))

(test-case "byte-ready-fn is boolean or procedure or #f"
  (check-true (or (procedure? byte-ready-fn) (not byte-ready-fn))))
