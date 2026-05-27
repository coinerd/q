#lang racket

;; BOUNDARY: integration

;; tests/test-terminal-native.rkt — Tests for tui/terminal-native.rkt
;;
;; Tests the native terminal functions and message predicates/accessors.

(require rackunit
         racket/port
         "../tui/terminal-native.rkt")

;; ============================================================
;; tui-term-available? is always #f (native-only)
;; ============================================================

(test-case "tui-term-available? is #f (native-only)"
  (check-false tui-term-available?))

;; ============================================================
;; make-tty-term returns a value
;; ============================================================

(test-case "make-tty-term returns a value"
  ;; In CI without a real TTY, this returns a gensym
  (check-not-exn (lambda () (make-tty-term))))

;; ============================================================
;; Native ANSI escape sequence functions (no crash)
;; ============================================================

(test-case "term-alternate-screen outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-alternate-screen 'native))
  (check-equal? (get-output-string out) "\x1b[?1049h"))

(test-case "term-normal-screen outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-normal-screen 'native))
  (check-equal? (get-output-string out) "\x1b[?1049l"))

(test-case "term-hide-cursor outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-hide-cursor 'native))
  (check-equal? (get-output-string out) "\x1b[?25l"))

(test-case "term-show-cursor outputs ANSI sequence"
  (define out (open-output-string))
  (parameterize ([current-output-port out])
    (term-show-cursor 'native))
  (check-equal? (get-output-string out) "\x1b[?25h"))

;; ============================================================
;; current-term-size returns two values
;; ============================================================

(test-case "current-term-size returns two positive integers"
  (define-values (cols rows) (current-term-size))
  (check-true (and (integer? cols) (positive? cols)))
  (check-true (and (integer? rows) (positive? rows))))

;; ============================================================
;; Message predicates and accessors (vector-based)
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
;; Native message functions (always #f — no tui-term)
;; ============================================================

(test-case "read-msg-fn is #f (native-only)"
  (check-false read-msg-fn))

(test-case "byte-ready-fn is #f (native-only)"
  (check-false byte-ready-fn))

;; ============================================================
;; Mouse message accessors
;; ============================================================

(test-case "tmousemsg-tui-term? is always #f"
  (check-false (tmousemsg-tui-term? (vector 'tmousemsg 'press 10 20 #t #f #f))))

(test-case "tmousemsg-kind extracts kind from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #t #f #f))
  (check-equal? (tmousemsg-kind msg) 'press))

(test-case "tmousemsg-pos-x extracts x from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #t #f #f))
  (check-equal? (tmousemsg-pos-x msg) 10))

(test-case "tmousemsg-pos-y extracts y from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #t #f #f))
  (check-equal? (tmousemsg-pos-y msg) 20))

(test-case "tmousemsg-left? extracts left button from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #t #f #f))
  (check-true (tmousemsg-left? msg)))

(test-case "tmousemsg-middle? extracts middle button from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #f #t #f))
  (check-true (tmousemsg-middle? msg)))

(test-case "tmousemsg-right? extracts right button from vector"
  (define msg (vector 'tmousemsg 'press 10 20 #f #f #t))
  (check-true (tmousemsg-right? msg)))

(test-case "tmousemsg-kind returns 'unknown for non-vector"
  (check-equal? (tmousemsg-kind "not-a-msg") 'unknown))

(test-case "tmousemsg-pos-x returns 0 for non-vector"
  (check-equal? (tmousemsg-pos-x "not-a-msg") 0))

(test-case "tmousemsg-pos-y returns 0 for non-vector"
  (check-equal? (tmousemsg-pos-y "not-a-msg") 0))
