#lang racket/base

;; q/tui/terminal-bridge.rkt — tui-term dynamic loading bridge + stub fallbacks
;;
;; Handles all dynamic-require logic for loading the tui-term library.
;; When tui-term is unavailable, provides stub implementations that use
;; direct ANSI escape sequences and raw stdin reading.
;;
;; Extracted from terminal.rkt for separation of concerns.

(require racket/port
         racket/string
         racket/list
         racket/system)

;; Bridge availability
(provide tui-term-available?

         ;; Selected implementations (real or stub)
         make-tty-term
         term-alternate-screen
         term-normal-screen
         term-hide-cursor
         term-show-cursor
         term-close
         current-term-size

         ;; Dynamic require results (for input selection in facade)
         read-msg-fn
         byte-ready-fn

         ;; Message predicate/accessor functions (tui-term or fallback)
         tkeymsg? ;; adapter predicate
         tkeymsg-key ;; adapter accessor
         tsizemsg? ;; adapter predicate
         tsizemsg-cols ;; adapter accessor
         tsizemsg-rows ;; adapter accessor
         tcmdmsg? ;; adapter predicate
         tcmdmsg-cmd ;; adapter accessor

         ;; Mouse message adapters (#1120)
         tmousemsg-tui-term? ;; true for tui-term struct mouse msgs
         tmousemsg-kind      ;; kind accessor (press/release/move/wheel-up/wheel-down)
         tmousemsg-pos-x     ;; x coordinate (0-based)
         tmousemsg-pos-y     ;; y coordinate (0-based)
         tmousemsg-left?     ;; left button held?
         tmousemsg-middle?   ;; middle button held?
         tmousemsg-right?)   ;; right button held?

;; ============================================================
;; Dynamic require: detect tui-term availability
;; ============================================================

(define tui-term-available?
  (with-handlers ([exn:fail? (lambda (e) #f)])
    (collection-path "tui")
    #t))

;; ============================================================
;; Dynamic requires (will be #f if not available)
;; ============================================================

(define make-tty-term-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'make-tty-term))))

(define term-alternate-screen-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-alternate-screen))))

(define term-normal-screen-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-normal-screen))))

(define term-hide-cursor-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-hide-cursor))))

(define term-show-cursor-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-show-cursor))))

(define term-close-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'term-close))))

(define current-term-size-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'current-term-size))))

(define read-msg-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'read-msg))))

(define byte-ready-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'byte-ready?))))

;; Dynamic require for message struct predicates and accessors
(define tkeymsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tkeymsg?))))

(define tkeymsg-key-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tkeymsg-key))))

(define tsizemsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg?))))

(define tsizemsg-cols-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg-cols))))

(define tsizemsg-rows-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tsizemsg-rows))))

(define tcmdmsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tcmdmsg?))))

(define tcmdmsg-cmd-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tcmdmsg-cmd))))

;; Mouse message dynamic requires (#1120)
(define tmousemsg?-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg?))))

(define tmousemsg-kind-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg-kind))))

(define tmousemsg-pos-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg-pos))))

(define tmousemsg-left-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg-left))))

(define tmousemsg-middle-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg-middle))))

(define tmousemsg-right-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term/messages 'tmousemsg-right))))

;; tpoint accessors for pos field
(define tpoint-x-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'tpoint-x))))

(define tpoint-y-fn
  (and tui-term-available?
       (with-handlers ([exn:fail? (lambda (e) #f)])
         (dynamic-require 'tui/term 'tpoint-y))))

;; ============================================================
;; Stub implementations (when tui-term not available)
;; ============================================================

(define (stub-make-tty-term #:tty [tty #f])
  ;; Put tty into raw mode so we can read individual keypresses.
  ;; Use system() so stty operates on the inherited terminal fd.
  ;; subprocess() doesn't inherit the controlling terminal properly.
  (with-handlers ([exn:fail? void])
    (system "stty raw -echo -icanon < /dev/tty 2>/dev/null"))
  (gensym 'stub-term))

(define (stub-term-alternate-screen term)
  (display "\x1b[?1049h")
  (flush-output))

(define (stub-term-normal-screen term)
  (display "\x1b[?1049l")
  (flush-output))

(define (stub-term-hide-cursor term)
  (display "\x1b[?25l")
  (flush-output))

(define (stub-term-show-cursor term)
  (display "\x1b[?25h")
  (flush-output))

(define (stub-term-close term)
  ;; Restore tty to sane mode
  (with-handlers ([exn:fail? void])
    (system "stty sane < /dev/tty 2>/dev/null")))

(define (stub-current-term-size)
  ;; Try environment variables first (set by shell in most terminals)
  (define env-cols (and (getenv "COLUMNS") (string->number (getenv "COLUMNS"))))
  (define env-rows (and (getenv "LINES") (string->number (getenv "LINES"))))
  (if (and env-cols env-rows (> env-cols 0) (> env-rows 0))
      (values env-cols env-rows)
      ;; Fallback: try stty size
      ;; Uses dynamic-wind to guarantee port cleanup and prevent fd leaks.
      ;; Previously leaked 3 fds per call → fd exhaustion after ~700 calls →
      ;; subprocess failed → hardcoded 80x24 fallback.
      (let ([result-cols 80]
            [result-rows 24])
        (define-values (sp stty-out stty-in stty-err)
          (with-handlers ([exn:fail? (lambda (e) (values #f #f #f #f))])
            (subprocess #f #f #f "/bin/sh" "-c" "stty size < /dev/tty 2>/dev/null")))
        (dynamic-wind (lambda () (void))
                      (lambda ()
                        (when stty-in
                          (close-output-port stty-in))
                        (when stty-out
                          (let* ([out-str (port->string stty-out)]
                                 [parts (string-split out-str)])
                            (when (= (length parts) 2)
                              (set! result-cols (string->number (second parts)))
                              (set! result-rows (string->number (first parts)))))))
                      (lambda ()
                        (when stty-out
                          (close-input-port stty-out))
                        (when stty-err
                          (close-input-port stty-err))
                        (when sp
                          (sync/timeout 1.0 sp))))
        (values result-cols result-rows))))

;; ============================================================
;; Select actual or stub implementations
;; ============================================================

(define make-tty-term (or make-tty-term-fn stub-make-tty-term))

(define term-alternate-screen (or term-alternate-screen-fn stub-term-alternate-screen))

(define term-normal-screen (or term-normal-screen-fn stub-term-normal-screen))

(define term-hide-cursor (or term-hide-cursor-fn stub-term-hide-cursor))

(define term-show-cursor (or term-show-cursor-fn stub-term-show-cursor))

(define term-close (or term-close-fn stub-term-close))

(define current-term-size (or current-term-size-fn stub-current-term-size))

;; ============================================================
;; Message struct bindings — dispatch to real tui-term or vector fallback
;; ============================================================

(define (tkeymsg? msg)
  (or (and tkeymsg?-fn (tkeymsg?-fn msg)) (and (vector? msg) (eq? (vector-ref msg 0) 'tkeymsg))))

(define (tkeymsg-key msg)
  (if (and tkeymsg-key-fn (tkeymsg?-fn msg))
      (tkeymsg-key-fn msg)
      (vector-ref msg 1)))

(define (tsizemsg? msg)
  (or (and tsizemsg?-fn (tsizemsg?-fn msg)) (and (vector? msg) (eq? (vector-ref msg 0) 'tsizemsg))))

(define (tsizemsg-cols msg)
  (if (and tsizemsg-cols-fn (tsizemsg?-fn msg))
      (tsizemsg-cols-fn msg)
      (vector-ref msg 1)))

(define (tsizemsg-rows msg)
  (if (and tsizemsg-rows-fn (tsizemsg?-fn msg))
      (tsizemsg-rows-fn msg)
      (vector-ref msg 2)))

(define (tcmdmsg? msg)
  (or (and tcmdmsg?-fn (tcmdmsg?-fn msg)) (and (vector? msg) (eq? (vector-ref msg 0) 'tcmdmsg))))

(define (tcmdmsg-cmd msg)
  (if (and tcmdmsg-cmd-fn (tcmdmsg?-fn msg))
      (tcmdmsg-cmd-fn msg)
      (vector-ref msg 1)))

;; ============================================================
;; Mouse message adapters (#1120)
;; tui-term returns actual tmousemsg structs (not vectors).
;; These adapters detect struct vs vector and dispatch accordingly.
;; ============================================================

(define (tmousemsg-tui-term? msg)
  ;; True if msg is a tui-term tmousemsg struct (not a vector).
  (and tmousemsg?-fn (tmousemsg?-fn msg)))

(define (tmousemsg-kind msg)
  ;; Returns kind symbol: 'press, 'release, 'move, 'wheel-up, 'wheel-down, 'leave
  (if (tmousemsg-tui-term? msg)
      (tmousemsg-kind-fn msg)
      'unknown))

(define (tmousemsg-pos-x msg)
  ;; Returns 0-based x coordinate
  (if (and (tmousemsg-tui-term? msg) tmousemsg-pos-fn tpoint-x-fn)
      (tpoint-x-fn (tmousemsg-pos-fn msg))
      0))

(define (tmousemsg-pos-y msg)
  ;; Returns 0-based y coordinate
  (if (and (tmousemsg-tui-term? msg) tmousemsg-pos-fn tpoint-y-fn)
      (tpoint-y-fn (tmousemsg-pos-fn msg))
      0))

(define (tmousemsg-left? msg)
  ;; True if left button is held
  (if (and (tmousemsg-tui-term? msg) tmousemsg-left-fn)
      (tmousemsg-left-fn msg)
      #f))

(define (tmousemsg-middle? msg)
  ;; True if middle button is held
  (if (and (tmousemsg-tui-term? msg) tmousemsg-middle-fn)
      (tmousemsg-middle-fn msg)
      #f))

(define (tmousemsg-right? msg)
  ;; True if right button is held
  (if (and (tmousemsg-tui-term? msg) tmousemsg-right-fn)
      (tmousemsg-right-fn msg)
      #f))
