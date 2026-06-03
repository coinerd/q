#lang racket/base

;; q/tui/terminal-native.rkt — Native terminal I/O (no external dependencies)
;;
;; Provides all terminal control functions using direct ANSI escape sequences,
;; stty commands, and native Racket I/O.
;;
;; Replaces the former terminal-bridge.rkt dynamic-loading module.
;;
;; Terminal capabilities:
;;   - Raw mode (stty raw -echo)
;;   - Alternate screen (DEC 1049)
;;   - Cursor show/hide (DEC 25)
;;   - Screen size (env vars + stty size)
;;   - Mouse tracking (SGR mode 1006)
;;   - Bracketed paste (DEC 2004)
;;   - Synchronized output (DEC 2026)
;;   - Kitty keyboard protocol (CSI-u)

(require "../util/error/error-helpers.rkt")
(require racket/port
         racket/string
         racket/list
         racket/system)

;; Terminal lifecycle
(provide make-tty-term
         term-alternate-screen
         term-normal-screen
         term-hide-cursor
         term-show-cursor
         term-close
         current-term-size

         ;; Message predicates and accessors (native vector-based)
         tkeymsg?
         tkeymsg-key
         tsizemsg?
         tsizemsg-cols
         tsizemsg-rows
         tcmdmsg?
         tcmdmsg-cmd

         ;; Mouse message accessors
         tmousemsg-kind
         tmousemsg-pos-x
         tmousemsg-pos-y
         tmousemsg-left?
         tmousemsg-middle?
         tmousemsg-right?

         read-msg-fn
         byte-ready-fn)

;; ============================================================
;; Compatibility (native-only, no external packages)
;; ============================================================

(define read-msg-fn #f)
(define byte-ready-fn #f)

;; ============================================================
;; Terminal lifecycle — native implementations
;; ============================================================

(define (make-tty-term #:tty [tty #f])
  ;; Put tty into raw mode so we can read individual keypresses.
  ;; Use system() so stty operates on the inherited terminal fd.
  (with-handlers ([exn:fail? void])
    (system "stty raw -echo -icanon < /dev/tty 2>/dev/null"))
  (gensym 'native-term))

(define (term-alternate-screen term)
  (display "\x1b[?1049h")
  (flush-output))

(define (term-normal-screen term)
  (display "\x1b[?1049l")
  (flush-output))

(define (term-hide-cursor term)
  (display "\x1b[?25l")
  (flush-output))

(define (term-show-cursor term)
  (display "\x1b[?25h")
  (flush-output))

(define (term-close term)
  ;; Restore tty to sane mode
  (with-handlers ([exn:fail? void])
    (system "stty sane < /dev/tty 2>/dev/null")))

(define (current-term-size)
  ;; Try environment variables first (set by shell in most terminals)
  (define env-cols (and (getenv "COLUMNS") (string->number (getenv "COLUMNS"))))
  (define env-rows (and (getenv "LINES") (string->number (getenv "LINES"))))
  (if (and env-cols env-rows (> env-cols 0) (> env-rows 0))
      (values env-cols env-rows)
      ;; Fallback: try stty size
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
;; Message predicates and accessors (vector-based)
;; ============================================================
;; Messages are represented as vectors: #(tag args...)
;; This is the native format produced by terminal-input.rkt.

(define (tkeymsg? msg)
  (and (vector? msg) (positive? (vector-length msg)) (eq? (vector-ref msg 0) 'tkeymsg)))

(define (tkeymsg-key msg)
  (vector-ref msg 1))

(define (tsizemsg? msg)
  (and (vector? msg) (positive? (vector-length msg)) (eq? (vector-ref msg 0) 'tsizemsg)))

(define (tsizemsg-cols msg)
  (vector-ref msg 1))

(define (tsizemsg-rows msg)
  (vector-ref msg 2))

(define (tcmdmsg? msg)
  (and (vector? msg) (positive? (vector-length msg)) (eq? (vector-ref msg 0) 'tcmdmsg)))

(define (tcmdmsg-cmd msg)
  (vector-ref msg 1))

;; ============================================================
;; Mouse message accessors (native)
;; ============================================================
;; Native mouse messages are vectors: #(tmousemsg kind x y left? middle? right?)

(define (tmousemsg-kind msg)
  (if (and (vector? msg) (> (vector-length msg) 1) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 1)
      'unknown))

(define (tmousemsg-pos-x msg)
  (if (and (vector? msg) (> (vector-length msg) 2) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 2)
      0))

(define (tmousemsg-pos-y msg)
  (if (and (vector? msg) (> (vector-length msg) 3) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 3)
      0))

(define (tmousemsg-left? msg)
  (if (and (vector? msg) (> (vector-length msg) 4) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 4)
      #f))

(define (tmousemsg-middle? msg)
  (if (and (vector? msg) (> (vector-length msg) 5) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 5)
      #f))

(define (tmousemsg-right? msg)
  (if (and (vector? msg) (> (vector-length msg) 6) (eq? (vector-ref msg 0) 'tmousemsg))
      (vector-ref msg 6)
      #f))
