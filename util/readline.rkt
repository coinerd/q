#lang racket/base

;; util/readline.rkt — readline FFI with graceful fallback
;;
;; Provides:
;;   readline-available?     — boolean: was readline loaded?
;;   read-line-with-history  — readline wrapper with history
;;
;; Extracted from cli/interactive.rkt (Issue #203).

(require racket/string
         (only-in ffi/unsafe ffi-lib get-ffi-obj _fun _string _int))

(provide readline-available?
         read-line-with-history)

;; ============================================================
;; Readline support
;; ============================================================

;; readline support: try to load, fall back to plain read-line
;; Prefer GNU libreadline over libedit for proper UTF-8 support.
;; libedit has known issues with multi-byte UTF-8 characters (äüö → C<C$C6C).
(void (unless (getenv "PLT_READLINE_LIB")
        (putenv "PLT_READLINE_LIB" "libreadline.so.8")))

(define readline-available?
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (void (dynamic-require 'readline/rktrl #f))
    ;; Disable bracketed paste mode — readline 8.2 enables it by default,
    ;; but the Racket rl_getc_function wrapper can mishandle the ANSI
    ;; escape sequences, causing pasted text to be duplicated in the
    ;; returned line (first few characters get prepended).
    (with-handlers ([exn:fail? void])
      (define librl (ffi-lib "libreadline" '("8.2" "8.1" "8" #f)))
      (define rl-variable-bind (get-ffi-obj "rl_variable_bind" librl (_fun _string _string -> _int)))
      (rl-variable-bind "enable-bracketed-paste" "off"))
    #t))

(define (read-line-with-history prompt in [out (current-output-port)])
  ;; Use readline when available and stdin is a terminal-like port
  ;; (not a pipe or string port, which would cause readline to fail)
  (cond
    [(and readline-available? (eq? in (current-input-port)))
     ;; Only use readline for real stdin — not for test ports
     (define rl-readline (dynamic-require 'readline/rktrl 'readline))
     (define rl-add-history (dynamic-require 'readline/rktrl 'add-history))
     (with-handlers ([exn:fail? (lambda (_)
                                  ;; readline failed (not a TTY) — fall back
                                  (display prompt out)
                                  (flush-output out)
                                  (read-line in))])
       (define line (rl-readline prompt))
       (when (and (string? line) (not (string=? (string-trim line) "")))
         (rl-add-history line))
       line)]
    [else
     ;; Non-stdin port (test mode) — display prompt to specified output
     (display prompt out)
     (flush-output out)
     (read-line in)]))
