#lang racket/base

;; util/output-guard.rkt — Stdout protection during TUI mode (#1181)
;;
;; Prevents stray stdout writes from corrupting the TUI display.
;; During TUI mode, current-output-port is redirected to a null sink
;; (or stderr). The TUI renderer uses a saved real port for its
;; intentional terminal writes.

(require racket/match
         racket/port)

(provide
 ;; Guard lifecycle
 call-with-output-guard
 with-output-guard
 ;; Access the real terminal port inside the guard
 guarded-real-output-port
 ;; Escape hatch for intentional raw output
 call-with-raw-output)

;; ============================================================
;; Parameters
;; ============================================================

;; Holds the real terminal output port while the guard is active.
;; #f when guard is not active.
(define guarded-real-output-port (make-parameter #f))

;; ============================================================
;; Output guard
;; ============================================================

;; Null sink — discards all output
(define (make-null-sink)
  (open-output-nowhere))

;; Call `thunk` with current-output-port redirected to a null sink.
;; The real terminal port is available via (guarded-real-output-port).
(define (call-with-output-guard thunk
                                #:redirect-to [redirect-port #f])
  (define real-port (current-output-port))
  (define sink (or redirect-port (make-null-sink)))
  (parameterize ([current-output-port sink]
                 [guarded-real-output-port real-port])
    (thunk)))

;; Macro form
(define-syntax-rule (with-output-guard #:redirect-to port body ...)
  (call-with-output-guard (lambda () body ...) #:redirect-to port))

;; ============================================================
;; Escape hatch
;; ============================================================

;; Call `thunk` with the real terminal output port restored.
;; Use this for intentional TUI rendering writes.
(define (call-with-raw-output thunk)
  (define real-port (guarded-real-output-port))
  (if real-port
      (parameterize ([current-output-port real-port])
        (thunk))
      (thunk))) ; not guarded, just pass through
