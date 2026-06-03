#lang racket/base

;; q/gui/components/streaming-cursor.rkt — Streaming cursor blink state
;;
;; Provides a timer-based cursor that blinks at the end of the last
;; assistant message while streaming is active. All timer logic is
;; testable in headless mode via a background thread.

(require racket/contract
         racket/format)

(provide (contract-out [make-streaming-cursor-state (-> hash?)]
                       [streaming-cursor-active? (-> hash? boolean?)]
                       [streaming-cursor-string (-> hash? string?)]
                       [streaming-cursor-start! (-> hash? (-> void?) void?)]
                       [streaming-cursor-stop! (-> hash? void?)]
                       [streaming-cursor-toggle-phase! (-> hash? void?)]))

;; ──────────────────────────────
;; Pure helpers
;; ──────────────────────────────

;; Create initial cursor state.
;; Returns a hash with:
;;   active?  — boolean, whether cursor is currently blinking
;;   phase    — exact-nonnegative-integer (0 = visible "|", 1 = hidden " ")
;;   thread   — #f or the background thread handle
(define (make-streaming-cursor-state)
  (make-hash (list (cons 'active? #f) (cons 'phase 0) (cons 'thread #f))))

(define (streaming-cursor-active? state)
  (hash-ref state 'active? #f))

;; Return the current cursor glyph based on phase.
;; When inactive, returns empty string.
(define (streaming-cursor-string state)
  (cond
    [(not (streaming-cursor-active? state)) ""]
    [(= 0 (hash-ref state 'phase 0)) "|"]
    [else " "]))

;; ──────────────────────────────
;; Timer / thread helpers (impure)
;; ──────────────────────────────

;; Toggle the blink phase between 0 (visible) and 1 (hidden).
(define (streaming-cursor-toggle-phase! state)
  (define current (hash-ref state 'phase 0))
  (hash-set! state 'phase (if (= current 0) 1 0)))

;; Start the cursor blink loop.
;; state: cursor state hash (mutable)
;; notify: thunk called after each phase toggle so GUI can refresh
(define (streaming-cursor-start! state notify)
  (hash-set! state 'active? #t)
  (hash-set! state 'phase 0)
  (define t
    (thread (lambda ()
              (let loop ()
                (when (streaming-cursor-active? state)
                  (sleep 0.5)
                  (when (streaming-cursor-active? state)
                    (streaming-cursor-toggle-phase! state)
                    (with-handlers ([exn:fail? (lambda (e)
                                                (log-debug "streaming-cursor: notify failed: ~a" (exn-message e)))])
                      (notify))
                    (loop)))))))
  (hash-set! state 'thread t))

;; Stop the cursor blink loop.
(define (streaming-cursor-stop! state)
  (hash-set! state 'active? #f)
  (hash-set! state 'phase 0)
  (define t (hash-ref state 'thread #f))
  (when (thread? t)
    (break-thread t)
    (hash-set! state 'thread #f)))
