#lang racket/base

;; q/ui-core/dispatch.rkt — Backend-agnostic UI action dispatch
;;
;; Provides dispatch functions for common UI actions (submit, cancel,
;; scroll, command). These functions work with any UI backend (TUI or GUI)
;; by emitting events to the session event bus.
;;
;; Architecture:
;;   User action → dispatch-*! → event bus → state reducer → UI update

(require racket/contract
         "event-types.rkt")

(provide (contract-out [dispatch-submit! (-> any/c string? void?)]
                       [dispatch-cancel! (-> any/c void?)]
                       [dispatch-scroll! (-> any/c (or/c 'up 'down 'top 'bottom) void?)]
                       [dispatch-command! (-> any/c string? (listof string?) void?)]
                       [dispatch-resize!
                        (-> any/c exact-nonnegative-integer? exact-nonnegative-integer? void?)]
                       [dispatch-focus! (-> any/c symbol? void?)]
                       [dispatch-input-append! (-> any/c string? void?)]
                       [dispatch-input-clear! (-> any/c void?)]))

;; Helper: emit a ui-event via the runtime's event mechanism.
(define (emit-ui-event! runtime type . payload-args)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (ui-event->hash (apply make-ui-event type payload-args)))))

;; ──────────────────────────────
;; Submit user input
;; ──────────────────────────────
(define (dispatch-submit! runtime text)
  (emit-ui-event! runtime "user.input" 'text text))

;; ──────────────────────────────
;; Cancel current operation
;; ──────────────────────────────
(define (dispatch-cancel! runtime)
  (emit-ui-event! runtime "user.cancel"))

;; ──────────────────────────────
;; Scroll transcript
;; ──────────────────────────────
(define (dispatch-scroll! runtime direction)
  (emit-ui-event! runtime "ui.scroll" 'direction direction))

;; ──────────────────────────────
;; Execute a slash command
;; ──────────────────────────────
(define (dispatch-command! runtime command-name args)
  (emit-ui-event! runtime "user.command" 'command command-name 'args args))

;; ──────────────────────────────
;; Resize event
;; ──────────────────────────────
(define (dispatch-resize! runtime cols rows)
  (emit-ui-event! runtime "ui.resize" 'cols cols 'rows rows))

;; ──────────────────────────────
;; Focus a specific component
;; ──────────────────────────────
(define (dispatch-focus! runtime component-id)
  (emit-ui-event! runtime "ui.focus" 'component component-id))

;; ──────────────────────────────
;; Append text to input
;; ──────────────────────────────
(define (dispatch-input-append! runtime text)
  (emit-ui-event! runtime "ui.input.append" 'text text))

;; ──────────────────────────────
;; Clear input field
;; ──────────────────────────────
(define (dispatch-input-clear! runtime)
  (emit-ui-event! runtime "ui.input.clear"))
