#lang racket/base

;; q/ui-core/dispatch.rkt — Backend-agnostic UI action dispatch
;;
;; Provides dispatch functions for common UI actions (submit, cancel,
;; scroll, command). These functions work with any UI backend (TUI or GUI)
;; by emitting events to the session event bus.
;;
;; Architecture:
;;   User action → dispatch-*! → event bus → state reducer → UI update

(require racket/contract)

(provide (contract-out [dispatch-submit! (-> any/c string? void?)]
                       [dispatch-cancel! (-> any/c void?)]
                       [dispatch-scroll! (-> any/c (or/c 'up 'down 'top 'bottom) void?)]
                       [dispatch-command! (-> any/c string? (listof string?) void?)]
                       [dispatch-resize!
                        (-> any/c exact-nonnegative-integer? exact-nonnegative-integer? void?)]
                       [dispatch-focus! (-> any/c symbol? void?)]
                       [dispatch-input-append! (-> any/c string? void?)]
                       [dispatch-input-clear! (-> any/c void?)]))

;; ──────────────────────────────
;; Submit user input
;; ──────────────────────────────
(define (dispatch-submit! runtime text)
  ;; In full wiring, this calls the submit handler on the agent session.
  ;; For now, emit a user-input event via the runtime's event mechanism.
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "user.input" 'text text))))

;; ──────────────────────────────
;; Cancel current operation
;; ──────────────────────────────
(define (dispatch-cancel! runtime)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "user.cancel"))))

;; ──────────────────────────────
;; Scroll transcript
;; ──────────────────────────────
(define (dispatch-scroll! runtime direction)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "ui.scroll" 'direction direction))))

;; ──────────────────────────────
;; Execute a slash command
;; ──────────────────────────────
(define (dispatch-command! runtime command-name args)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "user.command" 'command command-name 'args args))))

;; ──────────────────────────────
;; Resize event
;; ──────────────────────────────
(define (dispatch-resize! runtime cols rows)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "ui.resize" 'cols cols 'rows rows))))

;; ──────────────────────────────
;; Focus a specific component
;; ──────────────────────────────
(define (dispatch-focus! runtime component-id)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "ui.focus" 'component component-id))))

;; ──────────────────────────────
;; Append text to input
;; ──────────────────────────────
(define (dispatch-input-append! runtime text)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "ui.input.append" 'text text))))

;; ──────────────────────────────
;; Clear input field
;; ──────────────────────────────
(define (dispatch-input-clear! runtime)
  (define emit-fn (hash-ref runtime 'emit-event #f))
  (when emit-fn
    (emit-fn (hash 'type "ui.input.clear"))))
