#lang racket

;; q/tui/ui-action-adapter.rkt — TUI adapter for UI action events
;;
;; Subscribes to ui.* action events and converts ui-delta values
;; into TUI ui-state mutations.
;;
;; W1.3 (v0.94.1): Initial skeleton handling header, footer, status,
;; and widget register/unregister.

(require racket/contract
         (only-in "../ui-core/ui-delta.rkt"
                  ui-delta
                  ui-delta-type
                  ui-delta-payload
                  ui-delta?
                  ui-action->deltas
                  DELTA-SET-HEADER
                  DELTA-CLEAR-HEADER
                  DELTA-SET-FOOTER
                  DELTA-CLEAR-FOOTER
                  DELTA-SET-STATUS
                  DELTA-REGISTER-WIDGET
                  DELTA-UNREGISTER-WIDGET)
         (only-in "../tui/state-types.rkt"
                  ui-state
                  ui-state?
                  ui-state-custom-header
                  ui-state-custom-footer
                  ui-state-extension-widgets))

(provide (contract-out [tui-apply-delta (-> ui-delta? ui-state? ui-state?)]
                       [tui-apply-deltas (-> (listof ui-delta?) ui-state? ui-state?)]
                       [make-tui-action-handler (-> box? (-> hash? void?))]))

;; ── Pure delta application ─────────────────────────────────

(define (tui-apply-delta delta state)
  (define type (ui-delta-type delta))
  (define payload (ui-delta-payload delta))
  (cond
    [(eq? type DELTA-SET-HEADER) (struct-copy ui-state state [custom-header payload])]
    [(eq? type DELTA-CLEAR-HEADER) (struct-copy ui-state state [custom-header #f])]
    [(eq? type DELTA-SET-FOOTER) (struct-copy ui-state state [custom-footer payload])]
    [(eq? type DELTA-CLEAR-FOOTER) (struct-copy ui-state state [custom-footer #f])]
    ;; TUI ui-state doesn't have a direct status field — skip for now
    [(eq? type DELTA-SET-STATUS) state]
    [(eq? type DELTA-REGISTER-WIDGET)
     (match-define (list ext-name key descriptor) payload)
     (define widgets (ui-state-extension-widgets state))
     (define content (hash-ref descriptor 'content #f))
     (struct-copy ui-state state [extension-widgets (hash-set widgets (cons ext-name key) content)])]
    [(eq? type DELTA-UNREGISTER-WIDGET)
     (define ext-name (car payload))
     (define key (cdr payload))
     (define widgets (ui-state-extension-widgets state))
     (cond
       [key
        (struct-copy ui-state state [extension-widgets (hash-remove widgets (cons ext-name key))])]
       [else
        ;; Remove all widgets for this extension
        (define new-widgets
          (for/hash ([(k v) (in-hash widgets)]
                     #:when (not (equal? (car k) ext-name)))
            (values k v)))
        (struct-copy ui-state state [extension-widgets new-widgets])])]
    [else state]))

(define (tui-apply-deltas deltas state)
  (foldl (lambda (delta st) (tui-apply-delta delta st)) state deltas))

;; ── Event handler factory ──────────────────────────────────
;; Creates a function that can be subscribed to the event bus.
;; When a ui.* event arrives, it converts to deltas and applies them.

(define (make-tui-action-handler state-box)
  (lambda (event-hash)
    (define action-type (hash-ref event-hash 'type #f))
    (when (and action-type (string-prefix? action-type "ui."))
      (define deltas (ui-action->deltas action-type event-hash))
      (define old-state (unbox state-box))
      (when (ui-state? old-state)
        (define new-state (tui-apply-deltas deltas old-state))
        (set-box! state-box new-state)))))
