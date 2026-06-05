#lang racket

;; q/gui/ui-action-adapter.rkt — GUI adapter for UI action events
;;
;; Subscribes to ui.* action events and converts ui-delta values
;; into GUI gui-state mutations.
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
         (only-in "../gui/gui-types.rkt"
                  gui-state
                  gui-state?
                  gui-state-messages
                  gui-state-status
                  make-gui-message))

(provide (contract-out [gui-apply-delta (-> ui-delta? gui-state? gui-state?)]
                       [gui-apply-deltas (-> (listof ui-delta?) gui-state? gui-state?)]
                       [make-gui-action-handler (-> box? (-> hash? void?))]))

;; ── Pure delta application ─────────────────────────────────

(define (gui-apply-delta delta state)
  (define type (ui-delta-type delta))
  (define payload (ui-delta-payload delta))
  (cond
    [(eq? type DELTA-SET-STATUS) (struct-copy gui-state state [status payload])]
    ;; GUI doesn't have a direct custom-header field — skip for now
    [(eq? type DELTA-SET-HEADER) state]
    [(eq? type DELTA-CLEAR-HEADER) state]
    [(eq? type DELTA-SET-FOOTER) state]
    [(eq? type DELTA-CLEAR-FOOTER) state]
    ;; Widget registration in GUI is handled via extension slots
    [(eq? type DELTA-REGISTER-WIDGET) state]
    [(eq? type DELTA-UNREGISTER-WIDGET) state]
    [else state]))

(define (gui-apply-deltas deltas state)
  (foldl (lambda (delta st) (gui-apply-delta delta st)) state deltas))

;; ── Event handler factory ──────────────────────────────────

(define (make-gui-action-handler state-box)
  (lambda (event-hash)
    (define action-type (hash-ref event-hash 'type #f))
    (when (and action-type (string-prefix? action-type "ui."))
      (define deltas (ui-action->deltas action-type event-hash))
      (define old-state (unbox state-box))
      (when (gui-state? old-state)
        (define new-state (gui-apply-deltas deltas old-state))
        (set-box! state-box new-state)))))
