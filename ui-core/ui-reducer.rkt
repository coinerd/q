#lang racket

;; q/ui-core/ui-reducer.rkt — Shared pure UI reducer and backend-neutral delta
;;
;; Provides a single entry point for reducing UI action events into ui-delta
;; lists, and a backend-agnostic apply-deltas protocol using handler tables.
;;
;; TUI and GUI adapters register delta-type handlers; the shared reducer
;; dispatches through them. This eliminates duplication between the two
;; adapter modules.
;;
;; Architecture:
;;   UI action → ui-action->deltas → apply-deltas-with(handlers, state)
;;                                          ↓
;;                              TUI: ui-state handlers
;;                              GUI: gui-state handlers
;;
;; W2.1 (v0.94.2): Shared reducer with handler-table protocol.

(require racket/contract
         (only-in "ui-delta.rkt"
                  ui-delta
                  ui-delta?
                  ui-delta-type
                  ui-delta-payload
                  ui-action->deltas
                  all-delta-types
                  DELTA-SET-HEADER
                  DELTA-CLEAR-HEADER
                  DELTA-SET-FOOTER
                  DELTA-CLEAR-FOOTER
                  DELTA-SET-STATUS
                  DELTA-ADD-MESSAGE
                  DELTA-UPDATE-MESSAGE
                  DELTA-SET-THEME
                  DELTA-SET-LAYOUT
                  DELTA-SET-FOCUS
                  DELTA-REGISTER-WIDGET
                  DELTA-UNREGISTER-WIDGET
                  DELTA-SHOW-OVERLAY
                  DELTA-DISMISS-OVERLAY))

;; Handler table type
(provide (struct-out delta-handler-table)
         make-delta-handler-table

         ;; Shared apply functions
         (contract-out [apply-delta-with (-> delta-handler-table? ui-delta? any/c any/c)]
                       [apply-deltas-with (-> delta-handler-table? (listof ui-delta?) any/c any/c)]
                       [apply-action-with (-> delta-handler-table? string? hash? any/c any/c)])

         ;; Handler composition
         delta-handlers->table)

;; ── Handler table ──────────────────────────────────────────
;; Maps delta types to handler functions.
;; Each handler: (-> ui-delta-payload state state)

(struct delta-handler-table (handlers) #:transparent)

(define (make-delta-handler-table . key-val-pairs)
  (delta-handler-table (apply hasheq key-val-pairs)))

;; ── Shared apply ───────────────────────────────────────────

(define (apply-delta-with table delta state)
  (define type (ui-delta-type delta))
  (define payload (ui-delta-payload delta))
  (define handler (hash-ref (delta-handler-table-handlers table) type #f))
  (if handler
      (handler payload state)
      state))

(define (apply-deltas-with table deltas state)
  (foldl (lambda (delta st) (apply-delta-with table delta st)) state deltas))

(define (apply-action-with table action-type payload state)
  (define deltas (ui-action->deltas action-type payload))
  (apply-deltas-with table deltas state))

;; ── Handler composition helper ─────────────────────────────
;; Build a handler table from keyword arguments:
;;   (delta-handlers->table #:set-header (lambda (payload state) ...)
;;                          #:clear-header (lambda (payload state) ...))

(define (delta-handlers->table #:set-header [set-header #f]
                               #:clear-header [clear-header #f]
                               #:set-footer [set-footer #f]
                               #:clear-footer [clear-footer #f]
                               #:set-status [set-status #f]
                               #:add-message [add-message #f]
                               #:update-message [update-message #f]
                               #:set-theme [set-theme #f]
                               #:set-layout [set-layout #f]
                               #:set-focus [set-focus #f]
                               #:register-widget [register-widget #f]
                               #:unregister-widget [unregister-widget #f]
                               #:show-overlay [show-overlay #f]
                               #:dismiss-overlay [dismiss-overlay #f])
  (define pairs
    (filter cdr
            (list (cons 'set-header set-header)
                  (cons 'clear-header clear-header)
                  (cons 'set-footer set-footer)
                  (cons 'clear-footer clear-footer)
                  (cons 'set-status set-status)
                  (cons 'add-message add-message)
                  (cons 'update-message update-message)
                  (cons 'set-theme set-theme)
                  (cons 'set-layout set-layout)
                  (cons 'set-focus set-focus)
                  (cons 'register-widget register-widget)
                  (cons 'unregister-widget unregister-widget)
                  (cons 'show-overlay show-overlay)
                  (cons 'dismiss-overlay dismiss-overlay))))
  (delta-handler-table (for/hasheq ([p (in-list pairs)])
                         (values (car p) (cdr p)))))
