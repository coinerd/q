#lang racket

;; q/tui/ui-action-adapter.rkt — TUI adapter for UI action events
;;
;; Subscribes to ui.* action events and converts ui-delta values
;; into TUI ui-state mutations via the shared reducer.
;;
;; W2.2 (v0.94.2): Refactored to use ui-core/ui-reducer.rkt handler table.

(require racket/contract
         (only-in "../ui-core/ui-delta.rkt" ui-delta? ui-action->deltas)
         (only-in "../ui-core/ui-reducer.rkt"
                  delta-handler-table?
                  apply-delta-with
                  apply-deltas-with
                  apply-action-with
                  delta-handlers->table)
         (only-in "../tui/state-types.rkt" ui-state ui-state? ui-state-extension-widgets))

;; Shared handler table for TUI
(provide tui-delta-handlers

         ;; Convenience functions using the shared reducer
         (contract-out [tui-apply-delta (-> ui-delta? ui-state? ui-state?)]
                       [tui-apply-deltas (-> (listof ui-delta?) ui-state? ui-state?)]
                       [make-tui-action-handler (-> box? (-> hash? void?))]))

;; ── TUI handler table ──────────────────────────────────────

(define tui-delta-handlers
  (delta-handlers->table
   #:set-header (lambda (payload state) (struct-copy ui-state state [custom-header payload]))
   #:clear-header (lambda (payload state) (struct-copy ui-state state [custom-header #f]))
   #:set-footer (lambda (payload state) (struct-copy ui-state state [custom-footer payload]))
   #:clear-footer (lambda (payload state) (struct-copy ui-state state [custom-footer #f]))
   #:set-status (lambda (payload state)
                  ;; TUI ui-state doesn't have a direct status field — skip for now
                  state)
   #:set-theme (lambda (payload state)
                 ;; TUI theme is handled via tui/theme.rkt parameter, not ui-state.
                 state)
   #:set-layout (lambda (payload state)
                  ;; TUI layout is handled via terminal resize, not ui-state.
                  state)
   #:set-focus (lambda (payload state) state)
   #:register-widget
   (lambda (payload state)
     (match-define (list ext-name key descriptor) payload)
     (define widgets (ui-state-extension-widgets state))
     (define content (hash-ref descriptor 'content #f))
     (struct-copy ui-state state [extension-widgets (hash-set widgets (cons ext-name key) content)]))
   #:unregister-widget
   (lambda (payload state)
     (define ext-name (car payload))
     (define key (cdr payload))
     (define widgets (ui-state-extension-widgets state))
     (cond
       [key
        (struct-copy ui-state state [extension-widgets (hash-remove widgets (cons ext-name key))])]
       [else
        (define new-widgets
          (for/hash ([(k v) (in-hash widgets)]
                     #:when (not (equal? (car k) ext-name)))
            (values k v)))
        (struct-copy ui-state state [extension-widgets new-widgets])]))))

;; ── Convenience wrappers ───────────────────────────────────

(define (tui-apply-delta delta state)
  (apply-delta-with tui-delta-handlers delta state))

(define (tui-apply-deltas deltas state)
  (apply-deltas-with tui-delta-handlers deltas state))

;; ── Event handler factory ──────────────────────────────────

(define (make-tui-action-handler state-box)
  (lambda (event-hash)
    (define action-type (hash-ref event-hash 'type #f))
    (when (and action-type (string-prefix? action-type "ui."))
      (define deltas (ui-action->deltas action-type event-hash))
      (define old-state (unbox state-box))
      (when (ui-state? old-state)
        (define new-state (tui-apply-deltas deltas old-state))
        (set-box! state-box new-state)))))
