#lang racket

;; q/gui/ui-action-adapter.rkt — GUI adapter for UI action events
;;
;; Subscribes to ui.* action events and converts ui-delta values
;; into GUI gui-state mutations via the shared reducer.
;;
;; W2.3 (v0.94.2): Refactored to use ui-core/ui-reducer.rkt handler table.

(require racket/contract
         (only-in "../ui-core/ui-delta.rkt" ui-delta? ui-action->deltas)
         (only-in "../ui-core/ui-reducer.rkt"
                  apply-delta-with
                  apply-deltas-with
                  delta-handlers->table)
         (only-in "../gui/gui-types.rkt" gui-state gui-state? gui-state-status)
         ;; G-TM1 (v0.98.11): theme sync wiring
         (only-in "../gui/main.rkt" current-gui-theme-manager)
         (only-in "../gui/theme-manager.rkt" tm-switch-theme! theme-manager?))

;; Shared handler table for GUI
(provide gui-delta-handlers

         ;; Convenience functions using the shared reducer
         (contract-out [gui-apply-delta (-> ui-delta? gui-state? gui-state?)]
                       [gui-apply-deltas (-> (listof ui-delta?) gui-state? gui-state?)]
                       [make-gui-action-handler (-> box? (-> hash? void?))]))

;; ── GUI handler table ──────────────────────────────────────

(define gui-delta-handlers
  (delta-handlers->table #:set-status (lambda (payload state)
                                        (struct-copy gui-state state [status payload]))
                         ;; GUI doesn't have custom-header/custom-footer fields yet — skip
                         #:set-header (lambda (payload state) state)
                         #:clear-header (lambda (payload state) state)
                         #:set-footer (lambda (payload state) state)
                         #:clear-footer (lambda (payload state) state)
                         ;; Widget registration in GUI is handled via extension slots
                         #:register-widget (lambda (payload state) state)
                         #:unregister-widget (lambda (payload state) state)
                         #:set-theme (lambda (payload state)
                                       ;; G-TM1 (v0.98.11): wire DELTA-SET-THEME to theme-manager.
                                       (define mgr (current-gui-theme-manager))
                                       (when (and (theme-manager? mgr) (symbol? payload))
                                         (tm-switch-theme! mgr payload))
                                       state)
                         #:set-layout (lambda (payload state) state)
                         #:set-focus (lambda (payload state) state)))

;; ── Convenience wrappers ───────────────────────────────────

(define (gui-apply-delta delta state)
  (apply-delta-with gui-delta-handlers delta state))

(define (gui-apply-deltas deltas state)
  (apply-deltas-with gui-delta-handlers deltas state))

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
