#lang racket

;; q/gui/lifecycle-hooks.rkt — GUI lifecycle hook dispatch
;;
;; Wires declared GUI hooks from hook-types.rkt to actual dispatch points.
;; Hooks are dispatched as UI events on the event bus.
;;
;; W3.1 (v0.94.3): Lifecycle hook dispatch for GUI events.

(require racket/contract
         (only-in "../util/hook-types.rkt" valid-hook-name? hook-action-schemas)
         (only-in "../ui-core/ui-actions.rkt" current-ui-event-actions-enabled? emit-ui-action!))

(provide current-gui-lifecycle-hooks
         current-gui-event-runtime

         (contract-out [register-gui-lifecycle-hook! (-> symbol? (-> hash? void?) void?)]
                       [dispatch-gui-hook! (-> symbol? hash? void?)]
                       [gui-hook-registered? (-> symbol? boolean?)]))

;; ── Hook registry ──────────────────────────────────────────
;; Maps hook names to handler functions.

(define current-gui-lifecycle-hooks (make-parameter (hasheq)))

(define (register-gui-lifecycle-hook! hook-name handler)
  (unless (valid-hook-name? hook-name)
    (error 'register-gui-lifecycle-hook! "unknown hook: ~a" hook-name))
  (define current (current-gui-lifecycle-hooks))
  (define existing (hash-ref current hook-name '()))
  (current-gui-lifecycle-hooks (hash-set current hook-name (append existing (list handler)))))

(define (dispatch-gui-hook! hook-name payload)
  (unless (valid-hook-name? hook-name)
    (error 'dispatch-gui-hook! "unknown hook: ~a" hook-name))
  ;; Call registered handlers with error isolation
  (define handlers (hash-ref (current-gui-lifecycle-hooks) hook-name '()))
  (for ([handler (in-list handlers)])
    (with-handlers ([exn:fail? (lambda (e)
                                 (log-warning "dispatch-gui-hook!: handler error for ~a: ~a"
                                              hook-name
                                              (exn-message e)))])
      (handler payload)))
  ;; Also emit as UI event if feature flag is on
  (define runtime (current-gui-event-runtime))
  (when (and runtime (current-ui-event-actions-enabled?))
    (emit-ui-action! runtime (format "ui.gui.~a" hook-name) payload)))

;; Runtime for event emission (set during GUI init)
(define current-gui-event-runtime (make-parameter #f))

(define (gui-hook-registered? hook-name)
  (not (null? (hash-ref (current-gui-lifecycle-hooks) hook-name '()))))
