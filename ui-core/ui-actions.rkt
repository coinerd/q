#lang racket

;; q/ui-core/ui-actions.rkt — Backend-neutral UI action definitions
;;
;; Defines UI action event names, payload validators, and emitter functions.
;; Extension UI operations should emit these actions instead of directly
;; mutating backend state.
;;
;; Architecture:
;;   Extension → emit-ui-action → event bus → reducer → ui-delta → backend adapter
;;
;; W1.1 (v0.94.1): Initial action set covering header, footer, status,
;; widget, overlay, theme, layout, and focus.

(require racket/contract
         "event-types.rkt")

;; Feature flag
(provide current-ui-event-actions-enabled?

         ;; Action name constants (for reference/testing)
         UI-ACTION-HEADER-SET
         UI-ACTION-HEADER-CLEAR
         UI-ACTION-FOOTER-SET
         UI-ACTION-FOOTER-CLEAR
         UI-ACTION-STATUS-SET
         UI-ACTION-WIDGET-REGISTER
         UI-ACTION-WIDGET-UNREGISTER
         UI-ACTION-WIDGET-UNREGISTER-ALL
         UI-ACTION-OVERLAY-SHOW
         UI-ACTION-OVERLAY-DISMISS
         UI-ACTION-THEME-CHANGE
         UI-ACTION-LAYOUT-BREAKPOINT
         UI-ACTION-FOCUS-REQUEST

         ;; Validator
         valid-ui-action-name?
         ui-action-schema

         ;; Emitter
         emit-ui-action!

         ;; All known action names
         all-ui-action-names)

;; ── Feature flag ───────────────────────────────────────────

(define current-ui-event-actions-enabled? (make-parameter #f))

;; ── Action name constants ──────────────────────────────────

(define UI-ACTION-HEADER-SET "ui.header.set")
(define UI-ACTION-HEADER-CLEAR "ui.header.clear")
(define UI-ACTION-FOOTER-SET "ui.footer.set")
(define UI-ACTION-FOOTER-CLEAR "ui.footer.clear")
(define UI-ACTION-STATUS-SET "ui.status.set")
(define UI-ACTION-WIDGET-REGISTER "ui.widget.register")
(define UI-ACTION-WIDGET-UNREGISTER "ui.widget.unregister")
(define UI-ACTION-WIDGET-UNREGISTER-ALL "ui.widget.unregister-all")
(define UI-ACTION-OVERLAY-SHOW "ui.overlay.show")
(define UI-ACTION-OVERLAY-DISMISS "ui.overlay.dismiss")
(define UI-ACTION-THEME-CHANGE "ui.theme.change")
(define UI-ACTION-LAYOUT-BREAKPOINT "ui.layout.breakpoint")
(define UI-ACTION-FOCUS-REQUEST "ui.focus.request")

;; ── All known action names ─────────────────────────────────

(define all-ui-action-names
  (list UI-ACTION-HEADER-SET
        UI-ACTION-HEADER-CLEAR
        UI-ACTION-FOOTER-SET
        UI-ACTION-FOOTER-CLEAR
        UI-ACTION-STATUS-SET
        UI-ACTION-WIDGET-REGISTER
        UI-ACTION-WIDGET-UNREGISTER
        UI-ACTION-WIDGET-UNREGISTER-ALL
        UI-ACTION-OVERLAY-SHOW
        UI-ACTION-OVERLAY-DISMISS
        UI-ACTION-THEME-CHANGE
        UI-ACTION-LAYOUT-BREAKPOINT
        UI-ACTION-FOCUS-REQUEST))

;; ── Action name validator ──────────────────────────────────

(define (valid-ui-action-name? name)
  (and (string? name) (member name all-ui-action-names) #t))

;; ── Payload schema per action ──────────────────────────────
;; Each schema entry is: (listof symbol?) — required keys for the payload.
;; where keys are symbols.

(define ui-action-schema
  (hasheq UI-ACTION-HEADER-SET
          '(lines)
          UI-ACTION-HEADER-CLEAR
          '()
          UI-ACTION-FOOTER-SET
          '(lines)
          UI-ACTION-FOOTER-CLEAR
          '()
          UI-ACTION-STATUS-SET
          '(status)
          UI-ACTION-WIDGET-REGISTER
          '(ext-name key descriptor)
          UI-ACTION-WIDGET-UNREGISTER
          '(ext-name key)
          UI-ACTION-WIDGET-UNREGISTER-ALL
          '(ext-name)
          UI-ACTION-OVERLAY-SHOW
          '(content)
          UI-ACTION-OVERLAY-DISMISS
          '()
          UI-ACTION-THEME-CHANGE
          '(theme)
          UI-ACTION-LAYOUT-BREAKPOINT
          '(breakpoint)
          UI-ACTION-FOCUS-REQUEST
          '(component)))

;; ── Emitter ────────────────────────────────────────────────
;; Emits a UI action event to the runtime's event mechanism.
;; Only emits when the feature flag is enabled.

;; ── Payload validation ───────────────────────────────────
;; Consumes ui-action-schema to validate payloads at emission time.

(define (validate-action-payload! action-name payload)
  (define required-keys (hash-ref ui-action-schema action-name #f))
  (when required-keys
    (for ([key (in-list required-keys)])
      (unless (hash-has-key? payload key)
        (log-warning "ui-action: ~a missing required key '~a'" action-name key)))))

;; ── Emitter ────────────────────────────────────────────────
;; Emits a UI action event to the runtime's event mechanism.
;; Only emits when the feature flag is enabled.
;; Validates payload against schema when available.

(define (emit-ui-action! runtime action-name . payload-args)
  (when (current-ui-event-actions-enabled?)
    (define maybe-payload (and (pair? payload-args) (car payload-args)))
    (when (hash? maybe-payload)
      (validate-action-payload! action-name maybe-payload))
    (define emit-fn (hash-ref runtime 'emit-event #f))
    (when emit-fn
      (emit-fn (ui-event->hash (apply make-ui-event action-name payload-args))))))
