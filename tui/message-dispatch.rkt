#lang racket/base

;; q/tui/message-dispatch.rkt — TUI message dispatch
;;
;; Extracted from tui-render-loop.rkt (W18) to make the main loop
;; testable and reduce the render loop's responsibility.
;;
;; Handles: key, redraw, paste, mouse messages.
;; Resize handling stays in render-loop (circular dep on tui-ctx-resize-ubuf!).

(require "tui-keybindings.rkt"
         "submit-handler.rkt"
         "input.rkt"
         (only-in "state-types.rkt"
                  ui-state-active-overlay
                  overlay-state?
                  overlay-state-type
                  overlay-state-extra)
         (only-in "state-events/core-handlers.rkt" approval-overlay-remove-request)
         (only-in "approval-channel.rkt" approval-request-pending?))

;; A killed/interrupted approval owner cannot publish its terminal event.  The
;; channel registry is still authoritative, so every event-loop message prunes
;; terminal active requests and promotes queued live requests before dispatch.
(define (prune-stale-approval-overlays! ctx)
  (define state-box (tui-ctx-ui-state-box ctx))
  (let loop ()
    (define state (unbox state-box))
    (define overlay (ui-state-active-overlay state))
    (when (and (overlay-state? overlay) (eq? (overlay-state-type overlay) 'approval-prompt))
      (define extra (overlay-state-extra overlay))
      (define request-id (and (hash? extra) (hash-ref extra 'request-id #f)))
      (when (and (string? request-id) (not (approval-request-pending? request-id)))
        (define pruned (approval-overlay-remove-request state request-id))
        (unless (eq? pruned state)
          (set-box! state-box pruned)
          (mark-dirty! ctx)
          (loop))))))

;; Process a single TUI message (from next-message adapter).
;; Returns 'quit if quit requested, 'resize if resize needed, or void.
;; The caller handles 'resize by calling tui-ctx-resize-ubuf! etc.
(define (process-tui-message! ctx msg)
  (prune-stale-approval-overlays! ctx)
  (case (car msg)
    ;; Key press: dispatch to handler
    [(key)
     (define keycode (cadr msg))
     ;; G1.1: Intercept tree-browser overlay keys before normal handling
     (define tree-result (handle-tree-overlay-key ctx keycode))
     (cond
       [(eq? tree-result 'handled) (void)]
       [else
        ;; v0.99.25 §5.3: Intercept approval-prompt overlay keys
        (define approval-result (handle-approval-overlay-key ctx keycode))
        (cond
          [(eq? approval-result 'handled) (void)]
          [else
           (define result (handle-key ctx keycode))
           (cond
             [(eq? result 'quit)
              (set-box! (tui-ctx-running-box ctx) #f)
              'quit]
             [(and (list? result) (eq? (car result) 'submit))
              (define raw-text (cadr result))
              ;; G3.3: Expand !! inline bash prefix
              (define text (expand-inline-bash raw-text (unbox (tui-ctx-last-prompt-box ctx))))
              ;; Store prompt for /retry (#1378)
              (set-box! (tui-ctx-last-prompt-box ctx) text)
              (handle-user-submit! ctx text)]
             [(and (list? result) (eq? (car result) 'command))
              (define cmd (cadr result))
              (define raw-text
                (if (>= (length result) 3)
                    (caddr result)
                    ""))
              (process-slash-command ctx cmd raw-text)]
             [else (void)])])])]
    ;; Resize event: signal caller
    [(resize) 'resize]
    ;; Redraw command: mark dirty
    [(redraw) (mark-dirty! ctx)]
    ;; Paste and mouse are also blocked while approval is modal.  Probing with
    ;; a non-decision key has no state effect and keeps modality ownership in
    ;; the approval handler rather than duplicating overlay inspection here.
    [(paste)
     (unless (eq? (handle-approval-overlay-key ctx 'modal-probe) 'handled)
       (define text (cadr msg))
       (define inp (unbox (tui-ctx-input-state-box ctx)))
       (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text))
       (mark-dirty! ctx))]
    [(mouse)
     (unless (eq? (handle-approval-overlay-key ctx 'modal-probe) 'handled)
       (handle-mouse ctx (cdr msg)))]
    ;; Unknown message type - ignore
    [else (void)]))

(provide process-tui-message!)
