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
         "input.rkt")

;; Process a single TUI message (from next-message adapter).
;; Returns 'quit if quit requested, 'resize if resize needed, or void.
;; The caller handles 'resize by calling tui-ctx-resize-ubuf! etc.
(define (process-tui-message! ctx msg)
  (case (car msg)
    ;; Key press: dispatch to handler
    [(key)
     (define keycode (cadr msg))
     ;; G1.1: Intercept tree-browser overlay keys before normal handling
     (define tree-result (handle-tree-overlay-key ctx keycode))
     (cond
       [(eq? tree-result 'handled) (void)]
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
          [else (void)])])]
    ;; Resize event: signal caller
    [(resize) 'resize]
    ;; Redraw command: mark dirty
    [(redraw) (mark-dirty! ctx)]
    ;; Paste event: insert as single undo entry
    [(paste)
     (define text (cadr msg))
     (define inp (unbox (tui-ctx-input-state-box ctx)))
     (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text))
     (mark-dirty! ctx)]
    ;; Mouse event: dispatch to handler
    [(mouse) (handle-mouse ctx (cdr msg))]
    ;; Unknown message type - ignore
    [else (void)]))

(provide process-tui-message!)
