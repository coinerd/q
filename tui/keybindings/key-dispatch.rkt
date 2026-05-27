#lang racket/base

;; q/tui/keybindings/key-dispatch.rkt — Key dispatch
;;
;; Extracted from tui-keybindings.rkt (W20) to reduce the keybindings
;; hotspot. Contains dispatch-keymap-action and handle-key.

(require racket/match
         racket/string
         "../context.rkt"
         "../state.rkt"
         "../input.rkt"
         "../clipboard.rkt"
         "../terminal.rkt"
         "../layout.rkt"
         "../command-parse.rkt"
         "binding-resolver.rkt"
         "../selection.rkt"
         "../../agent/event-bus.rkt"
         "../../util/event.rkt"
         "../keymap.rkt"
         (only-in "../component.rkt"
                  component-handle-input
                  q-component-handle-input-fn
                  input-consumed?))

;; Dispatch a keymap action to the appropriate handler.
;; Returns 'handled if handled (maps to 'continue in handle-key),
;; or #f if not (falls through to hardcoded).
(define (dispatch-keymap-action ctx inp state action)
  (case action
    [(tui.input.submit submit) #f] ;; Complex — fall through to hardcoded for proper submit flow
    [(tui.input.backspace backspace)
     (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
     'handled]
    [(tui.input.delete delete)
     (set-box! (tui-ctx-input-state-box ctx) (input-delete inp))
     'handled]
    [(tui.navigation.home home)
     (set-box! (tui-ctx-input-state-box ctx) (input-home inp))
     'handled]
    [(tui.navigation.end end)
     (set-box! (tui-ctx-input-state-box ctx) (input-end inp))
     'handled]
    [(tui.navigation.history-up history-up)
     (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
     'handled]
    [(tui.navigation.history-down history-down)
     (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
     'handled]
    [(tui.editor.word-left word-left)
     (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-left inp))
     'handled]
    [(tui.editor.word-right word-right)
     (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-right inp))
     'handled]
    [(tui.editor.clear-input clear-input)
     (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-beginning inp))
     'handled]
    [(tui.display.clear-screen clear-screen)
     (mark-dirty! ctx)
     'handled]
    [(tui.editor.copy copy) #f] ;; Complex — let hardcoded handle
    [(tui.editor.paste paste)
     (define text (clipboard-paste))
     (when text
       (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text)))
     'handled]
    [(tui.editor.expand-file-ref expand-file-ref)
     (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
     'handled]
    [(tui.navigation.scroll-up scroll-up)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-up state 1))
     'handled]
    [(tui.navigation.scroll-down scroll-down)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-down state 1))
     'handled]
    [(tui.navigation.page-up page-up)
     (define-values (_cols rows) (tui-screen-size))
     (define layout (compute-layout rows _cols))
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-up state (max 1 (layout-region-height (layout-transcript layout)))))
     'handled]
    [(tui.navigation.page-down page-down)
     (define-values (_cols rows) (tui-screen-size))
     (define layout (compute-layout rows _cols))
     (set-box! (tui-ctx-ui-state-box ctx)
               (scroll-down state (max 1 (layout-region-height (layout-transcript layout)))))
     'handled]
    [(scroll-top)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-to-top state))
     'handled]
    [(scroll-bottom)
     (set-box! (tui-ctx-ui-state-box ctx) (scroll-to-bottom state))
     'handled]
    [else #f]))

;; Handle a single key event.
;; Returns: 'continue | 'quit | (list 'submit string) | (list 'command symbol)
(define (handle-key ctx keycode)
  (define inp (unbox (tui-ctx-input-state-box ctx)))
  (define state (unbox (tui-ctx-ui-state-box ctx)))
  ;; Any key that reaches here may change state — mark for redraw
  (mark-dirty! ctx)

  ;; 1. Check if a component is focused and can handle input
  (define focused-id (tui-ctx-focused-component-id ctx))
  (when focused-id
    (define reg-box (tui-ctx-component-registry-box ctx))
    (define reg (unbox reg-box))
    (define comp (and reg (hash-ref reg focused-id #f)))
    (when (and comp (q-component-handle-input-fn comp))
      (define-values (new-state result) (component-handle-input comp keycode state))
      (when (input-consumed? result)
        (set-box! (tui-ctx-ui-state-box ctx) new-state)
        (mark-dirty! ctx)
        (set! state new-state))))

  ;; 2. Check configurable keymap
  (define km (get-active-keymap))
  (define ks (keycode->key-spec-from-msg keycode))
  (define action (and ks (keymap-lookup km ks)))
  (match keycode
    [(? (lambda (k) (and action (eq? (dispatch-keymap-action ctx inp state action) 'handled))))
     'continue]
    ;; Fallback to hardcoded behavior
    [(? char?)
     (case keycode
       [(#\return)
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (match text
          [#f 'continue]
          [(? input-slash-command)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          [_ (list 'submit text)])]
       [(#\newline)
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-newline inp))
        'continue]
       [(#\tab)
        (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
        'continue]
       [(#\u001b) 'continue]
       [(#\backspace)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [(#\rubout)
        (set-box! (tui-ctx-input-state-box ctx) (input-backspace inp))
        'continue]
       [else
        (set-box! (tui-ctx-input-state-box ctx) (input-insert-char inp keycode))
        'continue])]
    [(? symbol?)
     (case keycode
       [(return kp-return enter kp-enter)
        (define-values (text new-inp) (input-submit inp))
        (set-box! (tui-ctx-input-state-box ctx) new-inp)
        (match text
          [#f 'continue]
          [(? input-slash-command)
           (define cmd (parse-tui-slash-command text))
           (list 'command (or cmd 'unknown) text)]
          [_ (list 'submit text)])]
       [(tab)
        (set-box! (tui-ctx-input-state-box ctx) (input-expand-file-ref inp))
        'continue]
       [(escape) 'continue]
       [(ctrl-z)
        (set-box! (tui-ctx-input-state-box ctx) (input-undo inp))
        'continue]
       [(ctrl-y)
        (set-box! (tui-ctx-input-state-box ctx) (input-redo inp))
        'continue]
       [(ctrl-w)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-word-backward inp))
        'continue]
       [(ctrl-u)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-beginning inp))
        'continue]
       [(ctrl-k)
        (set-box! (tui-ctx-input-state-box ctx) (input-kill-to-end inp))
        'continue]
       [(ctrl-v)
        (define text (clipboard-paste))
        (when text
          (set-box! (tui-ctx-input-state-box ctx) (input-insert-string inp text)))
        'continue]
       [(ctrl-left)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-left inp))
        'continue]
       [(ctrl-right)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-word-right inp))
        'continue]
       [(ctrl-c)
        (if (has-selection? state)
            (let ([text (selection-text ctx state)])
              (when (and text (not (string=? text "")))
                (copy-text! text)))
            (begin
              (when (tui-ctx-event-bus ctx)
                (publish! (tui-ctx-event-bus ctx)
                          (make-event "interrupt.requested"
                                      (inexact->exact (truncate (/ (current-inexact-milliseconds)
                                                                   1000)))
                                      (or (ui-state-session-id state) "")
                                      #f
                                      (hash))))
              (set-box! (tui-ctx-ui-state-box ctx)
                        (clear-streaming (set-pending-tool-name (set-busy state #f) #f)))))
        'continue]
       [(left kp-left)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-left inp))
        'continue]
       [(right kp-right)
        (set-box! (tui-ctx-input-state-box ctx) (input-cursor-right inp))
        'continue]
       [(home kp-home)
        (set-box! (tui-ctx-input-state-box ctx) (input-home inp))
        'continue]
       [(end kp-end)
        (set-box! (tui-ctx-input-state-box ctx) (input-end inp))
        'continue]
       [(up kp-up)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
        'continue]
       [(down kp-down)
        (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
        'continue]
       [(delete kp-delete)
        (set-box! (tui-ctx-input-state-box ctx) (input-delete inp))
        'continue]
       [(pgup kp-pgup)
        (let ()
          (define-values (_cols rows) (tui-screen-size))
          (define layout (compute-layout rows _cols))
          (set-box! (tui-ctx-ui-state-box ctx)
                    (scroll-up state (max 1 (layout-region-height (layout-transcript layout))))))
        'continue]
       [(pgdn kp-pgdn)
        (let ()
          (define-values (_cols rows) (tui-screen-size))
          (define layout (compute-layout rows _cols))
          (set-box! (tui-ctx-ui-state-box ctx)
                    (scroll-down state (max 1 (layout-region-height (layout-transcript layout))))))
        'continue]
       [else 'continue])]
    [_ 'continue]))

(provide dispatch-keymap-action
         handle-key)
