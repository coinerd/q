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
         "../keymap.rkt"
         (only-in "../component.rkt"
                  component-handle-input
                  q-component-handle-input-fn
                  input-consumed?
                  cycle-focus
                  focusable-components)
         (only-in "../commands/runtime-control.rkt" request-active-turn-interrupt!)
         "../../ui-core/ui-intents.rkt"
         "../../ui-core/disclosure-state.rkt"
         "../../ui-core/conversation-artifact.rkt"
         "../../ui-core/conversation-reducer.rkt"
         "../../ui-core/preferences.rkt"
         (only-in "../../ui-core/feature-flags.rkt" tui-multiline-composer-enabled)
         racket/list)

;; ============================================================
;; W4: Visual Up/Down composer navigation
;;
;; Plain Up/Down move the cursor through the visual (soft-wrapped) rows
;; of the draft via the shared composer layout engine
;; (q/ui-core/composer-layout.rkt) through the editing-ops bridge.
;; History navigation is reached exactly at the visual boundaries —
;; Up on the first visual row, Down on the last visual row — so a
;; single-visual-row draft keeps the classic history-only behavior.
;; The explicit Alt+Up / Alt+Down shortcuts (and custom
;; composer.history-up/down keybindings, resolved above) always invoke
;; history regardless of the cursor's visual position.
;; ============================================================

;; Composer display width from the live terminal (never below 1).
(define (composer-display-width)
  (define-values (cols _rows) (tui-screen-size))
  (max 1 cols))

;; Always-history move (Alt+Up/Alt+Down and explicit intents).
(define (history-move! ctx inp dir)
  (set-box! (tui-ctx-input-state-box ctx)
            (if (eq? dir 'up)
                (input-history-up inp)
                (input-history-down inp)))
  'handled)

;; Vertical move that is visual-first, history-at-boundary.
(define (vertical-move! ctx inp dir)
  (cond
    ;; Legacy single-line mode: plain Up/Down stay history-only.
    [(not (tui-multiline-composer-enabled)) (history-move! ctx inp dir)]
    [(eq? dir 'up)
     (define width (composer-display-width))
     (if (input-visual-first-row? inp width)
         (history-move! ctx inp 'up)
         (begin
           (set-box! (tui-ctx-input-state-box ctx) (input-visual-up inp width))
           'handled))]
    [else
     (define width (composer-display-width))
     (if (input-visual-last-row? inp width)
         (history-move! ctx inp 'down)
         (begin
           (set-box! (tui-ctx-input-state-box ctx) (input-visual-down inp width))
           'handled))]))

(define (toggle-transcript-detail! ctx state)
  ;; W5: the single disclosure toggle resolver. Every route to it — the
  ;; configurable keymap action 'ui.transcript.toggle-detail, the
  ;; preference-resolved custom keybinding intent, and the raw Ctrl+O decode
  ;; (byte 15 → 'ctrl-o → (key-spec #\o #t #f #f) → keymap hit) — resolves
  ;; targets identically: only canonical artifact ids. A focused component id
  ;; is merely a hint and is ignored unless it is also one of these ids.
  (define focused-id (tui-ctx-focused-component-id ctx))
  (define reducer (ui-state-conversation-reducer state))
  (define session-id (ui-state-session-id state))
  (define active-artifact
    (and (ui-state-streaming-thinking state)
         (string? session-id)
         (for/first ([turn-id (in-list (filter values
                                               (list (ui-state-active-model-turn-id state)
                                                     (ui-state-active-turn-id state))))]
                     #:do [(define artifact (reducer-thinking-artifact reducer session-id turn-id))]
                     #:when artifact)
           artifact)))
  (define active-id (and active-artifact (conversation-artifact-id active-artifact)))
  (define candidate-ids
    (for/list ([entry (in-list (reverse (ui-state-transcript state)))]
               #:when (eq? (transcript-entry-kind entry) 'thinking)
               #:do [(define id (hash-ref (transcript-entry-meta entry) 'artifact-id #f))]
               #:when (string? id))
      id))
  (define disclosure (ui-state-disclosure state))
  (define target-id (resolve-toggle-target disclosure focused-id active-id candidate-ids))
  (when target-id
    (set-box! (tui-ctx-ui-state-box ctx)
              (struct-copy ui-state state [disclosure (disclosure-toggle disclosure target-id)])))
  target-id)

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
    ;; W4: plain Up moves visually inside the draft; history only at the
    ;; first visual row.
    [(tui.navigation.history-up history-up) (vertical-move! ctx inp 'up)]
    ;; W4: plain Down moves visually inside the draft; history only at the
    ;; last visual row.
    [(tui.navigation.history-down history-down) (vertical-move! ctx inp 'down)]
    ;; W4: explicit history shortcut (Alt+Up) — always history,
    ;; regardless of the cursor's visual position.
    [(tui.navigation.history-up-explicit) (history-move! ctx inp 'up)]
    ;; W4: explicit history shortcut (Alt+Down) — always history,
    ;; regardless of the cursor's visual position.
    [(tui.navigation.history-down-explicit) (history-move! ctx inp 'down)]
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
    [(ui.transcript.toggle-detail toggle-detail)
     (toggle-transcript-detail! ctx state)
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

  ;; 2. Shared preference-aware key resolver (W3): custom keybindings from
  ;; the live preference snapshot win, then the configurable keymap, then
  ;; the hardcoded fallback branches below.
  (define prefs (or (tui-ctx-preferences ctx) (default-preferences)))
  (define-values (base-key k-shift? k-control? k-alt?)
    ;; W3 fix: only char/symbol keycodes have a representable base key;
    ;; other keycodes (e.g. raw numeric ids) bypass preference resolution
    ;; and fall through to the built-in branches below.
    (if (or (char? keycode) (symbol? keycode))
        (let* ([sym (if (symbol? keycode)
                        (symbol->string keycode)
                        (string keycode))]
               [ctl (or (string-prefix? sym "ctrl-") (string-prefix? sym "C-"))]
               [alt (string-prefix? sym "alt-")]
               [sft (string-prefix? sym "shift-")]
               [rest (cond
                       [(string-prefix? sym "ctrl-") (substring sym 5)]
                       [(string-prefix? sym "alt-") (substring sym 4)]
                       [(string-prefix? sym "shift-") (substring sym 6)]
                       [else sym])]
               [k (if (and (string=? rest "return") (eqv? (string-length rest) 6))
                      'return
                      (string->symbol rest))])
          (values k sft ctl alt))
        (values #f #f #f #f)))
  (define ks (keycode->key-spec-from-msg keycode))
  (define resolved-intent
    (and ks
         (resolve-key->intent base-key
                              #:shift? k-shift?
                              #:control? k-control?
                              #:alt? k-alt?
                              #:at-start? (input-at-beginning? inp)
                              #:at-end? (input-at-end? inp)
                              #:prefs prefs)))
  ;; 3. Check configurable keymap
  (define km (get-active-keymap))
  (define action (and ks (keymap-lookup km ks)))
  (match keycode
    [(? (lambda (k) (and action (eq? (dispatch-keymap-action ctx inp state action) 'handled))))
     'continue]
    ;; Preference-resolved intents (custom keybindings) take precedence
    ;; over the hardcoded fallback for the intents they cover.
    [(? (lambda (k)
          (and resolved-intent
               (case resolved-intent
                 [(ui.composer.insert-newline)
                  (set-box! (tui-ctx-input-state-box ctx) (input-insert-newline inp))
                  #t]
                 [(composer.history-up)
                  (set-box! (tui-ctx-input-state-box ctx) (input-history-up inp))
                  #t]
                 [(composer.history-down)
                  (set-box! (tui-ctx-input-state-box ctx) (input-history-down inp))
                  #t]
                 [(ui.transcript.toggle-detail)
                  (toggle-transcript-detail! ctx state)
                  #t]
                 [else #f]))))
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
            (let-values ([(new-state _published?)
                          (request-active-turn-interrupt! (tui-ctx-event-bus ctx) state)])
              (set-box! (tui-ctx-ui-state-box ctx) new-state)))
        'continue]
       ;; W5: the hardcoded ctrl-o fallback is gone. Raw byte 15 decodes to
       ;; 'ctrl-o (terminal-input.rkt), normalizes to (key-spec #\o #t #f #f)
       ;; (binding-resolver.rkt), and resolves through the shared keymap to
       ;; 'ui.transcript.toggle-detail — the single disclosure toggle path.
       ;; If the user unbinds Ctrl+O, no toggle happens, matching keymap
       ;; override semantics for every other action.
       [(alt-tab)
        ;; Cycle focus forward through focusable components
        (let ()
          (define reg-box (tui-ctx-component-registry-box ctx))
          (define reg (and reg-box (unbox reg-box)))
          (when reg
            (define comps (hash-values reg))
            (define current (tui-ctx-focused-component-id ctx))
            (define next-id (cycle-focus comps current 1))
            (tui-ctx-set-focused-component! ctx next-id)))
        'continue]
       [(shift-tab)
        ;; Cycle focus backward through focusable components
        (let ()
          (define reg-box (tui-ctx-component-registry-box ctx))
          (define reg (and reg-box (unbox reg-box)))
          (when reg
            (define comps (hash-values reg))
            (define current (tui-ctx-focused-component-id ctx))
            (define prev-id (cycle-focus comps current -1))
            (tui-ctx-set-focused-component! ctx prev-id)))
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
        ;; W4: visual move through wrapped rows; history at the boundary.
        (vertical-move! ctx inp 'up)
        'continue]
       [(down kp-down)
        ;; W4: visual move through wrapped rows; history at the boundary.
        (vertical-move! ctx inp 'down)
        'continue]
       [(alt-up alt-kp-up alt-down alt-kp-down)
        ;; W4: explicit history shortcuts — always history, regardless of
        ;; the cursor's visual position. Overridable via custom
        ;; keybindings (composer.history-up / composer.history-down
        ;; intents, resolved before this fallback).
        (history-move! ctx inp (if (memq keycode '(alt-down alt-kp-down)) 'down 'up))
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
