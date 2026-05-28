#lang racket/base

;; q/gui/components/rich-transcript-view.rkt — Rich transcript using text% + editor-canvas%
;;
;; Replaces the canvas% + draw-text approach with a text% editor that
;; supports native text selection, auto-wrap, and rich formatting.
;; All racket/gui classes loaded via dynamic-require for headless safety.

(require racket/contract
         racket/format
         racket/string
         racket/class
         racket/list
         "../../ui-core/theme-protocol.rkt")

(provide role->label
         role->color
         hex->color-object
         (contract-out
          [make-role-label-delta (-> string? (or/c string? #f) hash?)]
          [make-content-delta (-> (or/c string? #f) hash?)]
          [render-message-descriptor (-> hash? ui-theme? hash?)]
          [messages->render-plan (-> any/c ui-theme? (listof hash?))]
          [compute-transcript-diff (-> list? list? (listof hash?))]
          [apply-diff-to-plan (-> (listof hash?) (listof hash?) ui-theme? (listof hash?))]
          [update-last-message (-> list? string? list?)]
          [make-rich-transcript-gui-view (-> any/c any/c any/c any/c any/c any/c any/c any/c)]
          [make-scroll-state (->* () (boolean?) hash?)]
          [scroll-state-auto-scroll? (-> hash? boolean?)]
          [scroll-state-user-scrolled-up? (-> hash? boolean?)]
          [scroll-state-on-scroll (-> hash? (between/c 0.0 1.0) hash?)]
          [scroll-state-on-submit (-> hash? hash?)]
          [contains-code-blocks? (-> any/c boolean?)]
          [parse-code-blocks (-> any/c (listof hash?))]
          [render-message-with-code-blocks (-> hash? ui-theme? hash?)]
          [key-event->action (-> char? boolean? (or/c symbol? #f))]
          [lookup-keybinding (-> char? boolean? (or/c symbol? #f))]
          [list-keybindings (-> (listof pair?))]
          [default-keybindings hash?]
          [code-block-style (-> ui-theme? hash?)]
          [code-block-header-style (-> (or/c string? #f) hash?)]
          [input-key-should-submit? (-> any/c boolean? boolean? boolean?)]
          [prepare-input-for-submit (-> string? string?)]
          [input-line-count (-> string? exact-nonnegative-integer?)]
          [input-looks-like-code? (-> string? boolean?)]))

;; ──────────────────────────────
;; Pure helpers (headless-testable)
;; ──────────────────────────────

(define (role->label role)
  (case (string->symbol (or role ""))
    [(user) "You"]
    [(assistant) "Assistant"]
    [(system) "System"]
    [(tool) "Tool"]
    [else (string-titlecase (or role "Unknown"))]))

(define (role->color role theme)
  (case (string->symbol (or role ""))
    [(user) (theme-ref theme 'accent)]
    [(assistant) (theme-ref theme 'foreground)]
    [(system) (theme-ref theme 'muted)]
    [(tool) (theme-ref theme 'warning)]
    [else (theme-ref theme 'muted)]))

(define (hex->color-object hex-str)
  (define cleaned (string-trim (or hex-str "#000000") "#" #:left? #t))
  (define r (string->number (substring cleaned 0 2) 16))
  (define g (string->number (substring cleaned 2 4) 16))
  (define b (string->number (substring cleaned 4 6) 16))
  (hash 'r r 'g g 'b b 'hex (or hex-str "#000000")))

(define (make-role-label-delta label color-hex)
  (hash 'type 'role-label
        'label label
        'color color-hex
        'bold #t
        'family 'modern
        'size 12))

(define (make-content-delta color-hex)
  (hash 'type 'content
        'color color-hex
        'bold #f
        'family 'modern
        'size 12))

;; ──────────────────────────────
;; Render plan: message → styled segments
;; ──────────────────────────────

(define (render-message-descriptor msg theme)
  (define role (hash-ref msg 'role "system"))
  (define text (hash-ref msg 'text ""))
  (define label (role->label role))
  (define role-color (role->color role theme))
  (define content-color (theme-ref theme 'foreground))
  (define role-seg (hash 'type 'role-label
                         'text (string-append label ": ")
                         'style (make-role-label-delta label role-color)))
  ;; Use code-block-aware parsing when code blocks detected
  (define content-segs
    (if (contains-code-blocks? text)
        (let ()
          (define parsed (parse-code-blocks text))
          (for/list ([seg (in-list parsed)])
            (cond
              [(equal? (hash-ref seg 'type #f) 'code-block)
               (hash 'type 'code-block
                     'text (hash-ref seg 'text "")
                     'lang (hash-ref seg 'lang "")
                     'style (code-block-style theme))]
              [else
               (hash 'type 'content
                     'text (hash-ref seg 'text "")
                     'style (make-content-delta content-color))])))
        (list (hash 'type 'content
                    'text (string-append text "\n\n")
                    'style (make-content-delta content-color)))))
  (hash 'role role
        'text text
        'segments (cons role-seg content-segs)))

(define (messages->render-plan msgs theme)
  (for/list ([m (in-list (if (list? msgs) msgs '()))])
    (render-message-descriptor m theme)))

;; ──────────────────────────────
;; Code block detection and parsing (pure, headless-testable)
;; ──────────────────────────────

;; A code-block segment: (hash 'type 'code-block 'text "..." 'lang "racket" ...)
;; A text segment: (hash 'type 'text 'text "...")

;; Check if a string looks like it contains markdown code blocks
(define (contains-code-blocks? text)
  (and (string? text)
       (regexp-match? #rx"```" text)))

;; Parse text into segments: alternating text and code-block segments
;; Returns: (listof hash?) where each hash has 'type ('text or 'code-block) and 'text
(define (parse-code-blocks text)
  (cond
    [(not (string? text)) (list (hash 'type 'text 'text (~a text)))]
    [(not (contains-code-blocks? text))
     (list (hash 'type 'text 'text text))]
    [else
     ;; Split on ``` delimiters
     (define parts (regexp-split #rx"```" text))
     (define result '())
     (define in-code? #f)
     (define current-lang "")
     (for ([part (in-list parts)]
           [idx (in-naturals)])
       (cond
         [(string=? part "") (void)]  ;; skip empty between consecutive ```
         [in-code?
          ;; This part is code
          ;; First line might be the language
          (define lines (string-split part "\n" #:trim? #f))
          (define first-line (if (pair? lines) (car lines) ""))
          (define lang (if (regexp-match? #rx"^[a-zA-Z0-9+_-]+$" first-line)
                           first-line
                           ""))
          (define code-text
            (if (string=? lang "")
                part
                (string-join (cdr lines) "\n" #:after-last "")))
          (set! result
                (append result
                        (list (hash 'type 'code-block
                                    'text (string-trim code-text #:left? #f #:right? #t)
                                    'lang lang))))]
         [else
          ;; This part is regular text
          (when (> (string-length part) 0)
            (set! result
                  (append result
                          (list (hash 'type 'text 'text part)))))])
       (set! in-code? (not in-code?)))
     (if (null? result)
         (list (hash 'type 'text 'text text))
         result)]))

;; Render a message with code block awareness
;; Extends render-message-descriptor to split code blocks into segments
(define (render-message-with-code-blocks msg theme)
  (define text (hash-ref msg 'text ""))
  (define role (hash-ref msg 'role "system"))
  (define segments (parse-code-blocks text))
  (define label (role->label role))
  (define color (role->color role theme))
  (define role-seg (hash 'type 'text 'text (format "[~a] " label) 'style (hash 'color color 'weight 'bold)))
  (define content-segs
    (for/list ([seg (in-list segments)])
      (cond
        [(equal? (hash-ref seg 'type #f) 'code-block)
         (hash 'type 'code-block
               'text (hash-ref seg 'text "")
               'lang (hash-ref seg 'lang "")
               'style (hash 'background "#2a2a3e" 'font "monospace"))]
        [else
         (hash 'type 'text
               'text (hash-ref seg 'text "")
               'style (hash 'color (or color (theme-ref theme 'foreground))))])))
  (hash 'role role
        'segments (cons role-seg content-segs)))

;; ──────────────────────────────
;; Diff-based update logic
;; ──────────────────────────────

(define (update-last-message msgs new-text)
  (cond
    [(null? msgs) msgs]
    [else
     (define last (car (reverse msgs)))
     (define updated (hash-set last 'text new-text))
     (append (take msgs (- (length msgs) 1)) (list updated))]))

(define (compute-transcript-diff old-msgs new-msgs)
  (define old-len (length old-msgs))
  (define new-len (length new-msgs))
  (cond
    [(= old-len new-len)
     (cond
       [(and (> old-len 0)
             (not (equal? (hash-ref (list-ref old-msgs (- old-len 1)) 'text #f)
                          (hash-ref (list-ref new-msgs (- new-len 1)) 'text #f))))
        (list (hash 'op 'update-last
                    'msg (list-ref new-msgs (- new-len 1))))]
       [else '()])]
    [(> new-len old-len)
     (for/list ([i (in-range old-len new-len)])
       (hash 'op 'append 'msg (list-ref new-msgs i)))]
    [(< new-len old-len)
     (list (hash 'op 'reset 'msgs new-msgs))]))

(define (apply-diff-to-plan current-plan diff-ops theme)
  (for/fold ([plan current-plan])
            ([op (in-list diff-ops)])
    (case (hash-ref op 'op #f)
      [(append)
       (define msg (hash-ref op 'msg))
       (append plan (list (render-message-descriptor msg theme)))]
      [(update-last)
       (define msg (hash-ref op 'msg))
       (define new-desc (render-message-descriptor msg theme))
       (if (null? plan)
           (list new-desc)
           (append (take plan (- (length plan) 1)) (list new-desc)))]
      [(reset)
       (define msgs (hash-ref op 'msgs '()))
       (messages->render-plan msgs theme)]
      [else plan])))

;; ──────────────────────────────
;; Auto-scroll state management (pure, headless-testable)
;; ──────────────────────────────

;; Scroll state: tracks whether auto-scroll is enabled
;; and the last known scroll position ratio (0.0 = top, 1.0 = bottom)
(define (make-scroll-state [auto? #t])
  (hash 'auto-scroll auto?
        'scroll-ratio 1.0
        'user-scrolled-up #f))

(define (scroll-state-auto-scroll? ss)
  (hash-ref ss 'auto-scroll #t))

(define (scroll-state-user-scrolled-up? ss)
  (hash-ref ss 'user-scrolled-up #f))

;; On scroll event: update scroll state based on position
;; ratio: 0.0 = top, 1.0 = bottom
(define (scroll-state-on-scroll ss ratio)
  (cond
    [(>= ratio 0.95)
     ;; Near bottom → re-enable auto-scroll
     (hash-set (hash-set ss 'auto-scroll #t) 'user-scrolled-up #f)]
    [else
     ;; Scrolled up → disable auto-scroll
     (hash-set (hash-set ss 'auto-scroll #f) 'user-scrolled-up #t)]))

;; On user submit → reset auto-scroll to #t
(define (scroll-state-on-submit ss)
  (hash-set (hash-set ss 'auto-scroll #t) 'user-scrolled-up #f))

;; ──────────────────────────────
;; Multiline input helpers (pure, headless-testable)
;; ──────────────────────────────

;; Should this key event trigger submit?
;; Enter without Shift/Control → submit
;; Shift+Enter or Control+Enter → insert newline
(define (input-key-should-submit? key-code shift? control?)
  (and (equal? key-code 'return)
       (not shift?)
       (not control?)))

;; Process input text: trim trailing whitespace for submission
(define (prepare-input-for-submit text)
  (string-trim text #:left? #f))

;; Split input into lines for validation
(define (input-line-count text)
  (length (string-split text "\n")))

;; Check if input appears to be a code block (for auto-detection)
(define (input-looks-like-code? text)
  (or (contains-code-blocks? text)
      (ormap (lambda (pat) (string-contains? text pat))
             (list "(define " "(let " "(lambda " "(if " "(cond " "(for " "(when " "(set! "))))
;; ──────────────────────────────
;; Keyboard shortcut registry (pure, headless-testable)
;; ──────────────────────────────

;; Default keyboard shortcuts for the GUI transcript
(define default-keybindings
  (hash #\l 'clear
        #\k 'compact
        #\c 'interrupt
        #\s 'save
        #\q 'quit))

;; Look up a keybinding by key character + ctrl modifier
(define (lookup-keybinding key-char ctrl?)
  (if ctrl?
      (hash-ref default-keybindings key-char #f)
      #f))

;; Get action symbol for a key event
(define (key-event->action key-char ctrl?)
  (lookup-keybinding key-char ctrl?))

;; List all registered shortcuts as (char . action) pairs
(define (list-keybindings)
  (hash->list default-keybindings))

;; ──────────────────────────────
;; Code block styling helpers (headless-testable)
;; ──────────────────────────────

(define (code-block-style theme)
  (hash 'background (or (theme-ref theme 'background) "#1e1e2e")
        'foreground "#cdd6f4"
        'font "monospace"
        'border-left "#89b4fa"))

(define (code-block-header-style lang)
  (hash 'text (or lang "")
        'style (hash 'foreground "#6c7086" 'font "monospace" 'size 'small)))

;; ──────────────────────────────
;; GUI view constructor (runtime only)
;; ──────────────────────────────

;; (make-rich-transcript-gui-view text% editor-canvas% color% font% style-delta% theme)
;; Creates a custom gui-easy view using text% + editor-canvas%.
;; Returns: (values text-object view-ctor)
;; text-object: the text% instance for external append/update calls
;; view-ctor: (-> parent-panel void) — adds the editor-canvas% to parent
(define (make-rich-transcript-gui-view text%-cls editor-canvas%-cls
                                       color%-cls font%-cls style-delta%-cls
                                       theme queue-callback)
  (define bg-c (make-object color%-cls
                          (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 1 3) 16)
                          (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 3 5) 16)
                          (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 5 7) 16)))
  (define fg-c (make-object color%-cls
                          (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 1 3) 16)
                          (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 3 5) 16)
                          (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 5 7) 16)))
  (define mono-font (make-object font%-cls 12 'modern 'normal 'normal #f))

  (define text-obj (make-object text%-cls))
  (send text-obj auto-wrap #t)
  (send text-obj set-max-undo-history 0)
  (send text-obj lock #t)
  (send text-obj change-style
        (make-object style-delta%-cls 'change-normal)
        0 (send text-obj last-position))

  ;; Returns the text% object for external message insertion
  text-obj)
