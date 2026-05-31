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
         "../../ui-core/theme-protocol.rkt"
         "markdown-parser.rkt"
         "keybindings.rkt"
         "scroll-state.rkt"
         "input-helpers.rkt")

(provide role->label
         role->color
         hex->color-components
         (contract-out
          [make-role-label-delta (-> string? (or/c string? #f) hash?)]
          [make-content-delta (-> (or/c string? #f) hash?)]
          [render-message-descriptor (-> hash? ui-theme? hash?)]
          [messages->render-plan (-> any/c ui-theme? (listof hash?))]
          [compute-transcript-diff (-> list? list? (listof hash?))]
          [apply-diff-to-plan (-> (listof hash?) (listof hash?) ui-theme? (listof hash?))]
          [update-last-message (-> list? string? list?)]
          [make-rich-transcript-gui-view (-> any/c any/c any/c any/c any/c any/c any/c any/c)]
          [insert-message-into-text! (-> any/c hash? ui-theme? void?)]
          [clear-and-rebuild-text! (-> any/c list? ui-theme? void?)]
          [apply-diff-to-text!
           (-> any/c list? list? ui-theme? (or/c (box/c exact-nonnegative-integer?) #f) void?)])
         (all-from-out "markdown-parser.rkt")
         (all-from-out "keybindings.rkt")
         (all-from-out "scroll-state.rkt")
         (all-from-out "input-helpers.rkt"))

;; ──────────────────────────────
;; Pure helpers (headless-testable)
;; ──────────────────────────────

(define (role->label role)
  (case (string->symbol (or role ""))
    [(user) "You"]
    [(assistant) "Assistant"]
    [(system) "System"]
    [(tool tool-result) "Tool"]
    [else (string-titlecase (or role "Unknown"))]))

(define (role->color role theme)
  (case (string->symbol (or role ""))
    [(user) (theme-ref theme 'accent)]
    [(assistant) (theme-ref theme 'foreground)]
    [(system) (theme-ref theme 'muted)]
    [(tool tool-result) (theme-ref theme 'warning)]
    [else (theme-ref theme 'muted)]))

(define (hex->color-components hex-str)
  (define cleaned (string-trim (or hex-str "#000000") "#" #:left? #t))
  (define r (string->number (substring cleaned 0 2) 16))
  (define g (string->number (substring cleaned 2 4) 16))
  (define b (string->number (substring cleaned 4 6) 16))
  (hash 'r r 'g g 'b b 'hex (or hex-str "#000000")))

(define (make-role-label-delta label color-hex)
  (hash 'type 'role-label 'label label 'color color-hex 'bold #t 'family 'modern 'size 12))

(define (make-content-delta color-hex)
  (hash 'type 'content 'color color-hex 'bold #f 'family 'modern 'size 12))

;; ──────────────────────────────
;; Render plan: message → styled segments
;; ──────────────────────────────

(define (render-message-descriptor msg theme)
  (define role (hash-ref msg 'role "system"))
  (define text (hash-ref msg 'text ""))
  (define label (role->label role))
  (define role-color (role->color role theme))
  (define content-color (theme-ref theme 'foreground))
  (define role-seg
    (hash 'type
          'role-label
          'text
          (string-append label ": ")
          'style
          (make-role-label-delta label role-color)))
  ;; Use code-block-aware parsing when code blocks detected
  (define content-segs
    (if (contains-code-blocks? text)
        (let ()
          (define parsed (parse-code-blocks text))
          (for/list ([seg (in-list parsed)])
            (cond
              [(equal? (hash-ref seg 'type #f) 'code-block)
               (hash 'type
                     'code-block
                     'text
                     (hash-ref seg 'text "")
                     'lang
                     (hash-ref seg 'lang "")
                     'style
                     (code-block-style theme))]
              [else
               (hash 'type
                     'content
                     'text
                     (hash-ref seg 'text "")
                     'style
                     (make-content-delta content-color))])))
        (list (hash 'type
                    'content
                    'text
                    (string-append text "\n\n")
                    'style
                    (make-content-delta content-color)))))
  (hash 'role role 'text text 'segments (cons role-seg content-segs)))

(define (messages->render-plan msgs theme)
  (for/list ([m (in-list (if (list? msgs)
                             msgs
                             '()))])
    (render-message-descriptor m theme)))

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
        (list (hash 'op 'update-last 'msg (list-ref new-msgs (- new-len 1))))]
       [else '()])]
    [(> new-len old-len)
     (for/list ([i (in-range old-len new-len)])
       (hash 'op 'append 'msg (list-ref new-msgs i)))]
    [(< new-len old-len) (list (hash 'op 'reset 'msgs new-msgs))]))

(define (apply-diff-to-plan current-plan diff-ops theme)
  (for/fold ([plan current-plan]) ([op (in-list diff-ops)])
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
;; text% manipulation helpers (runtime only)
;; ──────────────────────────────

;; Insert a single message into a text% object with styled role label.
;; text-obj: a text% instance (must be unlocked externally)
;; msg: hash with 'role and 'text keys
;; theme: ui-theme
(define (insert-message-into-text! text-obj msg theme)
  (define role (hash-ref msg 'role "system"))
  (define text (hash-ref msg 'text ""))
  (define label (role->label role))
  (cond
    [(or (getenv "DISPLAY") (getenv "WAYLAND_DISPLAY"))
     ;; GUI available: apply rich formatting via style-delta%
     (define style-delta% (dynamic-require 'racket/gui 'style-delta%))
     (define color% (dynamic-require 'racket/gui 'color%))
     ;; Convert hex strings to color% objects for set-delta-foreground
     (define (hex->color-obj hex)
       (define cleaned (string-trim hex "#" #:left? #t))
       (make-object color%
                    (string->number (substring cleaned 0 2) 16)
                    (string->number (substring cleaned 2 4) 16)
                    (string->number (substring cleaned 4 6) 16)))
     (define role-color (hex->color-obj (role->color role theme)))
     (define content-color (hex->color-obj (theme-ref theme 'foreground)))
     ;; Apply bold role label with role color
     (define role-delta (make-object style-delta% 'change-bold))
     (send role-delta set-delta-foreground role-color)
     (send text-obj change-style role-delta)
     (send text-obj insert (format "~a: " label) (send text-obj last-position))
     ;; Apply normal content style with foreground color
     (define content-delta (make-object style-delta% 'change-normal))
     (send content-delta set-delta-foreground content-color)
     (send text-obj change-style content-delta)
     (send text-obj insert (string-append text "\n\n") (send text-obj last-position))]
    ;; Headless fallback: plain text insertion
    [else (send text-obj insert (format "~a: ~a\n\n" label text) (send text-obj last-position))]))

;; Clear a text% object and rebuild from a list of messages.
;; text-obj: a text% instance
;; msgs: list of message hashes
;; theme: ui-theme
(define (clear-and-rebuild-text! text-obj msgs theme)
  (send text-obj lock #f)
  (send text-obj delete 0 (send text-obj last-position))
  (for ([msg (in-list msgs)])
    (insert-message-into-text! text-obj msg theme))
  (send text-obj lock #t))

;; Apply diff between old and new messages to a text% object.
;; For append ops, inserts only new messages.
;; For update-last, uses incremental suffix append when text only grew
;;   (preserves user selection during streaming).
;; For reset, falls back to full rebuild.
;; last-len-box: (boxof exact-nonnegative-integer?) — tracks previous text length
;;   for incremental append. Pass #f to disable incremental mode.
(define (apply-diff-to-text! text-obj old-msgs new-msgs theme [last-len-box #f])
  (define diff (compute-transcript-diff old-msgs new-msgs))
  (cond
    ;; No structural change
    [(null? diff) (void)]
    [(and (= (length diff) 1) (eq? (hash-ref (car diff) 'op #f) 'append))
     ;; Single append → just insert the new message
     (send text-obj lock #f)
     (insert-message-into-text! text-obj (hash-ref (car diff) 'msg) theme)
     (send text-obj lock #t)]
    [(and (= (length diff) 1) (eq? (hash-ref (car diff) 'op #f) 'update-last))
     ;; Update last → try incremental suffix append first
     (send text-obj lock #f)
     (define total (send text-obj last-position))
     (define last-msg (hash-ref (car diff) 'msg))
     (define new-text (hash-ref last-msg 'text ""))
     (define old-text (and (pair? old-msgs) (hash-ref (last old-msgs) 'text "")))
     (cond
       ;; Incremental path: text only grew (streaming append)
       [(and last-len-box
             old-text
             (> (string-length new-text) (string-length old-text))
             (equal? (substring new-text 0 (string-length old-text)) old-text))
        (define suffix (substring new-text (string-length old-text)))
        ;; Insert only the new suffix at end of text% (before trailing newlines)
        (send text-obj insert suffix (- total 2))
        (when last-len-box
          (set-box! last-len-box (string-length new-text)))]
       ;; Slow path: full delete + reinsert of last message
       [else
        (define last-role (hash-ref last-msg 'role "system"))
        (define last-label (role->label last-role))
        (define search-str (string-append last-label ": "))
        (define start-pos
          (let loop ([pos (max 0 (- total (string-length search-str)))])
            (cond
              [(< pos 0) -1]
              [(and (>= (+ pos (string-length search-str)) total)
                    (equal? (send text-obj get-text 0 (min (string-length search-str) total))
                            search-str))
               0]
              [(and (< (+ pos (string-length search-str)) total)
                    (equal? (send text-obj get-text pos (+ pos (string-length search-str)))
                            search-str))
               pos]
              [(<= pos 0) -1]
              [else (loop (- pos 1))])))
        (cond
          [(>= start-pos 0)
           (send text-obj delete start-pos total)
           (insert-message-into-text! text-obj last-msg theme)]
          ;; Label not found — full rebuild
          [else (clear-and-rebuild-text! text-obj new-msgs theme)])
        (when last-len-box
          (set-box! last-len-box (string-length new-text)))])
     (send text-obj lock #t)]
    ;; Multiple appends or reset → full rebuild
    [else
     (clear-and-rebuild-text! text-obj new-msgs theme)
     (when last-len-box
       (set-box! last-len-box 0))]))

;; ──────────────────────────────
;; GUI view constructor (runtime only)
;; ──────────────────────────────

;; (make-rich-transcript-gui-view text% editor-canvas% color% font% style-delta% theme)
;; Creates a custom gui-easy view using text% + editor-canvas%.
;; Returns: (values text-object view-ctor)
;; text-object: the text% instance for external append/update calls
;; view-ctor: (-> parent-panel void) — adds the editor-canvas% to parent
(define (make-rich-transcript-gui-view text%-cls
                                       editor-canvas%-cls
                                       color%-cls
                                       font%-cls
                                       style-delta%-cls
                                       theme
                                       queue-callback)
  (define bg-c
    (make-object color%-cls
                 (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 1 3) 16)
                 (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 3 5) 16)
                 (string->number (substring (or (theme-ref theme 'background) "#1e1e2e") 5 7) 16)))
  (define fg-c
    (make-object color%-cls
                 (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 1 3) 16)
                 (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 3 5) 16)
                 (string->number (substring (or (theme-ref theme 'foreground) "#cdd6f4") 5 7) 16)))
  (define mono-font (make-object font%-cls 12 'modern 'normal 'normal #f))

  (define text-obj (make-object text%-cls))
  (send text-obj auto-wrap #t)
  (send text-obj set-max-undo-history 0)
  ;; Set default foreground color on the text%'s style
  (define fg-delta (make-object style-delta%-cls))
  (send fg-delta set-delta-foreground fg-c)
  (send text-obj change-style fg-delta 0 0)
  (send text-obj lock #t)

  ;; Returns the text% object for external message insertion
  text-obj)
