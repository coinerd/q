#lang racket/base

;; q/gui/components/rich-transcript-view.rkt — Rich transcript using text% + editor-canvas%
;;
;; Replaces the canvas% + draw-text approach with a text% editor that
;; supports native text selection, auto-wrap, and rich formatting.
;; All racket/gui classes loaded via dynamic-require for headless safety.

(require racket/contract
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
          [make-rich-transcript-gui-view (-> any/c any/c any/c any/c any/c any/c any/c any/c)]))

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
  (hash 'role role
        'text text
        'segments
        (list (hash 'type 'role-label
                    'text (string-append label ": ")
                    'style (make-role-label-delta label role-color))
              (hash 'type 'content
                    'text (string-append text "\n\n")
                    'style (make-content-delta content-color)))))

(define (messages->render-plan msgs theme)
  (for/list ([m (in-list (if (list? msgs) msgs '()))])
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
  (send text-obj set-max-undo 0)
  (send text-obj lock #t)
  (send text-obj change-style
        (make-object style-delta%-cls 'change-normal)
        0 (send text-obj last-position))

  ;; Returns the text% object for external message insertion
  text-obj)
