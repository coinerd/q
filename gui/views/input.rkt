#lang racket/base

;; q/gui/views/input.rkt — Input area view for GUI
;;
;; Renders an input text area with prompt indicator, text content,
;; cursor position, and placeholder. Produces view descriptors.

(require racket/contract
         racket/format
         racket/string
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-input-area
                        (->* (ui-theme?)
                             (#:text string?
                                     #:cursor exact-nonnegative-integer?
                                     #:placeholder (or/c string? #f)
                                     #:prompt string?
                                     #:multiline boolean?
                                     #:width exact-nonnegative-integer?
                                     #:focused boolean?)
                             hash?)]
                       [input-insert-text (-> string? exact-nonnegative-integer? string? string?)]
                       [input-delete-char (-> string? exact-nonnegative-integer? string?)]
                       [input-move-cursor
                        (-> string?
                            exact-nonnegative-integer?
                            (or/c 'left 'right 'home 'end)
                            exact-nonnegative-integer?)]))

;; ──────────────────────────────
;; Text manipulation (pure functions)
;; ──────────────────────────────
(define (input-insert-text text cursor insert)
  (define before (substring text 0 cursor))
  (define after (substring text cursor))
  (string-append before insert after))

(define (input-delete-char text cursor)
  (if (or (zero? cursor) (string=? text ""))
      text
      (string-append (substring text 0 (sub1 cursor)) (substring text cursor))))

(define (input-move-cursor text cursor direction)
  (case direction
    [(left) (max 0 (sub1 cursor))]
    [(right) (min (string-length text) (add1 cursor))]
    [(home) 0]
    [(end) (string-length text)]))

;; ──────────────────────────────
;; Render input area
;; ──────────────────────────────
(define (render-input-area theme
                           #:text [text ""]
                           #:cursor [cursor 0]
                           #:placeholder [placeholder #f]
                           #:prompt [prompt "> "]
                           #:multiline [multiline #f]
                           #:width [width 120]
                           #:focused [focused #t])
  (define display-text (if (and (string=? text "") placeholder) placeholder text))
  (define display-cursor (if (and (string=? text "") placeholder) 0 cursor))
  (hash 'view
        'input-area
        'text
        display-text
        'cursor
        display-cursor
        'prompt
        prompt
        'placeholder
        placeholder
        'multiline
        multiline
        'width
        width
        'focused
        focused
        'has-content
        (not (string=? text ""))
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
