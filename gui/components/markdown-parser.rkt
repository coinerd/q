#lang racket/base

;; q/gui/components/markdown-parser.rkt — Markdown code block parsing and styling
;;
;; Extracted from rich-transcript-view.rkt for SRP separation.
;; Pure functions, headless-testable, no racket/gui dependencies.

(require racket/contract
         racket/format
         racket/string
         racket/list
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out
          [contains-code-blocks? (-> any/c boolean?)]
          [parse-code-blocks (-> any/c (listof hash?))]
          [render-message-with-code-blocks (-> hash? ui-theme? hash?)]
          [code-block-style (-> ui-theme? hash?)]
          [code-block-header-style (-> (or/c string? #f) hash?)]))

;; ──────────────────────────────
;; Code block detection
;; ──────────────────────────────

;; Check if a string looks like it contains markdown code blocks
(define (contains-code-blocks? text)
  (and (string? text)
       (regexp-match? #rx"```" text)))

;; ──────────────────────────────
;; Code block parsing
;; ──────────────────────────────

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

;; ──────────────────────────────
;; Code block styling
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
;; Message rendering with code blocks
;; ──────────────────────────────

;; Render a message with code block awareness
;; Returns hash with 'role and 'segments
(define (render-message-with-code-blocks msg theme)
  (define text (hash-ref msg 'text ""))
  (define role (hash-ref msg 'role "system"))
  (define segments (parse-code-blocks text))
  (define label
    (case (string->symbol (or role ""))
      [(user) "You"]
      [(assistant) "Assistant"]
      [(system) "System"]
      [(tool) "Tool"]
      [else (string-titlecase (or role "Unknown"))]))
  (define color
    (case (string->symbol (or role ""))
      [(user) (theme-ref theme 'accent)]
      [(assistant) (theme-ref theme 'foreground)]
      [(system) (theme-ref theme 'muted)]
      [(tool) (theme-ref theme 'warning)]
      [else (theme-ref theme 'muted)]))
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
