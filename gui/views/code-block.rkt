#lang racket/base

;; q/gui/views/code-block.rkt — Code block view for GUI
;;
;; Renders fenced code blocks with syntax highlighting hints,
;; line numbers, and copy button metadata.

(require racket/contract
         racket/string
         "../../ui-core/theme-protocol.rkt")

(provide (contract-out [render-code-block
                        (->* (ui-theme? string?)
                             (#:language (or/c string? #f)
                                         #:line-numbers boolean?
                                         #:width exact-nonnegative-integer?)
                             hash?)]
                       [detect-language (-> string? (or/c string? #f))]))

;; ──────────────────────────────
;; Language detection from fence
;; ──────────────────────────────
(define (detect-language text)
  (define m (regexp-match #rx"^```([a-zA-Z0-9+-]+)" text))
  (and m (cadr m)))

;; ──────────────────────────────
;; Render code block
;; ──────────────────────────────
(define (render-code-block theme
                           code
                           #:language [language #f]
                           #:line-numbers [line-numbers #t]
                           #:width [width 80])
  (define lines (string-split code "\n"))
  (define lang (or language (detect-language code)))
  (hash 'view
        'code-block
        'code
        code
        'language
        lang
        'lines
        lines
        'line-count
        (length lines)
        'line-numbers
        line-numbers
        'width
        width
        'bg
        (theme-ref theme 'background)
        'fg
        (theme-ref theme 'foreground)
        'accent
        (theme-ref theme 'accent)))
