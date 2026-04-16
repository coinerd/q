#lang racket/base

;; q/tui/markdown.rkt — TUI markdown rendering facade
;;
;; Provides a unified API for rendering markdown to styled-lines in the TUI.
;; Wraps the parse-markdown tokenizer (from util/markdown.rkt) and the
;; md-format-assistant renderer (from render.rkt) into a single entry point.
;;
;; FEAT-69: Dedicated module for markdown → styled-line conversion.

(require racket/list
         racket/string
         (only-in "../util/markdown.rkt" parse-markdown md-token md-token-type md-token-content)
         "render.rkt")

;; Main entry point
(provide markdown->styled-lines
         ;; Token conversion (exposed for testing)
         markdown-tokens->styled-lines
         ;; Helpers
         styled-line-plain-text)

;; ============================================================
;; Main entry point
;; ============================================================

;; markdown->styled-lines : string? integer? -> (listof styled-line?)
;; Convert a markdown string to a list of styled-lines, width-aware.
;; This is the primary API for TUI rendering of markdown content.
(define (markdown->styled-lines text width)
  (md-format-assistant text width))

;; ============================================================
;; Token-based rendering (for testing / advanced use)
;; ============================================================

;; markdown-tokens->styled-lines : (listof md-token?) integer? -> (listof styled-line?)
;; Convert pre-parsed tokens to styled-lines. Useful when tokens are
;; already available (e.g., for caching or incremental rendering).
(define (markdown-tokens->styled-lines tokens width)
  (render-tokens tokens width))

;; Extract plain text from a styled-line (re-export convenience)
(define (styled-line-plain-text sl)
  (styled-line->text sl))

;; ============================================================
;; Internal: token renderer (same logic as md-format-assistant
;; but operates on pre-parsed tokens)
;; ============================================================

(define (render-tokens tokens width)
  (define (flush-current lines current-segs)
    (if (null? current-segs)
        lines
        (let* ([raw-line (styled-line current-segs)]
               [wrapped (wrap-styled-line raw-line width)])
          (append lines wrapped))))
  (define-values (lines current-segs)
    (for/fold ([lines '()]
               [current-segs '()])
              ([tok (in-list tokens)])
      (case (md-token-type tok)
        [(newline) (values (flush-current lines current-segs) '())]
        [(code-block)
         (define prev-lines (flush-current lines current-segs))
         (define code (cdr (md-token-content tok)))
         (define code-lines
           (for/list ([line (string-split code "\n" #:trim? #f)])
             (styled-line (list (styled-segment (string-append "  " line) (theme->style 'md-code))))))
         (values (append prev-lines code-lines) '())]
        [(header)
         (define prev-lines (flush-current lines current-segs))
         (define header-text (cdr (md-token-content tok)))
         (values (append prev-lines
                         (list (styled-line (list (styled-segment header-text
                                                                  (theme->style 'md-heading
                                                                                '(bold)))))))
                 '())]
        [(hr)
         (define prev-lines (flush-current lines current-segs))
         (values (append prev-lines
                         (list (styled-line (list (styled-segment (make-string (min width 60) #\—)
                                                                  (theme->style 'muted))))))
                 '())]
        [(blockquote)
         (define prev-lines (flush-current lines current-segs))
         (define depth (car (md-token-content tok)))
         (define inner-tokens (cdr (md-token-content tok)))
         (define prefix (make-string (* depth 2) #\space))
         (define inner-lines (render-tokens inner-tokens width))
         (define bq-lines
           (for/list ([line (in-list inner-lines)])
             (styled-line (cons (styled-segment (string-append prefix "│ ")
                                                (theme->style 'md-blockquote))
                                (styled-line-segments line)))))
         (values (append prev-lines bq-lines) '())]
        [(unordered-list)
         (define prev-lines (flush-current lines current-segs))
         (define indent (car (md-token-content tok)))
         (define inner-tokens (cdr (md-token-content tok)))
         (define prefix (make-string (* indent 2) #\space))
         (define bullet-line
           (styled-line (cons (styled-segment (string-append prefix "• ")
                                              (theme->style 'md-list-bullet))
                              (for/list ([tok (in-list inner-tokens)])
                                (md-token->segment tok)))))
         (values (append prev-lines (list bullet-line)) '())]
        [(ordered-list)
         (define prev-lines (flush-current lines current-segs))
         (define indent (car (md-token-content tok)))
         (define num (cadr (md-token-content tok)))
         (define inner-tokens (cddr (md-token-content tok)))
         (define prefix (make-string (* indent 2) #\space))
         (define bullet-line
           (styled-line (cons (styled-segment (string-append prefix (format "~a. " num))
                                              (theme->style 'md-list-bullet))
                              (for/list ([tok (in-list inner-tokens)])
                                (md-token->segment tok)))))
         (values (append prev-lines (list bullet-line)) '())]
        [else (values lines (cons (md-token->segment tok) current-segs))])))
  (flush-current lines current-segs))
