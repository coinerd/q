#lang racket/base

;; q/tui/theme.rkt — Semantic theme system for TUI colors
;;
;; Replaces hardcoded ANSI colors with named semantic slots.
;; Default dark theme matches the original hardcoded colors.

(require racket/contract)

(provide tui-theme
         tui-theme?
         tui-theme-text
         tui-theme-accent
         tui-theme-muted
         tui-theme-error
         tui-theme-success
         tui-theme-warning
         tui-theme-tool-title
         tui-theme-md-heading
         tui-theme-md-code
         tui-theme-md-code-block-bg
         tui-theme-md-bold
         tui-theme-md-italic
         tui-theme-md-link
         tui-theme-md-blockquote
         tui-theme-md-list-bullet
         tui-theme-md-hr
         tui-theme-status-bg
         tui-theme-selection-bg
         tui-theme-input-prompt
         tui-theme-border
         default-dark-theme
         default-light-theme
         current-tui-theme
         theme-ref
         theme-color->sgr
         theme-color->sgr-bg)

;; ============================================================
;; Theme struct — all fields are ANSI color names or #f for default
;; ============================================================

;; Each field is either:
;;   - An ANSI color symbol: 'black 'red 'green 'yellow 'blue 'magenta 'cyan 'white
;;   - A bright variant: 'bright-black 'bright-red ... 'bright-white
;;   - An SGR parameter string like "38;5;202" (256-color) or "38;2;255;128;0" (truecolor)
;;   - #f meaning "default terminal color"

(struct tui-theme
        (text accent
              muted
              error
              success
              warning
              tool-title
              md-heading
              md-code
              md-code-block-bg
              md-bold
              md-italic
              md-link
              md-blockquote
              md-list-bullet
              md-hr
              status-bg
              selection-bg
              input-prompt
              border)
  #:transparent)

;; Default dark theme — matches original hardcoded colors
(define default-dark-theme
  ;; Basic semantic colors
  (tui-theme 'white ; text
             'cyan ; accent
             'bright-black ; muted
             'red ; error
             'green ; success
             'yellow ; warning
             ;; Tool/UI
             'cyan ; tool-title
             ;; Markdown
             'cyan ; md-heading
             'bright-green ; md-code
             'bright-black ; md-code-block-bg
             'bright-white ; md-bold
             'bright-black ; md-italic
             'cyan ; md-link
             'bright-black ; md-blockquote
             'cyan ; md-list-bullet
             'bright-black ; md-hr
             ;; UI elements
             'blue ; status-bg
             'cyan ; selection-bg
             'cyan ; input-prompt
             'bright-black ; border
             ))

;; Default light theme
(define default-light-theme
  (tui-theme 'black ; text
             'blue ; accent
             'bright-black ; muted
             'red ; error
             'green ; success
             'yellow ; warning
             'blue ; tool-title
             'blue ; md-heading
             'green ; md-code
             'bright-white ; md-code-block-bg
             'black ; md-bold
             'bright-black ; md-italic
             'blue ; md-link
             'bright-black ; md-blockquote
             'blue ; md-list-bullet
             'bright-black ; md-hr
             'blue ; status-bg
             'cyan ; selection-bg
             'blue ; input-prompt
             'bright-black ; border
             ))

;; Current theme parameter
(define current-tui-theme (make-parameter default-dark-theme))

;; Resolve a theme field to an SGR parameter string.
;; Returns #f if the color is #f (use default).
(define (theme-ref field-name)
  (define theme (current-tui-theme))
  (case field-name
    [(text) (tui-theme-text theme)]
    [(accent) (tui-theme-accent theme)]
    [(muted) (tui-theme-muted theme)]
    [(error) (tui-theme-error theme)]
    [(success) (tui-theme-success theme)]
    [(warning) (tui-theme-warning theme)]
    [(tool-title) (tui-theme-tool-title theme)]
    [(md-heading) (tui-theme-md-heading theme)]
    [(md-code) (tui-theme-md-code theme)]
    [(md-code-bg) (tui-theme-md-code-block-bg theme)]
    [(md-bold) (tui-theme-md-bold theme)]
    [(md-italic) (tui-theme-md-italic theme)]
    [(md-link) (tui-theme-md-link theme)]
    [(md-blockquote) (tui-theme-md-blockquote theme)]
    [(md-bullet) (tui-theme-md-list-bullet theme)]
    [(md-hr) (tui-theme-md-hr theme)]
    [(status-bg) (tui-theme-status-bg theme)]
    [(selection-bg) (tui-theme-selection-bg theme)]
    [(input-prompt) (tui-theme-input-prompt theme)]
    [(border) (tui-theme-border theme)]
    [else #f]))

;; Convert a theme color value to an SGR foreground parameter string.
(define (theme-color->sgr color)
  (cond
    [(not color) #f]
    [(string? color) color] ;; Already an SGR parameter string
    [(symbol? color)
     (case color
       [(black) "30"]
       [(red) "31"]
       [(green) "32"]
       [(yellow) "33"]
       [(blue) "34"]
       [(magenta) "35"]
       [(cyan) "36"]
       [(white) "37"]
       [(bright-black) "90"]
       [(bright-red) "91"]
       [(bright-green) "92"]
       [(bright-yellow) "93"]
       [(bright-blue) "94"]
       [(bright-magenta) "95"]
       [(bright-cyan) "96"]
       [(bright-white) "97"]
       [else #f])]
    [else #f]))

;; Convert a theme color value to an SGR background parameter string.
(define (theme-color->sgr-bg color)
  (cond
    [(not color) #f]
    [(string? color)
     ;; Replace leading "3" with "4" for background
     (if (string-prefix? color "3")
         (string-append "4" (substring color 1))
         color)]
    [(symbol? color)
     (case color
       [(black) "40"]
       [(red) "41"]
       [(green) "42"]
       [(yellow) "43"]
       [(blue) "44"]
       [(magenta) "45"]
       [(cyan) "46"]
       [(white) "47"]
       [(bright-black) "100"]
       [(bright-red) "101"]
       [(bright-green) "102"]
       [(bright-yellow) "103"]
       [(bright-blue) "104"]
       [(bright-magenta) "105"]
       [(bright-cyan) "106"]
       [(bright-white) "107"]
       [else #f])]
    [else #f]))

(define (string-prefix? s prefix)
  (and (>= (string-length s) (string-length prefix))
       (string=? (substring s 0 (string-length prefix)) prefix)))
