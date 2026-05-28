#lang racket/base

;; q/ui-core/theme-protocol.rkt — Shared theme definitions for TUI + GUI
;;
;; Defines a unified theme struct that both TUI and GUI backends can use.
;; The theme maps semantic roles (e.g., 'header, 'error) to colors.
;; TUI uses ANSI colors; GUI uses hex colors — both derived from the same theme.

(require racket/contract)

(provide ui-theme?
         (contract-out [make-ui-theme
                        (->* ()
                             (#:background (or/c string? #f)
                                           #:foreground (or/c string? #f)
                                           #:accent (or/c string? #f)
                                           #:error (or/c string? #f)
                                           #:success (or/c string? #f)
                                           #:warning (or/c string? #f)
                                           #:muted (or/c string? #f)
                                           #:font-family (or/c string? #f)
                                           #:font-size (or/c exact-positive-integer? #f))
                             ui-theme?)]
                       [default-theme (-> ui-theme?)]
                       [theme-ref (-> ui-theme? symbol? any/c)]
                       [theme-merge (-> ui-theme? ui-theme? ui-theme?)]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct ui-theme (background foreground accent error success warning muted font-family font-size)
  #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-ui-theme #:background [background #f]
                       #:foreground [foreground #f]
                       #:accent [accent #f]
                       #:error [error #f]
                       #:success [success #f]
                       #:warning [warning #f]
                       #:muted [muted #f]
                       #:font-family [font-family #f]
                       #:font-size [font-size #f])
  (ui-theme background foreground accent error success warning muted font-family font-size))

;; ──────────────────────────────
;; Default theme (dark)
;; ──────────────────────────────
(define (default-theme)
  (ui-theme "#1e1e2e" ; background (dark)
            "#cdd6f4" ; foreground (light text)
            "#89b4fa" ; accent (blue)
            "#f38ba8" ; error (red)
            "#a6e3a1" ; success (green)
            "#f9e2af" ; warning (yellow)
            "#6c7086" ; muted (gray)
            "monospace" ; font-family
            14)) ; font-size

;; ──────────────────────────────
;; Access by semantic key
;; ──────────────────────────────
(define (theme-ref theme key)
  (case key
    [(background) (ui-theme-background theme)]
    [(foreground) (ui-theme-foreground theme)]
    [(accent) (ui-theme-accent theme)]
    [(error) (ui-theme-error theme)]
    [(success) (ui-theme-success theme)]
    [(warning) (ui-theme-warning theme)]
    [(muted) (ui-theme-muted theme)]
    [(font-family) (ui-theme-font-family theme)]
    [(font-size) (ui-theme-font-size theme)]
    [else #f]))

;; ──────────────────────────────
;; Merge: override takes precedence
;; ──────────────────────────────
(define (theme-merge base override)
  (ui-theme (or (ui-theme-background override) (ui-theme-background base))
            (or (ui-theme-foreground override) (ui-theme-foreground base))
            (or (ui-theme-accent override) (ui-theme-accent base))
            (or (ui-theme-error override) (ui-theme-error base))
            (or (ui-theme-success override) (ui-theme-success base))
            (or (ui-theme-warning override) (ui-theme-warning base))
            (or (ui-theme-muted override) (ui-theme-muted base))
            (or (ui-theme-font-family override) (ui-theme-font-family base))
            (or (ui-theme-font-size override) (ui-theme-font-size base))))
