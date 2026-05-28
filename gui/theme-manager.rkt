#lang racket/base

;; q/gui/theme-manager.rkt — Theme switching and management for GUI
;;
;; Manages multiple themes (light, dark, custom) with switching
;; and customization. Uses the theme-protocol structs.

(require racket/contract
         "../ui-core/theme-protocol.rkt")

(provide theme-manager?
         (contract-out [make-theme-manager (->* [] [ui-theme?] theme-manager?)]
                       [tm-current-theme (-> theme-manager? ui-theme?)]
                       [tm-switch-theme! (-> theme-manager? symbol? theme-manager?)]
                       [tm-register-theme! (-> theme-manager? symbol? ui-theme? theme-manager?)]
                       [tm-list-themes (-> theme-manager? (listof symbol?))]
                       [tm-customize-color (-> theme-manager? symbol? string? theme-manager?)]))

;; ──────────────────────────────
;; Struct
;; ──────────────────────────────
(struct theme-manager (current-name-box themes-box) #:transparent)

;; ──────────────────────────────
;; Constructor
;; ──────────────────────────────
(define (make-theme-manager [default (default-theme)])
  (define themes (make-hash))
  (hash-set! themes 'default default)
  (hash-set! themes 'light (make-light-theme))
  (hash-set! themes 'dark (make-dark-theme))
  (theme-manager (box 'default) (box themes)))

;; ──────────────────────────────
;; Operations
;; ──────────────────────────────
(define (tm-current-theme mgr)
  (define name (unbox (theme-manager-current-name-box mgr)))
  (hash-ref (unbox (theme-manager-themes-box mgr)) name (default-theme)))

(define (tm-switch-theme! mgr name)
  (define themes (unbox (theme-manager-themes-box mgr)))
  (when (hash-has-key? themes name)
    (set-box! (theme-manager-current-name-box mgr) name))
  mgr)

(define (tm-register-theme! mgr name theme)
  (hash-set! (unbox (theme-manager-themes-box mgr)) name theme)
  mgr)

(define (tm-list-themes mgr)
  (hash-keys (unbox (theme-manager-themes-box mgr))))

(define (tm-customize-color mgr color-name value)
  (define current (tm-current-theme mgr))
  (define updated
    (theme-merge current
                 (make-ui-theme #:background (if (eq? color-name 'background) value #f)
                                #:foreground (if (eq? color-name 'foreground) value #f)
                                #:accent (if (eq? color-name 'accent) value #f)
                                #:error (if (eq? color-name 'error) value #f)
                                #:success (if (eq? color-name 'success) value #f)
                                #:warning (if (eq? color-name 'warning) value #f)
                                #:muted (if (eq? color-name 'muted) value #f))))
  (define name (unbox (theme-manager-current-name-box mgr)))
  (hash-set! (unbox (theme-manager-themes-box mgr)) name updated)
  mgr)

;; ──────────────────────────────
;; Light/Dark theme presets
;; ──────────────────────────────
(define (make-light-theme)
  (make-ui-theme #:background "#ffffff"
                 #:foreground "#1e1e2e"
                 #:accent "#1a73e8"
                 #:muted "#9ca3af"
                 #:error "#dc2626"
                 #:warning "#f59e0b"
                 #:success "#16a34a"))

(define (make-dark-theme)
  (make-ui-theme #:background "#1e1e2e"
                 #:foreground "#cdd6f4"
                 #:accent "#89b4fa"
                 #:muted "#6c7086"
                 #:error "#f38ba8"
                 #:warning "#fab387"
                 #:success "#a6e3a1"))
