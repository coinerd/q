#lang racket

;; browser/settings.rkt — Browser-specific settings
;;
;; Struct for browser configuration, default values,
;; and integration with the q runtime settings system.
;;
;; ARCH-01: This module imports from runtime/settings.rkt (upward coupling).
;; Intended migration: extract `setting-ref` to a util/ module in v0.99.x.

(require "../runtime/settings.rkt")

(provide (struct-out browser-settings)
         default-browser-settings
         current-browser-settings
         load-browser-settings)

;; ---------------------------------------------------------------------------
;; Browser settings struct
;; ---------------------------------------------------------------------------

(struct browser-settings
        (enabled? ; boolean? — master switch
         allowed-schemes ; (listof string?)
         allowed-domains ; (listof string?) — empty = all public
         allowed-localhost-ports ; (listof exact-nonnegative-integer?)
         blocked-private-networks? ; boolean?
         blocked-paths ; (listof string?)
         max-sessions ; exact-nonnegative-integer?
         max-actions-per-session ; exact-nonnegative-integer?
         default-timeout-ms ; exact-nonnegative-integer?
         screenshot-max-bytes ; exact-nonnegative-integer?
         sidecar-path ; (or/c string? #f)
         sidecar-timeout-ms ; exact-nonnegative-integer?
         profile-kind ; symbol? — 'ephemeral | 'persistent
         headless? ; boolean? — launch browser without visible GUI
         vision-enabled? ; boolean? — feature flag for multimodal screenshots
         vision-detail ; (or/c "auto" "low" "high") — OpenAI detail level
         vision-ephemeral-turns) ; exact-positive-integer? — turns to keep images
  #:transparent)

;; ---------------------------------------------------------------------------
;; Defaults
;; ---------------------------------------------------------------------------

(define (default-browser-settings)
  (browser-settings #f ; enabled? — disabled by default
                    '("https" "http") ; allowed-schemes
                    '() ; allowed-domains (all public)
                    '(3000 8080 5173) ; allowed-localhost-ports
                    #t ; blocked-private-networks?
                    '("/admin" "/debug" "/internal") ; blocked-paths
                    3 ; max-sessions
                    100 ; max-actions-per-session
                    30000 ; default-timeout-ms (30s)
                    524288 ; screenshot-max-bytes (512KB)
                    #f ; sidecar-path (auto-discover)
                    60000 ; sidecar-timeout-ms (60s)
                    'ephemeral ; profile-kind
                    #t ; headless? — safe default
                    #f ; vision-enabled? (default OFF)
                    "auto" ; vision-detail
                    5)) ; vision-ephemeral-turns

;; ---------------------------------------------------------------------------
;; Parameter for current session
;; ---------------------------------------------------------------------------

(define current-browser-settings (make-parameter (default-browser-settings)))

;; ---------------------------------------------------------------------------
;; Load from q runtime settings
;; ---------------------------------------------------------------------------

;; L3: Convert JSON null to #f for optional string fields
(define (json-null->false v)
  (if (eq? v 'null) #f v))

(define (load-browser-settings q-settings)
  ;; M4: Derive defaults from default-browser-settings to prevent drift
  (define defaults (default-browser-settings))
  (define bs (setting-ref q-settings 'browser (hash)))
  (browser-settings
   (hash-ref bs 'enabled? (browser-settings-enabled? defaults))
   (hash-ref bs 'allowed-schemes (browser-settings-allowed-schemes defaults))
   (hash-ref bs 'allowed-domains (browser-settings-allowed-domains defaults))
   (hash-ref bs 'allowed-localhost-ports (browser-settings-allowed-localhost-ports defaults))
   (hash-ref bs 'blocked-private-networks? (browser-settings-blocked-private-networks? defaults))
   (hash-ref bs 'blocked-paths (browser-settings-blocked-paths defaults))
   (hash-ref bs 'max-sessions (browser-settings-max-sessions defaults))
   (hash-ref bs 'max-actions-per-session (browser-settings-max-actions-per-session defaults))
   (hash-ref bs 'default-timeout-ms (browser-settings-default-timeout-ms defaults))
   (hash-ref bs 'screenshot-max-bytes (browser-settings-screenshot-max-bytes defaults))
   (json-null->false (hash-ref bs 'sidecar-path (browser-settings-sidecar-path defaults)))
   (hash-ref bs 'sidecar-timeout-ms (browser-settings-sidecar-timeout-ms defaults))
   (let ([pk (hash-ref bs 'profile-kind (browser-settings-profile-kind defaults))])
     (if (string? pk)
         (string->symbol pk)
         pk))
   (hash-ref bs 'headless? (browser-settings-headless? defaults))
   (hash-ref bs 'vision-enabled? (browser-settings-vision-enabled? defaults))
   (hash-ref bs 'vision-detail (browser-settings-vision-detail defaults))
   (hash-ref bs 'vision-ephemeral-turns (browser-settings-vision-ephemeral-turns defaults))))
