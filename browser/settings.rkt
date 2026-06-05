#lang racket

;; browser/settings.rkt — Browser-specific settings
;;
;; Struct for browser configuration, default values,
;; and integration with the q runtime settings system.

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
         headless?) ; boolean? — launch browser without visible GUI
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
                    #t)) ; headless? — safe default

;; ---------------------------------------------------------------------------
;; Parameter for current session
;; ---------------------------------------------------------------------------

(define current-browser-settings (make-parameter (default-browser-settings)))

;; ---------------------------------------------------------------------------
;; Load from q runtime settings
;; ---------------------------------------------------------------------------

(define (json-null->false v)
  (if (eq? v 'null) #f v))

(define (load-browser-settings q-settings)
  (define bs (setting-ref q-settings 'browser (hash)))
  (browser-settings (hash-ref bs 'enabled? #f)
                    (hash-ref bs 'allowed-schemes '("https" "http"))
                    (hash-ref bs 'allowed-domains '())
                    (hash-ref bs 'allowed-localhost-ports '(3000 8080 5173))
                    (hash-ref bs 'blocked-private-networks? #t)
                    (hash-ref bs 'blocked-paths '("/admin" "/debug" "/internal"))
                    (hash-ref bs 'max-sessions 3)
                    (hash-ref bs 'max-actions-per-session 100)
                    (hash-ref bs 'default-timeout-ms 30000)
                    (hash-ref bs 'screenshot-max-bytes 524288)
                    (json-null->false (hash-ref bs 'sidecar-path #f))
                    (hash-ref bs 'sidecar-timeout-ms 60000)
                    (hash-ref bs 'profile-kind 'ephemeral)
                    (hash-ref bs 'headless? #t)))
