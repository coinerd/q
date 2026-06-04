#lang racket

;; browser/settings.rkt — Browser-specific settings
;;
;; Struct for browser configuration, default values,
;; and integration with the q runtime settings system.

(require racket/contract)

(provide
 (struct-out browser-settings)
 default-browser-settings
 current-browser-settings)

;; ---------------------------------------------------------------------------
;; Browser settings struct
;; ---------------------------------------------------------------------------

(struct browser-settings
  (enabled?                  ; boolean? — master switch
   allowed-schemes           ; (listof string?)
   allowed-domains           ; (listof string?) — empty = all public
   allowed-localhost-ports   ; (listof exact-nonnegative-integer?)
   blocked-private-networks? ; boolean?
   blocked-paths             ; (listof string?)
   max-sessions              ; exact-nonnegative-integer?
   max-actions-per-session   ; exact-nonnegative-integer?
   default-timeout-ms        ; exact-nonnegative-integer?
   screenshot-max-bytes      ; exact-nonnegative-integer?
   sidecar-path              ; (or/c string? #f)
   sidecar-timeout-ms        ; exact-nonnegative-integer?
   profile-kind)             ; symbol? — 'ephemeral | 'persistent
  #:transparent)

;; ---------------------------------------------------------------------------
;; Defaults
;; ---------------------------------------------------------------------------

(define (default-browser-settings)
  (browser-settings
   #f                         ; enabled? — disabled by default
   '("https" "http")          ; allowed-schemes
   '()                        ; allowed-domains (all public)
   '(3000 8080 5173)          ; allowed-localhost-ports
   #t                         ; blocked-private-networks?
   '("/admin" "/debug" "/internal") ; blocked-paths
   3                          ; max-sessions
   100                        ; max-actions-per-session
   30000                      ; default-timeout-ms (30s)
   524288                     ; screenshot-max-bytes (512KB)
   #f                         ; sidecar-path (auto-discover)
   60000                      ; sidecar-timeout-ms (60s)
   'ephemeral))               ; profile-kind

;; ---------------------------------------------------------------------------
;; Parameter for current session
;; ---------------------------------------------------------------------------

(define current-browser-settings (make-parameter (default-browser-settings)))
