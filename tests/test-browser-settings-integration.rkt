#lang racket

;; @speed slow
;; @suite default

;; tests/test-browser-settings-integration.rkt — F5: load-browser-settings

(require rackunit
         "../browser/settings.rkt"
         "../runtime/settings.rkt")

;; ---------------------------------------------------------------------------
;; load-browser-settings from q runtime config
;; ---------------------------------------------------------------------------

(test-case "load-browser-settings: defaults when no browser config"
  (define q-cfg (q-settings (hash) (hash) (hash)))
  (define bs (load-browser-settings q-cfg))
  (check-true (browser-settings? bs))
  (check-false (browser-settings-enabled? bs))
  (check-equal? (browser-settings-allowed-schemes bs) '("https" "http"))
  (check-equal? (browser-settings-max-sessions bs) 3))

(test-case "load-browser-settings: enabled? from config"
  (define q-cfg (q-settings (hash) (hash) (hasheq 'browser (hasheq 'enabled? #t))))
  (define bs (load-browser-settings q-cfg))
  (check-true (browser-settings? bs))
  (check-true (browser-settings-enabled? bs)))

(test-case "load-browser-settings: returns browser-settings? struct"
  (define q-cfg (q-settings (hash) (hash) (hash)))
  (define bs (load-browser-settings q-cfg))
  (check-true (browser-settings? bs))
  (check-false (browser-settings-enabled? bs))
  (check-equal? (browser-settings-allowed-schemes bs) '("https" "http"))
  (check-equal? (browser-settings-allowed-domains bs) '())
  (check-true (browser-settings-blocked-private-networks? bs))
  (check-equal? (browser-settings-max-sessions bs) 3)
  (check-equal? (browser-settings-max-actions-per-session bs) 100)
  (check-equal? (browser-settings-default-timeout-ms bs) 30000)
  (check-equal? (browser-settings-screenshot-max-bytes bs) 524288)
  (check-false (browser-settings-sidecar-path bs))
  (check-equal? (browser-settings-sidecar-timeout-ms bs) 60000)
  (check-eq? (browser-settings-profile-kind bs) 'ephemeral))

(test-case "load-browser-settings vs default-browser-settings parity"
  (define q-cfg (q-settings (hash) (hash) (hash)))
  (define loaded (load-browser-settings q-cfg))
  (define default (default-browser-settings))
  (check-equal? (browser-settings-enabled? loaded) (browser-settings-enabled? default))
  (check-equal? (browser-settings-allowed-schemes loaded) (browser-settings-allowed-schemes default))
  (check-equal? (browser-settings-allowed-domains loaded) (browser-settings-allowed-domains default))
  (check-equal? (browser-settings-blocked-private-networks? loaded) (browser-settings-blocked-private-networks? default))
  (check-equal? (browser-settings-max-sessions loaded) (browser-settings-max-sessions default))
  (check-equal? (browser-settings-profile-kind loaded) (browser-settings-profile-kind default)))

(test-case "settings.rkt no longer imports racket/contract"
  (define content (file->string "../browser/settings.rkt"))
  (check-false (regexp-match? #rx"racket/contract" content))
  (check-not-false (regexp-match? #rx"runtime/settings" content)))

(test-case "mock.rkt no longer imports event-structs/base"
  (define content (file->string "../browser/adapters/mock.rkt"))
  (check-false (regexp-match? #rx"event-structs/base" content)))
