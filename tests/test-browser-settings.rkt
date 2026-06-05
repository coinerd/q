#lang racket

;; tests/test-browser-settings.rkt — Browser settings tests
;;
;; Tests for browser/settings.rkt: construction, defaults, parameter.

(require rackunit
         "../browser/settings.rkt"
         "../runtime/settings.rkt")

;; ---------------------------------------------------------------------------
;; Default settings
;; ---------------------------------------------------------------------------

(test-case "default-browser-settings constructs"
  (define s (default-browser-settings))
  (check-true (browser-settings? s)))

(test-case "default: enabled? is #f"
  (check-false (browser-settings-enabled? (default-browser-settings))))

(test-case "default: allowed-schemes includes https and http"
  (define s (default-browser-settings))
  (check-not-false (member "https" (browser-settings-allowed-schemes s)))
  (check-not-false (member "http" (browser-settings-allowed-schemes s))))

(test-case "default: allowed-domains is empty (all public)"
  (check-equal? (browser-settings-allowed-domains (default-browser-settings)) '()))

(test-case "default: localhost ports include 3000, 8080, 5173"
  (define ports (browser-settings-allowed-localhost-ports (default-browser-settings)))
  (check-not-false (member 3000 ports))
  (check-not-false (member 8080 ports))
  (check-not-false (member 5173 ports)))

(test-case "default: blocked-private-networks? is #t"
  (check-true (browser-settings-blocked-private-networks? (default-browser-settings))))

(test-case "default: blocked-paths includes /admin, /debug, /internal"
  (define paths (browser-settings-blocked-paths (default-browser-settings)))
  (check-not-false (member "/admin" paths))
  (check-not-false (member "/debug" paths))
  (check-not-false (member "/internal" paths)))

(test-case "default: max-sessions is 3"
  (check-equal? (browser-settings-max-sessions (default-browser-settings)) 3))

(test-case "default: max-actions-per-session is 100"
  (check-equal? (browser-settings-max-actions-per-session (default-browser-settings)) 100))

(test-case "default: default-timeout-ms is 30000"
  (check-equal? (browser-settings-default-timeout-ms (default-browser-settings)) 30000))

(test-case "default: screenshot-max-bytes is 524288"
  (check-equal? (browser-settings-screenshot-max-bytes (default-browser-settings)) 524288))

(test-case "default: sidecar-path is #f"
  (check-false (browser-settings-sidecar-path (default-browser-settings))))

(test-case "default: sidecar-timeout-ms is 60000"
  (check-equal? (browser-settings-sidecar-timeout-ms (default-browser-settings)) 60000))

(test-case "default: profile-kind is 'ephemeral"
  (check-equal? (browser-settings-profile-kind (default-browser-settings)) 'ephemeral))

;; ---------------------------------------------------------------------------
;; Custom settings
;; ---------------------------------------------------------------------------

(test-case "custom settings construction"
  (define s
    (browser-settings #t
                      '("https")
                      '("example.com")
                      '(443)
                      #t
                      '()
                      5
                      200
                      60000
                      1048576
                      "/usr/local/bin/node"
                      120000
                      'persistent
                      #f))
  (check-true (browser-settings-enabled? s))
  (check-equal? (browser-settings-max-sessions s) 5)
  (check-equal? (browser-settings-profile-kind s) 'persistent)
  (check-false (browser-settings-headless? s)))

(test-case "load-browser-settings treats JSON null sidecar-path as auto-discover #f"
  (define settings
    (make-minimal-settings #:overrides (hasheq 'browser (hasheq 'enabled? #t 'sidecar-path 'null))))
  (define browser-cfg (load-browser-settings settings))
  (check-true (browser-settings-enabled? browser-cfg))
  (check-false (browser-settings-sidecar-path browser-cfg)))

(test-case "load-browser-settings reads headless? with default #t and override #f"
  (check-true (browser-settings-headless? (load-browser-settings (make-minimal-settings))))
  (define settings
    (make-minimal-settings #:overrides (hasheq 'browser (hasheq 'enabled? #t 'headless? #f))))
  (check-false (browser-settings-headless? (load-browser-settings settings))))

;; ---------------------------------------------------------------------------
;; current-browser-settings parameter
;; ---------------------------------------------------------------------------

(test-case "current-browser-settings parameter defaults"
  (check-true (browser-settings? (current-browser-settings)))
  (check-false (browser-settings-enabled? (current-browser-settings))))

(test-case "current-browser-settings parameterize"
  (parameterize
      ([current-browser-settings
        (browser-settings #t '("https") '() '() #t '() 1 10 5000 1024 #f 30000 'ephemeral #t)])
    (check-true (browser-settings-enabled? (current-browser-settings)))
    (check-equal? (browser-settings-max-sessions (current-browser-settings)) 1)))
