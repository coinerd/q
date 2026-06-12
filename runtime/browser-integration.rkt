#lang racket/base

;; runtime/browser-integration.rkt — Browser adapter auto-detection and wiring (AXIS1-F01)
;; STABILITY: internal
;;
;; Extracted from runtime/agent-session.rkt (v0.98.16 W1) for single-responsibility
;; separation. Handles browser adapter lifecycle:
;;   1. Auto-detect Playwright sidecar availability
;;   2. Fall back to mock adapter when sidecar unavailable
;;   3. Build browser-adapter from mock adapter primitives
;;   4. Configure browser settings from session config
;;   5. Initialize browser service for a session

(require racket/runtime-path
         (only-in "../browser/service.rkt" make-secure-browser-service current-browser-service)
         (only-in "../browser/settings.rkt"
                  load-browser-settings
                  browser-settings?
                  browser-settings-enabled?
                  browser-settings-sidecar-path
                  browser-settings-sidecar-timeout-ms
                  browser-settings-headless?)
         (only-in "../browser/adapters/mock.rkt"
                  make-mock-adapter
                  mock-open
                  mock-close
                  mock-navigate
                  mock-observe
                  mock-act
                  mock-screenshot)
         (only-in "../browser/adapters/playwright-sidecar.rkt" make-playwright-adapter)
         (only-in "../browser/adapter.rkt" make-browser-adapter))

(provide try-make-playwright-adapter
         make-mock-browser-adapter
         configure-session-browser!)

;; Default sidecar path, resolved relative to q/runtime/browser-integration.rkt.
(define-runtime-path default-playwright-sidecar-path "../sidecars/playwright/q-playwright-sidecar.js")

;; try-make-playwright-adapter : browser-settings? -> (values adapter? #f)
;; Try to create a Playwright adapter; fall back to mock.
(define (try-make-playwright-adapter browser-cfg)
  (define sidecar-path (browser-settings-sidecar-path browser-cfg))
  (define timeout-ms (browser-settings-sidecar-timeout-ms browser-cfg))
  (define headless? (browser-settings-headless? browser-cfg))
  (define sidecar-js
    (if sidecar-path
        (string->path sidecar-path)
        default-playwright-sidecar-path))
  (cond
    [(and (file-exists? sidecar-js) (find-executable-path "node"))
     (with-handlers ([exn:fail? (lambda (e)
                                  (log-warning "Playwright adapter failed, falling back to mock: ~a"
                                               (exn-message e))
                                  (values (make-mock-browser-adapter) #f))])
       (define adapter
         (make-playwright-adapter (path->string sidecar-js)
                                  #:timeout-ms timeout-ms
                                  #:headless? headless?))
       (values adapter #f))]
    [else
     (log-warning "Browser: Node.js or sidecar not found, using mock adapter")
     (values (make-mock-browser-adapter) #f)]))

;; make-mock-browser-adapter : -> adapter?
;; Build a mock adapter inline (avoids circular dependency issues)
(define (make-mock-browser-adapter)
  (define mock (make-mock-adapter))
  (make-browser-adapter #:open (lambda (sid target) (mock-open mock target #f))
                        #:close (lambda (sid) (mock-close mock sid))
                        #:navigate (lambda (sid url) (mock-navigate mock sid url))
                        #:observe (lambda (sid selector) (mock-observe mock sid selector))
                        #:act (lambda (sid action) (mock-act mock sid action))
                        #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))

;; configure-session-browser! : q-settings? event-bus? -> void?
;; Initialize browser service for a session if browser is enabled in settings.
;; Pass the result of (config-settings cfg) as the first argument.
(define (configure-session-browser! q-cfg event-bus)
  (when q-cfg
    (define browser-cfg (load-browser-settings q-cfg))
    (when (browser-settings-enabled? browser-cfg)
      (define-values (adapter _sidecar-state) (try-make-playwright-adapter browser-cfg))
      (define browser-svc
        (make-secure-browser-service adapter #:settings browser-cfg #:event-bus event-bus))
      (current-browser-service browser-svc))))
