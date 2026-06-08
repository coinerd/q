#lang racket

;; @speed fast
;; @suite default

;; tests/test-browser-lifecycle.rkt — F7/F8: exec-context + agent-session lifecycle

(require rackunit
         "../tools/exec-context.rkt"
         "../tools/permission-gate.rkt"
         "../browser/service.rkt"
         "../browser/settings.rkt"
         "../browser/adapter.rkt"
         "../browser/adapters/mock.rkt"
         "../runtime/settings.rkt")

;; ---------------------------------------------------------------------------
;; F7: exec-context browser-service field
;; ---------------------------------------------------------------------------

(test-case "exec-context has browser-service field"
  (define ctx (make-exec-context))
  (check-false (exec-context-browser-service ctx)))

(test-case "exec-context browser-service can be set"
  (define mock (make-mock-adapter))
  (define adapter
    (make-browser-adapter
     #:open (lambda (sid target) (mock-open mock target #f))
     #:close (lambda (sid) (mock-close mock sid))
     #:navigate (lambda (sid url) (mock-navigate mock sid url))
     #:observe (lambda (sid selector) (mock-observe mock sid selector))
     #:act (lambda (sid action) (mock-act mock sid action))
     #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))
  (define svc (make-secure-browser-service adapter))
  (define ctx (make-exec-context #:browser-service svc))
  (check-true (secure-browser-service? (exec-context-browser-service ctx))))

(test-case "exec-context with all fields including browser-service"
  (define ctx (make-exec-context
               #:working-directory "/tmp"
               #:call-id "test-123"
               #:permission-config (make-default-permission-config)
               #:browser-service #f))
  (check-equal? (exec-context-call-id ctx) "test-123")
  (check-false (exec-context-browser-service ctx)))

;; ---------------------------------------------------------------------------
;; F8: current-browser-service parameter lifecycle
;; ---------------------------------------------------------------------------

(test-case "current-browser-service default is #f"
  (check-false (current-browser-service)))

(test-case "current-browser-service can be set and cleared"
  (define mock (make-mock-adapter))
  (define adapter
    (make-browser-adapter
     #:open (lambda (sid target) (mock-open mock target #f))
     #:close (lambda (sid) (mock-close mock sid))
     #:navigate (lambda (sid url) (mock-navigate mock sid url))
     #:observe (lambda (sid selector) (mock-observe mock sid selector))
     #:act (lambda (sid action) (mock-act mock sid action))
     #:screenshot (lambda (sid sel fp) (mock-screenshot mock sid sel "png"))))
  (define svc (make-secure-browser-service adapter))
  (parameterize ([current-browser-service svc])
    (check-true (secure-browser-service? (current-browser-service))))
  ;; After parameterize exits, back to default
  (check-false (current-browser-service)))

(test-case "load-browser-settings from config returns browser-settings?"
  (define bs (load-browser-settings
              ;; Create a minimal q-settings with browser config
              (q-settings (hash) (hash)
                          (hasheq 'browser (hasheq 'enabled? #t)))))
  (check-true (browser-settings? bs))
  (check-true (browser-settings-enabled? bs)))
