#lang racket

;; browser/service.rkt — SecureBrowserService
;;
;; Security chokepoint composing policy + session + adapter + audit.
;; All browser operations flow through this service.

(require racket/match
         "types.rkt"
         "policy.rkt"
         "session.rkt"
         "audit.rkt"
         "settings.rkt"
         "adapters/mock.rkt"
         "../util/error/errors.rkt")

(provide
 secure-browser-service?
 make-secure-browser-service
 browser-open!
 browser-observe!
 browser-act!
 browser-close!
 browser-navigate!
 browser-screenshot!
 current-browser-service)

;; ---------------------------------------------------------------------------
;; Service struct
;; ---------------------------------------------------------------------------

(struct secure-browser-service
  (adapter           ; mock-browser-adapter
   session-manager   ; browser-session-manager
   policy-settings   ; browser-policy-settings
   event-bus         ; (or/c #f any)
   artifact-dir)     ; (or/c string? #f)
  #:transparent)

(define (make-secure-browser-service adapter
                                      #:settings [settings (default-browser-settings)]
                                      #:event-bus [event-bus #f]
                                      #:artifact-dir [artifact-dir #f])
  (define policy (browser-settings->policy-settings settings))
  (define mgr (make-browser-session-manager
               #:max-sessions (browser-settings-max-sessions settings)
               #:max-actions (browser-settings-max-actions-per-session settings)))
  (secure-browser-service adapter mgr policy event-bus artifact-dir))

;; ---------------------------------------------------------------------------
;; Convert browser-settings → browser-policy-settings
;; ---------------------------------------------------------------------------

(define (browser-settings->policy-settings s)
  (make-browser-policy-settings
   #:allowed-schemes (browser-settings-allowed-schemes s)
   #:allowed-domains (browser-settings-allowed-domains s)
   #:allowed-localhost-ports (browser-settings-allowed-localhost-ports s)
   #:blocked-private-networks? (browser-settings-blocked-private-networks? s)
   #:blocked-paths (browser-settings-blocked-paths s)))

;; ---------------------------------------------------------------------------
;; Open session
;; ---------------------------------------------------------------------------

(define (browser-open! svc url #:options [options #f])
  (define adapter (secure-browser-service-adapter svc))
  (define policy (secure-browser-service-policy-settings svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define bus (secure-browser-service-event-bus svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  ;; 1. Policy check
  (define-values (ok? reason) (validate-browser-url url policy))
  (unless ok?
    (raise-browser-error
     (format "browser policy blocked: ~a" reason)
     'url-blocked
     (hash 'url url 'reason reason)))

  ;; 2. Create session
  (define session-id (format "bs-~a-~a" (current-milliseconds) (random 100000)))
  (define info (browser-session-info session-id 'active
                                     (current-milliseconds) (current-milliseconds)
                                     'ephemeral (or artifact-dir "/tmp")))
  (browser-session-manager-create! mgr session-id info #:artifact-dir artifact-dir)

  ;; 3. Adapter open
  (define result
    (with-browser-error-wrap
     (mock-open adapter url options)))

  ;; 4. Audit log
  (log-browser-action! session-id 'open result artifact-dir)

  ;; 5. Emit event
  (emit-browser-event! bus 'browser.session-opened session-id (hash 'url url))

  (values session-id result))

;; ---------------------------------------------------------------------------
;; Observe
;; ---------------------------------------------------------------------------

(define (browser-observe! svc session-id #:selector [selector #f])
  (define adapter (secure-browser-service-adapter svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define bus (secure-browser-service-event-bus svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  (check-session! mgr session-id)
  (browser-session-manager-record-action! mgr session-id)

  (define result
    (with-browser-error-wrap
     (mock-observe adapter session-id selector)))

  (log-browser-action! session-id 'observe result artifact-dir)
  (emit-browser-event! bus 'browser.action-completed session-id (hash 'action "observe"))
  result)

;; ---------------------------------------------------------------------------
;; Act
;; ---------------------------------------------------------------------------

(define (browser-act! svc session-id action)
  (define adapter (secure-browser-service-adapter svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define bus (secure-browser-service-event-bus svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  (check-session! mgr session-id)
  (browser-session-manager-record-action! mgr session-id)

  (define result
    (with-browser-error-wrap
     (mock-act adapter session-id action)))

  (log-browser-action! session-id 'act result artifact-dir)
  (emit-browser-event! bus 'browser.action-completed session-id (hash 'action "act"))
  result)

;; ---------------------------------------------------------------------------
;; Navigate
;; ---------------------------------------------------------------------------

(define (browser-navigate! svc session-id url)
  (define adapter (secure-browser-service-adapter svc))
  (define policy (secure-browser-service-policy-settings svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define bus (secure-browser-service-event-bus svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  ;; Policy check for new URL
  (define-values (ok? reason) (validate-browser-url url policy))
  (unless ok?
    (raise-browser-error
     (format "browser policy blocked: ~a" reason)
     'url-blocked
     (hash 'url url 'reason reason)))

  (check-session! mgr session-id)
  (browser-session-manager-record-action! mgr session-id)

  (define result
    (with-browser-error-wrap
     (mock-navigate adapter session-id url)))

  (log-browser-action! session-id 'navigate result artifact-dir)
  (emit-browser-event! bus 'browser.action-completed session-id (hash 'action "navigate" 'url url))
  result)

;; ---------------------------------------------------------------------------
;; Screenshot
;; ---------------------------------------------------------------------------

(define (browser-screenshot! svc session-id #:selector [selector #f] #:format [fmt "png"])
  (define adapter (secure-browser-service-adapter svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  (check-session! mgr session-id)
  (browser-session-manager-record-action! mgr session-id)

  (define result
    (with-browser-error-wrap
     (mock-screenshot adapter session-id selector fmt)))

  (log-browser-action! session-id 'screenshot result artifact-dir)
  result)

;; ---------------------------------------------------------------------------
;; Close session
;; ---------------------------------------------------------------------------

(define (browser-close! svc session-id)
  (define adapter (secure-browser-service-adapter svc))
  (define mgr (secure-browser-service-session-manager svc))
  (define bus (secure-browser-service-event-bus svc))
  (define artifact-dir (secure-browser-service-artifact-dir svc))

  (with-browser-error-wrap
   (mock-close adapter session-id))

  (browser-session-manager-destroy! mgr session-id)
  (log-browser-action! session-id 'close 'ok artifact-dir)
  (emit-browser-event! bus 'browser.session-closed session-id (hash)))

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(define (check-session! mgr session-id)
  (unless (browser-session-manager-get mgr session-id)
    (raise-browser-error
     (format "session ~a not found" session-id)
     'session-expired
     (hash 'session-id session-id))))

(define-syntax-rule (with-browser-error-wrap expr)
  (with-handlers
    ([exn:fail?
      (lambda (e)
        (if (q-browser-error? e)
            (raise e)
            (raise-browser-error
             (format "adapter error: ~a" (exn-message e))
             'adapter-error
             (hash 'original-message (exn-message e)))))])
    expr))

(define current-browser-service (make-parameter #f))
