#lang racket/base
;; tools/exec-context.rkt — Tool execution context
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(require racket/contract
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../runtime/settings.rkt" q-settings?)
         (only-in "../util/capability.rkt"
                  current-session-capabilities
                  canonical-capabilities-snapshot)
         (only-in "../tools/permission-gate.rkt" permission-config? make-strict-permission-config))

(provide exec-context
         exec-context?
         exec-context-working-directory
         exec-context-cancellation-token
         exec-context-event-publisher
         exec-context-runtime-settings
         exec-context-call-id
         exec-context-session-metadata
         exec-context-progress-callback
         exec-context-permission-config
         exec-context-bytes-written
         exec-context-browser-service
         exec-context-capabilities
         (contract-out [make-exec-context
                        (->* ()
                             ;; path-string? covers both path? and string?
                             (#:working-directory (or/c path-string? #f)
                                                  #:cancellation-token (or/c cancellation-token? #f)
                                                  #:event-publisher (or/c procedure? #f)
                                                  #:runtime-settings (or/c hash? q-settings? #f)
                                                  #:call-id (or/c string? #f)
                                                  #:session-metadata (or/c hash? #f)
                                                  #:progress-callback (or/c procedure? #f)
                                                  #:permission-config permission-config?
                                                  #:browser-service (or/c any/c #f)
                                                  #:capabilities any/c)
                             exec-context?)]))

;; ============================================================
;; Execution context
;; ============================================================

(struct exec-context
        (working-directory cancellation-token
                           event-publisher
                           runtime-settings
                           call-id
                           session-metadata
                           progress-callback
                           permission-config
                           bytes-written
                           browser-service ; (or/c secure-browser-service? #f) — F7
                           capabilities)
  #:transparent
  #:guard (lambda (working-directory
                   cancellation-token
                   event-publisher
                   runtime-settings
                   call-id
                   session-metadata
                   progress-callback
                   permission-config
                   bytes-written
                   browser-service
                   capabilities
                   _type-name)
            (values working-directory
                    cancellation-token
                    event-publisher
                    runtime-settings
                    call-id
                    session-metadata
                    progress-callback
                    permission-config
                    bytes-written
                    browser-service
                    (canonical-capabilities-snapshot capabilities))))

;; v0.99.66 (W1, finding #1 CRITICAL): the default permission-config is
;; now make-strict-permission-config instead of #f.  This closes the
;; bypass where a #f config caused scheduler-execution to short-circuit
;; the gate and run dangerous tools without any enforcement.
(define (make-exec-context #:working-directory [working-directory (current-directory)]
                           #:cancellation-token [cancellation-token #f]
                           #:event-publisher [event-publisher #f]
                           #:runtime-settings [runtime-settings #f]
                           #:call-id [call-id ""]
                           #:session-metadata [session-metadata #f]
                           #:progress-callback [progress-callback #f]
                           #:permission-config [permission-config (make-strict-permission-config)]
                           #:browser-service [browser-service #f]
                           #:capabilities [capabilities (current-session-capabilities)])
  (exec-context working-directory
                cancellation-token
                event-publisher
                runtime-settings
                call-id
                session-metadata
                progress-callback
                permission-config
                (box 0)
                browser-service
                (canonical-capabilities-snapshot capabilities)))
