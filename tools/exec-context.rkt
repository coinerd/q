#lang racket/base
;; tools/exec-context.rkt — Tool execution context
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(require racket/contract
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../runtime/settings.rkt" q-settings?)
         (only-in "../tools/permission-gate.rkt" permission-config?))

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
                                                  #:permission-config (or/c permission-config? #f)
                                                  #:browser-service (or/c any/c #f))
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
                           bytes-written ; G3.4: permission gate config or #f
                           browser-service) ; (or/c secure-browser-service? #f) — F7
  #:transparent)

(define (make-exec-context #:working-directory [working-directory (current-directory)]
                           #:cancellation-token [cancellation-token #f]
                           #:event-publisher [event-publisher #f]
                           #:runtime-settings [runtime-settings #f]
                           #:call-id [call-id ""]
                           #:session-metadata [session-metadata #f]
                           #:progress-callback [progress-callback #f]
                           #:permission-config [permission-config #f]
                           #:browser-service [browser-service #f])
  (exec-context working-directory
                cancellation-token
                event-publisher
                runtime-settings
                call-id
                session-metadata
                progress-callback
                permission-config
                (box 0)
                browser-service))
