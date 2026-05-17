#lang racket/base
;; tools/exec-context.rkt — Tool execution context
;; Extracted from tools/tool.rkt (v0.30.8 W0)
;; STABILITY: stable

(require racket/contract
         (only-in "../util/cancellation.rkt" cancellation-token?)
         (only-in "../runtime/settings.rkt" q-settings?)
         (only-in "../tools/permission-gate.rkt" permission-config?))

(provide (struct-out exec-context)
         exec-context?
         (contract-out [make-exec-context
                        (->* ()
                             (#:working-directory (or/c path-string?
                                                        #f) ; path-string? covers path? and string?
                                                  #:cancellation-token (or/c cancellation-token? #f)
                                                  #:event-publisher (or/c procedure? #f)
                                                  #:runtime-settings (or/c hash? q-settings? #f)
                                                  #:call-id (or/c string? #f)
                                                  #:session-metadata (or/c hash? #f)
                                                  #:progress-callback (or/c procedure? #f)
                                                  #:permission-config (or/c permission-config? #f))
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
                           bytes-written) ; G3.4: permission gate config or #f
  #:transparent)

(define (make-exec-context #:working-directory [working-directory (current-directory)]
                           #:cancellation-token [cancellation-token #f]
                           #:event-publisher [event-publisher #f]
                           #:runtime-settings [runtime-settings #f]
                           #:call-id [call-id ""]
                           #:session-metadata [session-metadata #f]
                           #:progress-callback [progress-callback #f]
                           #:permission-config [permission-config #f])
  (exec-context working-directory
                cancellation-token
                event-publisher
                runtime-settings
                call-id
                session-metadata
                progress-callback
                permission-config
                (box 0)))
