#lang racket/base

;; runtime/session-switch.rkt — Atomic Session Switching Lifecycle (#704-#707)
;;
;; Manages the full lifecycle of switching between sessions:
;;   - #704: Teardown old extension state (emit session-shutdown)
;;   - #705: Rebind extensions to new session (fresh extension-ctx)
;;   - #706: Session start event with reason (new/resume/fork)
;;   - #707: Parent feature combining all of the above
;;
;; Lifecycle: before-switch → teardown old → create new → rebind → emit start

(require racket/contract
         racket/match
         "../agent/event-bus.rkt"
         "../extensions/hooks.rkt"
         "../extensions/context.rkt"
         "../util/protocol-types.rkt")

(provide
 ;; #704: Teardown
 teardown-session-extensions!
 emit-session-shutdown!

 ;; #705: Rebind
 rebind-extensions!
 make-rebind-ctx

 ;; #706: Session start with reason
 emit-session-start!
 session-start-reason?

 ;; #707: Full lifecycle
 switch-session!
 switch-result
 switch-result?
 switch-result-old-session-id
 switch-result-new-session-id
 switch-result-reason)

;; ============================================================
;; Structs
;; ============================================================

;; Result of a session switch
(struct switch-result
  (old-session-id   ; string or #f
   new-session-id   ; string
   reason)          ; 'new, 'resume, or 'fork
  #:transparent)

;; Valid session start reasons
(define (session-start-reason? v)
  (and (symbol? v)
       (or (eq? v 'new)
           (eq? v 'resume)
           (eq? v 'fork))))

;; ============================================================
;; #704: Teardown old extension state
;; ============================================================

;; Emit session-shutdown event and dispatch hook.
;; Extensions use this to clean up connections, flush logs, deregister tools.
(define (emit-session-shutdown! bus session-id extension-registry
                                #:duration [duration 0])
  ;; Dispatch 'session-shutdown hook
  (when extension-registry
    (dispatch-hooks 'session-shutdown
                    (hasheq 'session-id session-id
                            'duration duration)
                    extension-registry))
  ;; Publish session.shutdown event
  (when bus
    (define evt (make-event "session.shutdown"
                            (current-seconds)
                            session-id
                            #f
                            (hasheq 'session-id session-id
                                    'duration duration)))
    (publish! bus evt)))

;; Full teardown: emit shutdown + close old session extensions.
(define (teardown-session-extensions! old-session-id
                                       bus
                                       extension-registry
                                       #:duration [duration 0])
  (emit-session-shutdown! bus old-session-id extension-registry
                          #:duration duration))

;; ============================================================
;; #705: Rebind extensions to new session
;; ============================================================

;; Create a fresh extension-ctx for the new session.
(define (make-rebind-ctx new-session-id
                          new-session-dir
                          bus
                          extension-registry
                          #:model-name [model-name #f]
                          #:working-directory [working-directory #f])
  (make-extension-ctx #:session-id new-session-id
                       #:session-dir new-session-dir
                       #:event-bus bus
                       #:extension-registry extension-registry
                       #:model-name model-name
                       #:working-directory working-directory))

;; Rebind all extensions to a new session context.
;; Returns the new extension-ctx.
(define (rebind-extensions! new-session-id
                             new-session-dir
                             bus
                             extension-registry
                             #:model-name [model-name #f]
                             #:working-directory [working-directory #f])
  (define ctx (make-rebind-ctx new-session-id
                                new-session-dir
                                bus
                                extension-registry
                                #:model-name model-name
                                #:working-directory working-directory))
  ;; Dispatch 'session-rebind hook so extensions can reconnect
  (when extension-registry
    (dispatch-hooks 'session-rebind
                    (hasheq 'session-id new-session-id
                            'session-dir new-session-dir
                            'ctx ctx)
                    extension-registry))
  ctx)

;; ============================================================
;; #706: Session start event with reason
;; ============================================================

;; Emit session.start event with reason (new/resume/fork).
(define (emit-session-start! bus session-id reason
                              #:previous-session-id [prev-id #f]
                              #:session-dir [session-dir #f])
  (unless (session-start-reason? reason)
    (raise-argument-error 'emit-session-start! "session-start-reason?" reason))
  (when bus
    (define payload
      (hasheq 'session-id session-id
              'reason reason
              'previous-session-id prev-id
              'session-dir session-dir))
    (publish! bus (make-event "session.start"
                              (current-seconds)
                              session-id
                              #f
                              payload))))

;; ============================================================
;; #707: Full atomic session switch lifecycle
;; ============================================================

;; Execute full session switch: teardown old → rebind → start new.
(define (switch-session! #:old-session-id [old-session-id #f]
                          #:old-bus [old-bus #f]
                          #:old-extension-registry [old-ext-reg #f]
                          #:old-duration [old-duration 0]
                          #:new-session-id new-session-id
                          #:new-session-dir new-session-dir
                          #:new-bus new-bus
                          #:new-extension-registry [new-ext-reg #f]
                          #:reason reason
                          #:model-name [model-name #f]
                          #:working-directory [working-directory #f])
  (unless (session-start-reason? reason)
    (raise-argument-error 'switch-session! "session-start-reason?" reason))

  ;; Step 1: Teardown old session (if any)
  (when old-session-id
    (teardown-session-extensions! old-session-id old-bus old-ext-reg
                                   #:duration old-duration))

  ;; Step 2: Rebind extensions to new session
  (define ctx (rebind-extensions! new-session-id
                                   new-session-dir
                                   new-bus
                                   new-ext-reg
                                   #:model-name model-name
                                   #:working-directory working-directory))

  ;; Step 3: Emit session.start with reason
  (emit-session-start! new-bus new-session-id reason
                        #:previous-session-id old-session-id
                        #:session-dir new-session-dir)

  ;; Return result
  (switch-result old-session-id new-session-id reason))
