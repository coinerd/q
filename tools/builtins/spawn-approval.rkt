#lang racket/base

;; Digest-bound dangerous-spawn authorization.
;; With no active interactive broker lease, dangerous work is denied
;; unconditionally. A publisher is telemetry, never approval authority.

(require "../../tools/tool.rkt"
         "spawn-execution-plan.rkt"
         (only-in "../../runtime/approval/broker.rkt"
                  current-approval-channel
                  register-approval-request-for-channel!
                  approval-await-grant
                  cancel-approval-request!))

(provide request-spawn-approval
         request-batch-spawn-approval)

(define (publish-safely publisher event-type payload)
  (when publisher
    (with-handlers ([exn:fail? (lambda (_) (void))])
      (publisher event-type payload))))

(define (request-plan-approval plan exec-ctx)
  (unless (spawn-execution-plan? plan)
    (raise-argument-error 'request-plan-approval "spawn-execution-plan?" plan))
  (define publisher (and exec-ctx (exec-context-event-publisher exec-ctx)))
  (define channel (current-approval-channel))
  (define digest (spawn-execution-plan-digest plan))
  (define presentation-digest (spawn-execution-plan-presentation-digest plan))
  (define presentation (spawn-execution-plan-presentation plan))
  (define base-payload
    (hash-set* presentation
               'commitment-digest
               digest
               'presentation-digest
               presentation-digest
               'plan-kind
               (symbol->string (spawn-execution-plan-kind plan))))
  (cond
    [(not channel)
     (publish-safely publisher
                     "mas.spawn-approval-terminal"
                     (hash-set base-payload 'terminal-status "denied-headless"))
     #f]
    [else
     (define request-id (register-approval-request-for-channel! channel digest base-payload))
     (cond
       [(or (not request-id) (not publisher))
        (when request-id
          (cancel-approval-request! request-id))
        #f]
       [else
        (define terminal-status "cancelled")
        (dynamic-wind void
                      (lambda ()
                        (publisher "mas.spawn-approval-requested"
                                   (hasheq 'request-id
                                           request-id
                                           'commitment-digest
                                           digest
                                           'presentation-digest
                                           presentation-digest))
                        (define-values (outcome grant) (approval-await-grant request-id digest))
                        (set! terminal-status (symbol->string outcome))
                        (and (eq? outcome 'approved) grant))
                      (lambda ()
                        (cancel-approval-request! request-id)
                        (publish-safely publisher
                                        "mas.spawn-approval-terminal"
                                        (hasheq 'request-id
                                                request-id
                                                'commitment-digest
                                                digest
                                                'presentation-digest
                                                presentation-digest
                                                'terminal-status
                                                terminal-status))))])]))

(define (request-spawn-approval plan exec-ctx)
  (request-plan-approval plan exec-ctx))

(define (request-batch-spawn-approval plan exec-ctx)
  (request-plan-approval plan exec-ctx))
